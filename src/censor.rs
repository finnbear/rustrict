use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::feature_cell::FeatureCell;
use crate::mtch::*;
use crate::trie::*;
use crate::{is_whitespace, Type};
use crate::{Map, Set};
use lazy_static::lazy_static;
use std::iter::Filter;
use std::mem;
use std::str::Chars;
use unicode_normalization::{Decompositions, Recompositions, UnicodeNormalization};

lazy_static! {
    static ref TRIE: FeatureCell<Trie> = FeatureCell::new(
        include_str!("profanity.csv")
            .lines()
            .skip(1)
            .map(|line| {
                let mut split = line.split(',');
                (
                    split.next().unwrap(),
                    Type::from_weights(
                        &[0; Type::WEIGHT_COUNT].map(|_| split.next().unwrap().parse().unwrap()),
                    ),
                )
            })
            .chain(
                include_str!("safe.txt")
                    .lines()
                    .filter(|line| !line.is_empty() && !line.starts_with('#'))
                    .map(|line| { (line, Type::SAFE) })
            )
            .chain(
                include_str!("false_positives.txt")
                    .lines()
                    .filter(|line| !line.is_empty())
                    .map(|line| { (line, Type::NONE) })
            )
            .collect()
    );
    static ref REPLACEMENTS: Map<char, &'static str> = include_str!("replacements.csv")
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let comma = line.find(',').unwrap();
            (line[..comma].chars().next().unwrap(), &line[comma + 1..])
        })
        .collect();
    static ref BANNED: FeatureCell<Set<char>> = FeatureCell::new(
        include_str!("banned_chars.txt")
            .lines()
            .filter(|s| s.starts_with("U+"))
            .map(|s| {
                u32::from_str_radix(&s[2..], 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap()
            })
            .collect()
    );
}

/// Censor is a flexible profanity filter that can analyze and/or censor arbitrary text.
///
/// You can also make use of `Censor` via traits `CensorStr` and `CensorIter`, which allow inline
/// checking and censoring of `&str` and `Iterator<Item = char>` respectively.
pub struct Censor<I: Iterator<Item = char>> {
    /// Options
    ignore_false_positives: bool,
    ignore_self_censoring: bool,
    censor_first_character_threshold: Type,
    //preserve_accents: bool,
    censor_replacement: char,
    censor_threshold: Type,
    /// Where potential matches are kept between calls to Self::next.
    matches: Set<Match>,
    /// Where potential matches are temporarily shuffled. Only allocate this once.
    matches_tmp: Set<Match>,
    /// Whether the last character can be considered a separator.
    separate: bool,
    /// The last position matched against.
    last_pos: usize,
    /// An accumulation of the different types of inappropriateness.
    typ: Type,
    /// Counters (mainly for spam detection).
    uppercase: u8,
    repetitions: u8,
    last: Option<char>,
    gibberish: u8,
    replacements: u8,
    /// How many instances of censor replacement in the raw text?
    self_censoring: u8,
    /// Is the input completely safe.
    safe: bool,
    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    match_ptrs: usize,
    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    total_matches: usize,
    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    total_match_characters: usize,
    /// Where matches are kept after they are complete but may be cancelled due to false positives.
    pending_commit: Vec<Match>,
    /// A buffer of the input that stores unconfirmed characters (may need to censor before flushing).
    /// This is so the censored output is unaffected by the subsequent iterator machinery.
    buffer: BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>>,
    /// Whether already appended a space at the end.
    space_appended: bool,
    /// Whether all processing of characters has completed.
    done: bool,
}

impl<'a> Censor<Chars<'a>> {
    /// Creates a `Censor` from a `&str`, ready to censor or analyze it.
    pub fn from_str(s: &'a str) -> Self {
        Self::new(s.chars())
    }
}

impl<I: Iterator<Item = char>> Censor<I> {
    /// Allocates a new `Censor` for analyzing and/or censoring text.
    pub fn new(text: I) -> Self {
        Self {
            // Default options
            ignore_false_positives: false,
            ignore_self_censoring: false,
            censor_first_character_threshold: Type::OFFENSIVE & Type::SEVERE,
            //preserve_accents: false,
            censor_replacement: '*',
            censor_threshold: Default::default(),
            // The beginning of the sequence is a separator.
            separate: true,
            // Nothing was detected yet.
            typ: Type::NONE,
            uppercase: 0,
            repetitions: 0,
            last: None,
            gibberish: 0,
            replacements: 0,
            self_censoring: 0,
            safe: false,
            space_appended: false,
            done: false,
            last_pos: usize::MAX,
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            match_ptrs: 0,
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            total_matches: 0,
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            total_match_characters: 0,
            matches: Set::default(),
            matches_tmp: Set::default(),
            pending_commit: Vec::new(),
            buffer: Self::buffer_from(text),
        }
    }

    fn buffer_from(
        text: I,
    ) -> BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>> {
        // Detects if a char isn't a diacritical mark (accent) or banned, such that such characters may be
        // filtered on that basis.
        fn filter_char(c: &char) -> bool {
            use finl_unicode::categories::{CharacterCategories, MinorCategory};
            let category = c.get_minor_category();
            let nok = matches!(
                category,
                MinorCategory::Cn | MinorCategory::Co | MinorCategory::Mn
            );

            !(nok || BANNED.contains(c))
        }

        BufferProxyIterator::new(
            text
                // The following three transformers are to ignore diacritical marks.
                .nfd()
                .filter(filter_char as fn(&char) -> bool)
                .nfc(),
        )
    }

    /// Resets the `Censor` with new text. Does not change any configured options.
    /// This avoids reallocation of internal buffers on the heap.
    ///
    /// TODO: This is untested.
    #[cfg(feature = "reset_censor")]
    pub fn reset(&mut self, text: I) {
        self.separate = true;
        self.typ = Type::NONE;
        self.uppercase = 0;
        self.last = None;
        self.repetitions = 0;
        self.gibberish = 0;
        self.replacements = 0;
        self.self_censoring = 0;
        self.space_appended = false;
        self.done = false;
        self.last_pos = usize::MAX;
        #[cfg(any(feature = "find_false_positives", feature = "trace"))]
        self.total_matches = 0;
        #[cfg(any(feature = "find_false_positives", feature = "trace"))]
        self.total_match_characters = 0;
        self.matches.clear();
        self.matches_tmp.clear();
        self.pending_commit.clear();
        self.buffer = Self::buffer_from(text);
    }

    /// Selects a threshold to apply while censoring. Only words that meet or exceed the threshold
    /// are censored.
    ///
    /// At present, [`Type::SPAM`] cannot be censored.
    ///
    /// The default is [`Type::Inappropriate`].
    pub fn with_censor_threshold(&mut self, censor_threshold: Type) -> &mut Self {
        self.censor_threshold = censor_threshold;
        self
    }

    /// Censor words like "sh*t" in "push it," which heavily increases false positives, but
    /// slightly decreases false negatives.
    ///
    /// The default is `false`.
    pub fn with_ignore_false_positives(&mut self, ignore_false_positives: bool) -> &mut Self {
        self.ignore_false_positives = ignore_false_positives;
        self
    }

    /// Do not count instances of censor replacement in the input text as possible profanity.
    ///
    /// If `false`, the input `"****"` will be assumed to be profane since if censor replacement is
    /// set to `'*'`. This can help in cases like `"mother******"` where, if the user hadn't self
    /// censored, the censored version would have been `"m***********"`.
    ///
    /// At present, only affects analysis and not censoring.
    ///
    /// The default is `false`.
    pub fn with_ignore_self_censoring(&mut self, ignore_self_censoring: bool) -> &mut Self {
        self.ignore_self_censoring = ignore_self_censoring;
        self
    }

    /// Censor all characters e.g. "xxxx," instead of all but the first e.g. "fxxx," if the word
    /// meets this threshold.
    ///
    /// The default is `false`.
    pub fn with_censor_first_character_threshold(
        &mut self,
        censor_first_character_threshold: Type,
    ) -> &mut Self {
        self.censor_first_character_threshold = censor_first_character_threshold;
        self
    }

    /*
    /// Preserve diacritics/accents, at the cost of detecting accented words such as f̸̪͇͘ų̷̖̽c̸͙̎̚k̶͚̗͛.
    ///
    /// The default is false.
    pub fn with_preserve_accents(&mut self, preserve_accents: bool) {
        self.preserve_accents = preserve_accents;
    }
     */

    /// Sets the character used to censor detected words.
    ///
    /// The default is `'*'`.
    pub fn with_censor_replacement(&mut self, censor_replacement: char) -> &mut Self {
        self.censor_replacement = censor_replacement;
        self
    }

    /// Useful for processing sub-slices of profanity.
    #[cfg(feature = "find_false_positives")]
    pub fn with_separate(&mut self, separate: bool) -> &mut Self {
        self.separate = separate;
        self
    }

    /// Produces a censored string. If called, it must be the first form of processing. It
    /// entirely consumes and censors the input characters.
    ///
    /// # Unfortunate Side Effects
    ///
    /// All diacritical marks (accents) are removed by the current implementation. This is subject
    /// to change, as a better implementation would make this optional.
    ///
    /// # Panics
    ///
    /// If called after analyze or a previous call to censor (except if reset is called in between).
    pub fn censor(&mut self) -> String {
        assert!(
            !self.buffer.index().is_some(),
            "censor must be called before any other form of processing"
        );
        self.collect()
    }

    /// Fully analyzes a the input characters, to determine the type of inappropriateness present, if any.
    ///
    /// The return value can be introspected with `Type::is`.
    pub fn analyze(&mut self) -> Type {
        self.ensure_done();
        self.analysis()
    }

    /// See the documentation of censor and analyze.
    pub fn censor_and_analyze(&mut self) -> (String, Type) {
        // It is important that censor is called first, so that the input is processed.
        let censored = self.censor();
        // After that, analysis is ready to call.
        (censored, self.analysis())
    }

    /// Converts internal weights to a `Type`.
    fn analysis(&self) -> Type {
        self.typ | self.safe_self_censoring_and_spam_detection()
    }

    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    pub fn match_ptrs(&self) -> usize {
        self.match_ptrs
    }

    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    pub fn total_matches(&self) -> usize {
        self.total_matches
    }

    #[cfg(any(feature = "find_false_positives", feature = "trace"))]
    pub fn total_match_characters(&self) -> usize {
        self.total_match_characters
    }

    fn ensure_done(&mut self) {
        if !self.done {
            for _ in self {}
        }
    }

    fn safe_self_censoring_and_spam_detection(&self) -> Type {
        let safe = if self.safe { Type::SAFE } else { Type::NONE };

        if self.last_pos < 6 {
            // Short strings consisting of a single acronym are problematic percentage-wise.
            return safe;
        }

        // Total opportunities for spam and self censoring. A bias is added so that a few words in a
        // relatively short string won't create massive percentages.
        let total = self.last_pos.saturating_add(6).min(u16::MAX as usize) as u16;

        // Total spam.
        let spam = self
            .uppercase
            .max(self.repetitions)
            .max(self.gibberish / 2)
            .max(self.replacements) as u16;

        // Calculate percents.
        let percent_spam = 100 * spam / total;
        let percent_self_censoring = 100 * self.self_censoring as u16 / total;

        // Assess amount of spam.
        let spam = if percent_spam >= 70 && self.last_pos >= 20 {
            Type::SPAM & Type::SEVERE
        } else if percent_spam >= 50 && self.last_pos >= 10 {
            Type::SPAM & Type::MODERATE
        } else if percent_spam >= 30 {
            Type::SPAM & Type::MILD
        } else {
            Type::NONE
        };

        // Assess amount of self-censoring.
        let self_censoring = if !self.ignore_self_censoring && percent_self_censoring > 20 {
            Type::PROFANE & Type::MILD
        } else {
            Type::NONE
        };

        safe | spam | self_censoring
    }
}

impl<I: Iterator<Item = char>> Iterator for Censor<I> {
    type Item = char;

    /// Retrieves the next (potentially censored) character.
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(raw_c) = self.buffer.next().or_else(|| {
            if self.space_appended {
                None
            } else {
                self.space_appended = true;
                Some(' ')
            }
        }) {
            if !self.space_appended && raw_c != '!' && raw_c != '.' && raw_c != '?' {
                // The input is not over yet, so any previous notion of safety is irrelevant.
                self.safe = false;
            }

            let pos = self.buffer.index();

            self.uppercase = self.uppercase.saturating_add(raw_c.is_uppercase() as u8);
            /*
            // Very old whitelist (allows a ton of abuse):
            let skippable = match c {
                ' ' | '~' | '-' | '−' | '_' | '.' | '!' | '?' | ',' | '*' | '"' | '\'' | '\n' | '\r'
                | '\t' => true,
                _ => false,
            };

            // More recent whitelist (still allows abuse like f^u^c^k):
            let skippable = raw_c.is_punctuation()
                || raw_c.is_separator()
                || is_whitespace(raw_c)
                || matches!(raw_c, '(' | ')');
            // Use a blacklist instead:
             */
            let skippable = !raw_c.is_alphanumeric() || is_whitespace(raw_c);
            let replacement = REPLACEMENTS.get(&raw_c);

            #[cfg(feature = "trace")]
            println!(
                "Read '{}', skippable={}, replacing with={:?}",
                raw_c, skippable, replacement
            );

            if (!self.separate || self.last == Some(self.censor_replacement))
                && raw_c == self.censor_replacement
            {
                // Censor replacement found but not beginning of word.
                self.self_censoring = self.self_censoring.saturating_add(1);
            }

            if let Some(last) = self.last {
                if raw_c == last {
                    self.repetitions = self.repetitions.saturating_add(1);
                }

                // Characters on the home-row of a QWERTY keyboard.
                fn is_gibberish(c: char) -> bool {
                    matches!(c, 'a' | 's' | 'd' | 'f' | 'j' | 'k' | 'l' | ';')
                }

                // Single gibberish characters don't count. Must have been preceded by another gibberish character.
                if is_gibberish(raw_c) && is_gibberish(last) {
                    self.gibberish = self.gibberish.saturating_add(1);
                }
            }

            if let Some(pos) = pos {
                // Must special-case all skippable, non-replaced characters that may start
                // a profanity, so that these profanities are detected.
                //
                // Not adding a match is mainly an optimization.
                if !(skippable && replacement.is_none() && !matches!(raw_c, ' ' | '_' | '🖕')) {
                    let begin_camel_case_word = raw_c.is_ascii_uppercase()
                        && self.last.map(|c| !c.is_ascii_uppercase()).unwrap_or(false);

                    // Seed a new match for every character read.
                    self.matches.insert(Match {
                        node: &TRIE.root,
                        start: pos, // will immediately be incremented if match is kept.
                        end: usize::MAX, // sentinel.
                        last: 0 as char, // sentinel.
                        begin_separate: self.separate || begin_camel_case_word,
                        end_separate: false, // unknown at this time.
                        spaces: 0,
                        skipped: 0,
                        replacements: 0,
                        low_confidence_replacements: 0,
                    });
                }
            }

            self.separate = skippable;

            if self.separate {
                for pending in self.pending_commit.iter_mut() {
                    if pending.end == self.last_pos {
                        pending.end_separate = true;
                    }
                }
            }

            let mut drain_start: Option<usize> = None;
            let mut safety_end = usize::MAX;
            let mut replacement_counted = false;
            let raw_c_lower = raw_c.to_lowercase().next().unwrap();

            mem::swap(&mut self.matches, &mut self.matches_tmp);
            for c in replacement
                .unwrap_or(&&*raw_c.encode_utf8(&mut [0; 4]))
                .chars()
            {
                // This replacement (uppercase to lower case) raises absolutely zero suspicion.
                let benign_replacement = c == raw_c_lower;

                // This counts as a replacement, mainly for spam detection purposes.
                let countable_replacement = !(replacement_counted
                    || benign_replacement
                    || raw_c.is_ascii_alphabetic()
                    || (raw_c.is_ascii_digit()
                        && self.last.map(|l| l.is_ascii_digit()).unwrap_or(false)));

                if countable_replacement {
                    self.replacements = self.replacements.saturating_add(1);
                    replacement_counted = true;
                }

                #[cfg(feature = "trace")]
                println!(
                    " - Replacement '{}', benign={}, countable={}",
                    c, benign_replacement, countable_replacement
                );

                // These separators don't invalidate a false-positive match.
                //
                // -
                // half-right =/= frig
                //
                // '
                // invalidating false positives in cases like didn't (it where ( is a space.
                // also, so "i'm fine" matches "im fine" for safety purposes.
                let ignore_sep = matches!(c, '-' | '\'' | '\n' | '\r');

                for m in self.matches_tmp.iter() {
                    let m = m.clone();

                    safety_end = safety_end.min(m.start);

                    #[cfg(feature = "trace")]
                    println!(
                        "  - Consider match \"{}\" with spaces={}",
                        m.node.trace, m.spaces
                    );

                    if (skippable || c == m.last) && m.start != pos.unwrap_or(0) {
                        // Undo remove.
                        #[cfg(feature = "trace")]
                        println!("undo remove \"{}\" where last={}, node last={:?} and initial spaces={}", m.node.trace, m.last, m.node.last, m.spaces);

                        // Here, '.' is primarily for allowing ellipsis ("...") as a form of
                        // space.
                        // ( and ) are for ignoring appositive phrases.
                        // Checking node.last is to collapse multiple spaces into one, to avoid
                        let new_space = matches!(c, ' ' | '.' | ',' | ':' | ';' | '…' | '(' | ')')
                            // && skippable
                            && m.node.last != Some(' ');
                        // && !ignore_sep;

                        let new_skip = skippable && !ignore_sep;
                        let new_replacement = !benign_replacement && !self.separate;
                        let new_low_confidence_replacement =
                            !benign_replacement && raw_c.is_ascii_digit();

                        let undo_m = Match {
                            spaces: m.spaces.saturating_add(new_space as u8),
                            skipped: m.skipped.saturating_add(new_skip as u8),
                            replacements: m.replacements.saturating_add(new_replacement as u8),
                            low_confidence_replacements: m
                                .low_confidence_replacements
                                .saturating_add(new_low_confidence_replacement as u8),
                            ..m
                        };
                        if let Some(existing) = self.matches.get(&undo_m) {
                            let replacement = existing.combine(&undo_m);
                            self.matches.replace(replacement);
                        } else {
                            self.matches.insert(undo_m);
                        }
                    }

                    if let Some(next) = m.node.children.get(&c) {
                        let next_m = Match {
                            node: next,
                            spaces: m
                                .spaces
                                .saturating_add((c != raw_c && self.separate && c != '\'') as u8),
                            replacements: m
                                .replacements
                                .saturating_add((!benign_replacement && !self.separate) as u8),
                            low_confidence_replacements: m
                                .low_confidence_replacements
                                .saturating_add(
                                    (!benign_replacement && raw_c.is_ascii_digit()) as u8,
                                ),
                            last: c,
                            ..m
                        };

                        #[cfg(feature = "trace")]
                        println!(
                            "     - Next is \"{}\", with spaces={}, replacements = {}",
                            next.trace, next_m.spaces, next_m.replacements
                        );

                        if next.word {
                            if next_m.node.typ.is(Type::SAFE)
                                && next_m.start == 0
                                && next_m.spaces == 0
                                && next_m.skipped == 0
                                && !self.ignore_false_positives
                            {
                                // Everything in the input until now is safe.
                                #[cfg(feature = "trace")]
                                println!("found safe word: {}", next_m.node.trace);
                                self.safe = true;
                            }

                            #[cfg(feature = "trace")]
                            if !next_m.node.typ.is(Type::ANY) {
                                if self.ignore_false_positives {
                                    print!("ignoring");
                                } else {
                                    print!("found");
                                }
                                println!(
                                    " false positive \"{}\", spaces={}, skipped={}, replacements={}",
                                    next_m.node.trace, next_m.spaces, next_m.skipped, next_m.replacements
                                );
                            }

                            if next_m.node.typ.is(Type::ANY) {
                                self.pending_commit.push(Match {
                                    end: pos.unwrap(),
                                    ..next_m
                                });
                            } else if next_m.spaces == 0
                                && next_m.skipped == 0
                                && next_m.replacements == 0
                                && !self.ignore_false_positives
                            {
                                // Is false positive, so invalidate internal matches.
                                #[cfg(feature = "trace")]
                                println!("Found false positive {}", next_m.node.trace);
                                drain_start = Some(
                                    drain_start
                                        .map(|start| start.min(next_m.start))
                                        .unwrap_or(next_m.start),
                                );
                            }
                        }

                        if let Some(existing) = self.matches.get(&next_m) {
                            let replacement = existing.combine(&next_m);
                            self.matches.replace(replacement);
                        } else {
                            self.matches.insert(next_m);
                        }
                    }
                }
            }
            self.matches_tmp.clear();
            self.last = Some(raw_c);
            if let Some(pos) = pos {
                self.last_pos = pos;
            }

            let typ = &mut self.typ;
            let spy = &mut self.buffer;
            let censor_threshold = self.censor_threshold;
            let censor_first_character_threshold = self.censor_first_character_threshold;
            let censor_replacement = self.censor_replacement;
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            let first_match_ptr = &mut self.match_ptrs;
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            let total_matches = &mut self.total_matches;
            #[cfg(any(feature = "find_false_positives", feature = "trace"))]
            let total_match_characters = &mut self.total_match_characters;

            self.pending_commit.retain(|pending| {
                #[cfg(feature = "trace")]
                println!("Consider whether to cancel pending commit {} with start={} against drain_start={:?}", pending.node.trace, pending.start, drain_start);

                // Cancel due to false positive.
                if let Some(start) = drain_start {
                    if pending.start >= start {
                        #[cfg(feature = "trace")]
                        println!("Cancelled {}", pending.node.trace);
                        return false;
                    }
                }

                // Can pre-commit due to lack of false positive matches.
                if pending.end < safety_end {
                    if pending.commit(
                        typ,
                        spy,
                        censor_threshold,
                        censor_first_character_threshold,
                        censor_replacement,
                    ) {
                        #[cfg(any(feature = "find_false_positives", feature = "trace"))]
                        {
                            *first_match_ptr ^= pending.node as *const _ as usize;
                            *total_matches += 1;
                            *total_match_characters += pending.end - pending.start;
                        }
                    }
                    return false;
                }

                // At this point, don't know whether this match will be committed or cancelled, so
                // return.
                true
            });

            // Yield one character if possible.
            if let Some(spy_next_index) = self.buffer.spy_next_index() {
                // This covers all in-flight matches.
                let mut safe_until = spy_next_index < safety_end;

                // This covers all pending commit matches.
                for pending in &self.pending_commit {
                    if pending.start <= spy_next_index {
                        safe_until = false;
                        break;
                    }
                }
                if safe_until {
                    return self.buffer.spy_next();
                }
            }
        }

        let residual = mem::take(&mut self.pending_commit);
        #[cfg(feature = "trace")]
        if !residual.is_empty() {
            println!("{} residuals", residual.len());
        }
        for pending in residual {
            if pending.commit(
                &mut self.typ,
                &mut self.buffer,
                self.censor_threshold,
                self.censor_first_character_threshold,
                self.censor_replacement,
            ) {
                #[cfg(any(feature = "find_false_positives", feature = "trace"))]
                {
                    self.match_ptrs ^= pending.node as *const _ as usize;
                    self.total_matches += 1;
                    self.total_match_characters += pending.end - pending.start;
                }
            }
        }

        if let Some(c) = self.buffer.spy_next() {
            return Some(c);
        }

        self.done = true;

        None
    }
}

/// CensorStr makes it easy to sanitize a `String` or `&str` by calling `.censor()`.
pub trait CensorStr: Sized {
    /// The output is a newly allocated, censored string.
    fn censor(self) -> String;

    /// Returns `true` if the text is inappropriate.
    fn is_inappropriate(self) -> bool {
        self.is(Type::INAPPROPRIATE)
    }

    /// Returns `true` if text meets the provided threshold.
    fn is(self, threshold: Type) -> bool;

    /// Returns `true` if text **does not** meet the provided threshold.
    fn isnt(self, threshold: Type) -> bool {
        !self.is(threshold)
    }
}

impl CensorStr for &str {
    fn censor(self) -> String {
        if should_skip_censor(self) {
            self.to_owned()
        } else {
            Censor::new(self.chars()).censor()
        }
    }

    fn is(self, threshold: Type) -> bool {
        Censor::from_str(self).analyze().is(threshold)
    }
}

/// CensorIter makes it easy to sanitize an arbitrary `Iterator<Item=char>` by calling `.censor()`.
pub trait CensorIter {
    type Iterator: Iterator<Item = char>;

    /// Iteratively censor characters, yielding (except accents) those that are not inappropriate, and replacing
    /// those that are with `'*'`.
    fn censor(self) -> Self::Iterator;
}

impl<I: Iterator<Item = char> + Clone> CensorIter for I {
    type Iterator = Censor<I>;

    /// Censors text, keeping (except accents) those that are not inappropriate, and replacing
    /// those that are with `'*'`.
    fn censor(self) -> Self::Iterator {
        Censor::new(self)
    }
}

/// Returns true if censoring won't work but will likely damage the input (e.g. by removing
/// diacritics). Will consider the entire input.
pub(crate) fn should_skip_censor(string: &str) -> bool {
    let mut some_special = false;
    for c in string.chars() {
        use finl_unicode::categories::CharacterCategories;
        // Devanagari is compromised by normalization and diacritic removal.
        if ('\u{0900}'..='\u{097F}').contains(&c) {
            some_special = true;
        } else if !(c.is_whitespace() || c.is_separator()) {
            return false;
        }
    }
    some_special
}

/// Adds a word, with the given type. The type can be `Type::SAFE`, or a combination of `Type::PROFANE`,
/// `Type::Sexual`, `Type::Offensive`, `Type::Mean`, `Type::Mild`, `Type::Moderate`, and `Type::Severe`,
/// but NOT both (can't be safe and unsafe).
///
/// It is recommended to use all lower-case, which will match both cases. Upper-case characters will
/// only match upper-case.
///
/// # Warning
///
/// Any profanity words added this way will not support false positives. For example, if you add the word
/// "field," you can expect "cornfield" to be detected as well, unless you call `add_word("cornfield", Type::None)`.
///
/// # Safety
///
/// This must not be called when the crate is being used in any other way. It is best to call this
/// from the main thread, near the beginning of the program.
#[cfg(feature = "customize")]
pub unsafe fn add_word(word: &str, typ: Type) {
    TRIE.get_mut().add(word, typ, true);
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]

    extern crate test;
    use crate::censor::should_skip_censor;
    use crate::{Censor, CensorIter, CensorStr, Type};
    use bitflags::_core::ops::Not;
    use rand::prelude::ThreadRng;
    use rand::{thread_rng, Rng};
    use serial_test::serial;
    use std::fs::File;
    use std::io::BufReader;
    use std::time::{Duration, Instant};
    use test::Bencher;

    #[test]
    #[serial]
    fn short_replacement() {
        "99".isnt(Type::PROFANE);
        "900".isnt(Type::PROFANE);
        "kkk".is(Type::OFFENSIVE);
    }

    #[test]
    #[serial]
    fn unicode_whitespace() {
        assert!("fu\u{1160}ck".is(Type::PROFANE));
        assert!(!"fu\u{1161}ck".is(Type::PROFANE));
    }

    #[test]
    #[serial]
    fn unicode_abuse() {
        let mut rng = thread_rng();

        fn random_string(rng: &mut ThreadRng, len: usize) -> String {
            rng.sample_iter::<char, _>(rand::distributions::Standard)
                .take(len)
                .collect()
        }

        for _ in 0..10 {
            let input = random_string(&mut rng, 100);
            let censored = input.censor();

            // Most of the characters should be removed for being invalid.
            assert!(censored.len() < input.len() / 2);

            println!("{} -> {}", input, censored);
        }
    }

    #[allow(dead_code)]
    fn find_detection(text: &str) {
        let holistic = Censor::from_str(text).analyze();

        if holistic & Type::SPAM.not() != Type::NONE {
            println!("{}", text);

            // There was some non-spam detection.
            let mut start = 0;
            let mut end = text.chars().count();

            while start < end
                && Censor::new(text.chars().skip(start).take(end - start))
                    .analyze()
                    .is(Type::ANY)
            {
                start += 1;
            }
            start = start.saturating_sub(1);
            while start < end
                && Censor::new(text.chars().skip(start).take(end - start))
                    .analyze()
                    .is(Type::ANY)
            {
                end -= 1;
            }
            end += 1;
            for _ in 0..start {
                print!("-");
            }
            for _ in start..end {
                print!("^");
            }
            print!(" ");
            println!(
                "(\"{}\" is {:?})",
                text.chars()
                    .skip(start)
                    .take(end - start)
                    .collect::<String>(),
                holistic
            );
        } else {
            println!("{} ({:?})", text, holistic);
        }
    }

    #[test]
    #[serial]
    fn issue_1() {
        // https://github.com/finnbear/rustrict/issues/1#issuecomment-1024426326
        assert!("I could say I miss you but it’s not the truth".isnt(Type::ANY));
    }

    #[test]
    #[serial]
    fn curated() {
        let mut cases: Vec<(&str, bool, Option<bool>)> = vec![("", false, Some(false))];
        cases.extend(
            include_str!("test_positive.txt")
                .split('\n')
                .filter(|l| !l.is_empty())
                .map(|l| (l, true, Some(false))),
        );
        cases.extend(
            include_str!("test_negative.txt")
                .split('\n')
                .filter(|l| !l.is_empty())
                .map(|l| (l, false, None)),
        );
        cases.extend(
            include_str!("safe.txt")
                .split('\n')
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .map(|l| (l, false, Some(true))),
        );
        cases.extend(
            include_str!("test_safe.txt")
                .split('\n')
                .filter(|l| !l.is_empty())
                .map(|l| (l, false, Some(true))),
        );

        for (case, any_truth, safe_truth) in cases {
            /*
            #[cfg(debug_assertions)]
            println!("Case: \"{}\"", case);
             */

            let typ = Censor::from_str(case).analyze();
            let any = typ.is(Type::ANY);
            let safe = typ.is(Type::SAFE);

            //let (censored, analysis) = Censor::from_str(case).with_censor_threshold(Type::ANY).censor_and_analyze();
            //println!("\"{}\" -> \"{}\" ({}, {})", case, censored, prediction, analysis.is(Type::ANY));

            if any != any_truth {
                find_detection(case);
                panic!("FAIL: Predicted {:?} for {}", typ, case);
            }
            if !any {
                // None of the current test cases contain any abusive Unicode characters.
                assert_eq!(case, case.censor());
            }
            if let Some(safe_truth) = safe_truth {
                if safe != safe_truth {
                    panic!("FAIL: Predicted safe={} for {}", safe, case);
                }
            }
        }
    }

    #[test]
    #[serial]
    fn censor() {
        let censored = Censor::from_str("HELLO fučk Shit nigga WORLD!")
            .with_censor_replacement('#')
            .censor();

        assert_eq!(censored, "HELLO f### S### ##### WORLD!");

        // Minor mean-ness is not considered inappropriate
        assert_eq!("fcking coward".censor(), "f***** coward");

        let censored = Censor::from_str("卍")
            .with_censor_first_character_threshold(Type::NONE)
            .censor();

        assert_eq!(censored, "*");
    }

    #[test]
    #[serial]
    fn bidirectional() {
        // Censoring removes direction overrides, so that the text output is the text that was analyzed.
        assert_eq!("an toidi", "an \u{202e}toidi".censor());
    }

    #[test]
    #[serial]
    fn analyze() {
        let analysis = Censor::from_str("HELLO fuck shit WORLD!").analyze();

        assert_ne!(analysis, Type::NONE);
        assert!(analysis.is(Type::INAPPROPRIATE));
        assert!(analysis.is(Type::PROFANE));
        assert!(analysis.isnt(Type::SEXUAL & Type::SEVERE));
        assert!(analysis.isnt(Type::OFFENSIVE));
        assert!(analysis.isnt(Type::MEAN));
    }

    /// This exists purely to ensure all the APIs keep compiling.
    #[test]
    #[serial]
    fn apis() {
        "abcd".censor();
        String::from("abcd").censor();
        let _ = "abcd".chars().censor().collect::<String>();
        let (_, _) = Censor::new("abcd".chars())
            .with_censor_replacement('?')
            .censor_and_analyze();
        let mut censor = Censor::from_str("abcd");
        let _ = censor.censor();
        let _ = censor.analyze();
        let (_, _) = Censor::from_str("HELLO crap WORLD!").censor_and_analyze();
    }

    #[test]
    #[serial]
    fn levels() {
        assert!("poo".is(Type::PROFANE & Type::MILD));
        assert!("poo".is(Type::PROFANE & Type::MILD_OR_HIGHER));
        assert!("poo".isnt(Type::PROFANE & Type::MODERATE));
        assert!("poo".isnt(Type::PROFANE & Type::MODERATE_OR_HIGHER));
        assert!("poo".isnt(Type::PROFANE & Type::SEVERE));
        assert!("arse".is(Type::PROFANE & Type::MODERATE));
        assert!("arse".is(Type::PROFANE & Type::MILD_OR_HIGHER));
        assert!("arse".is(Type::PROFANE & Type::MODERATE_OR_HIGHER));
        assert!("arse".isnt(Type::PROFANE & Type::MILD));
        assert!("arse".isnt(Type::PROFANE & Type::SEVERE));
        assert!("i hope you die".is(Type::MEAN & Type::SEVERE));
        assert!("i hope you die".is(Type::MEAN & Type::MILD_OR_HIGHER));
        assert!("i hope you die".is(Type::MEAN & Type::MODERATE_OR_HIGHER));
        assert!("i hope you die".isnt(Type::MEAN & Type::MILD));
        assert!("i hope you die".isnt(Type::MEAN & Type::MODERATE));
        assert!("You said your mother only smiled on her TV show".isnt(
            Type::PROFANE
                | Type::OFFENSIVE
                | Type::SEXUAL & Type::MODERATE_OR_HIGHER
                | Type::MEAN & Type::SEVERE
        ));
    }

    #[test]
    #[serial]
    #[cfg(not(debug_assertions))]
    fn accuracy() {
        fn rustrict(s: &str) -> bool {
            s.is(Type::ANY)
        }

        #[allow(dead_code)]
        fn rustrict_old(s: &str) -> bool {
            rustrict_old::CensorStr::is(s, rustrict_old::Type::ANY)
        }

        fn censor(s: &str) -> bool {
            use censor_crate::*;
            let filter = Standard + Sex + Zealous;
            filter.check(s)
        }

        println!("| Crate | Accuracy | Positive Accuracy | Negative Accuracy | Time |");
        println!("|-------|----------|-------------------|-------------------|------|");
        print_accuracy(
            "https://crates.io/crates/rustrict",
            rustrict,
            false, // true,
            None,  // Some(rustrict_old),
        );
        print_accuracy("https://crates.io/crates/censor", censor, false, None);
    }

    #[allow(dead_code)]
    fn print_accuracy(
        link: &str,
        checker: fn(&str) -> bool,
        find_detections: bool,
        compare_to: Option<fn(&str) -> bool>,
    ) {
        let start = Instant::now();
        let (total, positive, negative) = accuracy_of(checker, find_detections, compare_to);
        println!(
            "| [{}]({}) | {:.2}% | {:.2}% | {:.2}% | {:.2}s |",
            link.split('/').last().unwrap(),
            link,
            total * 100.0,
            positive * 100.0,
            negative * 100.0,
            start.elapsed().as_secs()
        );
    }

    #[allow(dead_code)]
    fn accuracy_of(
        checker: fn(&str) -> bool,
        find_detections: bool,
        compare_to: Option<fn(&str) -> bool>,
    ) -> (f32, f32, f32) {
        let file = File::open("test.csv").unwrap();
        let reader = BufReader::new(file);
        let mut csv = csv::Reader::from_reader(reader);

        let mut correct_positive = 0;
        let mut correct_negative = 0;
        let mut total_positive = 0;
        let mut total_negative = 0;

        for line in csv.records().take(100000) {
            let record = line.unwrap();
            let truth = record[0].parse::<i8>().unwrap() == 1;
            let text = &record[1];
            let prediction = checker(text);
            //assert_eq!(is(text), is(text), "With ({})", text);
            if prediction == truth {
                if truth {
                    correct_positive += 1;
                } else {
                    correct_negative += 1;
                }
            } else if find_detections && text.len() < 100 {
                println!("{}: {}", truth, text);
                if prediction {
                    find_detection(text);
                }
            }
            if let Some(checker) = compare_to {
                let compare_prediction = checker(text);
                if prediction != compare_prediction && text.len() < 100 {
                    println!("COMPARISON: On \"{}\", output {} instead", text, prediction);
                }
            }
            if truth {
                total_positive += 1;
            } else {
                total_negative += 1;
            }
        }

        (
            (correct_positive + correct_negative) as f32 / (total_positive + total_negative) as f32,
            correct_positive as f32 / total_positive as f32,
            correct_negative as f32 / total_negative as f32,
        )
    }

    #[test]
    #[serial]
    fn devanagari() {
        println!("f\u{0900}u\u{0900}c\u{0900}k");
        const TEST: &'static str = "हत्यारा मकसहूद भाई तुम बड़ा मस्त काम करती।";
        assert!(should_skip_censor(TEST));
        assert_eq!(TEST, TEST.censor());
    }

    #[test]
    #[serial]
    fn pancakes() {
        assert_eq!(
            "🥞",
            std::str::from_utf8(&[240, 159, 165, 158]).unwrap().censor()
        );
    }

    #[test]
    #[serial]
    fn bandwidth() {
        let file = File::open("test.csv").unwrap();
        let total_len = file.metadata().unwrap().len() as usize;
        let reader = BufReader::new(file);
        let mut csv = csv::Reader::from_reader(reader);

        let mut text = String::with_capacity(total_len);

        for line in csv.records().take(100000) {
            let record = line.unwrap();
            text.push_str(&record[1]);
        }

        for power in 1..16 {
            let len = 2usize.pow(power);

            if len > text.len() {
                break;
            }

            let now = Instant::now();

            let (_, _) = Censor::from_str(&text[0..len]).censor_and_analyze();

            let elapsed = now.elapsed();

            println!(
                "{}, {}, {}",
                len,
                elapsed.as_secs_f32(),
                len as f32 / elapsed.as_secs_f32() / 1000.0 / 1000.0
            );
        }
    }

    #[cfg(feature = "customize")]
    #[test]
    #[serial]
    fn customize() {
        use crate::add_word;

        let test_profanity = "thisisafakeprofanityfortesting";
        let test_profanity_issue_7 = "плохоеслово";
        let test_safe = "thisisafakesafewordfortesting";

        // SAFETY: Tests are run serially, so concurrent mutation is avoided.
        unsafe {
            add_word(test_profanity, Type::PROFANE & Type::SEVERE);
            add_word(test_profanity_issue_7, Type::PROFANE & Type::SEVERE);
            add_word(test_safe, Type::SAFE);
        }

        assert!(test_profanity.is(Type::PROFANE & Type::SEVERE));
        assert!(test_profanity_issue_7.is(Type::PROFANE & Type::SEVERE));
        assert!(test_safe.is(Type::SAFE));

        unsafe {
            add_word(test_profanity, Type::NONE);
        }

        assert!(test_profanity.isnt(Type::PROFANE));
    }

    #[allow(soft_unstable)]
    #[bench]
    fn bench_is_inappropriate(b: &mut Bencher) {
        b.iter(|| test::black_box("hello fuck world shit").is_inappropriate());
    }

    #[allow(soft_unstable)]
    #[bench]
    fn bench_is_inappropriate_long(b: &mut Bencher) {
        b.iter(|| test::black_box("hello fuck world shit hello fuck world shit hello fuck world shit hello fuck world shit hello fuck world shit hello fuck world shit hello fuck world shit").is_inappropriate());
    }

    #[allow(soft_unstable)]
    #[bench]
    fn bench_censor(b: &mut Bencher) {
        b.iter(|| test::black_box("hello fuck world shit").censor());
    }
}
