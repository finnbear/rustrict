#![cfg_attr(test, feature(test))]

use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::feature_cell::FeatureCell;
use crate::mtch::*;
use crate::radix::*;
use bitflags::bitflags;
use lazy_static::lazy_static;
use rustc_hash::{FxHashMap, FxHashSet};
use std::iter::Filter;
use std::mem;
use std::str::Chars;
use unicode_categories::UnicodeCategories;
use unicode_normalization::{Decompositions, Recompositions, UnicodeNormalization};

mod buffer_proxy_iterator;
mod feature_cell;
mod mtch;
mod radix;

/// Number of weights.
const WEIGHT_COUNT: usize = 5;
/// Bits per weight;
const WEIGHT_BITS: usize = 3;

lazy_static! {
    static ref TREE: FeatureCell<Tree> = FeatureCell::new(
        include_str!("profanity.csv")
            .split('\n')
            .skip(1)
            .filter(|line| !line.is_empty())
            .map(|line| {
                let mut split = line.split(',');
                (
                    split.next().unwrap(),
                    Type::from_weights(
                        &[0; WEIGHT_COUNT].map(|_| split.next().unwrap().parse().unwrap()),
                    ),
                )
            })
            .chain(
                include_str!("safe.txt")
                    .split('\n')
                    .filter(|line| !line.is_empty() && !line.starts_with('#'))
                    .map(|line| { (line, Type::SAFE) })
            )
            .chain(
                include_str!("false_positives.txt")
                    .split('\n')
                    .filter(|line| !line.is_empty())
                    .map(|line| { (line, Type::NONE) })
            )
            .collect()
    );
    static ref REPLACEMENTS: FxHashMap<char, &'static str> = include_str!("replacements.csv")
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| {
            let comma = line.find(',').unwrap();
            (line[..comma].chars().next().unwrap(), &line[comma + 1..])
        })
        .collect();
    static ref BANNED: FeatureCell<FxHashSet<char>> = FeatureCell::new(
        include_str!("banned_chars.txt")
            .split('\n')
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
    matches: FxHashSet<Match>,
    /// Where potential matches are temporarily shuffled. Only allocate this once.
    matches_tmp: FxHashSet<Match>,
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

bitflags! {
    /// Type is represents a type or severity of inappropriateness. They can be combined with bitwise operators. They are **not** mutually exclusive.
    pub struct Type: u32 {
        /// Bad words.
        const PROFANE   = 0b0_000_000_000_000_000_111;
        /// Offensive words.
        const OFFENSIVE = 0b0_000_000_000_000_111_000;
        /// Sexual words.
        const SEXUAL    = 0b0_000_000_000_111_000_000;
        /// Mean words.
        const MEAN      = 0b0_000_000_111_000_000_000;
        /// Words intended to evade detection.
        const EVASIVE   = 0b0_000_111_000_000_000_000;
        /// Spam/gibberish/SHOUTING.
        const SPAM      = 0b0_111_000_000_000_000_000;

        /// One of a very small number of safe phases.
        /// Recommended to enforce this on users who repeatedly evade the filter.
        const SAFE      = 0b1_000_000_000_000_000_000;

        /// Not that bad.
        const MILD      = 0b0_001_001_001_001_001_001;
        /// Bad.
        const MODERATE  = 0b0_010_010_010_010_010_010;
        /// Cover your eyes!
        const SEVERE    = 0b0_100_100_100_100_100_100;

        /// Any level; `Type::MILD`, `Type::MODERATE`, or `Type::SEVERE`.
        const MILD_OR_HIGHER = Self::MILD.bits | Self::MODERATE.bits | Self::SEVERE.bits;

        /// Any level in excess of `Type::MILD`.
        const MODERATE_OR_HIGHER = Self::MODERATE.bits | Self::SEVERE.bits;

        /// The default `Type`, meaning profane, offensive, sexual, or severely mean.
        const INAPPROPRIATE = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | (Self::MEAN.bits & Self::SEVERE.bits);

        /// Any type of detection (except SAFE). This will be expanded to cover all future types.
        const ANY = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | Self::MEAN.bits | Self::EVASIVE.bits | Self::SPAM.bits;

        /// No type of detection.
        const NONE = 0;
    }
}

const SEVERE_WEIGHT: i8 = 3;
const MODERATE_WEIGHT: i8 = 2;
const MILD_WEIGHT: i8 = 1;

impl Type {
    /// Returns `true` if and only if self, the analysis result, meets the given threshold.
    pub fn is(self, threshold: Self) -> bool {
        self & threshold != Type::NONE
    }

    /// Logical opposite of `Self::is`.
    pub fn isnt(self, threshold: Self) -> bool {
        self & threshold == Type::NONE
    }

    #[allow(dead_code)]
    fn to_weights(self) -> [i8; WEIGHT_COUNT] {
        fn bits_to_weight(bits: u32) -> i8 {
            if bits == 0 {
                0
            } else if bits & 0b1 != 0 {
                MILD_WEIGHT
            } else if bits & 0b10 != 0 {
                MODERATE_WEIGHT
            } else {
                SEVERE_WEIGHT
            }
        }

        let mut i = 0;
        [0; WEIGHT_COUNT].map(|_| {
            let ret = bits_to_weight((self.bits >> i) & 0b111);
            i += WEIGHT_BITS;
            ret
        })
    }

    fn from_weights(weights: &[i8; WEIGHT_COUNT]) -> Type {
        let mut result = 0;
        for (i, &weight) in weights.iter().enumerate() {
            let severity: u32 = if weight >= SEVERE_WEIGHT {
                0b100
            } else if weight == MODERATE_WEIGHT {
                0b010
            } else if weight == MILD_WEIGHT {
                0b001
            } else {
                0 // none
            };

            result |= severity << (i * WEIGHT_BITS)
        }
        Type { bits: result }
    }
}

impl Default for Type {
    /// Returns a reasonable default for censoring or blocking.
    fn default() -> Self {
        Self::INAPPROPRIATE
    }
}

/// This serves as replacement for Debug that isn't blocked by
/// https://github.com/bitflags/bitflags/issues/218
#[cfg(test)]
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn description(bits: u32) -> &'static str {
            if bits & 0b100 != 0 {
                "severely"
            } else if bits & 0b010 != 0 {
                "moderately"
            } else if bits & 0b001 != 0 {
                "mildly"
            } else {
                "not"
            }
        }
        let mut count = 0;
        if *self & Self::PROFANE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} profane", description((*self & Self::PROFANE).bits()))?;
            count += 1;
        }
        if *self & Self::OFFENSIVE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} offensive",
                description((*self & Self::OFFENSIVE).bits() >> 3)
            )?;
            count += 1;
        }
        if *self & Self::SEXUAL != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} sexual",
                description((*self & Self::SEXUAL).bits() >> 6)
            )?;
            count += 1;
        }
        if *self & Self::MEAN != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} mean", description((*self & Self::MEAN).bits() >> 9))?;
            count += 1;
        }
        if *self & Self::EVASIVE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} evasive",
                description((*self & Self::EVASIVE).bits() >> 12)
            )?;
            count += 1;
        }
        if *self & Self::SPAM != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} spam", description((*self & Self::SPAM).bits() >> 15))?;
            count += 1;
        }

        if count == 0 {
            write!(f, "no detections")
        } else {
            write!(f, "")
        }
    }
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
            matches: FxHashSet::default(),
            matches_tmp: FxHashSet::default(),
            pending_commit: Vec::new(),
            buffer: Self::buffer_from(text),
        }
    }

    fn buffer_from(
        text: I,
    ) -> BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>> {
        // Detects if a char isn't a diacritical mark (accent) or banned, such that such characters may be
        // filtered on that basis.
        fn isnt_mark_nonspacing_or_banned(c: &char) -> bool {
            !(c.is_mark_nonspacing() || BANNED.contains(c))
        }

        BufferProxyIterator::new(
            text
                // The following three transformers are to ignore diacritical marks.
                .nfd()
                .filter(isnt_mark_nonspacing_or_banned as fn(&char) -> bool)
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
            let skippable = raw_c.is_punctuation() || raw_c.is_separator() || raw_c.is_other();
            let replacement = REPLACEMENTS.get(&raw_c);

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
                if !(skippable && replacement.is_none()) {
                    // Seed a new match for every character read.
                    self.matches.insert(Match {
                        node: &TREE.root,
                        start: pos, // will immediately be incremented if match is kept.
                        end: usize::MAX, // sentinel.
                        last: 0 as char, // sentinel.
                        space_before: self.separate,
                        space_after: false, // unknown at this time.
                        spaces: 0,
                        replacements: 0,
                    });
                }
            }

            /*
            let skippable = match c {
                ' ' | '~' | '-' | '−' | '_' | '.' | '!' | '?' | ',' | '*' | '"' | '\'' | '\n' | '\r'
                | '\t' => true,
                _ => false,
            };
             */

            self.separate = skippable;

            if self.separate {
                for pending in self.pending_commit.iter_mut() {
                    if pending.end == self.last_pos {
                        pending.space_after = true;
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
                let benign_replacement = c == raw_c_lower;

                if !(replacement_counted
                    || benign_replacement
                    || raw_c.is_ascii_alphabetic()
                    || (raw_c.is_ascii_digit()
                        && self.last.map(|l| l.is_ascii_digit()).unwrap_or(false)))
                {
                    self.replacements = self.replacements.saturating_add(1);
                    replacement_counted = true;
                }

                for m in self.matches_tmp.iter() {
                    let m = m.clone();

                    safety_end = safety_end.min(m.start);

                    if (skippable || c == m.last) && m.start != pos.unwrap_or(0) {
                        // Undo remove.
                        let undo_m = Match {
                            spaces: m.spaces.saturating_add(
                                ((c == ' ' || c != raw_c) && self.separate && c != '\'') as u8,
                            ),
                            replacements: m
                                .replacements
                                .saturating_add((!benign_replacement && !self.separate) as u8),
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
                            last: c,
                            ..m
                        };

                        if next.word {
                            if next_m.node.typ.is(Type::SAFE)
                                && next_m.start == 0
                                && next_m.spaces == 0
                                && !self.ignore_false_positives
                            {
                                // Everything in the input until now is safe.
                                self.safe = true;
                            }

                            if next_m.node.typ.is(Type::ANY) {
                                self.pending_commit.push(Match {
                                    end: pos.unwrap(),
                                    ..next_m
                                });
                            } else if next_m.spaces == 0
                                && next_m.replacements == 0
                                && !self.ignore_false_positives
                            {
                                // Is false positive, so invalidate internal matches.
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

            self.pending_commit.retain(|pending| {
                // Cancel due to false positive.
                if let Some(start) = drain_start {
                    if pending.start >= start {
                        return false;
                    }
                }

                // Can pre-commit due to lack of false positive matches.
                if pending.end < safety_end {
                    pending.commit(
                        typ,
                        spy,
                        censor_threshold,
                        censor_first_character_threshold,
                        censor_replacement,
                    );
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

        for pending in mem::take(&mut self.pending_commit) {
            pending.commit(
                &mut self.typ,
                &mut self.buffer,
                self.censor_threshold,
                self.censor_first_character_threshold,
                self.censor_replacement,
            );
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
        Censor::new(self.chars()).censor()
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

#[cfg(feature = "context")]
use std::collections::VecDeque;
#[cfg(feature = "context")]
use std::fmt::{Display, Formatter};
#[cfg(feature = "context")]
use std::time::{Duration, Instant};

/// Context is useful for taking moderation actions on a per-user basis i.e. each user would get
/// their own Context.
#[cfg(feature = "context")]
pub struct Context {
    history: VecDeque<(String, Instant)>,
    rate_limit: Duration,
    burst: u8,
    burst_used: u8,
    suspicion: u8,
    reports: u8,
    total: u16,
    total_inappropriate: u16,
    muted_until: Option<Instant>,
    only_safe_until: Option<Instant>,
    rate_limited_until: Option<Instant>,
    last_message: Option<Instant>,
}

#[cfg(feature = "context")]
impl Context {
    /// VecDeque internally adds 1 (resulting in 8), and then finds the next power of 2 (still 8).
    const CAPACITY: usize = 7;

    /// Track repetitions (spam) within this time frame.
    const REPETITION_DURATION: Duration = Duration::from_secs(60);

    /// How many repetitions are tolerated.
    const REPETITION_LIMIT: usize = 3;

    /// Maximum "safe only" timeout.
    const MAX_TIMEOUT: Duration = Duration::from_secs(3600);

    /// rate_limit is minimum time between messages.
    /// burst allows a certain amount of messages beyond the rate limit.
    pub fn new(rate_limit: Duration, burst: u8) -> Self {
        Self {
            history: VecDeque::with_capacity(Self::CAPACITY),
            rate_limit,
            burst,
            burst_used: 0,
            suspicion: 0,
            reports: 0,
            total: 0,
            total_inappropriate: 0,
            only_safe_until: None,
            rate_limited_until: None,
            muted_until: None,
            last_message: None,
        }
    }

    /// Returns None if expired is None or has been reached, resulting in expiry being set to None.
    /// Otherwise, returns duration before expiry.
    fn remaining_duration(expiry: &mut Option<Instant>, now: Instant) -> Option<Duration> {
        if let Some(time) = *expiry {
            if now >= time {
                *expiry = None;
                None
            } else {
                Some(time - now)
            }
        } else {
            None
        }
    }

    /// Takes user message, returns censored message trimmed of whitespace (if it should be sent)
    /// or `BlockReason` (explaining why it should be blocked entirely).
    pub fn process(&mut self, message: String) -> Result<String, BlockReason> {
        let now = Instant::now();
        let elapsed = self.last_message.map(|l| now - l).unwrap_or(Duration::ZERO);

        let suspicion = self.suspicion.max(1).saturating_mul(self.reports.max(1));

        // How convinced are we that the user is a bad actor.
        let is_kinda_sus = suspicion >= 2;
        let is_impostor = suspicion >= 15;

        // Don't give bad actors the benefit of the doubt when it comes to meanness.
        let meanness_threshold = if is_impostor {
            Type::MILD_OR_HIGHER
        } else if is_kinda_sus {
            Type::MODERATE_OR_HIGHER
        } else {
            Type::SEVERE
        };

        let censor_threshold =
            Type::PROFANE | Type::OFFENSIVE | Type::SEXUAL | (Type::MEAN & meanness_threshold);

        // Don't give bad actors the benefit of letting their first character through.
        let censor_first_character_threshold = if is_kinda_sus {
            censor_threshold
        } else {
            // Mainly for protection against the n-word being discernible.
            Type::OFFENSIVE & Type::SEVERE
        };

        let (mut censored, analysis) = Censor::from_str(trim_whitespace(&message))
            .with_censor_threshold(censor_threshold)
            .with_censor_first_character_threshold(censor_first_character_threshold)
            .censor_and_analyze();

        // Censoring can remove certain characters, rendering the prefix or suffix whitespace.
        let trimmed_censored = trim_whitespace(&censored);
        if trimmed_censored.len() < censored.len() {
            censored = String::from(trimmed_censored);
        }

        self.total = self.total.saturating_add(1);
        if analysis.is(Type::INAPPROPRIATE) {
            self.total_inappropriate = self.total_inappropriate.saturating_add(1);
        }

        // Collecting suspicion.
        let type_to_sus = |typ: Type| -> u8 {
            let combined = analysis & typ;
            if combined.is(Type::SEVERE) {
                3
            } else if combined.is(Type::MODERATE) {
                2
            } else if combined.is(Type::MILD) {
                1
            } else {
                0
            }
        };

        // Repetition detection.
        self.history
            .retain(|&(_, t)| now - t < Self::REPETITION_DURATION);
        let mut recent_similar = 0;

        for (recent_message, _) in &self.history {
            if strsim::normalized_levenshtein(&recent_message, &message) >= 2.0 / 3.0 {
                recent_similar += 1;
            }
        }

        let mut new_suspicion = type_to_sus(Type::PROFANE | Type::OFFENSIVE | Type::SEXUAL)
            .saturating_add(type_to_sus(Type::EVASIVE))
            .saturating_add(type_to_sus(Type::SPAM));

        if recent_similar >= 2 {
            // Don't penalize as much for repeated messages, since an innocent user may repeat their
            // message multiple times if it was erroneously detected.
            new_suspicion /= 2;
        }

        if (is_kinda_sus && new_suspicion >= 4) || (is_impostor && new_suspicion >= 2) {
            if let Some(only_safe_until) =
                self.only_safe_until
                    .unwrap_or(now)
                    .checked_add(if self.reports > 0 {
                        Duration::from_secs(10 * 60)
                    } else {
                        Duration::from_secs(5 * 60)
                    })
            {
                self.only_safe_until = Some(only_safe_until.min(now + Self::MAX_TIMEOUT));
            }
        }

        self.suspicion = self.suspicion.saturating_add(new_suspicion);

        let remaining_rate_limit = Self::remaining_duration(&mut self.rate_limited_until, now);

        if let Some(dur) = Self::remaining_duration(&mut self.muted_until, now) {
            Err(BlockReason::Muted(dur))
        } else if censored.is_empty() {
            Err(BlockReason::Empty)
        } else if let Some(dur) = remaining_rate_limit.filter(|_| self.burst_used >= self.burst) {
            Err(BlockReason::Spam(dur))
        } else if recent_similar >= Self::REPETITION_LIMIT {
            Err(BlockReason::Repetitious(recent_similar))
        } else if analysis.is(Type::INAPPROPRIATE & Type::SEVERE) {
            Err(BlockReason::Inappropriate(analysis))
        } else if let Some(dur) = Self::remaining_duration(&mut self.only_safe_until, now)
            .filter(|_| !analysis.is(Type::SAFE))
        {
            Err(BlockReason::Unsafe(dur))
        } else {
            if self.history.len() >= Self::CAPACITY {
                self.history.pop_front();
            }
            self.history.push_back((message, now));
            self.last_message = Some(now);
            if self.rate_limit > Duration::ZERO {
                self.burst_used = if remaining_rate_limit.is_some() {
                    self.burst_used.saturating_add(1)
                } else {
                    self.burst_used.saturating_sub(
                        (elapsed.as_nanos() / self.rate_limit.as_nanos()).min(u8::MAX as u128)
                            as u8,
                    )
                };
                if let Some(rate_limited_until) = self
                    .rate_limited_until
                    .unwrap_or(now)
                    .checked_add(self.rate_limit)
                {
                    self.rate_limited_until = Some(rate_limited_until);
                }
            }
            // Forgiveness (minus one suspicion per safe message, and also per minute between messages).
            self.suspicion = self.suspicion.saturating_sub(
                (elapsed.as_secs() / 60).clamp(analysis.is(Type::SAFE) as u64, u8::MAX as u64)
                    as u8,
            );
            Ok(censored)
        }
    }

    /// Manually mute this user's messages for a duration. Overwrites any previous manual mute.
    /// Passing `Duration::ZERO` will therefore un-mute.
    pub fn mute_for(&mut self, duration: Duration) {
        self.muted_until = Some(Instant::now() + duration);
    }

    /// Call if another user "reports" this user's message(s). The function of reports is for
    /// suspicion of bad behavior to be confirmed faster.
    pub fn report(&mut self) {
        self.reports = self.reports.saturating_add(1);
    }

    /// Returns number of reports received via `Self::report()`. It is not guaranteed that the full
    /// range of `usize` of reports will be counted (currently only `u8::MAX` are counted).
    pub fn reports(&self) -> usize {
        self.reports as usize
    }

    /// Returns total number of messages processed. It is not guaranteed that the full
    /// range of `usize` of messages will be counted (currently only `u16::MAX` are counted).
    pub fn total(&self) -> usize {
        self.total as usize
    }

    /// Returns total number of messages processed that were `Type::INAPPROPRIATE`. It is not
    /// guaranteed that the full range of `usize` of messages will be counted (currently only
    /// `u16::MAX` are counted).
    pub fn total_inappropriate(&self) -> usize {
        self.total_inappropriate as usize
    }
}

#[cfg(feature = "context")]
impl Default for Context {
    fn default() -> Self {
        Self::new(Duration::from_secs(5), 3)
    }
}

#[cfg(feature = "context")]
#[derive(Copy, Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum BlockReason {
    /// The particular message was *severely* inappropriate, more specifically, `Type`.
    Inappropriate(Type),
    /// Recent messages were generally inappropriate, and this message isn't on the safe list.
    /// Try again after `Duration`.
    Unsafe(Duration),
    /// This message was too similar to `usize` recent messages.
    Repetitious(usize),
    /// Too many messages per unit time, try again after `Duration`.
    Spam(Duration),
    /// Manually muted for `Duration`.
    Muted(Duration),
    /// Message was, at least after censoring, completely empty.
    Empty,
}

#[cfg(feature = "context")]
impl BlockReason {
    /// You may display `BlockReason` in any manner you choose, but this will return a reasonable
    /// default warning to send to the user.
    pub fn generic_str(self) -> &'static str {
        match self {
            Self::Inappropriate(_) => "Your message was held for severe profanity",
            Self::Unsafe(_) => "You have been temporarily restricted due to profanity/spam",
            Self::Repetitious(_) => "Your message was too similar to recent messages",
            Self::Spam(_) => "You have been temporarily muted due to excessive frequency",
            Self::Muted(_) => "You have been temporarily muted",
            Self::Empty => "Your message was empty",
        }
    }

    /// You may display `BlockReason` in any manner you choose, but this will return a reasonable
    /// default warning to send to the user that includes some context (such as how long they are
    /// muted for).
    pub fn contextual_str(self) -> String {
        match self {
            Self::Inappropriate(typ) => String::from(if typ.is(Type::OFFENSIVE) {
                "Your message was held for being highly offensive"
            } else if typ.is(Type::SEXUAL) {
                "Your message was held for being overly sexual"
            } else if typ.is(Type::MEAN) {
                "Your message was held for being overly mean"
            } else {
                "Your message was held for severe profanity"
            }),
            Self::Unsafe(dur) => format!(
                "You have been restricted for {} due to profanity/spam",
                FormattedDuration(dur)
            ),
            Self::Repetitious(count) => {
                format!("Your message was too similar to {} recent messages", count)
            }
            Self::Spam(dur) => format!(
                "You have been muted for {} due to excessive frequency",
                FormattedDuration(dur)
            ),
            Self::Muted(dur) => format!("You have been muted for {}", FormattedDuration(dur)),
            _ => String::from(self.generic_str()),
        }
    }
}

#[cfg(feature = "context")]
struct FormattedDuration(Duration);

#[cfg(feature = "context")]
impl Display for FormattedDuration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0 >= Duration::from_secs(3600) {
            write!(f, "{}h", self.0.as_secs() / 3600)
        } else if self.0 >= Duration::from_secs(60) {
            write!(f, "{}m", self.0.as_secs() / 60)
        } else {
            write!(f, "{}s", self.0.as_secs().max(1))
        }
    }
}

/// Trims whitespace characters from both ends of a string, according to the definition of
/// `crate::is_whitespace`.
pub fn trim_whitespace(s: &str) -> &str {
    s.trim_matches(is_whitespace)
}

/// Returns true iff the character is effectively whitespace. The definition of whitespace is broader
/// than that of Unicode, because it includes control characters and a few additional blank characters.
pub fn is_whitespace(c: char) -> bool {
    // NOTE: The following characters are not detected by standard means but show up as blank.
    // https://www.compart.com/en/unicode/U+2800
    // https://www.compart.com/en/unicode/U+3164
    c.is_whitespace() || c.is_other() || c == '\u{2800}' || c == '\u{3164}'
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
/// "field," you can expect "cornfield" to be detected as well.
///
/// # Safety
///
/// This must not be called when the crate is being used in any other way. It is best to call this
/// from the main thread, near the beginning of the program.
#[cfg(feature = "customize")]
pub unsafe fn add_word(word: &str, typ: Type) {
    TREE.get_mut().add(word, typ);
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]

    extern crate test;
    use crate::{Censor, CensorIter, CensorStr, Type};
    use bitflags::_core::ops::Not;
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
                "(\"{}\" is {})",
                text.chars()
                    .skip(start)
                    .take(end - start)
                    .collect::<String>(),
                holistic
            );
        } else {
            println!("{} ({})", text, holistic);
        }
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
                panic!("FAIL: Predicted {} for {}", typ, case);
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

        assert!(!analysis.is_empty());
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
    }

    #[test]
    #[serial]
    fn accuracy() {
        fn rustrict(s: &str) -> bool {
            s.is(Type::ANY)
        }

        #[allow(dead_code)]
        fn rustrict_old(s: &str) -> bool {
            rustrict_old::CensorStr::is(s, rustrict_old::Type::ANY)
        }

        fn censor(s: &str) -> bool {
            use censor::*;
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

    #[cfg(not(debug_assertions))]
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
        let test_safe = "thisisafakesafewordfortesting";

        // SAFETY: Tests are run serially, so concurrent mutation is avoided.
        unsafe {
            add_word(test_profanity, Type::PROFANE & Type::SEVERE);
            add_word(test_safe, Type::SAFE);
        }

        assert!(test_profanity.is(Type::PROFANE & Type::SEVERE));
        assert!(test_safe.is(Type::SAFE));
    }

    /*
           Self::Inappropriate(_) => "Your message was held for severe profanity",
           Self::Unsafe(_) => "You have been temporarily restricted due to profanity/spam",
           Self::Repetitious(_) => "Your message was too similar to recent messages",
           Self::Spam(_) => "You have been temporarily muted due to excessive frequency",
           Self::Muted(_) => "You have been temporarily muted",
           Self::Empty => "Your message was empty"
    */

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_inappropriate() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
        assert!(matches!(
            ctx.process(String::from("nigga")),
            Err(BlockReason::Inappropriate(_))
        ));
    }

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_unsafe() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        for _ in 0..30 {
            ctx.report();
        }

        let res = ctx.process(String::from("shit"));
        assert!(matches!(res, Err(BlockReason::Unsafe(_))), "1 {:?}", res);

        let res = ctx.process(String::from("not common message"));
        assert!(matches!(res, Err(BlockReason::Unsafe(_))), "2 {:?}", res);
    }

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_repetitious() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        for _ in 0..Context::REPETITION_LIMIT {
            assert!(ctx.process(String::from("one")).is_ok());
        }

        let res = ctx.process(String::from("onne"));
        assert!(matches!(res, Err(BlockReason::Repetitious(_))), "{:?}", res);
    }

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_spam() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::from_millis(350), 2);

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
        assert_eq!(ctx.process(String::from("two")), Ok(String::from("two")));
        assert_eq!(
            ctx.process(String::from("three")),
            Ok(String::from("three"))
        );
        assert!(matches!(
            ctx.process(String::from("four")),
            Err(BlockReason::Spam(_))
        ));

        std::thread::sleep(Duration::from_secs(2));

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
    }

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_muted() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        ctx.mute_for(Duration::from_secs(5));

        let res = ctx.process(String::from("hello"));
        assert!(matches!(res, Err(BlockReason::Muted(_))), "{:?}", res);
    }

    #[cfg(feature = "context")]
    #[test]
    #[serial]
    fn context_empty() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::from_secs(1), 2);
        assert_eq!(ctx.process(String::from("   ")), Err(BlockReason::Empty));
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

use doc_comment::doctest;
doctest!("../README.md");
