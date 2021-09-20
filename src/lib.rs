#![cfg_attr(test, feature(test))]

use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::mtch::*;
use crate::radix::*;
use bitflags::bitflags;
use lazy_static::lazy_static;
use rustc_hash::{FxHashMap, FxHashSet};
use std::char::ToLowercase;
use std::iter::{Filter, FlatMap};
use std::mem;
use std::str::Chars;
use unicode_categories::UnicodeCategories;
use unicode_normalization::{Decompositions, Recompositions, UnicodeNormalization};

mod buffer_proxy_iterator;
mod mtch;
mod radix;

lazy_static! {
    static ref TREE: Tree = include_str!("profanity.csv")
        .split('\n')
        .skip(1)
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut split = line.split(",");
            (
                split.next().unwrap(),
                [
                    split.next().unwrap().parse().unwrap(),
                    split.next().unwrap().parse().unwrap(),
                    split.next().unwrap().parse().unwrap(),
                    split.next().unwrap().parse().unwrap(),
                ],
            )
        })
        .chain(
            include_str!("false_positives.txt")
                .split('\n')
                .filter(|line| {
                    return !line.is_empty();
                })
                .map(|line| { (line, [-1; 4],) })
        )
        .collect();
    static ref REPLACEMENTS: FxHashMap<char, &'static str> = include_str!("replacements.csv")
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            let comma = line.find(",").unwrap();
            (line[..comma].chars().next().unwrap(), &line[comma + 1..])
        })
        .collect();
}

/// Censor is a flexible profanity filter that can analyze and/or censor arbitrary text.
///
/// You can also make use of `Censor` via traits `CensorStr` and `CensorIter`, which allow inline
/// checking and censoring of `&str` and `Iterator<Item = char>` respectively.
pub struct Censor<I: Iterator<Item = char>> {
    /// Options
    ignore_false_positives: bool,
    censor_first_character: bool,
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
    weights: [i8; 4],
    /// Where matches are kept after they are complete but may be cancelled due to false positives.
    pending_commit: Vec<Match>,
    /// A buffer of the input that stores unconfirmed characters (may need to censor before flushing).
    /// This is so the censored output is unaffected by the subsequent iterator machinery.
    buffer: BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>>,
    /// Iterator machinery to canonicalize input text.
    chars: FlatMap<
        BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>>,
        ToLowercase,
        fn(char) -> ToLowercase,
    >,
    /// Whether already appended a space at the end.
    space_apended: bool,
    /// Whether all processing of characters has completed.
    done: bool,
}

bitflags! {
    /// Type is represents a type or severity of inappropriateness. They can be combined with bitwise operators. They are **not** mutually exclusive.
    pub struct Type: u32 {
        /// Bad words.
        const PROFANE   = 0b000000000111;
        /// Offensive words.
        const OFFENSIVE = 0b000000111000;
        /// Sexual words.
        const SEXUAL    = 0b000111000000;
        /// Mean words.
        const MEAN      = 0b111000000000;

        /// Not that bad.
        const MILD      = 0b111111111111;
        /// Bad.
        const MODERATE  = 0b110110110110;
        /// Cover your eyes!
        const SEVERE    = 0b100100100100;

        /// The default `Type`, meaning profane, offensive, sexual, or severely mean.
        const INAPPROPRIATE = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | (Self::MEAN.bits & Self::SEVERE.bits);

        /// Any type of detection. This will be expanded to cover all future types.
        const ANY = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | Self::MEAN.bits;
    }
}

impl Type {
    /// Returns `true` if and only if self, the analysis result, meets the given threshold.
    pub fn is(self, threshold: Self) -> bool {
        self & threshold != Type { bits: 0 }
    }

    /// Logical opposite of `Self::is`.
    pub fn isnt(self, threshold: Self) -> bool {
        self & threshold == Type { bits: 0 }
    }
}

impl Default for Type {
    /// Returns a reasonable default for censoring or blocking.
    fn default() -> Self {
        Self::INAPPROPRIATE
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
        let (buffer, chars) = Self::buffers_from(text);

        Self {
            // Default options
            ignore_false_positives: false,
            censor_first_character: false,
            //preserve_accents: false,
            censor_replacement: '*',
            censor_threshold: Default::default(),
            // The beginning of the sequence is a separator.
            separate: true,
            // Nothing was detected yet.
            weights: [0; 4],
            space_apended: false,
            done: false,
            last_pos: usize::MAX,
            matches: FxHashSet::default(),
            matches_tmp: FxHashSet::default(),
            pending_commit: Vec::new(),
            buffer,
            chars,
        }
    }

    fn buffers_from(
        text: I,
    ) -> (
        BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>>,
        FlatMap<
            BufferProxyIterator<Recompositions<Filter<Decompositions<I>, fn(&char) -> bool>>>,
            ToLowercase,
            fn(char) -> ToLowercase,
        >,
    ) {
        // Detects if a char isn't a diacritical mark (accent), such that such characters may be
        // filtered on that basis.
        fn isnt_mark_nonspacing(c: &char) -> bool {
            !c.is_mark_nonspacing()
        }

        // TODO: Replace Rc via Pin<Self> or otherwise avoid allocation.
        let buffer = BufferProxyIterator::new(
            text
                // The following three transformers are to ignore diacritical marks.
                .nfd()
                .filter(isnt_mark_nonspacing as fn(&char) -> bool)
                .nfc(),
        );

        // Detections not case sensitive.
        (
            buffer.clone(),
            buffer.flat_map(char::to_lowercase as fn(char) -> ToLowercase),
        )
    }

    /// Resets the `Censor` with new text. Does not change any configured options.
    /// This avoids reallocation of internal buffers on the heap.
    pub fn reset(&mut self, text: I) {
        let (buffer, chars) = Self::buffers_from(text);

        self.separate = true;
        self.weights = [0; 4];
        self.space_apended = false;
        self.done = false;
        self.last_pos = usize::MAX;
        self.matches.clear();
        self.matches_tmp.clear();
        self.pending_commit.clear();
        self.buffer = buffer;
        self.chars = chars;
    }

    /// Selects a threshold to apply while censoring. Only words that meet or exceed the threshold
    /// are censored.
    ///
    /// The default is `Type::Inappropriate`.
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

    /// Censor all characters e.g. "****," instead of all but the first e.g. "f***."
    ///
    /// The default is `false`.
    pub fn with_censor_first_character(&mut self, censor_first_character: bool) -> &mut Self {
        self.censor_first_character = censor_first_character;
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
        if self.buffer.index().is_some() {
            // The input was at least partially read already.
            panic!("censor must be called before any other form of processing");
        }
        let size_hint = self.chars.size_hint();
        let initial_cap = size_hint.1.unwrap_or(size_hint.0);
        let mut s = String::with_capacity(initial_cap);
        while let Some(c) = self.next() {
            s.push(c);
        }
        s
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
        let censored = self.censor();
        (censored, self.analysis())
    }

    /// Converts internal weights to a `Type`.
    fn analysis(&self) -> Type {
        let mut result = 0;
        for (i, &weight) in self.weights.iter().enumerate() {
            let severity: u32 = if weight >= 3 {
                0b100 // severe
            } else if weight == 2 {
                0b010 // moderate
            } else if weight == 1 {
                0b001 // mild
            } else {
                0 // none
            };

            result |= severity << (i * 3)
        }
        Type { bits: result }
    }

    fn ensure_done(&mut self) {
        if !self.done {
            while let Some(_) = self.next() {}
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Censor<I> {
    type Item = char;

    /// Retrieves the next (potentially censored) character.
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.chars.next().or_else(|| {
            if self.space_apended {
                None
            } else {
                self.space_apended = true;
                Some(' ')
            }
        }) {
            let pos = self.buffer.index();

            let skippable = c.is_punctuation() || c.is_separator();

            if let Some(pos) = pos {
                if !skippable {
                    // Seed a new match for every character read.
                    self.matches.insert(Match {
                        node: &TREE.root,
                        start: pos, // will immediately be incremented if match is kept.
                        end: usize::MAX, // sentinel.
                        last: 0 as char, // sentinel.
                        space_before: self.separate,
                        space_after: false, // unknown at this time.
                        spaces: 0,
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

            mem::swap(&mut self.matches, &mut self.matches_tmp);
            for c in REPLACEMENTS
                .get(&c)
                .unwrap_or(&&*c.encode_utf8(&mut [0; 4]))
                .chars()
            {
                for m in self.matches_tmp.iter() {
                    let m = m.clone();

                    safety_end = safety_end.min(m.start);

                    if (skippable || c == m.last) && m.start != pos.unwrap_or(0) {
                        // Undo remove.
                        let undo_m = Match {
                            spaces: m.spaces.saturating_add(self.separate as u8),
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
                            spaces: m.spaces.saturating_add(self.separate as u8),
                            last: c,
                            ..m
                        };

                        if next.is_word() {
                            let length = pos.unwrap() - next_m.start;
                            if next_m.node.weights.iter().any(|&w| w < 0) {
                                // Is false positive, so invalidate internal matches.
                                if next_m.spaces == 0 && !self.ignore_false_positives {
                                    drain_start = Some(
                                        drain_start
                                            .map(|start| start.min(next_m.start))
                                            .unwrap_or(next_m.start),
                                    );
                                }
                            } else if length > 1 {
                                self.pending_commit.push(Match {
                                    end: pos.unwrap(),
                                    ..next_m
                                });
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
            if let Some(pos) = pos {
                self.last_pos = pos;
            }

            let weights = &mut self.weights;
            let spy = &self.buffer;
            let censor_first_character = self.censor_first_character;
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
                    pending.commit(weights, spy, censor_first_character, censor_replacement);
                    return false;
                }

                // At this point, don't know whether this match will be committed or cancelled, so
                // return.
                true
            });

            // Yield one character if possible.
            if let Some(spy_next_index) = self.buffer.spy_next_index() {
                // This covers all in-flight matches.
                let mut safe = spy_next_index < safety_end;

                // This covers all pending commit matches.
                for pending in &self.pending_commit {
                    if pending.start <= spy_next_index {
                        safe = false;
                        break;
                    }
                }
                if safe {
                    return self.buffer.spy_next();
                }
            }
        }

        for pending in mem::take(&mut self.pending_commit) {
            pending.commit(
                &mut self.weights,
                &self.buffer,
                self.censor_first_character,
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
        Censor::new(self.chars()).analyze().is(threshold)
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

#[cfg(test)]
mod tests {
    extern crate test;
    use crate::{Censor, CensorIter, CensorStr, Type};
    use std::fs::File;
    use std::io::BufReader;
    use test::Bencher;

    #[allow(dead_code)]
    fn find_detection(text: &str) {
        println!("{}", text);
        let mut start = 0;
        let mut end = text.chars().count();

        while start < end && !text[start..end].is_inappropriate() {
            start += 1;
        }
        start -= 1;
        while start < end && !text[start..end].is_inappropriate() {
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
        println!("({})", String::from(&text[start..end]));
    }

    #[test]
    fn is_pure() {
        let mut cases: Vec<(&str, bool)> = Vec::new();
        cases.extend(
            include_str!("test_positive.txt")
                .split('\n')
                .filter(|l| !l.is_empty())
                .map(|l| (l, true)),
        );
        cases.extend(
            include_str!("test_negative.txt")
                .split('\n')
                .filter(|l| !l.is_empty())
                .map(|l| (l, false)),
        );

        for (case, truth) in cases {
            let prediction = case.is_inappropriate();
            if !truth {
                //assert_eq!(case, case.chars().censor().collect::<String>());
            }
            if prediction != truth {
                panic!("FAIL: Predicted {} for {}", prediction, case);
            }
        }
    }

    #[test]
    fn censor() {
        let censored = Censor::from_str("HELLO fučk Shit WORLD!")
            .with_censor_replacement('#')
            .censor();

        assert_eq!(censored, "HELLO f### S### WORLD!");
    }

    #[test]
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
    fn accuracy() {
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
            let prediction = text.is_inappropriate();
            //assert_eq!(is(text), is(text), "With ({})", text);
            if prediction == truth {
                if truth {
                    correct_positive += 1;
                } else {
                    correct_negative += 1;
                }
            } else if text.len() < 100 {
                //println!("{}: {}", truth, text);
                if prediction {
                    //find_detection(text);
                }
            }
            if truth {
                total_positive += 1;
            } else {
                total_negative += 1;
            }
        }

        println!(
            "Accuracy: {}, Positive Accuracy: {}, Negative Accuracy: {}",
            (correct_positive + correct_negative) as f32 / (total_positive + total_negative) as f32,
            correct_positive as f32 / total_positive as f32,
            correct_negative as f32 / total_negative as f32
        );
    }

    #[allow(soft_unstable)]
    #[bench]
    fn bench_is_inappropriate(b: &mut Bencher) {
        b.iter(|| test::black_box("hello fuck world shit").is_inappropriate());
    }

    #[allow(soft_unstable)]
    #[bench]
    fn bench_censor(b: &mut Bencher) {
        b.iter(|| test::black_box("hello fuck world shit").censor());
    }
}

use doc_comment::doctest;
doctest!("../README.md");
