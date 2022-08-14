use indicatif::ProgressBar;
use itertools::Itertools;
use lazy_static::lazy_static;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use regex::Regex;
use rustrict::{Censor, Type};
use std::collections::HashSet;
use std::fs;
use std::sync::Mutex;

lazy_static! {
    static ref DICTIONARY: HashSet<&'static str> = include_str!("dictionary.txt")
        .split("\r\n")
        .chain(include_str!("dictionary_extra.txt").split('\n'))
        .filter(|&word| !word.is_empty() && !is_blacklisted(word))
        .collect();
    static ref VALID_SHORT: HashSet<&'static str> =
        include_str!("dictionary_common_valid_short.txt")
            .split("\n")
            .filter(|l| !l.is_empty())
            .collect();
    static ref CONCAT_DICTIONARY: HashSet<&'static str> = include_str!("dictionary_common.txt")
        .lines()
        .filter(|&w| {
            (w.len() > 3 || VALID_SHORT.contains(w))
                && !is_blacklisted(w)
                && is_ignore_fp(w.chars(), true).0 == 0
        })
        .collect();
    static ref PROFANITY: Vec<&'static str> = include_str!("profanity.csv")
        .lines()
        .skip(1)
        .map(|l| &l[..l.find(',').unwrap()])
        .collect();
    static ref BLACKLIST: Vec<Regex> = include_str!("profanity.csv")
        .lines()
        .skip(1)
        // must trim starting spaces, as they don't count when comparing to blacklist.
        .map(|l| l[..l.find(',').unwrap()].trim_start_matches(' '))
        .chain(
            include_str!("dictionary_blacklist.txt")
                .split("\n")
                .filter(|l| !l.is_empty())
        )
        .map(|l| Regex::new(&l.replace("\\", "\\\\")).unwrap())
        .collect();
}

pub fn is_ignore_fp<C: Iterator<Item = char>>(text: C, start_separate: bool) -> (usize, usize) {
    let mut censor = Censor::new(text);
    censor.with_ignore_false_positives(true);
    censor.with_separate(start_separate);

    if censor
        .analyze()
        .is(Type::PROFANE | Type::OFFENSIVE | Type::SEXUAL | Type::MEAN)
    {
        (censor.total_match_characters().max(1), censor.match_ptrs())
    } else {
        (0, 0)
    }
}

fn maybe_false_positive<C: Iterator<Item = char> + Clone>(word: C) -> Option<String> {
    let (baseline, baseline_first_match_ptr) = is_ignore_fp(word.clone(), true);
    if baseline > 0 {
        let word: String = word.collect();
        let word = &word[..];

        if is_blacklisted(word) {
            return None;
        }
        let index_range = 0..=word.len();
        let mut shortest_subslice = word;
        for perm in index_range.permutations(2) {
            // TODO: Cannot always remove prefix, because false positive wont take effect if starts
            // after the profanity in question. For example, "to helicopter" -> "heli"
            let start = perm[0];
            let end = perm[1];

            if start >= end {
                continue;
            }

            let sub_slice = &word[start..end];

            if sub_slice.len() >= shortest_subslice.len() {
                continue;
            }

            let valid = if sub_slice.contains(' ') {
                let mut split = sub_slice.split(' ');
                split.all(|w| DICTIONARY.contains(w))
            } else {
                DICTIONARY.contains(sub_slice)
            };

            let (subslice_matches, first_match_ptr) = is_ignore_fp(sub_slice.chars(), start == 0);
            if valid
                && subslice_matches >= baseline
                && first_match_ptr == baseline_first_match_ptr
                && !is_blacklisted(sub_slice)
            {
                shortest_subslice = sub_slice;
            }
        }
        return Some(String::from(shortest_subslice));
    }
    None
}

fn main() {
    for word in DICTIONARY.iter() {
        if is_sus(word) {
            println!("\"{}\" is suspiciously like a profanity", word);
        }
    }

    let progress = ProgressBar::new(DICTIONARY.len() as u64);

    let false_positives: HashSet<String> = DICTIONARY
        .par_iter()
        .filter_map(|&word| {
            progress.inc(1);
            maybe_false_positive(word.chars())
        })
        .collect();

    let progress = ProgressBar::new((CONCAT_DICTIONARY.len() as u64).pow(2));
    progress.eta();

    let false_positives = Mutex::new(false_positives);

    CONCAT_DICTIONARY.par_iter().for_each(|word1| {
        for word2 in CONCAT_DICTIONARY.iter() {
            progress.inc(1);
            if let Some(false_positive) =
                maybe_false_positive(word1.chars().chain(" ".chars()).chain(word2.chars()))
            {
                //println!("fp: {}", false_positive);
                false_positives.lock().unwrap().insert(false_positive);
            }
        }
    });

    let mut sorted: Vec<_> = false_positives.into_inner().unwrap().into_iter().collect();
    sorted.sort();

    fs::write("src/false_positives.txt", sorted.join("\n")).unwrap();

    //println!("{:?}", sorted);
}

fn is_blacklisted(phrase: &str) -> bool {
    BLACKLIST.iter().any(|p| {
        p.find(phrase)
            .map(|m| m.start() == 0 && m.end() == phrase.len())
            .unwrap_or(false)
    })
}

#[allow(dead_code)]
fn is_sus(phrase: &str) -> bool {
    let trimmed = phrase.trim_end_matches('s');
    PROFANITY.iter().any(|&p| p == trimmed)
}
