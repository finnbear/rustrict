use indicatif::ProgressBar;
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
        .lines()
        .chain(include_str!("dictionary_extra.txt").split('\n'))
        .chain(include_str!("dictionary_common.txt").lines())
        .chain(include_str!("dictionary_common_valid_short.txt").lines())
        .filter(|&word| !word.is_empty() && !is_blacklisted(word))
        .collect();
    static ref VALID_SHORT: HashSet<&'static str> =
        include_str!("dictionary_common_valid_short.txt")
            .lines()
            .filter(|l| !l.is_empty())
            .collect();
    static ref CONCAT_DICTIONARY: HashSet<&'static str> = include_str!("dictionary_common.txt")
        .lines()
        .chain(include_str!("dictionary_common_valid_short.txt")
        .lines())
        .filter(|&w| {
            let long_enough = w.len() > 3 || VALID_SHORT.contains(w);
            let allowed = !is_blacklisted(w);
            long_enough
                && allowed
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
        .map(|l| l[..l.find(',').expect(l)].trim_start_matches(' '))
        .map(|w| Regex::new(&regex::escape(w)).unwrap())
        .chain(
            include_str!("dictionary_blacklist.txt")
                .split("\n")
                .filter(|l| !l.is_empty())
                .map(|l| Regex::new(l).unwrap())
        )
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

fn maybe_false_positive<C: Iterator<Item = char> + Clone>(
    word: C,
    baseline_match_ptr: usize,
) -> Option<String> {
    let (baseline, baseline_first_match_ptr) = is_ignore_fp(word.clone(), true);
    if baseline > 0 && baseline_first_match_ptr != baseline_match_ptr {
        let word: String = word.collect();
        let word = &word[..];

        if is_blacklisted(word) {
            return None;
        }

        let mut shortest_subslice = word;
        // TODO: Cannot always remove prefix, because false positive wont take effect if starts
        // after the profanity in question. For example, "to helicopter" -> "heli"
        for len in 1..word.len() {
            // break
            // len = 2
            // word.len() - len = 3
            // br    start = 0
            //  re   start = 1
            //   ea  start = 2
            //    ak start = 3 end = 5
            for start in 0..=word.len() - len {
                let end = start + len;
                let sub_slice = &word[start..end];

                if sub_slice.len() >= shortest_subslice.len() {
                    continue;
                }

                let valid = sub_slice.split(' ').all(|w| DICTIONARY.contains(w));

                if !valid {
                    continue;
                }

                let (subslice_matches, first_match_ptr) = is_ignore_fp(
                    sub_slice.chars(),
                    start == 0 || word.as_bytes()[start - 1] == b' ',
                );
                if subslice_matches >= baseline
                    && first_match_ptr == baseline_first_match_ptr
                    && !is_blacklisted(sub_slice)
                {
                    shortest_subslice = sub_slice;
                }
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
            maybe_false_positive(word.chars(), 0)
        })
        .collect();

    progress.finish();

    let progress = ProgressBar::new(CONCAT_DICTIONARY.len() as u64);
    progress.eta();

    let false_positives = Mutex::new(false_positives);

    CONCAT_DICTIONARY.par_iter().for_each(|word1| {
        let word1_ptr = is_ignore_fp(word1.chars(), true).1;
        for word2 in CONCAT_DICTIONARY.iter() {
            let word2_ptr = is_ignore_fp(word2.chars(), true).1;
            if let Some(false_positive) = maybe_false_positive(
                word1
                    .chars()
                    .chain(std::iter::once(' '))
                    .chain(word2.chars()),
                word1_ptr ^ word2_ptr,
            ) {
                //println!("fp: {}", false_positive);
                false_positives.lock().unwrap().insert(false_positive);
            }
        }
        progress.inc(1);
    });

    progress.finish();

    let mut false_positives = false_positives.into_inner().unwrap();

    let clone = false_positives.clone();
    false_positives.retain(|false_positive| {
        let baseline = is_ignore_fp(false_positive.chars(), true);
        for clone in &clone {
            if false_positive.len() != clone.len()
                && (false_positive.starts_with(clone) || false_positive.ends_with(clone))
            {
                let shorter = is_ignore_fp(clone.chars(), true);
                if baseline == shorter {
                    println!("filter out {false_positive} in favor of {clone}");
                    return false;
                }
            }
        }
        true
    });

    let mut sorted: Vec<_> = false_positives.into_iter().collect();
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
