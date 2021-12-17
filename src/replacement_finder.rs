use csv::Writer;
use std::collections::{BTreeMap, BTreeSet};

fn main() {
    let mut replacements: BTreeMap<char, BTreeSet<char>> = BTreeMap::new();

    let mut append_replacement = |(k, v): (char, String)| {
        replacements
            .entry(k)
            .or_insert_with(BTreeSet::new)
            .extend(v.chars())
    };

    // Unicode confusables
    include_str!("unicode_confusables.txt")
        .split("\n")
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .filter_map(|line| {
            let mut segments = line.split(';');
            segments
                .next()
                .zip(segments.next())
                .and_then(|(find, replace)| {
                    let find_char = u32::from_str_radix(find.trim(), 16)
                        .ok()
                        .and_then(char::from_u32);
                    let replace_char = u32::from_str_radix(replace.trim(), 16)
                        .ok()
                        .and_then(char::from_u32)
                        .map(|c| c.to_ascii_lowercase());

                    find_char.zip(replace_char).and_then(|(find, replace)| {
                        if replace.is_digit(36) {
                            println!("{} -> {}", find, replace);
                            Some((find, replace.to_string()))
                        } else {
                            None
                        }
                    })
                })
        })
        .for_each(&mut append_replacement);

    // Upper to lower case.
    (0..=0xFFFFFF)
        .filter_map(char::from_u32)
        .filter_map(|c| {
            let r = c.to_lowercase().next().unwrap();
            if r != c {
                Some((c, r.to_string()))
            } else {
                None
            }
        })
        .for_each(&mut append_replacement);

    // Extra confusables.
    include_str!("replacements_extra.csv")
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            let comma = line.find(",").unwrap();
            (
                line[..comma].chars().next().unwrap(),
                String::from(&line[comma + 1..]),
            )
        })
        .for_each(&mut append_replacement);

    let mut writer = Writer::from_path("src/replacements.csv").unwrap();
    for (find, mut replace) in replacements {
        // Keep original character accessible.
        if find.is_digit(36) {
            replace.insert(find);
        }

        writer
            .write_record(&[&find.to_string(), &replace.iter().collect()])
            .unwrap();
    }
    writer.flush().unwrap();
}
