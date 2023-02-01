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
        .lines()
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
                            println!("{find} -> {replace}");
                            let mut replace = replace.to_string();
                            replace.push(find);
                            Some((find, replace))
                        } else if find.is_digit(36) {
                            panic!("reversed!");
                            //println!("{replace} -> {find} (REV)");
                            //Some((replace, find.to_string()))
                        } else {
                            None
                        }
                    })
                })
        })
        .for_each(&mut append_replacement);

    include_str!("unicode_fonts.txt")
        .lines()
        .filter(|line| !line.is_empty())
        .for_each(|line| {
            let chars: Vec<_> = line.chars().collect();

            if chars.len() != 26 {
                panic!("alphabet doesn't have 26 chars: {}", line);
            }

            for (i, c) in chars.into_iter().enumerate() {
                if c.is_ascii() {
                    // not all fonts have all letters.
                    continue;
                }

                append_replacement((c, String::from_utf8(vec![b'a' + i as u8]).unwrap()))
            }
        });

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
            let before_comma = &line[..comma];
            let c = if before_comma.chars().count() == 1 {
                before_comma.chars().next().unwrap()
            } else {
                let escape = before_comma
                    .strip_prefix("\\u{")
                    .expect(before_comma)
                    .strip_suffix("}")
                    .unwrap();
                let escape_int = u32::from_str_radix(escape, 16).unwrap();
                // println!("ESCAPE: {escape} {escape_int}");
                char::from_u32(escape_int).unwrap()
            };

            use unicode_normalization::UnicodeNormalization;
            use finl_unicode::categories::{CharacterCategories};
            let c_string = String::from(c);
            let c_string_2 = c_string
                .nfd()
                .filter(|c| !c.is_mark_nonspacing())
                .nfc()
                .collect::<String>();

            if c_string != c_string_2 {
                println!("Warning (Mn): {c_string} -> {c_string_2}");
            }
            assert_eq!(c_string_2.chars().count(), 1);

            (c_string_2.chars().next().unwrap(), String::from(&line[comma + 1..]))
        })
        .for_each(&mut append_replacement);

    let mut writer = Writer::from_path("src/replacements.csv").unwrap();
    for (find, mut replace) in replacements {
        // Keep original character accessible.
        if find.is_ascii() {
            replace.insert(find);
        }

        for c in replace.clone() {
            let lower = c.to_lowercase().next().unwrap();
            if c.is_uppercase() && !replace.contains(&lower) {
                println!("WARNING: Replacing {find} with {replace:?}, so adding {lower}");
                replace.insert(lower);
            }
        }

        writer
            .write_record(&[&find.to_string(), &replace.iter().collect()])
            .unwrap();
    }
    writer.flush().unwrap();
}
