use csv::Writer;
use std::collections::BTreeMap;

fn main() {
    let replacements: BTreeMap<char, String> = include_str!("unicode_confusables.txt")
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
        .chain(
            include_str!("replacements_extra.csv")
                .split("\n")
                .filter(|line| !line.is_empty())
                .map(|line| {
                    let comma = line.find(",").unwrap();
                    (
                        line[..comma].chars().next().unwrap(),
                        String::from(&line[comma + 1..]),
                    )
                }),
        )
        .collect();

    let mut writer = Writer::from_path("src/replacements.csv").unwrap();
    for (find, mut replace) in replacements {
        // Keep original character accessible.
        if find.is_digit(36) && !find.is_ascii_uppercase() {
            replace.push(find);
        }

        writer.write_record(&[&find.to_string(), &replace]).unwrap();
    }
    writer.flush().unwrap();
}
