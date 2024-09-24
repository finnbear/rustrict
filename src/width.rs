use crate::is_whitespace;
use std::str::from_utf8;

const MODE_WIDTH: u8 = 10;

lazy_static::lazy_static! {
    static ref WIDTHS: Vec<(char, u8)> = {
        use std::io::Read;
        // Format of this file is documented in character_analyzer.rs
        let mut raw = include_bytes!("character_widths.bin").as_slice();

        // First byte is mode length.
        let mut mode = [0u8];
        raw.read(&mut mode).unwrap();
        let mode = mode[0];

        assert_eq!(mode, MODE_WIDTH);

        let mut widths = Vec::new();

        while !raw.is_empty() {
            // Read one UTF-8 character.
            // TODO: Once stable, use: utf8_char_width(raw[0])
            let s = from_utf8(&raw[..1])
                .or_else(|_| from_utf8(&raw[..2]))
                .or_else(|_| from_utf8(&raw[..3]))
                .or_else(|_| from_utf8(&raw[..4]))
                .unwrap();
            let c = s.chars().next().unwrap();
            raw = &raw[c.len_utf8()..];

            // After character comes a byte of length.
            let mut len = [0u8];
            raw.read(&mut len).unwrap();
            let len = len[0];

            widths.push((c, len));
        }

        widths
    };
}

/// Returns an estimate of the worst-case display width in milli-`m`'s (thousandths of the
/// the width of an `m` character).
///
/// For example, `width('m')` returns 1000 and `width('\u{FDFD}')` returns 10300 (wouldn't you like
/// to know if your user's text is 10.3X longer per character than you might have expected?).
///
/// Precision is not necessarily 1 milli-`m` (currently, it is 100 milli-`m`'s).
#[cfg_attr(doc, doc(cfg(feature = "width")))]
pub fn width(c: char) -> usize {
    let width = match WIDTHS.binary_search_by_key(&c, |&(c, _)| c) {
        Ok(idx) => WIDTHS[idx].1,
        Err(_) => MODE_WIDTH,
    } as usize;

    width * 100
}

/// Convenience method for getting the width, in `m`'s, of an entire string.
///
/// Warning: If the width overflows, the result is undefined (e.g. panic or overflow).
#[cfg_attr(doc, doc(cfg(feature = "width")))]
pub fn width_str(s: &str) -> usize {
    s.chars().map(|c| width(c) / 100).sum::<usize>() / 10
}

/// How text is expected to be displayed.
///
/// Eventually, `BreakWord` will be supported.
#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum WordBreak {
    // TODO: BreakWord
    BreakAll,
}

/// Like `width_str` but computes the width of the max unbroken (no line break) part of the string.
///
/// In certain cases, not even CSS's `word-break: break-all;` (or equivalents) will be able to
/// break a string, so it's good to know how long the lines might get.
///
/// For example, try selecting the following unbroken part: ௌௌௌௌ
pub fn width_str_max_unbroken(s: &str, _word_break: WordBreak) -> usize {
    let mut start = 0;
    break_all_linebreaks(&s)
        .map(|p| {
            let unbroken = &s[start..p];
            start = p;
            width_str(unbroken.trim_end_matches(is_whitespace))
        })
        .max()
        .unwrap_or(0)
}

// TODO unicode-linebreak = { version = "0.1.5", optional = true }

fn break_all_linebreaks(s: &str) -> impl Iterator<Item = usize> + '_ {
    use finl_unicode::categories::{CharacterCategories, MinorCategory};

    use itertools::Itertools;
    s.char_indices()
        .tuple_windows()
        .filter_map(|((_, c1), (p, c2))| {
            let c1 = c1.get_minor_category();
            let c2 = c2.get_minor_category();
            let break_all = !matches!(c1, MinorCategory::Mn | MinorCategory::Mc)
                && !matches!(c2, MinorCategory::Mn | MinorCategory::Mc);
            if break_all
                || [c1, c2]
                    .into_iter()
                    .any(|c| matches!(c, MinorCategory::Zs | MinorCategory::Zl))
            {
                Some(p)
            } else {
                None
            }
        })
        .chain(std::iter::once(s.len()))
}

/// Trims a string to a maximum number of `m`'s. A budget of 5 would allow five m, or more narrower
/// characters, or fewer wider characters.
pub fn trim_to_width(s: &str, mut budget: usize) -> &str {
    // Convert to milli-`m`'s.
    budget *= 10;
    for (idx, c) in s.char_indices() {
        match budget.checked_sub(width(c) / 100) {
            Some(new_budget) => budget = new_budget,
            None => return &s[..idx],
        }
    }
    return s;
}

#[cfg(test)]
mod test {
    use crate::width::{trim_to_width, width_str, WordBreak};
    use crate::{width, width_str_max_unbroken, CensorStr};
    use serial_test::serial;

    /*
    #[test]
    pub fn i() {
        assert_eq!(width('i'), 600);
    }
     */

    #[test]
    pub fn unbroken() {
        let tests = [
            ("", 0),
            ("m", 1),
            ("mm", 1),
            ("m m", 1),
            ("m     m", 1),
            ("mm m", 1),
            ("m mm", 1),
            ("m;m", 1),
        ];
        for (s, w) in tests {
            assert_eq!(width_str_max_unbroken(s, WordBreak::BreakAll), w, "{s} {w}");
        }
    }

    #[test]
    pub fn m() {
        assert_eq!(width('m'), 1000);
    }

    #[test]
    pub fn fdfd() {
        // https://commons.wikimedia.org/wiki/File:Lateef_unicode_U%2BFDFD_2020-03-09_122519.png
        assert_eq!(width('\u{FDFD}'), 10300)
    }

    #[test]
    pub fn three_em_dash() {
        assert!(width('⸻') >= 2500);
    }

    #[test]
    pub fn lattice() {
        assert!(width('𒐫') >= 3000);
    }

    #[test]
    pub fn cuneiform() {
        assert!(width('𒈙') >= 3000);
    }

    #[test]
    pub fn javanese() {
        assert!(width('꧅') >= 1500);
    }

    #[test]
    pub fn tamil() {
        assert_eq!(
            width_str_max_unbroken("abc ௌௌௌௌ def", WordBreak::BreakAll),
            10
        );
        assert_eq!(width_str_max_unbroken("abc ௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌௌ", WordBreak::BreakAll), 345);
    }

    #[test]
    pub fn emoji() {
        assert_eq!(width_str("😀🐿"), 4);
    }

    #[test]
    pub fn cjk() {
        assert_eq!(width_str("大はㅂ"), 6)
    }

    #[test]
    pub fn string() {
        //assert_eq!(width_str("abc‱Ǆဪ"), 7);
        assert_eq!(width_str("abc‱Ǆဪ"), 8);
    }

    #[test]
    #[serial]
    pub fn tall() {
        assert_eq!("a꧁a".censor(), "aa");
    }

    #[test]
    #[serial]
    pub fn trim() {
        assert_eq!(trim_to_width("aa", 0), "");
        assert_eq!(trim_to_width("mmm", 1), "m");
        assert_eq!(trim_to_width("mmm", 2), "mm");
        assert_eq!(trim_to_width("mmm", 3), "mmm");
        assert_eq!(trim_to_width("mmm", 4), "mmm");

        let mut s = String::new();
        for u in 0..10000 {
            if let Some(c) = char::from_u32(u) {
                s.push(c);
            }
        }
        for b in 0..1000 {
            let t = trim_to_width(&s, b);
            let w = width_str(t);
            assert!(w <= b);
            assert!(w + 15 >= b)
        }
    }
}
