#![cfg_attr(test, feature(test))]

#[cfg(feature = "censor")]
pub(crate) mod buffer_proxy_iterator;
#[cfg(feature = "censor")]
pub(crate) mod censor;
#[cfg(feature = "censor")]
pub(crate) mod feature_cell;
#[cfg(feature = "censor")]
pub(crate) mod mtch;
#[cfg(feature = "censor")]
pub(crate) mod trie;
#[cfg(feature = "censor")]
pub(crate) mod typ;

#[cfg(feature = "context")]
pub(crate) mod context;

#[cfg(feature = "width")]
pub(crate) mod width;

#[cfg(feature = "width")]
pub use width::{trim_to_width, width, width_str};

#[cfg(feature = "censor")]
pub use typ::Type;

#[cfg(feature = "censor")]
pub use censor::{Censor, CensorIter, CensorStr};

// Facilitate experimentation with different hash collections.
#[cfg(feature = "censor")]
pub(crate) type Map<K, V> = rustc_hash::FxHashMap<K, V>;

#[cfg(feature = "censor")]
pub(crate) type Set<V> = rustc_hash::FxHashSet<V>;

#[cfg(feature = "customize")]
pub use censor::add_word;

#[cfg(feature = "context")]
pub use context::{
    BlockReason, Context, ContextProcessingOptions, ContextRateLimitOptions,
    ContextRepetitionLimitOptions,
};

/// Trims whitespace characters from both ends of a string, according to the definition of
/// `crate::is_whitespace`.
pub fn trim_whitespace(s: &str) -> &str {
    // Some characters are effectively whitespace if they are at the beginning of a string.
    // https://www.compart.com/en/unicode/U+0488
    // https://www.compart.com/en/unicode/U+0489
    s.trim_start_matches(|c| is_whitespace(c) || matches!(c, '\u{0488}' | '\u{0489}'))
        .trim_end_matches(is_whitespace)
}

/// Returns true iff the character is effectively whitespace. The definition of whitespace is broader
/// than that of Unicode, because it includes control characters and a few additional blank characters.
pub fn is_whitespace(c: char) -> bool {
    use finl_unicode::categories::CharacterCategories;

    // NOTE: The following characters are not detected by standard means but show up as blank.

    // https://www.compart.com/en/unicode/U+115F
    // https://www.compart.com/en/unicode/U+1160
    // https://www.compart.com/en/unicode/U+2800
    // https://www.compart.com/en/unicode/U+3164
    // https://www.compart.com/en/unicode/U+FFA0
    c.is_whitespace()
        || c.is_other()
        || matches!(
            c,
            '\u{115F}' | '\u{1160}' | '\u{2800}' | '\u{3164}' | '\u{FFA0}'
        )
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]
    extern crate test;

    #[test]
    fn trim_whitespace() {
        // General.
        assert_eq!(crate::trim_whitespace("\u{0020}\u{00A0}\u{2000}\u{2001}\u{2002}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200A}\u{200B}\u{200C}\u{200D}\u{2028}\u{205F}\u{3000}"), "");

        // Extra cases.
        assert_eq!(
            crate::trim_whitespace(" \u{1160} \u{2800} abc \u{3164} \u{FFA0} \t \u{115F} \n "),
            "abc"
        );

        // Special cases.
        assert_eq!(
            crate::trim_whitespace(
                "\u{0488}\u{1160}\u{0489}\u{1160}\u{0488}\u{1160}\u{0489}abc\u{0488}\u{0489}"
            ),
            "abc\u{0488}\u{0489}"
        )
    }

    #[test]
    fn is_whitespace() {
        assert!(crate::is_whitespace(' '));
        assert!(crate::is_whitespace('\u{2800}'));
        assert!(!crate::is_whitespace('a'));
    }
}

doc_comment::doctest!("../README.md");
