#![cfg_attr(test, feature(test))]

use unicode_categories::UnicodeCategories;

#[cfg(feature = "censor")]
pub(crate) mod buffer_proxy_iterator;
#[cfg(feature = "censor")]
pub(crate) mod censor;
#[cfg(feature = "censor")]
pub(crate) mod feature_cell;
#[cfg(feature = "censor")]
pub(crate) mod mtch;
#[cfg(feature = "censor")]
pub(crate) mod radix;
#[cfg(feature = "censor")]
pub(crate) mod typ;

#[cfg(feature = "context")]
pub(crate) mod context;

#[cfg(feature = "censor")]
pub use typ::Type;

#[cfg(feature = "censor")]
pub use censor::{Censor, CensorIter, CensorStr};

#[cfg(feature = "customize")]
pub use censor::add_word;

#[cfg(feature = "context")]
pub use context::{BlockReason, Context};

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

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]
    extern crate test;

    #[test]
    fn trim_whitespace() {
        assert_eq!(
            crate::trim_whitespace(" \u{2800} abc  \u{3164} \t \n  "),
            "abc"
        );
    }

    #[test]
    fn is_whitespace() {
        assert!(crate::is_whitespace(' '));
        assert!(crate::is_whitespace('\u{2800}'));
        assert!(!crate::is_whitespace('a'));
    }
}

use doc_comment::doctest;
doctest!("../README.md");
