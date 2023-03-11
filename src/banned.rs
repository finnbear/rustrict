use crate::feature_cell::FeatureCell;
use crate::Set;
use lazy_static::lazy_static;
use std::ops::Deref;

lazy_static! {
    pub(crate) static ref BANNED: FeatureCell<Banned> = FeatureCell::new(Banned(
        include_str!("banned_chars.txt")
            .lines()
            .filter(|s| s.starts_with("U+"))
            .map(|s| {
                u32::from_str_radix(&s[2..], 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap()
            })
            .collect()
    ));
}

/// Set of character to strip from input without replacement.
#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Banned(Set<char>);

impl Default for Banned {
    fn default() -> Self {
        BANNED.deref().deref().clone()
    }
}

// TODO: Export this for users.
#[allow(unused)]
impl Banned {
    /// Empty.
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub(crate) fn contains(&self, c: char) -> bool {
        self.0.contains(&c)
    }

    /// Adds a banned character.
    pub fn insert(&mut self, c: char) {
        self.0.insert(c);
    }

    /// Removes a banned character.
    pub fn remove(&mut self, c: char) {
        self.0.remove(&c);
    }
}
