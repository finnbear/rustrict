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
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Banned(Set<char>);

impl Default for Banned {
    fn default() -> Self {
        BANNED.deref().deref().clone()
    }
}

impl Banned {
    /// Empty.
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Allows direct mutable access to the global default set of banned characters.
    ///
    /// # Safety
    ///
    /// You must manually avoid concurrent access/censoring.
    #[cfg(feature = "customize")]
    pub unsafe fn customize_default() -> &'static mut Self {
        BANNED.get_mut()
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
