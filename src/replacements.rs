use crate::feature_cell::FeatureCell;
use crate::Map;
use arrayvec::ArrayString;
use lazy_static::lazy_static;
use std::collections::hash_map::Entry;
use std::ops::Deref;

lazy_static! {
    pub(crate) static ref REPLACEMENTS: FeatureCell<Replacements> = FeatureCell::new(Replacements(
        include_str!("replacements.csv")
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                let comma = line.find(',').unwrap();
                (
                    line[..comma].chars().next().unwrap(),
                    ArrayString::from(&line[comma + 1..]).unwrap(),
                )
            })
            .collect()
    ));
}

/// Set of possible interpretations for an input character.
///
/// For example, `A` can be replaced with `a` so the word `apple` matches `Apple`.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Replacements(Map<char, ArrayString<12>>);

impl Default for Replacements {
    fn default() -> Self {
        REPLACEMENTS.deref().deref().clone()
    }
}

impl Replacements {
    /// Empty.
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Allows direct mutable access to the global default set of replacements.
    ///
    /// Prefer the safe API `Censor::with_replacements`.
    ///
    /// # Safety
    ///
    /// You must manually avoid concurrent access/censoring.
    #[cfg(feature = "customize")]
    pub unsafe fn customize_default() -> &'static mut Self {
        REPLACEMENTS.get_mut()
    }

    pub(crate) fn get(&self, src: char) -> Option<&ArrayString<12>> {
        self.0.get(&src)
    }

    /// Adds a new replacement character.
    ///
    /// # Panics
    ///
    /// Panics if the total replacement characters exceed 12 bytes.
    pub fn insert(&mut self, src: char, dst: char) {
        let replacements = self.0.entry(src).or_default();
        if !replacements.contains(dst) {
            replacements.push(dst);
        }
    }

    /// Removes a replacement character.
    pub fn remove(&mut self, src: char, dst: char) {
        if let Entry::Occupied(mut occupied) = self.0.entry(src) {
            let mut filtered = ArrayString::default();
            for c in occupied.get().chars() {
                if c != dst {
                    filtered.push(c);
                }
            }
            if filtered.is_empty() {
                occupied.remove();
            } else {
                occupied.insert(filtered);
            }
        }
    }
}
