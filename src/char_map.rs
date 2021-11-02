use rustc_hash::FxHashMap;
use std::mem;

#[derive(Debug)]
pub(crate) enum CharMap<V> {
    // All keys are common.
    Common([Option<Box<V>>; 26]),
    // Some keys are rare.
    Rare(FxHashMap<char, V>),
}

impl<V> CharMap<V> {
    pub fn new() -> Self {
        Self::Common(Default::default())
    }

    fn to_common_idx(key: char) -> Option<usize> {
        /*
        ('a'..='z')
            .contains(&key)
            .then(|| key as usize - 'a' as usize)
        */

        // A bit faster.
        ('a' <= key && key <= 'z').then(|| key as usize - 'a' as usize)
    }

    fn from_common_idx(idx: usize) -> char {
        (idx as u8 + 'a' as u8) as char
    }

    pub fn insert(&mut self, key: char, value: V) {
        let common_idx = Self::to_common_idx(key);

        if common_idx.is_none() {
            if let Self::Common(arr) = self {
                let mut map = FxHashMap::default();

                for (idx, value) in arr.into_iter().enumerate() {
                    if let Some(value) = mem::take(value) {
                        map.insert(Self::from_common_idx(idx), *value);
                    }
                }

                *self = Self::Rare(map);
            }
        }

        match self {
            Self::Common(arr) => arr[common_idx.unwrap()] = Some(Box::new(value)),
            Self::Rare(map) => {
                map.insert(key, value);
            }
        }
    }

    pub fn get(&self, key: char) -> Option<&V> {
        match self {
            Self::Common(arr) => Self::to_common_idx(key).and_then(|k| arr[k].as_deref()),
            Self::Rare(map) => map.get(&key),
        }
    }

    pub fn get_mut(&mut self, key: char) -> Option<&mut V> {
        match self {
            Self::Common(arr) => {
                if let Some(k) = Self::to_common_idx(key) {
                    arr[k].as_deref_mut()
                } else {
                    None
                }
            }
            Self::Rare(map) => map.get_mut(&key),
        }
    }
}
