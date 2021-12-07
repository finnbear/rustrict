use rustc_hash::FxHashMap;
use std::iter::FromIterator;

#[derive(Debug)]
pub(crate) struct Tree {
    pub(crate) root: Node,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub children: FxHashMap<char, Node>,
    pub weights: [i8; 4],
    pub safe: bool,
    pub depth: u8,
}

impl Tree {
    /// Never set weights and safe=true at the same time.
    pub fn add(&mut self, word: &str, weights: [i8; 4], safe: bool) {
        let mut current = &mut self.root;
        for (i, c) in word.chars().enumerate() {
            let next = current.children.entry(c);
            current = next.or_insert_with(|| Node {
                children: FxHashMap::default(),
                weights: [0; 4],
                safe: false,
                depth: (i + 1) as u8,
            });
        }
        if safe {
            current.safe = safe;
        } else {
            current.weights = weights;
        }
    }
}

impl FromIterator<(&'static str, [i8; 4], bool)> for Tree {
    fn from_iter<T: IntoIterator<Item = (&'static str, [i8; 4], bool)>>(iter: T) -> Self {
        let mut ret = Self {
            root: Node {
                children: FxHashMap::default(),
                weights: [0; 4],
                safe: false,
                depth: 0,
            },
        };
        for (word, weights, safe) in iter.into_iter() {
            ret.add(word, weights, safe);
        }
        ret
    }
}

impl Node {
    pub fn is_word(&self) -> bool {
        self.weights != [0; 4] || self.safe
    }
}
