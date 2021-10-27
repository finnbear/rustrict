use rustc_hash::FxHashMap;
use std::iter::FromIterator;

#[derive(Debug)]
pub(crate) struct Tree {
    pub(crate) root: Node,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub children: FxHashMap<char, Node>,
    pub(crate) weights: [i8; 4],
    pub depth: u8,
    start: char,
    #[cfg(debug_assertions)]
    pub phrase: &'static str,
}

impl Tree {
    pub fn add(&mut self, word: &'static str, weights: [i8; 4]) {
        let mut current = &mut self.root;
        for (i, c) in word.chars().enumerate() {
            let next = current.children.entry(c);
            current = next.or_insert_with(|| Node {
                children: FxHashMap::default(),
                weights: [0; 4],
                depth: (i + 1) as u8,
                start: word.chars().next().unwrap(),
                #[cfg(debug_assertions)]
                phrase: &word[0..=i],
            });
        }
        current.weights = weights;
    }
}

impl FromIterator<(&'static str, [i8; 4])> for Tree {
    fn from_iter<T: IntoIterator<Item = (&'static str, [i8; 4])>>(iter: T) -> Self {
        let mut ret = Self {
            root: Node {
                children: FxHashMap::default(),
                weights: [0; 4],
                depth: 0,
                start: 0 as char,
                #[cfg(debug_assertions)]
                phrase: "",
            },
        };
        for (word, weights) in iter.into_iter() {
            ret.add(word, weights);
        }
        ret
    }
}

impl Node {
    pub fn is_word(&self) -> bool {
        self.weights != [0; 4]
    }
}
