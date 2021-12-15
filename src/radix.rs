use crate::Type;
use rustc_hash::FxHashMap;
use std::iter::FromIterator;

#[derive(Debug)]
pub(crate) struct Tree {
    pub(crate) root: Node,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub children: FxHashMap<char, Node>,
    pub word: bool,
    pub typ: Type,
    pub depth: u8,
}

impl Tree {
    pub fn add(&mut self, word: &str, typ: Type) {
        let mut current = &mut self.root;
        for (i, c) in word.chars().enumerate() {
            let next = current.children.entry(c);
            current = next.or_insert_with(|| Node {
                children: FxHashMap::default(),
                word: false,
                typ: Type::NONE,
                depth: (i + 1) as u8,
            });
        }
        current.word = true;
        current.typ |= typ;
        debug_assert!(
            !(current.typ.is(Type::ANY) && current.typ.is(Type::SAFE)),
            "if word is Type::SAFE, it cannot be anything else"
        );
    }
}

impl FromIterator<(&'static str, Type)> for Tree {
    fn from_iter<T: IntoIterator<Item = (&'static str, Type)>>(iter: T) -> Self {
        let mut ret = Self {
            root: Node {
                children: FxHashMap::default(),
                word: false,
                typ: Type::NONE,
                depth: 0,
            },
        };
        for (word, typ) in iter.into_iter() {
            ret.add(word, typ);
        }
        ret
    }
}
