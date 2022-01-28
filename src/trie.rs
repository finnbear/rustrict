use crate::Type;
use rustc_hash::FxHashMap;
use std::iter::FromIterator;

#[derive(Debug)]
pub(crate) struct Trie {
    pub(crate) root: Node,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub children: FxHashMap<char, Node>,
    pub word: bool,
    pub typ: Type,
    pub depth: u8,
    #[cfg(feature = "trace")]
    pub trace: String,
}

impl Trie {
    pub fn add(&mut self, word: &str, typ: Type, overwrite: bool) {
        let mut current = &mut self.root;
        for (i, c) in word.chars().enumerate() {
            let next = current.children.entry(c);
            current = next.or_insert_with(|| Node {
                children: FxHashMap::default(),
                word: false,
                typ: Type::NONE,
                depth: (i + 1) as u8,
                #[cfg(feature = "trace")]
                trace: word.chars().take(i + 1).collect(),
            });
        }
        current.word = true;
        if overwrite {
            current.typ = typ;
        } else {
            current.typ |= typ;
        }
        debug_assert!(
            !(current.typ.is(Type::ANY) && current.typ.is(Type::SAFE)),
            "if word is Type::SAFE, it cannot be anything else"
        );
    }
}

impl FromIterator<(&'static str, Type)> for Trie {
    fn from_iter<T: IntoIterator<Item = (&'static str, Type)>>(iter: T) -> Self {
        let mut ret = Self {
            root: Node {
                children: FxHashMap::default(),
                word: false,
                typ: Type::NONE,
                depth: 0,
                #[cfg(feature = "trace")]
                trace: String::new(),
            },
        };
        for (word, typ) in iter.into_iter() {
            ret.add(word, typ, false);
        }
        ret
    }
}
