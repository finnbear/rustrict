use crate::Map;
use crate::Type;
use std::iter::FromIterator;

#[derive(Debug)]
pub(crate) struct Trie {
    pub(crate) root: Node,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub children: Map<char, Node>,
    pub word: bool,
    /// word contains space.
    pub contains_space: bool,
    pub typ: Type,
    pub depth: u8,
    /// Character from parent to self.
    pub last: Option<char>,
    #[cfg(feature = "trace")]
    pub trace: String,
}

impl Trie {
    pub fn add(&mut self, mut word: &str, typ: Type, overwrite: bool) {
        let mut current = &mut self.root;
        let mut contains_space = false;
        if word.starts_with(' ') {
            // Chomp the first space, since what we actually want is to only match separate
            // strings, not only strings that start with a space character.
            contains_space = true;
            word = word.trim_start_matches(' ');
        }
        for (i, c) in word.chars().enumerate() {
            let next = current.children.entry(c);
            contains_space |= c == ' ';
            current = next.or_insert_with(|| Node {
                children: Map::default(),
                word: false,
                contains_space: false,
                typ: Type::NONE,
                depth: (i + 1) as u8,
                last: Some(c),
                #[cfg(feature = "trace")]
                trace: word.chars().take(i + 1).collect(),
            });
        }
        current.word = true;
        if overwrite {
            current.typ = typ;
            current.contains_space = contains_space;
        } else {
            current.typ |= typ;
            current.contains_space |= contains_space;
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
                children: Map::default(),
                word: false,
                contains_space: false,
                typ: Type::NONE,
                depth: 0,
                last: None,
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
