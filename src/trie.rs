use crate::feature_cell::FeatureCell;
use crate::Map;
use crate::Type;
use lazy_static::lazy_static;
use std::ops::Deref;

lazy_static! {
    pub(crate) static ref TRIE: FeatureCell<Trie> = FeatureCell::new(
        include_str!("profanity.csv")
            .lines()
            .skip(1)
            .map(|line| {
                let mut split = line.split(',');
                (
                    split.next().unwrap(),
                    Type::from_weights(
                        &[0; Type::WEIGHT_COUNT]
                            .map(|_| split.next().expect(line).parse().unwrap()),
                    ),
                )
            })
            .chain(
                include_str!("safe.txt")
                    .lines()
                    .filter(|line| !line.is_empty() && !line.starts_with('#'))
                    .map(|line| { (line, Type::SAFE) })
            )
            .chain(
                include_str!("false_positives.txt")
                    .lines()
                    .filter(|line| !line.is_empty())
                    .map(|line| { (line, Type::NONE) })
            )
            .collect()
    );
}

/// Efficiently stores profanity, false positives, and safe words.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Trie {
    pub(crate) root: Node,
}

impl Default for Trie {
    fn default() -> Self {
        TRIE.deref().deref().clone()
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    /// Empty.
    pub fn new() -> Self {
        Self {
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
        }
    }

    /// Allows direct mutable access to the global default trie of words.
    ///
    /// Prefer the safe API `Censor::with_trie`.
    ///
    /// # Safety
    ///
    /// You must manually avoid concurrent access/censoring.
    #[cfg(feature = "customize")]
    #[cfg_attr(doc, doc(cfg(feature = "customize")))]
    pub unsafe fn customize_default() -> &'static mut Self {
        TRIE.get_mut()
    }

    /// Adds a word, with the given type. The type can be `Type::SAFE`, or a combination of `Type::PROFANE`,
    /// `Type::Sexual`, `Type::Offensive`, `Type::Mean`, `Type::Mild`, `Type::Moderate`, and `Type::Severe`,
    /// but NOT both (can't be safe and unsafe).
    ///
    /// It is recommended to use all lower-case, which will match both cases. Upper-case characters will
    /// only match upper-case.
    ///
    /// # Warning
    ///
    /// Any profanity words added this way will not support false positives. For example, if you add the word
    /// "field," you can expect "cornfield" to be detected as well, unless you call `add_word("cornfield", Type::None)`.
    pub fn set(&mut self, word: &str, typ: Type) {
        self.add(word, typ, true);
    }

    fn add(&mut self, mut word: &str, typ: Type, overwrite: bool) {
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
        let mut ret = Self::new();
        for (word, typ) in iter.into_iter() {
            ret.add(word, typ, false);
        }
        ret
    }
}
