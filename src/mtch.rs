use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::radix::Node;
use crate::Type;
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub(crate) struct Match {
    /// The word being matched.
    pub node: &'static Node,
    /// Stores the index in the string when this match was created.
    pub start: usize,
    // Stores the index in the string when this match was completed.
    pub end: usize,
    /// Stores the last matched character.
    pub last: char,
    /// Whether the match was preceded by a separator.
    pub space_before: bool,
    /// Whether the match was followed by a separator.
    pub space_after: bool,
    /// Stores how many spaces appeared within the match, excluding spaces that directly correspond to the pattern.
    pub spaces: u8,
    /// Stores how many replacements took place while matching.
    pub replacements: u8,
}

impl Match {
    /// Combines in a way that the order of matches doesn't matter.
    pub(crate) fn combine(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            spaces: self.spaces.min(other.spaces),
            replacements: self.replacements.min(other.replacements),
            last: self.last.min(other.last),
            ..*self
        }
    }

    pub(crate) fn commit<I: Iterator<Item = char>>(
        &self,
        typ: &mut Type,
        spy: &mut BufferProxyIterator<I>,
        censor_threshold: Type,
        censor_first_character_threshold: Type,
        censor_replacement: char,
    ) {
        //let length = m.end - m.start;
        if (!(self.space_before && self.space_after)
            && self.node.depth > 1
            && self.spaces as usize + 4 > self.node.depth as usize)
            || (self.replacements >= self.node.depth
                && self.node.depth <= 3
                && !self.node.typ.is(Type::SEVERE))
        {
            // Match isn't strong enough.
            return;
        }

        // Apply detection.
        *typ |= self.node.typ
            | if self.replacements >= 2 {
                Type::EVASIVE & Type::MILD
            } else {
                Type::NONE
            };

        if self.node.typ.isnt(censor_threshold) {
            // Match isn't severe enough to censor.
            return;
        }

        // Censor.
        let offset = if self.node.typ.is(censor_first_character_threshold) || self.node.depth == 1 {
            0
        } else {
            1
        };
        spy.censor(self.start + offset..=self.end, censor_replacement);
    }
}

impl PartialEq for Match {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.node, other.node) && self.space_before == other.space_before
    }
}

impl Eq for Match {}

impl Hash for Match {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.node as *const _ as usize);
        state.write_u8(self.space_before as u8);
    }
}
