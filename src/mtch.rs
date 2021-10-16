use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::radix::Node;
use crate::{weights_to_type, Type};
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub(crate) struct Match {
    pub(crate) node: &'static Node,
    /// stores the index in the string when this match was created.
    /// this is ignored by Eq and Hash operations
    pub(crate) start: usize,
    // Stores the index in the string when this match was completed.
    pub(crate) end: usize,
    /// Stores the last matched character.
    pub(crate) last: char,
    /// Whether the match was preceded by a separator.
    pub(crate) space_before: bool,
    /// Whether the match was followed by a separator.
    pub(crate) space_after: bool,
    /// Stores how many spaces appeared within the match, excluding spaces that directly correspond to the pattern.
    pub(crate) spaces: u8,
}

impl Match {
    /// Combines in a way that the order of matches doesn't matter.
    pub(crate) fn combine(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            spaces: self.spaces.min(other.spaces),
            last: self.last.min(other.last),
            ..*self
        }
    }

    pub(crate) fn commit<I: Iterator<Item = char>>(
        &self,
        weights: &mut [i8; 4],
        spy: &BufferProxyIterator<I>,
        censor_threshold: Type,
        censor_first_character_threshold: Type,
        censor_replacement: char,
    ) {
        //let length = m.end - m.start;
        if !(self.space_before && self.space_after)
            && self.spaces as usize + 4 > self.node.depth as usize
        {
            // Match isn't strong enough.
            return;
        }

        let typ = weights_to_type(&self.node.weights);

        if typ.isnt(censor_threshold) {
            // Match isn't severe enough.
            return;
        }

        // Censor.
        let offset = if typ.is(censor_first_character_threshold) {
            0
        } else {
            1
        };
        spy.censor(self.start + offset..=self.end, censor_replacement);

        // Apply weights.
        for (i, weight) in self.node.weights.iter().enumerate() {
            weights[i] = weights[i].saturating_add(*weight);
        }
    }
}

impl PartialEq for Match {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.node, other.node)
    }
}

impl Eq for Match {}

impl Hash for Match {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.node as *const _ as usize);
    }
}
