use crate::buffer_proxy_iterator::BufferProxyIterator;
use crate::trie::Node;
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
    pub begin_separate: bool,
    /// Whether the match was followed by a separator.
    pub end_separate: bool,
    /// Stores how many spaces appeared within the match, excluding spaces that directly correspond to the pattern.
    pub spaces: u8,
    /// Stores how many characters were skipped.
    pub skipped: u8,
    /// Stores how many replacements took place while matching.
    pub replacements: u8,
    /// Stores how many extra repretitions took place while matching.
    pub repetitions: u8,
    /// Stores how many low-confidence replacements took place while matching.
    pub low_confidence_replacements: u8,
}

impl Match {
    /// Combines in a way that the order of matches doesn't matter.
    pub(crate) fn combine(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            spaces: self.spaces.min(other.spaces),
            skipped: self.skipped.min(other.skipped),
            replacements: self.replacements.min(other.replacements),
            low_confidence_replacements: self
                .low_confidence_replacements
                .min(other.low_confidence_replacements),
            repetitions: self.repetitions.min(other.repetitions),
            last: self.last.min(other.last),
            ..*self
        }
    }

    fn confidence(&self) -> i64 {
        let mut confidence: i64 = 0;
        confidence += self.node.depth.max(1).ilog2() as i64;
        confidence += (self.end - self.start).max(1).ilog2() as i64;
        if self.node.depth == 1 {
            confidence += 1;
        } else {
            if !self.begin_separate {
                confidence -= 2;
                if self.node.contains_space {
                    confidence -= 3;
                }
            }
            if !self.end_separate {
                confidence -= 1;
            }
            if !self.begin_separate && !self.end_separate {
                confidence -= 1;
            }
        }
        if self.node.typ.is(Type::SEVERE) {
            confidence += 3;
        } else if self.node.typ.is(Type::MODERATE_OR_HIGHER)
            && (self.node.depth == 1 || self.node.typ.isnt(Type::EVASIVE & Type::SEVERE))
        {
            confidence += 2
        } else if self.node.typ.is(Type::MILD_OR_HIGHER)
            && (self.node.depth == 1
                || self.node.typ.isnt(Type::EVASIVE & Type::MODERATE_OR_HIGHER))
        {
            confidence += 1;
        };
        confidence -= (self.skipped as u16 + self.spaces as u16 + self.replacements as u16 + 1)
            .ilog2() as i64;
        confidence -= (self.low_confidence_replacements + 1).ilog2() as i64;
        if self.node.depth == 2 && self.low_confidence_replacements > 0 {
            // h8
            confidence -= 2;
        }
        if self.node.typ.is(Type::EVASIVE & Type::SEVERE) {
            confidence -= 3;
        } else if self.node.typ.is(Type::EVASIVE & Type::MODERATE_OR_HIGHER) {
            confidence -= 2;
        } else if self.node.typ.is(Type::EVASIVE & Type::MILD) {
            confidence -= 1;
        }
        confidence
    }

    /// Returns whether committed.
    pub(crate) fn commit<I: Iterator<Item = char>>(
        &self,
        typ: &mut Type,
        spy: &mut BufferProxyIterator<I>,
        censor_threshold: Type,
        censor_first_character_threshold: Type,
        censor_replacement: char,
    ) -> bool {
        #[cfg(feature = "trace")]
        print!(
            "Committing {} with begin_separate={}, spaces={}, skipped={}, end_separate={}, depth={}, replacements={}, lcr={}, contains_space={}: ",
            self.node.trace,
            self.begin_separate,
            self.spaces,
            self.skipped,
            self.end_separate,
            self.node.depth,
            self.replacements,
            self.low_confidence_replacements,
            self.node.contains_space
        );

        let confidence = self.confidence();

        if confidence <= 0 {
            #[cfg(feature = "trace")]
            println!("rejected with confidence {confidence}");
            return false;
        }
        #[cfg(feature = "trace")]
        println!("accepted with confidence {confidence}");

        /*
        let too_many_replacements = !(self.begin_separate
            && (self.end_separate
                || (self.spaces == 0
                    && self.node.depth > 2
                    && self.node.typ.is(Type::MODERATE_OR_HIGHER))))
            && self.node.depth > 1
            // In theory, prevents blahsex, but allows blahsexblah.
            && (!(self.end_separate || self.begin_separate) || self.node.depth < 3 || self.spaces.max(self.skipped).max(self.replacements) > 0 || self.node.typ.isnt(Type::MODERATE_OR_HIGHER))
            && self.spaces.max(self.skipped).max(self.replacements) as usize + 4 > self.node.depth as usize;

        let low_confidence_replacements = self.low_confidence_replacements > 0
            && (self.low_confidence_replacements as usize
                > (self.end - self.start).saturating_sub(1) || self.low_confidence_replacements as usize > (self.end - self.start).max(10) / 5 || self.node.depth < 3)
            && self.node.depth > 1;

        let low_confidence_short = self.replacements >= self.node.depth
            && self.node.depth <= 3
            && !self.node.typ.is(Type::SEVERE);

        // Make it so "squirrels word" doesn't contain "s word"
        let low_confidence_special = self.node.contains_space && !self.begin_separate;

        if too_many_replacements
            || low_confidence_replacements
            || low_confidence_short
            || low_confidence_special
        {
            // Match isn't strong enough.
            #[cfg(feature = "trace")]
            println!(
                "(rejected: {} {} {} {})",
                too_many_replacements,
                low_confidence_replacements,
                low_confidence_short,
                low_confidence_special
            );
            return false;
        }
        */

        // Apply detection.
        *typ |= self.node.typ
            | if self.replacements >= 2 {
                Type::EVASIVE & Type::MILD
            } else {
                Type::NONE
            };

        // Decide whether to censor.
        if self.node.typ.is(censor_threshold) {
            // Decide whether to censor the first character.
            let offset =
                if self.node.typ.is(censor_first_character_threshold) || self.node.depth == 1 {
                    0
                } else {
                    1
                };
            spy.censor(self.start + offset..=self.end, censor_replacement);
        }

        true
    }
}

impl PartialEq for Match {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.node, other.node) && self.begin_separate == other.begin_separate
    }
}

impl Eq for Match {}

impl Hash for Match {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.node as *const _ as usize);
        state.write_u8(self.begin_separate as u8);
    }
}
