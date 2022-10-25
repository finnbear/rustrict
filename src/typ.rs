use bitflags::bitflags;
use std::fmt::Debug;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not};

bitflags! {
    struct TypeRepr: u32 {
        const PROFANE   = 0b0_000_000_000_000_000_111;
        const OFFENSIVE = 0b0_000_000_000_000_111_000;
        const SEXUAL    = 0b0_000_000_000_111_000_000;
        const MEAN      = 0b0_000_000_111_000_000_000;
        const EVASIVE   = 0b0_000_111_000_000_000_000;
        const SPAM      = 0b0_111_000_000_000_000_000;

        const SAFE      = 0b1_000_000_000_000_000_000;

        const MILD      = 0b0_001_001_001_001_001_001;
        const MODERATE  = 0b0_010_010_010_010_010_010;
        const SEVERE    = 0b0_100_100_100_100_100_100;

        const MILD_OR_HIGHER = Self::MILD.bits | Self::MODERATE.bits | Self::SEVERE.bits;
        const MODERATE_OR_HIGHER = Self::MODERATE.bits | Self::SEVERE.bits;
        const INAPPROPRIATE = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | (Self::MEAN.bits & Self::SEVERE.bits);

        const ANY = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | Self::MEAN.bits | Self::EVASIVE.bits | Self::SPAM.bits;
        const NONE = 0;
    }
}

/// Type is represents a type or severity of inappropriateness.
/// They can be combined with bitwise AND and OR operators, and are **not** mutually exclusive.
///
/// For example, the following means profane or at-least moderately mean:
/// `Type::PROFANE | (Type::MEAN & Type::MODERATE_OR_HIGHER)`
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Type(TypeRepr);

const SEVERE_WEIGHT: i8 = 3;
const MODERATE_WEIGHT: i8 = 2;
const MILD_WEIGHT: i8 = 1;

impl Type {
    /// Bad words.
    pub const PROFANE: Self = Self(TypeRepr::PROFANE);

    /// Offensive words.
    pub const OFFENSIVE: Self = Self(TypeRepr::OFFENSIVE);

    /// Sexual words.
    pub const SEXUAL: Self = Self(TypeRepr::SEXUAL);

    /// Mean words.
    pub const MEAN: Self = Self(TypeRepr::MEAN);

    /// Words intended to evade detection.
    pub const EVASIVE: Self = Self(TypeRepr::EVASIVE);

    /// Spam/gibberish/SHOUTING.
    pub const SPAM: Self = Self(TypeRepr::SPAM);

    /// One of a very small number of safe phases.
    /// Recommended to enforce this on users who repeatedly evade the filter.
    pub const SAFE: Self = Self(TypeRepr::SAFE);

    /// Not that bad.
    pub const MILD: Self = Self(TypeRepr::MILD);

    /// Bad.
    pub const MODERATE: Self = Self(TypeRepr::MODERATE);

    /// Cover your eyes!
    pub const SEVERE: Self = Self(TypeRepr::SEVERE);

    /// Any level; `Type::MILD`, `Type::MODERATE`, or `Type::SEVERE`.
    pub const MILD_OR_HIGHER: Self = Self(TypeRepr::MILD_OR_HIGHER);

    /// Any level in excess of `Type::MILD`.
    pub const MODERATE_OR_HIGHER: Self = Self(TypeRepr::MODERATE_OR_HIGHER);

    /// The default `Type`, meaning profane, offensive, sexual, or severely mean.
    pub const INAPPROPRIATE: Self = Self(TypeRepr::INAPPROPRIATE);

    /// Any type of detection (except SAFE). This will be expanded to cover all future types.
    pub const ANY: Self = Self(TypeRepr::ANY);

    /// No type of detection.
    pub const NONE: Self = Self(TypeRepr::NONE);

    /// Number of weights.
    pub(crate) const WEIGHT_COUNT: usize = 5;
    /// Bits per weight;
    const WEIGHT_BITS: usize = 3;

    /// Returns `true` if and only if self, the analysis result, meets the given threshold.
    pub fn is(self, threshold: Self) -> bool {
        self & threshold != Type::NONE
    }

    /// Logical opposite of `Self::is`.
    pub fn isnt(self, threshold: Self) -> bool {
        self & threshold == Type::NONE
    }

    #[deprecated(note = "this is for backwards-compatibility, use Type::NONE instead")]
    pub fn empty() -> Self {
        Self::NONE
    }

    #[deprecated(note = "this is for backwards-compatibility, compare with Type::NONE instead")]
    pub fn is_empty(self) -> bool {
        self.0.is_empty()
    }

    #[deprecated(note = "this is for backwards-compatibility, there is no replacement")]
    pub fn bits(self) -> u32 {
        self.0.bits
    }

    #[allow(dead_code)]
    pub(crate) fn to_weights(self) -> [i8; Self::WEIGHT_COUNT] {
        fn bits_to_weight(bits: u32) -> i8 {
            if bits == 0 {
                0
            } else if bits & 0b1 != 0 {
                MILD_WEIGHT
            } else if bits & 0b10 != 0 {
                MODERATE_WEIGHT
            } else {
                SEVERE_WEIGHT
            }
        }

        let mut i = 0;
        [0; Self::WEIGHT_COUNT].map(|_| {
            let ret = bits_to_weight((self.0.bits >> i) & 0b111);
            i += Self::WEIGHT_BITS;
            ret
        })
    }

    pub(crate) fn from_weights(weights: &[i8; Self::WEIGHT_COUNT]) -> Type {
        let mut result = 0;
        for (i, &weight) in weights.iter().enumerate() {
            let severity: u32 = if weight >= SEVERE_WEIGHT {
                0b100
            } else if weight == MODERATE_WEIGHT {
                0b010
            } else if weight == MILD_WEIGHT {
                0b001
            } else {
                0 // none
            };

            result |= severity << (i * Self::WEIGHT_BITS)
        }
        Self(TypeRepr { bits: result })
    }
}

impl Default for Type {
    /// Returns a reasonable default for censoring or blocking.
    fn default() -> Self {
        Self::INAPPROPRIATE
    }
}

impl BitAnd for Type {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        // Delegate to bitflags
        Self(self.0.bitand(rhs.0))
    }
}

impl BitOr for Type {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        // Delegate to bitflags
        Self(self.0.bitor(rhs.0))
    }
}

impl BitAndAssign for Type {
    fn bitand_assign(&mut self, rhs: Self) {
        // Delegate to bitflags
        self.0.bitand_assign(rhs.0)
    }
}

impl BitOrAssign for Type {
    fn bitor_assign(&mut self, rhs: Self) {
        // Delegate to bitflags
        self.0.bitor_assign(rhs.0)
    }
}

impl Not for Type {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(self.0.not())
    }
}

/// Note: Can't impl directly on TypeRepr due to https://github.com/bitflags/bitflags/issues/218
impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn description(bits: u32) -> &'static str {
            if bits & 0b100 != 0 {
                "severely"
            } else if bits & 0b010 != 0 {
                "moderately"
            } else if bits & 0b001 != 0 {
                "mildly"
            } else {
                "not"
            }
        }
        let mut count = 0;
        if *self & Self::PROFANE != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} profane",
                description((*self & Self::PROFANE).0.bits())
            )?;
            count += 1;
        }
        if *self & Self::OFFENSIVE != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} offensive",
                description((*self & Self::OFFENSIVE).0.bits() >> 3)
            )?;
            count += 1;
        }
        if *self & Self::SEXUAL != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} sexual",
                description((*self & Self::SEXUAL).0.bits() >> 6)
            )?;
            count += 1;
        }
        if *self & Self::MEAN != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} mean",
                description((*self & Self::MEAN).0.bits() >> 9)
            )?;
            count += 1;
        }
        if *self & Self::EVASIVE != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} evasive",
                description((*self & Self::EVASIVE).0.bits() >> 12)
            )?;
            count += 1;
        }
        if *self & Self::SPAM != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} spam",
                description((*self & Self::SPAM).0.bits() >> 15)
            )?;
            count += 1;
        }
        if *self & Self::SAFE != Self::NONE {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "safe")
        } else if count == 0 {
            write!(f, "no detections")
        } else {
            write!(f, "")
        }
    }
}
