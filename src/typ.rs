use bitflags::bitflags;

bitflags! {
    /// Type is represents a type or severity of inappropriateness.
    /// They can be combined with bitwise operators, and are **not** mutually exclusive.
    pub struct Type: u32 {
        /// Bad words.
        const PROFANE   = 0b0_000_000_000_000_000_111;
        /// Offensive words.
        const OFFENSIVE = 0b0_000_000_000_000_111_000;
        /// Sexual words.
        const SEXUAL    = 0b0_000_000_000_111_000_000;
        /// Mean words.
        const MEAN      = 0b0_000_000_111_000_000_000;
        /// Words intended to evade detection.
        const EVASIVE   = 0b0_000_111_000_000_000_000;
        /// Spam/gibberish/SHOUTING.
        const SPAM      = 0b0_111_000_000_000_000_000;

        /// One of a very small number of safe phases.
        /// Recommended to enforce this on users who repeatedly evade the filter.
        const SAFE      = 0b1_000_000_000_000_000_000;

        /// Not that bad.
        const MILD      = 0b0_001_001_001_001_001_001;
        /// Bad.
        const MODERATE  = 0b0_010_010_010_010_010_010;
        /// Cover your eyes!
        const SEVERE    = 0b0_100_100_100_100_100_100;

        /// Any level; `Type::MILD`, `Type::MODERATE`, or `Type::SEVERE`.
        const MILD_OR_HIGHER = Self::MILD.bits | Self::MODERATE.bits | Self::SEVERE.bits;

        /// Any level in excess of `Type::MILD`.
        const MODERATE_OR_HIGHER = Self::MODERATE.bits | Self::SEVERE.bits;

        /// The default `Type`, meaning profane, offensive, sexual, or severely mean.
        const INAPPROPRIATE = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | (Self::MEAN.bits & Self::SEVERE.bits);

        /// Any type of detection (except SAFE). This will be expanded to cover all future types.
        const ANY = Self::PROFANE.bits | Self::OFFENSIVE.bits | Self::SEXUAL.bits | Self::MEAN.bits | Self::EVASIVE.bits | Self::SPAM.bits;

        /// No type of detection.
        const NONE = 0;
    }
}

const SEVERE_WEIGHT: i8 = 3;
const MODERATE_WEIGHT: i8 = 2;
const MILD_WEIGHT: i8 = 1;

impl Type {
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
            let ret = bits_to_weight((self.bits >> i) & 0b111);
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
        Type { bits: result }
    }
}

impl Default for Type {
    /// Returns a reasonable default for censoring or blocking.
    fn default() -> Self {
        Self::INAPPROPRIATE
    }
}

/// This serves as replacement for Debug that isn't blocked by
/// https://github.com/bitflags/bitflags/issues/218
#[cfg(test)]
impl std::fmt::Display for Type {
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
        if *self & Self::PROFANE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} profane", description((*self & Self::PROFANE).bits()))?;
            count += 1;
        }
        if *self & Self::OFFENSIVE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} offensive",
                description((*self & Self::OFFENSIVE).bits() >> 3)
            )?;
            count += 1;
        }
        if *self & Self::SEXUAL != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} sexual",
                description((*self & Self::SEXUAL).bits() >> 6)
            )?;
            count += 1;
        }
        if *self & Self::MEAN != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} mean", description((*self & Self::MEAN).bits() >> 9))?;
            count += 1;
        }
        if *self & Self::EVASIVE != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{} evasive",
                description((*self & Self::EVASIVE).bits() >> 12)
            )?;
            count += 1;
        }
        if *self & Self::SPAM != Type::empty() {
            if count > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} spam", description((*self & Self::SPAM).bits() >> 15))?;
            count += 1;
        }

        if count == 0 {
            write!(f, "no detections")
        } else {
            write!(f, "")
        }
    }
}
