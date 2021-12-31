use crate::{trim_whitespace, Censor, Type};

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::time::{Duration, Instant};

/// Context is useful for taking moderation actions on a per-user basis i.e. each user would get
/// their own Context.
pub struct Context {
    history: VecDeque<(String, Instant)>,
    rate_limit: Duration,
    burst: u8,
    burst_used: u8,
    suspicion: u8,
    reports: u8,
    total: u16,
    total_inappropriate: u16,
    muted_until: Option<Instant>,
    only_safe_until: Option<Instant>,
    rate_limited_until: Option<Instant>,
    last_message: Option<Instant>,
}

impl Context {
    /// VecDeque internally adds 1 (resulting in 8), and then finds the next power of 2 (still 8).
    const CAPACITY: usize = 7;

    /// Track repetitions (spam) within this time frame.
    const REPETITION_DURATION: Duration = Duration::from_secs(60);

    /// How many repetitions are tolerated.
    const REPETITION_LIMIT: usize = 3;

    /// Maximum "safe only" timeout.
    const MAX_TIMEOUT: Duration = Duration::from_secs(3600);

    /// rate_limit is minimum time between messages.
    /// burst allows a certain amount of messages beyond the rate limit.
    pub fn new(rate_limit: Duration, burst: u8) -> Self {
        Self {
            history: VecDeque::with_capacity(Self::CAPACITY),
            rate_limit,
            burst,
            burst_used: 0,
            suspicion: 0,
            reports: 0,
            total: 0,
            total_inappropriate: 0,
            only_safe_until: None,
            rate_limited_until: None,
            muted_until: None,
            last_message: None,
        }
    }

    /// Returns None if expired is None or has been reached, resulting in expiry being set to None.
    /// Otherwise, returns duration before expiry.
    fn remaining_duration(expiry: &mut Option<Instant>, now: Instant) -> Option<Duration> {
        if let Some(time) = *expiry {
            if now >= time {
                *expiry = None;
                None
            } else {
                Some(time - now)
            }
        } else {
            None
        }
    }

    /// Takes user message, returns censored message trimmed of whitespace (if it should be sent)
    /// or `BlockReason` (explaining why it should be blocked entirely).
    pub fn process(&mut self, message: String) -> Result<String, BlockReason> {
        let now = Instant::now();
        let elapsed = self.last_message.map(|l| now - l).unwrap_or(Duration::ZERO);

        let suspicion = self.suspicion.max(1).saturating_mul(self.reports.max(1));

        // How convinced are we that the user is a bad actor.
        let is_kinda_sus = suspicion >= 2;
        let is_impostor = suspicion >= 15;

        // Don't give bad actors the benefit of the doubt when it comes to meanness.
        let meanness_threshold = if is_impostor {
            Type::MILD_OR_HIGHER
        } else if is_kinda_sus {
            Type::MODERATE_OR_HIGHER
        } else {
            Type::SEVERE
        };

        let censor_threshold =
            Type::PROFANE | Type::OFFENSIVE | Type::SEXUAL | (Type::MEAN & meanness_threshold);

        // Don't give bad actors the benefit of letting their first character through.
        let censor_first_character_threshold = if is_kinda_sus {
            censor_threshold
        } else {
            // Mainly for protection against the n-word being discernible.
            Type::OFFENSIVE & Type::SEVERE
        };

        let (mut censored, analysis) = Censor::from_str(trim_whitespace(&message))
            .with_censor_threshold(censor_threshold)
            .with_censor_first_character_threshold(censor_first_character_threshold)
            .censor_and_analyze();

        // Censoring can remove certain characters, rendering the prefix or suffix whitespace.
        let trimmed_censored = trim_whitespace(&censored);
        if trimmed_censored.len() < censored.len() {
            censored = String::from(trimmed_censored);
        }

        self.total = self.total.saturating_add(1);
        if analysis.is(Type::INAPPROPRIATE) {
            self.total_inappropriate = self.total_inappropriate.saturating_add(1);
        }

        // Collecting suspicion.
        let type_to_sus = |typ: Type| -> u8 {
            let combined = analysis & typ;
            if combined.is(Type::SEVERE) {
                3
            } else if combined.is(Type::MODERATE) {
                2
            } else if combined.is(Type::MILD) {
                1
            } else {
                0
            }
        };

        // Repetition detection.
        self.history
            .retain(|&(_, t)| now - t < Self::REPETITION_DURATION);
        let mut recent_similar = 0;

        for (recent_message, _) in &self.history {
            if strsim::normalized_levenshtein(&recent_message, &message) >= 2.0 / 3.0 {
                recent_similar += 1;
            }
        }

        let mut new_suspicion = type_to_sus(Type::PROFANE | Type::OFFENSIVE | Type::SEXUAL)
            + type_to_sus(Type::EVASIVE)
            + type_to_sus(Type::SPAM);

        if recent_similar >= 2 {
            // Don't penalize as much for repeated messages, since an innocent user may repeat their
            // message multiple times if it was erroneously detected.
            new_suspicion /= 2;
        }

        if (is_kinda_sus && new_suspicion >= 4) || (is_impostor && new_suspicion >= 2) {
            if let Some(only_safe_until) =
                self.only_safe_until
                    .unwrap_or(now)
                    .checked_add(if self.reports > 0 {
                        Duration::from_secs(10 * 60)
                    } else {
                        Duration::from_secs(5 * 60)
                    })
            {
                self.only_safe_until = Some(only_safe_until.min(now + Self::MAX_TIMEOUT));
            }
        }

        self.suspicion = self.suspicion.saturating_add(new_suspicion);

        let remaining_rate_limit = Self::remaining_duration(&mut self.rate_limited_until, now);

        if let Some(dur) = Self::remaining_duration(&mut self.muted_until, now) {
            Err(BlockReason::Muted(dur))
        } else if censored.is_empty() {
            Err(BlockReason::Empty)
        } else if let Some(dur) = remaining_rate_limit.filter(|_| self.burst_used >= self.burst) {
            Err(BlockReason::Spam(dur))
        } else if recent_similar >= Self::REPETITION_LIMIT {
            Err(BlockReason::Repetitious(recent_similar))
        } else if analysis.is(Type::INAPPROPRIATE & Type::SEVERE) {
            Err(BlockReason::Inappropriate(analysis))
        } else if let Some(dur) = Self::remaining_duration(&mut self.only_safe_until, now)
            .filter(|_| !analysis.is(Type::SAFE))
        {
            Err(BlockReason::Unsafe(dur))
        } else {
            if self.history.len() >= Self::CAPACITY {
                self.history.pop_front();
            }
            self.history.push_back((message, now));
            self.last_message = Some(now);
            if self.rate_limit > Duration::ZERO {
                self.burst_used = if remaining_rate_limit.is_some() {
                    self.burst_used.saturating_add(1)
                } else {
                    self.burst_used.saturating_sub(
                        (elapsed.as_nanos() / self.rate_limit.as_nanos()).min(u8::MAX as u128)
                            as u8,
                    )
                };
                if let Some(rate_limited_until) = self
                    .rate_limited_until
                    .unwrap_or(now)
                    .checked_add(self.rate_limit * (1 + new_suspicion as u32))
                {
                    self.rate_limited_until = Some(rate_limited_until);
                }
            }
            // Forgiveness (minus one suspicion per safe message, and also per minute between messages).
            self.suspicion = self.suspicion.saturating_sub(
                (elapsed.as_secs() / 60).clamp(analysis.is(Type::SAFE) as u64, u8::MAX as u64)
                    as u8,
            );
            Ok(censored)
        }
    }

    /// Manually mute this user's messages for a duration. Overwrites any previous manual mute.
    /// Passing `Duration::ZERO` will therefore un-mute.
    pub fn mute_for(&mut self, duration: Duration) {
        self.muted_until = Some(Instant::now() + duration);
    }

    /// Manually restrict this user's messages to known safe phrases for a duration. Overwrites any
    /// previous manual restriction. Passing `Duration::ZERO` will therefore un-restrict.
    pub fn restrict_for(&mut self, duration: Duration) {
        self.only_safe_until = Some(Instant::now() + duration);
    }

    /// Call if another user "reports" this user's message(s). The function of reports is for
    /// suspicion of bad behavior to be confirmed faster.
    pub fn report(&mut self) {
        self.reports = self.reports.saturating_add(1);
    }

    /// Returns number of reports received via `Self::report()`. It is not guaranteed that the full
    /// range of `usize` of reports will be counted (currently only `u8::MAX` are counted).
    pub fn reports(&self) -> usize {
        self.reports as usize
    }

    /// Returns total number of messages processed. It is not guaranteed that the full
    /// range of `usize` of messages will be counted (currently only `u16::MAX` are counted).
    pub fn total(&self) -> usize {
        self.total as usize
    }

    /// Returns total number of messages processed that were `Type::INAPPROPRIATE`. It is not
    /// guaranteed that the full range of `usize` of messages will be counted (currently only
    /// `u16::MAX` are counted).
    pub fn total_inappropriate(&self) -> usize {
        self.total_inappropriate as usize
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new(Duration::from_secs(5), 3)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum BlockReason {
    /// The particular message was *severely* inappropriate, more specifically, `Type`.
    Inappropriate(Type),
    /// Recent messages were generally inappropriate, and this message isn't on the safe list.
    /// Try again after `Duration`.
    Unsafe(Duration),
    /// This message was too similar to `usize` recent messages.
    Repetitious(usize),
    /// Too many messages per unit time, try again after `Duration`.
    Spam(Duration),
    /// Manually muted for `Duration`.
    Muted(Duration),
    /// Message was, at least after censoring, completely empty.
    Empty,
}

impl BlockReason {
    /// You may display `BlockReason` in any manner you choose, but this will return a reasonable
    /// default warning to send to the user.
    pub fn generic_str(self) -> &'static str {
        match self {
            Self::Inappropriate(_) => "Your message was held for severe profanity",
            Self::Unsafe(_) => "You have been temporarily restricted due to profanity/spam",
            Self::Repetitious(_) => "Your message was too similar to recent messages",
            Self::Spam(_) => "You have been temporarily muted due to excessive frequency",
            Self::Muted(_) => "You have been temporarily muted",
            Self::Empty => "Your message was empty",
        }
    }

    #[deprecated = "use contextual_string"]
    pub fn contextual_str(self) -> String {
        self.contextual_string()
    }

    /// You may display `BlockReason` in any manner you choose, but this will return a reasonable
    /// default warning to send to the user that includes some context (such as how long they are
    /// muted for).
    pub fn contextual_string(self) -> String {
        match self {
            Self::Inappropriate(typ) => String::from(if typ.is(Type::OFFENSIVE) {
                "Your message was held for being highly offensive"
            } else if typ.is(Type::SEXUAL) {
                "Your message was held for being overly sexual"
            } else if typ.is(Type::MEAN) {
                "Your message was held for being overly mean"
            } else {
                "Your message was held for severe profanity"
            }),
            Self::Unsafe(dur) => format!(
                "You have been restricted for {} due to profanity/spam",
                FormattedDuration(dur)
            ),
            Self::Repetitious(count) => {
                format!("Your message was too similar to {} recent messages", count)
            }
            Self::Spam(dur) => format!(
                "You have been muted for {} due to excessive frequency",
                FormattedDuration(dur)
            ),
            Self::Muted(dur) => format!("You have been muted for {}", FormattedDuration(dur)),
            _ => String::from(self.generic_str()),
        }
    }
}

struct FormattedDuration(Duration);

impl Display for FormattedDuration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0 >= Duration::from_secs(3600) {
            write!(f, "{}h", self.0.as_secs() / 3600)
        } else if self.0 >= Duration::from_secs(60) {
            write!(f, "{}m", self.0.as_secs() / 60)
        } else {
            write!(f, "{}s", self.0.as_secs().max(1))
        }
    }
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]

    extern crate test;
    use crate::{Censor, CensorIter, CensorStr, Type};
    use bitflags::_core::ops::Not;
    use std::fs::File;
    use std::io::BufReader;
    use std::time::{Duration, Instant};
    use test::Bencher;

    #[test]
    fn context_inappropriate() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
        assert!(matches!(
            ctx.process(String::from("nigga")),
            Err(BlockReason::Inappropriate(_))
        ));
    }

    #[test]
    fn context_unsafe() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        for _ in 0..30 {
            ctx.report();
        }

        let res = ctx.process(String::from("shit"));
        assert!(matches!(res, Err(BlockReason::Unsafe(_))), "1 {:?}", res);

        let res = ctx.process(String::from("not common message"));
        assert!(matches!(res, Err(BlockReason::Unsafe(_))), "2 {:?}", res);
    }

    #[test]
    fn context_repetitious() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        for _ in 0..Context::REPETITION_LIMIT {
            assert!(ctx.process(String::from("one")).is_ok());
        }

        let res = ctx.process(String::from("onne"));
        assert!(matches!(res, Err(BlockReason::Repetitious(_))), "{:?}", res);
    }

    #[test]
    fn context_spam() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::from_millis(350), 2);

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
        assert_eq!(ctx.process(String::from("two")), Ok(String::from("two")));
        assert_eq!(
            ctx.process(String::from("three")),
            Ok(String::from("three"))
        );
        assert!(matches!(
            ctx.process(String::from("four")),
            Err(BlockReason::Spam(_))
        ));

        std::thread::sleep(Duration::from_secs(2));

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
    }

    #[test]
    fn context_muted() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::ZERO, 0);

        ctx.mute_for(Duration::from_secs(5));

        let res = ctx.process(String::from("hello"));
        assert!(matches!(res, Err(BlockReason::Muted(_))), "{:?}", res);
    }

    #[test]
    fn context_empty() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new(Duration::from_secs(1), 2);
        assert_eq!(ctx.process(String::from("   ")), Err(BlockReason::Empty));
    }
}
