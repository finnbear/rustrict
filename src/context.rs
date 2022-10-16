use crate::{trim_whitespace, Censor, Type};

use crate::censor::should_skip_censor;
use std::collections::VecDeque;
use std::fmt::{self, Debug, Display, Formatter};
use std::num::{NonZeroU16, NonZeroUsize};
use std::time::{Duration, Instant};

/// Context is useful for taking moderation actions on a per-user basis i.e. each user would get
/// their own Context.
#[derive(Clone)]
pub struct Context {
    history: VecDeque<(String, Instant)>,
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

impl Debug for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Don't debug history field.
        f.debug_struct("Context")
            .field("burst_used", &self.burst_used)
            .field("suspicion", &self.suspicion)
            .field("reports", &self.reports)
            .field("total", &self.total)
            .field("total_inappropriate", &self.total_inappropriate)
            .field("muted_until", &self.muted_until)
            .field("only_safe_until", &self.only_safe_until)
            .field("rate_limited_until", &self.rate_limited_until)
            .field("last_message", &self.last_message)
            .finish_non_exhaustive()
    }
}

/// Options for customizing `Context::process_with_options`. Always initialize with ..Default::default(),
/// as new fields may be added in the future.
#[derive(Clone, Debug)]
pub struct ContextProcessingOptions {
    /// Block messages if the user has been manually muted.
    pub block_if_muted: bool,
    /// Block messages if they are empty (after whitespace is trimmed, if applicable).
    pub block_if_empty: bool,
    /// Block messages, as opposed to censoring, if severe inappropriateness is detected.
    pub block_if_severely_inappropriate: bool,
    /// Block all messages if they are unsafe (useful for implementing moderator-activated "safe mode").
    /// Note that unsafe messages from certain users may also be blocked automatically.
    pub safe_mode_until: Option<Instant>,
    /// Character count (or, with the `width` feature, number of `m`-equivalent widths).
    ///
    /// Messages will be trimmed to fit.
    pub character_limit: Option<NonZeroUsize>,
    /// Rate-limiting options.
    pub rate_limit: Option<ContextRateLimitOptions>,
    /// Block messages if they are very similar to this many previous message.
    pub repetition_limit: Option<ContextRepetitionLimitOptions>,
    /// Maximum automatic "safe" timeouts can last. If set too high, users have more time/incentive to
    /// try and find ways around the system. If zero, "safe" timeouts won't be used.
    pub max_safe_timeout: Duration,
    /// Trim whitespace from beginning and end before returning censored output.
    pub trim_whitespace: bool,
}

impl Default for ContextProcessingOptions {
    fn default() -> Self {
        Self {
            block_if_muted: true,
            block_if_empty: true,
            block_if_severely_inappropriate: true,
            safe_mode_until: None,
            character_limit: Some(NonZeroUsize::new(2048).unwrap()),
            rate_limit: Some(ContextRateLimitOptions::default()),
            repetition_limit: Some(ContextRepetitionLimitOptions::default()),
            max_safe_timeout: Duration::from_secs(30 * 60),
            trim_whitespace: true,
        }
    }
}

/// Options that control rate-limiting.
#[derive(Clone, Debug)]
pub struct ContextRateLimitOptions {
    /// Minimum time between messages (zero means infinite rate, 2s means 0.5 messages per second).
    pub limit: Duration,
    /// Allows a certain amount of messages beyond the rate limit.
    pub burst: u8,
    /// Count a message against the rate limit up to 3 times, once for each unit of this many characters.
    ///
    /// If the `width` feature is enabled, the length of the text is interpreted as the number
    /// of `m`'s it would take to reach the same length, or the number of characters, whichever
    /// is higher.
    pub character_limit: Option<NonZeroU16>,
}

impl Default for ContextRateLimitOptions {
    fn default() -> Self {
        Self {
            limit: Duration::from_secs(5),
            burst: 3,
            character_limit: Some(NonZeroU16::new(16).unwrap()),
        }
    }
}

impl ContextRateLimitOptions {
    /// Alternate defaults for slow mode.
    pub fn slow_mode() -> Self {
        Self {
            limit: Duration::from_secs(10),
            burst: 2,
            character_limit: Some(NonZeroU16::new(10).unwrap()),
        }
    }
}

/// Options that control repetition-limiting.
#[derive(Clone, Debug)]
pub struct ContextRepetitionLimitOptions {
    /// How many recent strings can be similar before blocking ensues.
    pub limit: u8,
    /// How long recent input is remembered for.
    pub memory: Duration,
    /// Normalized levenshtein threshold to consider "too similar."
    pub similarity_threshold: f32,
}

impl Default for ContextRepetitionLimitOptions {
    fn default() -> Self {
        Self {
            limit: 3,
            memory: Duration::from_secs(60),
            similarity_threshold: 2.0 / 3.0,
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            history: VecDeque::new(),
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
        self.process_with_options(message, &ContextProcessingOptions::default())
    }

    /// Takes user message, returns censored message trimmed of whitespace (if it should be sent)
    /// or `BlockReason` (explaining why it should be blocked entirely).
    ///
    /// Takes a set of options for fine-tuning the processing.
    pub fn process_with_options(
        &mut self,
        message: String,
        options: &ContextProcessingOptions,
    ) -> Result<String, BlockReason> {
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

        let (mut censored, analysis) = Censor::from_str(&message)
            .with_censor_threshold(censor_threshold)
            .with_censor_first_character_threshold(censor_first_character_threshold)
            .censor_and_analyze();

        let mut censored_str = if should_skip_censor(&message) {
            message.as_str()
        } else {
            censored.as_str()
        };

        if let Some(character_limit) = options.character_limit {
            #[cfg(feature = "width")]
            {
                censored_str = crate::trim_to_width(censored_str, character_limit.get());
            }
            if let Some((limit, _)) = censored_str.char_indices().nth(character_limit.get()) {
                censored_str = &censored_str[..limit];
            }
        }

        if options.trim_whitespace {
            censored_str = trim_whitespace(censored_str);
        }

        if censored_str.len() < censored.len() {
            // Something was trimmed, must must re-allocate.
            censored = String::from(censored_str);
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
        let mut recent_similar = 0;

        if let Some(opts) = options.repetition_limit.as_ref() {
            self.history.retain(|&(_, t)| now - t < opts.memory);

            for (recent_message, _) in &self.history {
                if strsim::normalized_levenshtein(recent_message, &message)
                    >= opts.similarity_threshold as f64
                {
                    recent_similar += 1;
                }
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

        if ((is_kinda_sus && new_suspicion >= 4) || (is_impostor && new_suspicion >= 2))
            && !options.max_safe_timeout.is_zero()
        {
            if let Some(only_safe_until) =
                self.only_safe_until
                    .unwrap_or(now)
                    .checked_add(if self.reports > 0 {
                        Duration::from_secs(10 * 60)
                    } else {
                        Duration::from_secs(5 * 60)
                    })
            {
                self.only_safe_until = Some(only_safe_until.min(now + options.max_safe_timeout));
            }
        }

        self.suspicion = self.suspicion.saturating_add(new_suspicion);

        let remaining_rate_limit = Self::remaining_duration(&mut self.rate_limited_until, now);

        if let Some(remaining) = options
            .safe_mode_until
            .filter(|_| analysis.isnt(Type::SAFE))
            .and_then(|until| until.checked_duration_since(now))
        {
            Err(BlockReason::Unsafe {
                remaining,
                targeted: false,
            })
        } else if let Some(dur) =
            Self::remaining_duration(&mut self.muted_until, now).filter(|_| options.block_if_muted)
        {
            Err(BlockReason::Muted(dur))
        } else if options.block_if_empty && censored.is_empty() {
            Err(BlockReason::Empty)
        } else if let Some(dur) = options
            .rate_limit
            .as_ref()
            .and_then(|opt| remaining_rate_limit.filter(|_| self.burst_used >= opt.burst))
        {
            Err(BlockReason::Spam(dur))
        } else if options
            .repetition_limit
            .as_ref()
            .map(|opts| recent_similar >= opts.limit)
            .unwrap_or(false)
        {
            Err(BlockReason::Repetitious(recent_similar as usize))
        } else if options.block_if_severely_inappropriate
            && analysis.is(Type::INAPPROPRIATE & Type::SEVERE)
        {
            Err(BlockReason::Inappropriate(analysis))
        } else if let Some(remaining) = Self::remaining_duration(&mut self.only_safe_until, now)
            .filter(|_| !(analysis.is(Type::SAFE) || options.max_safe_timeout.is_zero()))
        {
            Err(BlockReason::Unsafe {
                remaining,
                targeted: true,
            })
        } else {
            self.last_message = Some(now);
            if let Some(rate_limit_options) = options.rate_limit.as_ref() {
                // How many messages does this count for against the rate limit.
                let rate_limit_messages =
                    if let Some(char_limit) = rate_limit_options.character_limit {
                        let char_count = message.chars().count();

                        #[cfg(feature = "width")]
                        let char_count = char_count.max(crate::width_str(&message));

                        (char_count / char_limit.get() as usize).clamp(1, 3) as u8
                    } else {
                        1
                    };

                self.burst_used = if remaining_rate_limit.is_some() {
                    self.burst_used.saturating_add(rate_limit_messages)
                } else {
                    self.burst_used.saturating_sub(
                        (elapsed.as_nanos() / rate_limit_options.limit.as_nanos())
                            .min(u8::MAX as u128) as u8,
                    )
                };
                if let Some(rate_limited_until) =
                    self.rate_limited_until.unwrap_or(now).checked_add(
                        rate_limit_options.limit * (rate_limit_messages + new_suspicion) as u32,
                    )
                {
                    self.rate_limited_until = Some(rate_limited_until);
                }
            }
            // Forgiveness (minus one suspicion per safe message, and also per minute between messages).
            self.suspicion = self.suspicion.saturating_sub(
                (elapsed.as_secs() / 60).clamp(analysis.is(Type::SAFE) as u64, u8::MAX as u64)
                    as u8,
            );

            if let Some(repetition_blocking_options) = options.repetition_limit.as_ref() {
                if self.history.len() >= repetition_blocking_options.limit as usize * 2 {
                    self.history.pop_front();
                }

                self.history.push_back((message, now));
            }

            Ok(censored)
        }
    }

    /// Returns how long the user is muted for (possibly [`Duration::ZERO`]).
    pub fn muted_for(&self) -> Duration {
        self.muted_until
            .map(|muted_until| muted_until.saturating_duration_since(Instant::now()))
            .unwrap_or(Duration::ZERO)
    }

    /// Returns the latest instant the user is muted (possibly in the past).
    pub fn muted_until(&self) -> Option<Instant> {
        self.muted_until
    }

    /// Returns how long the user is restricted to [`Type::SAFE`] for (possibly [`Duration::ZERO`]).
    pub fn restricted_for(&self) -> Duration {
        self.only_safe_until
            .map(|restricted_until| restricted_until.saturating_duration_since(Instant::now()))
            .unwrap_or(Duration::ZERO)
    }

    /// Returns the latest instant the user is restricted (possibly in the past).
    pub fn restricted_until(&self) -> Option<Instant> {
        self.only_safe_until
    }

    /// Manually mute this user's messages for a duration. Overwrites any previous manual mute.
    /// Passing `Duration::ZERO` will therefore un-mute.
    pub fn mute_for(&mut self, duration: Duration) {
        self.mute_until(Instant::now() + duration);
    }

    /// Manually mute this user's messages until an instant. Overwrites any previous manual mute.
    /// Passing an instant in the past will therefore un-mute.
    pub fn mute_until(&mut self, instant: Instant) {
        self.muted_until = Some(instant);
    }

    /// Manually restrict this user's messages to known safe phrases for a duration. Overwrites any
    /// previous manual restriction. Passing `Duration::ZERO` will therefore un-restrict.
    pub fn restrict_for(&mut self, duration: Duration) {
        self.restrict_until(Instant::now() + duration);
    }

    /// Manually restrict this user's messages to known safe phrases until an instant. Overwrites any
    /// previous manual restriction. Passing an instant in the past will therefore un-restrict.
    pub fn restrict_until(&mut self, instant: Instant) {
        self.only_safe_until = Some(instant);
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

    /// Clear suspicion and reports, and automatic mutes (not manual mute or rate limit).
    pub fn exonerate(&mut self) {
        self.suspicion = 0;
        self.reports = 0;
        self.only_safe_until = None;
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
        Self::new()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum BlockReason {
    /// The particular message was *severely* inappropriate, more specifically, `Type`.
    Inappropriate(Type),
    /// Recent messages were generally inappropriate, and this message isn't on the safe list.
    /// Alternatively, if targeted is false, safe mode was configured globally.
    /// Try again after `Duration`.
    Unsafe {
        remaining: Duration,
        /// Whether unsafe mode was targeted at this user (as opposed to configured globally).
        targeted: bool,
    },
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
            Self::Unsafe { .. } => "You have been temporarily restricted due to profanity/spam",
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
            Self::Unsafe {
                remaining,
                targeted: true,
            } => format!(
                "You have been restricted for {} due to profanity/spam",
                FormattedDuration(remaining)
            ),
            Self::Unsafe {
                remaining,
                targeted: false,
            } => format!("Safe mode is active for {}", FormattedDuration(remaining)),
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
    use crate::context::{
        ContextProcessingOptions, ContextRateLimitOptions, ContextRepetitionLimitOptions,
    };
    use crate::{Censor, CensorIter, CensorStr, Type};
    use serial_test::serial;
    use std::fs::File;
    use std::io::BufReader;
    use std::num::NonZeroUsize;
    use std::time::{Duration, Instant};
    use test::Bencher;

    #[test]
    fn context_inappropriate() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();

        assert_eq!(ctx.process(String::from("one")), Ok(String::from("one")));
        assert!(matches!(
            ctx.process(String::from("nigga")),
            Err(BlockReason::Inappropriate(_))
        ));
    }

    #[test]
    fn context_unsafe() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();

        for _ in 0..30 {
            ctx.report();
        }

        let res = ctx.process(String::from("shit"));
        assert!(
            matches!(res, Err(BlockReason::Unsafe { targeted: true, .. })),
            "1 {:?}",
            res
        );

        let res = ctx.process(String::from("not common message"));
        assert!(
            matches!(res, Err(BlockReason::Unsafe { targeted: true, .. })),
            "2 {:?}",
            res
        );
    }

    #[test]
    fn context_repetitious() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();

        for _ in 0..ContextRepetitionLimitOptions::default().limit {
            assert!(ctx.process(String::from("one")).is_ok());
        }

        let res = ctx.process(String::from("onne"));
        assert!(matches!(res, Err(BlockReason::Repetitious(_))), "{:?}", res);
    }

    #[test]
    #[serial]
    fn context_spam() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();
        let opts = ContextProcessingOptions {
            rate_limit: Some(ContextRateLimitOptions {
                limit: Duration::from_millis(350),
                burst: 2,
                ..Default::default()
            }),
            ..Default::default()
        };

        assert_eq!(
            ctx.process_with_options(String::from("one"), &opts),
            Ok(String::from("one"))
        );
        assert_eq!(
            ctx.process_with_options(String::from("two"), &opts),
            Ok(String::from("two"))
        );
        assert_eq!(
            ctx.process_with_options(String::from("three"), &opts),
            Ok(String::from("three"))
        );
        let res = ctx.process_with_options(String::from("four"), &opts);
        assert!(matches!(res, Err(BlockReason::Spam(_))), "{:?}", res);

        std::thread::sleep(Duration::from_secs(2));

        assert_eq!(
            ctx.process_with_options(String::from("one"), &opts),
            Ok(String::from("one"))
        );
    }

    #[test]
    #[serial]
    fn context_spam_long_message() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();
        let opts = ContextProcessingOptions {
            rate_limit: Some(ContextRateLimitOptions {
                limit: Duration::from_millis(350),
                burst: 2,
                ..Default::default()
            }),
            ..Default::default()
        };

        assert_eq!(
            ctx.process_with_options(String::from("three"), &opts),
            Ok(String::from("three"))
        );
        assert!(ctx.process_with_options(String::from("one two three one two three one two three one two three one two three one two three one two three one two three one two three"), &opts).is_ok());
        let result = ctx.process_with_options(String::from("four"), &opts);
        assert!(matches!(result, Err(BlockReason::Spam(_))), "{:?}", result);
    }

    #[test]
    fn context_muted() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();

        ctx.mute_for(Duration::from_secs(5));

        let res = ctx.process(String::from("hello"));
        assert!(matches!(res, Err(BlockReason::Muted(_))), "{:?}", res);
    }

    #[test]
    fn context_safe_mode() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();

        let res = ctx.process_with_options(
            String::from("not on the safe list"),
            &ContextProcessingOptions {
                safe_mode_until: Some(Instant::now() + Duration::from_secs(100)),
                ..Default::default()
            },
        );
        assert!(
            matches!(
                res,
                Err(BlockReason::Unsafe {
                    targeted: false,
                    ..
                })
            ),
            "{:?}",
            res
        );
    }

    #[test]
    fn context_empty() {
        use crate::{BlockReason, Context};

        let mut ctx = Context::new();
        assert_eq!(ctx.process(String::from("   ")), Err(BlockReason::Empty));
    }

    #[test]
    #[cfg(feature = "width")]
    fn character_limit() {
        use crate::{BlockReason, Context, ContextProcessingOptions};
        let mut ctx = Context::new();

        let opts = ContextProcessingOptions {
            character_limit: Some(NonZeroUsize::new(5).unwrap()),
            ..Default::default()
        };

        assert_eq!(
            ctx.process_with_options(String::from("abcdefgh"), &opts),
            Ok(String::from("abcde"))
        );

        #[cfg(feature = "width")]
        assert_eq!(
            ctx.process_with_options(String::from("a﷽"), &opts),
            Ok(String::from("a"))
        );
    }
}
