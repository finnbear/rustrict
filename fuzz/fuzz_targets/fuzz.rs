#![no_main]
use libfuzzer_sys::fuzz_target;
use rustrict::{Censor, Context, Type};

fuzz_target!(|data: &[u8]| {
    if !data.is_empty() {
        let flags = data[0];
        let input = &data[1..];

        if let Ok(text) = std::str::from_utf8(input) {
            let _ = rustrict::width_str(text);
            let _ = rustrict::trim_to_width(text, 10);

            let (_censored, _analysis) = Censor::from_str(text)
                .with_ignore_self_censoring(flag(flags, 0))
                .with_ignore_false_positives(flag(flags, 1))
                .with_censor_threshold(if flag(flags, 2) {
                    Type::INAPPROPRIATE
                } else {
                    Type::SPAM
                })
                .with_censor_first_character_threshold(if flag(flags, 3) {
                    Type::INAPPROPRIATE
                } else {
                    Type::SPAM
                })
                .with_censor_replacement(if flag(flags, 4) { '#' } else { '*' })
                .censor_and_analyze();

            let mut ctx = Context::new();

            for _ in 0..3 {
                let _ = ctx.process(String::from(text));
                let _ = ctx.process(String::from("hi"));
                let _ = ctx.process(String::from(text));
            }
        }
    }
});

fn flag(flags: u8, index: u8) -> bool {
    ((flags >> index) & 1) == 1
}
