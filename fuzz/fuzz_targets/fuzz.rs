#![no_main]
use libfuzzer_sys::fuzz_target;
use rustrict::{Censor, Type};

fuzz_target!(|data: &[u8]| {
    if !data.is_empty() {
        let flags = data[0];
        let input = &data[1..];

        if let Ok(text) = std::str::from_utf8(input) {
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
        }
    }
});

fn flag(flags: u8, index: u8) -> bool {
    ((flags >> index) & 1) == 1
}
