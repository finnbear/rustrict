use rustrict::{Censor, Type};
use std::env::args;

pub fn main() {
    let input = args().skip(1).collect::<Vec<_>>().join(" ");
    let (censored, analysis) = Censor::from_str(&input).censor_and_analyze();
    println!(
        "\"{}\" -> \"{}\" ({} {} {} {} {} {})",
        input,
        censored,
        analysis.is(Type::PROFANE),
        analysis.is(Type::OFFENSIVE),
        analysis.is(Type::SEXUAL),
        analysis.is(Type::MEAN),
        analysis.is(Type::EVASIVE),
        analysis.is(Type::SPAM)
    );
}
