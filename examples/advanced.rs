use rustrict::{Censor, Type};

fn main() {
    let (censored, analysis) = Censor::from_str("123 Crap")
        .with_censor_first_character_threshold(Type::OFFENSIVE & Type::SEVERE)
        .with_ignore_false_positives(false)
        .with_censor_replacement('?')
        .censor_and_analyze();

    assert_eq!(censored, "123 C???");
    assert!(analysis.is(Type::INAPPROPRIATE));
    assert!(analysis.isnt(Type::PROFANE & Type::SEVERE | Type::SEXUAL));
}
