use rustrict::Censor;
use std::env::args;

pub fn main() {
    let input = args().skip(1).collect::<Vec<_>>().join(" ");

    trace(&input, false);
    //trace(&input, true);

    use unicode_normalization::UnicodeNormalization;
    use finl_unicode::categories::{CharacterCategories};
    println!("Without diacritics: {}", input
        .nfd()
        .filter(|c| !c.is_mark_nonspacing())
        .nfc()
        .collect::<String>());
}

pub fn trace(s: &str, ignore_fp: bool) {
    let mut censor = Censor::from_str(s);
    censor.with_ignore_false_positives(ignore_fp);
    let (censored, analysis) = censor.censor_and_analyze();
    println!(
        "ignore_fp={}, \"{}\" -> \"{}\" ({:?} with {} matches and {} matching characters)",
        ignore_fp,
        s,
        censored,
        analysis,
        censor.total_matches(),
        censor.total_match_characters()
    );
}
