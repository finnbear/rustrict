use rustrict::Censor;
use std::env::args;

pub fn main() {
    let input = args().skip(1).collect::<Vec<_>>().join(" ");
    let censored = Censor::from_str(&input).censor();
    println!("\"{}\" -> \"{}\"", input, censored);
}
