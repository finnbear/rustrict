use rustrict::{CensorStr, Type};

fn main() {
    show_analysis("Helló world!");
    show_analysis("Hello shit world ass");
    show_analysis("assassin push it");
    show_analysis("$#1t f-u_c_k βιτ⊂η d u m b a s s");
}

fn show_analysis(text: &str) {
    println!("\"{}\" is mean? {}", text, text.is(Type::MEAN));
}
