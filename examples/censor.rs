use rustrict::CensorStr;

fn main() {
    // Okay words are unaffected (with the exception of having their accents removed).
    show_censor("Helló world!");

    // Bad words are censored.
    show_censor("Hello shit world ass");

    // False positives are avoided.
    show_censor("assassin push it");

    // Obfuscation is mostly ignored.
    show_censor("$#1t f-u_c_k βιτ⊂η d u m b a s s");
}

fn show_censor(text: &str) {
    println!("{} -> {}", text, text.censor());
}
