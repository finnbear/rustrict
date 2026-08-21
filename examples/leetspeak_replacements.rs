//! Demonstrates removing specific digit/letter substitutions from the default
//! `Replacements` table. Useful when digit-heavy text (zip codes, numeric IDs,
//! prices, etc.) is decoding into flagged words via leetspeak-style
//! substitutions (e.g. `9` -> `g`, `0` -> `o`) that don't apply to your use-case.
//!
//! Run with: `cargo run --example leetspeak_replacements --features customize`

use rustrict::Replacements;

fn main() {
    // SAFETY: call once, synchronously, before any concurrent access to censoring
    // (e.g. at startup, before spawning any tasks that censor text).
    unsafe {
        let replacements = Replacements::customize_default();
        replacements.remove('9', 'g');
        replacements.remove('0', 'o');
        // ...repeat for any substitution causing false positives in your use-case.
    }

    // Digits still match themselves; only the letter interpretations are removed.
    println!("Replacements customized; '9' no longer maps to 'g', '0' no longer maps to 'o'.");
}
