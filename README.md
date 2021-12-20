# rustrict

`rustrict` is a sophisticated profanity filter for Rust. 

## Features

- Multiple types (profane, offensive, sexual, mean, spam)
- Multiple levels (mild, moderate, severe)
- Resistant to evasion
  - Alternative spellings (like "fck")
  - Repeated characters (like "craaaap")
  - Confusable characters (like 'ᑭ' vs 'P')
  - Spacing (like "c r_a-p")
  - Accents (like "pÓöp")
  - Bidirectional Unicode ([related reading](https://blog.rust-lang.org/2021/11/01/cve-2021-42574.html))
  - Self-censoring (like "f*ck")
  - Safe phrase list for known bad actors
  - Battle-tested in [Mk48.io](https://mk48.io)
- Resistant to false positives
  - One word (like "**ass**assin")
  - Two words (like "pu**sh it**")
- Flexible
  - Censor and/or analyze
  - Input `&str` or `Iterator<Type = char>`
  - Can add words with the `customize` feature
  - Plenty of options
- Performant
  - O(n) analysis and censoring
  - No `regex` (uses custom radix trie)
  - 4 MB/s in `release` mode
  - 150 KB/s in `debug` mode

## Limitations

- English only
- Censoring removes diacritics (accents)
- Does not detect right-to-left profanity while analyzing, so...
- Censoring forces Unicode to be left-to-right
- Doesn't understand context
- Not resistant to false positives affecting profanities added at runtime

## Usage

### Strings (`&str`)
```rust
use rustrict::CensorStr;

let censored: String = "hello crap".censor();
let inappropriate: bool = "f u c k".is_inappropriate();

assert_eq!(censored, "hello c***");
assert!(inappropriate);
```

### Iterators (`Iterator<Type = char>`)

```rust
use rustrict::CensorIter;

let censored: String = "hello crap".chars().censor().collect();

assert_eq!(censored, "hello c***");
```

### Advanced

By constructing a `Censor`, one can avoid scanning text multiple times to get a censored `String` and/or
answer multiple `is` queries. This also opens up more customization options (defaults are below).

```rust
use rustrict::{Censor, Type};

let (censored, analysis) = Censor::from_str("123 Crap")
    .with_censor_threshold(Type::INAPPROPRIATE)
    .with_censor_first_character_threshold(Type::OFFENSIVE & Type::SEVERE)
    .with_ignore_false_positives(false)
    .with_ignore_self_censoring(false)
    .with_censor_replacement('*')
    .censor_and_analyze();

assert_eq!(censored, "123 C***");
assert!(analysis.is(Type::INAPPROPRIATE));
assert!(analysis.isnt(Type::PROFANE & Type::SEVERE | Type::SEXUAL));
```

If you cannot afford to let anything slip though, or have reason to believe a particular user
is trying to evade the filter, you can check if their input matches a [short list of safe strings](src/safe.txt):

```rust
use rustrict::{CensorStr, Type};

// Figure out if a user is trying to evade the filter.
assert!("pron".is(Type::EVASIVE));
assert!("porn".isnt(Type::EVASIVE));

// Only let safe messages through.
assert!("Hello there!".is(Type::SAFE));
assert!("nice work.".is(Type::SAFE));
assert!("yes".is(Type::SAFE));
assert!("NVM".is(Type::SAFE));
assert!("gtg".is(Type::SAFE));
assert!("not a common phrase".isnt(Type::SAFE));
```

If you want to add custom profanities or safe words, enable the "customize" feature.

```rust
#[cfg(feature = "customize")]
{
    use rustrict::{add_word, CensorStr, Type};

    // You must take care not to call these when the crate is being
    // used in any other way (to avoid concurrent mutation).
    unsafe {
        add_word("reallyreallybadword", (Type::PROFANE & Type::SEVERE) | Type::MEAN);
        add_word("mybrandname", Type::SAFE);
    }
    
    assert!("Reallllllyreallllllybaaaadword".is(Type::PROFANE));
    assert!("MyBrandName".is(Type::SAFE));
}
```

## Comparison

To compare filters, the first 100,000 items of [this list](https://raw.githubusercontent.com/vzhou842/profanity-check/master/profanity_check/data/clean_data.csv)
is used as a dataset. Positive accuracy is the percentage of profanity detected as profanity. Negative accuracy is the percentage of clean text detected as clean.

| Crate | Accuracy | Positive Accuracy | Negative Accuracy | Time |
|-------|----------|-------------------|-------------------|------|
| [rustrict](https://crates.io/crates/rustrict) | 90.84% | 91.53% | 90.67% | 23s |
| [censor](https://crates.io/crates/censor) | 76.16% | 72.76% | 77.01% | 23s |

## Development

If you make an adjustment that would affect false positives, you will need to run `false_positive_finder`:
1. Run `./download.sh` to get the required word lists.
2. Run `cargo run --bin false_positive_finder --release --all-features`

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.