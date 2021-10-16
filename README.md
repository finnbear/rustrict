# rustrict

`rustrict` is a profanity filter for Rust. 

When evaluated against the first 100,000 items of [this list](https://raw.githubusercontent.com/vzhou842/profanity-check/master/profanity_check/data/clean_data.csv),
it has **92.13% accuracy** (85% positive accuracy, 94% negative accuracy), as of version `0.1.5`.

## Setup

```toml
rustrict = "0.1.5"
```

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

assert_eq!(censored, "hello c***")
```

### Advanced

By constructing a `Censor`, one can avoid scanning text multiple times to get a censored `String` and/or
answer multiple `is` queries. This also opens up more customization options (defaults are below).

```rust
use rustrict::{Censor, Type};

let (censored, analysis) = Censor::from_str("123 Crap")
    .with_censor_first_character_threshold(Type::OFFENSIVE & Type::SEVERE)
    .with_ignore_false_positives(false)
    .with_censor_replacement('*')
    .censor_and_analyze();

assert_eq!(censored, "123 C***");
assert!(analysis.is(Type::INAPPROPRIATE));
assert!(analysis.isnt(Type::PROFANE & Type::SEVERE | Type::SEXUAL));
```

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