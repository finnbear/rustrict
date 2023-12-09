.PHONY: fuzz

all: test

downloads:
	wget -O test.csv https://raw.githubusercontent.com/vzhou842/profanity-check/master/profanity_check/data/clean_data.csv
	wget -O src/dictionary.txt https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt
	wget -O src/dictionary_common.txt https://raw.githubusercontent.com/first20hours/google-10000-english/master/google-10000-english.txt
	wget -O src/unicode_confusables.txt https://www.unicode.org/Public/security/14.0.0/confusables.txt
	# TODO: ttf fonts

false_positives:
	cargo run --bin false_positive_finder --release --features censor,regex,indicatif,rayon,find_false_positives

replacements:
	cargo run --bin replacement_finder --features find_replacements

widths:
	cargo run --bin character_analyzer --release --features imageproc,image,rusttype,walkdir,rayon,unicode-width

test:
	cargo test --release --features width,serde -- --nocapture

compare:
	COMPARE=1 make test

# Skips accuracy analysis so finishes faster.
test_debug:
	cargo test

fuzz:
	cargo fuzz run fuzz

test_customize:
	cargo test --release --features customize --no-default-features