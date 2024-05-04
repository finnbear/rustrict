use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;

lazy_static! {
    static ref PHONE : Regex = Regex::new(r#"(\+\d{1,2})?\s*\(?\d{3}\)?[\s\.-]*\d{3}[\s\.-]*\d{4}"#).unwrap();
    static ref IP_ADDRESS : Regex  = Regex::new(r#"(?:[0-9]{1,3}\.){3}[0-9]{1,3}"#).unwrap();
    static ref EMAIL_ADDRESS : Regex = Regex::new(r#"(?i)[a-z0-9_\-]{3,}\s*(@|[\[\(\s]at[\s\)\]])\s*[a-z0-9_\-]{5,}\s*(\.|dot)\s*[a-z]{2,3}"#).unwrap();
    //static ref ADDRESS : Regex = Regex::new(r#"(?i)\d+[ ](?:[A-Za-z0-9\.-]+ )+(?:Avenue|Lane|Road|Boulevard|Drive|Street|Ave|Dr|Rd|Blvd|Ln|St)\.?(\s+#[0-9]{1,5})?"#).unwrap();
    static ref NAME : Regex = Regex::new(r#"(?i)(real\s)?name\s+is:?\s[a-zA-Z]+(\s[a-zA-z]+)?"#).unwrap();
    static ref URL : Regex = Regex::new(r#"(?i)(https?:?/*)?[a-zA-Z0-9]{4,}\.[a-zA-Z]{2,3}"#).unwrap();
}

/// Returns [`s`] with personally-identifiable information censored out, and a `true` if
/// anything was censored.
///  - phone numbers
///  - physical addresses (disabled for now, due to excessive false positives)
///  - ip addresses
///  - email addresses
///  - self-described full names
///  - urls
pub fn censor_and_analyze_pii(s: &str) -> (String, bool) {
    let ret = Cow::Borrowed(s);
    let mut censored = false;
    let ret = PHONE.replace_all(&ret, "***-****-****");
    censored |= matches!(ret, Cow::Owned(_));
    let ret = IP_ADDRESS.replace_all(&ret, "***.***.***.***");
    censored |= matches!(ret, Cow::Owned(_));
    let ret = EMAIL_ADDRESS.replace_all(&ret, "****@*****.***");
    censored |= matches!(ret, Cow::Owned(_));
    // too many false positives
    //let ret = ADDRESS.replace_all(&ret, "***** **** Ave #***");
    //censored |= matches!(ret, Cow::Owned(_));
    let ret = NAME.replace_all(&ret, "name is ***** *****");
    censored |= matches!(ret, Cow::Owned(_));
    let ret = URL.replace_all(&ret, "******.***");
    censored |= matches!(ret, Cow::Owned(_));
    (ret.into_owned(), censored)
}

#[cfg(test)]
mod tests {
    use super::censor_and_analyze_pii;

    fn censor_pii(s: &str) -> String {
        censor_and_analyze_pii(s).0
    }

    fn has_pii(s: &str) -> bool {
        censor_and_analyze_pii(s).1
    }

    #[test]
    fn pii() {
        /*
        12345 SW 54th ST #150
        go to 1234 Main Street for free candy
        */
        let pii = r#"
            hello@gmail.com
            hello f00 @ gmail.com
            sus@yahoo.biz sus
            foo[at]yahoo.com
            foo  [at]  yahoo  dot  com
            foo at yahoo dot com
            foo @ twitch.tv
            foo AT twitch.tv
            1234567890
            (123)4567890
            +1 1234567890
            +1 (123) 4567890
            +12  (123)  456  7890
            +1 (123) 456-7890
            +1 123-456-7890
            +1 123.456.7890
            123.123.123.123
            8.8.8.8
            999.999.999.999
            my name is: ALEX Smith
            my real name is Alex smith
            his name is alex smith
            her real name is alex Smith
            my name is alex. smith
            hello.com
            http://hello.com
            https://foooo.com
            barrr.com
            example.org
            twitch.tv
            http:/chat.dev
        "#;
        for line in pii.lines() {
            if line.trim().is_empty() {
                continue;
            }
            assert!(has_pii(line), "{line}");
        }
        println!("{}", censor_pii(pii));
    }

    #[test]
    fn not_pii() {
        for line in include_str!("./safe.txt")
            .lines()
            .chain(include_str!("./false_positives.txt").lines())
            .chain(r#"1234 Have 1234"#.lines())
        {
            assert!(!has_pii(line), "{line}");
        }
        assert!(!has_pii("123 i have 4"));
    }

    #[test]
    fn censor_pii_test() {
        assert_eq!(
            censor_pii("mail me at foo@barrr.com, bye"),
            "mail me at ****@*****.***, bye"
        );
    }
}
