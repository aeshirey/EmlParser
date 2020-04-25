use crate::eml::*;
use crate::errors::EmlError;
use regex::Regex;
use std::fs;
use std::io;
use std::iter::Peekable;
use std::path::Path;

#[derive(Debug)]
enum LwspState {
    ReadingContent,
    LF,     // Found a line feed (without first seeing a carriage return). EOLs are messed up.
    CR,     // Found a carriage return
    CRLF,   // Found a carriage return followed by a line feed
    CRLFCR, // Found a CRLF followed by a new CR
}

#[derive(Debug)]
enum InputType {
    CR,
    LF,
    WSP,
    NonWsp,
}

#[derive(Debug)]
enum BodyHandling {
    None,
    Preview(usize),
    All,
}

#[derive(Debug)]
pub struct EmlParser {
    content: String,
    position: usize,

    body_handling: BodyHandling,
}

impl EmlParser {
    /// Read an .eml file from disk, parsing its contents.
    /// Note that the current implementation loads the entire file to memory since `std::fs::File`
    /// doesn't provide an iterator over `char` that could give a `Peekable`.
    // One possible TODO is rolling something like https://github.com/C4K3/peekable-reader-rs into
    // this project.
    //pub fn from_file(filename: &str) -> Result<Self, io::Error> {
    pub fn from_file(filename: impl AsRef<Path>) -> Result<Self, io::Error> {
        let content = fs::read_to_string(filename)?;

        Ok(EmlParser {
            content,
            position: 0,
            body_handling: BodyHandling::All,
        })
    }

    pub fn from_string(content: String) -> Self {
        EmlParser {
            content,
            position: 0,
            body_handling: BodyHandling::All,
        }
    }

    // Builder pattern methods
    pub fn ignore_body(mut self) -> Self {
        self.body_handling = BodyHandling::None;
        self
    }

    pub fn with_body(mut self) -> Self {
        self.body_handling = BodyHandling::All;
        self
    }

    pub fn with_body_preview(mut self, bytes: usize) -> Self {
        self.body_handling = BodyHandling::Preview(bytes);
        self
    }

    pub fn parse(&mut self) -> Result<Eml, EmlError> {
        if self.content.is_empty() {
            return Err(EmlError::UnexpectedEndOfStream(String::from("Empty input")));
        }

        let content = self.content.to_string(); // making a copy so we can have a mutable reference
        let chars = content.chars();
        let mut char_input = chars.peekable();
        let eml = self.parse_email(&mut char_input)?;

        Ok(eml)
    }

    fn parse_email<T: Iterator<Item = char>>(
        &mut self,
        mut char_input: &mut Peekable<T>,
    ) -> Result<Eml, EmlError> {
        let headers = self.parse_header_field(&mut char_input)?;

        self.remove_header_body_separator(&mut char_input)?;

        let mut result = Eml::default();
        result.body = self.parse_body();

        for header in headers {
            match (&header.name[..], &header.value) {
                ("To", _) => result.to = Some(header.value),
                ("From", _) => result.from = Some(header.value),
                ("Subject", HeaderFieldValue::Unstructured(subj)) => {
                    result.subject = Some((*subj).to_string())
                }
                _ => result.headers.push(header),
            }
        }

        Ok(result)
    }

    fn parse_header_field<T: Iterator<Item = char>>(
        &mut self,
        mut char_input: &mut Peekable<T>,
    ) -> Result<Vec<HeaderField>, EmlError> {
        use HeaderFieldValue::*;
        let mut headers = Vec::new();

        while let Some((name, value)) = self.read_raw_header_field(&mut char_input)? {
            // Attempt to structure this header value
            let value = match (&name[..], value) {
                ("From", v)| ("To", v) | ("Reply-To", v) | ("Delivered-To", v) | ("X-Original-To", v) | ("Return-Path", v) => {
                    EmlParser::parse_email_address(v)
                }
                (_, v) if v.is_empty() => Empty,
                (_, v) => Unstructured(v),
            };
            headers.push(HeaderField { name, value });
        }
        Ok(headers)
    }

    fn parse_email_address(value: String) -> HeaderFieldValue {
        // Email address header values can span multiple lines. Clean those up first
        let mut remaining = value.replace("\n", "").replace("\r", "");

        let mut found_addresses = Vec::new();

        let name_addr_re = Regex::new(r#"^"(.?+)" <\s*([^>]+)\s*>[ ,]*"#).unwrap(); // for matching: "John Smith" <jsmith@example.com>
        let addr_re1 = Regex::new(r#"^\s*<\s*([^>]+)\s*>[ ,]*"#).unwrap(); // for matching the email in brackets without name: <jsmith@example.com>
        let addr_re2 = Regex::new(r#"^\s*([^"<>@]+@[^"<>@\s,]+)[ ,]*"#).unwrap(); // for matching the email without brackets: jsmith@example.com


        while !remaining.is_empty() {
            if let Some(cap) = name_addr_re.captures(&remaining) {
                let name = cap.get(1).unwrap().as_str().to_string();
                let address = cap.get(2).unwrap().as_str().to_string();
                found_addresses.push(EmailAddress::NameAndEmailAddress { name, address });

                let entire_match = cap.get(0).unwrap();
                remaining = remaining[entire_match.end()..].to_string();
            } else if let Some(cap) = addr_re1.captures(&remaining) {
                let address = cap.get(1).unwrap().as_str().to_string();
                found_addresses.push(EmailAddress::AddressOnly { address });

                let entire_match = cap.get(0).unwrap();
                remaining = remaining[entire_match.end()..].to_string();
            } else if let Some(cap) = addr_re2.captures(&remaining) {
                let address = cap.get(1).unwrap().as_str().to_string();
                found_addresses.push(EmailAddress::AddressOnly { address });

                let entire_match = cap.get(0).unwrap();
                remaining = remaining[entire_match.end()..].to_string();
            } else {
                // Something weird
                return HeaderFieldValue::Unstructured(value);
            }
        }

        //match found_addresses {
        if found_addresses.len() == 1 {
            HeaderFieldValue::SingleEmailAddress(found_addresses.into_iter().next().unwrap())
        } else {
            HeaderFieldValue::MultipleEmailAddresses(found_addresses)
        }
    }

    fn read_raw_header_field<T: Iterator<Item = char>>(
        &mut self,
        mut char_input: &mut Peekable<T>,
    ) -> Result<Option<(String, String)>, EmlError> {
        match char_input.peek() {
            Some('\n') | Some('\r') => return Ok(None), // finding a CR or LF when looking for a header means the body is about to start
            Some(_) => {}
            None => {
                return Err(EmlError::UnexpectedEndOfStream(String::from(
                    "Expected the beginning of a header field name",
                )))
            }
        };

        if let Some(name) = self.read_field_name(&mut char_input)? {
            match char_input.peek() {
                Some(':') => {
                    self.position += 1;
                    char_input.next();
                }
                Some(c) => {
                    return Err(EmlError::UnexpectedContent(format!(
                        "Expected ':' to terminate header field '{}'; got '{}' (byte value {})",
                        name, c, *c as u8
                    )))
                }
                None => {
                    return Err(EmlError::UnexpectedEndOfStream(format!(
                        "Expected ':' to terminate header field '{}'",
                        name
                    )))
                }
            };

            match char_input.peek() {
                Some(' ') => {
                    self.position += 1;
                    char_input.next();
                }
                Some(_) => {}
                None => {
                    return Err(EmlError::UnexpectedEndOfStream(format!(
                        "Expected non-empty content for header field '{}'",
                        name
                    )))
                }
            };

            let value = self.read_field_body(&mut char_input)?;

            Ok(Some((name, value)))
        } else {
            Ok(None)
        }
    }

    // 1*<any CHAR, excluding CTLs, SPACE, and ":">
    fn read_field_name<T: Iterator<Item = char>>(
        &mut self,
        char_input: &mut Peekable<T>,
    ) -> Result<Option<String>, EmlError> {
        let start_position = self.position;
        let mut end_position = self.position;

        while let Some(c) = char_input.peek() {
            if c == &'\n' || c == &'\r' {
                // we shouldn't see CR or LF in a field name; if we do, it's likely the end of the
                // header
                return Ok(None);
            } else if c != &' ' && c != &':' && !c.is_control() {
                char_input.next();
                end_position += 1;
            } else {
                break;
            }
        }

        if end_position == self.content.len() {
            Err(EmlError::UnexpectedEndOfStream(String::from(
                "Expected content for header field",
            )))
        } else {
            self.position = end_position;
            Ok(Some(String::from(
                &self.content[start_position..end_position],
            )))
        }
    }

    // Read until we've found a CRLF that does NOT have white whitespace after it
    fn read_field_body<T: Iterator<Item = char>>(
        &mut self,
        char_input: &mut Peekable<T>,
    ) -> Result<String, EmlError> {
        let start_position = self.position;
        let mut end_position = self.position;
        let mut state = LwspState::ReadingContent;

        while let Some(next_char) = char_input.peek() {
            let ws = EmlParser::next_char_type(*next_char);

            match (&state, ws) {
                (LwspState::ReadingContent, InputType::WSP)
                | (LwspState::ReadingContent, InputType::NonWsp) => {
                    // While reading input, anything not CR or LF gets included
                    char_input.next();
                    end_position += 1;
                }

                (LwspState::ReadingContent, InputType::CR) => {
                    state = LwspState::CR;
                    char_input.next();
                    end_position += 1;
                }

                (LwspState::ReadingContent, InputType::LF) => {
                    state = LwspState::LF;
                    char_input.next();
                    end_position += 1;
                }

                (LwspState::LF, InputType::WSP)
                | (LwspState::CR, InputType::WSP)
                | (LwspState::CRLF, InputType::WSP) => {
                    // A newline followed by whitespace is the definition of linear whitespace.
                    // This is an input that spans multiple lines; for example:
                    //       X-Received: by 0000:111:222e:: with SMTP id abcdef;
                    //               Mon, 13 Apr 2020 14:04:07 -0700 (PDT)
                    // In this case, we return to the ReadingContent state
                    state = LwspState::ReadingContent;
                    char_input.next();
                    end_position += 1;
                }

                (LwspState::LF, InputType::NonWsp)
                | (LwspState::CR, InputType::NonWsp)
                | (LwspState::CRLF, InputType::NonWsp) => {
                    // A newline followed by non-whitespace means we're at the end of this
                    // header item.
                    break;
                }

                (LwspState::LF, InputType::LF) | (LwspState::CR, InputType::CR) => {
                    // Two line feeds or two carriage returns should signal the end of the header.
                    // Subtract out the last CR or LF from the input
                    end_position -= 1;
                    break;
                }

                (LwspState::CRLFCR, InputType::LF) => {
                    // CRLF + CRLF should similarly signal the end of the header, but we have to
                    // subtract out three characters from the input
                    end_position -= 3;
                    break;
                }

                (LwspState::CR, InputType::LF) => {
                    // CR+LF will probably lead to CRLF+CRLF
                    state = LwspState::CRLF;
                    char_input.next();
                    end_position += 1;
                }

                (LwspState::CRLF, InputType::CR) => {
                    // Approaching CRLF+CRLF
                    state = LwspState::CRLFCR;
                    char_input.next();
                    end_position += 1;
                }

                // Rather strict handling of line endings when we're at the border of the header
                // and body. According to RFC0822, the body "is separated from the headers by a
                // null line (i.e., a line with nothing preceding the CRLF)."
                // In reality, we'll see "\n\n" or possibly even "\r\r" separating lines.
                // It's not unreasonable to think that we'd see other unusual input such as
                // "\r\n\n" separating the header from the body, but for now, I'm only accepting
                // "\r\r", "\n\n", and "\r\n\r\n". The following situations are all erroneous:
                // We should really just see
                (LwspState::CRLFCR, _) => {
                    // CRLF + CR shouldn't be followed by anything but a line feed.
                    return Err(EmlError::UnexpectedContent(String::from(
                        "Found CRLF+CR in header without expected LF",
                    )));
                }

                (LwspState::CRLF, InputType::LF) => {
                    // CRLF should have had an additional CR before the LF.
                    return Err(EmlError::UnexpectedContent(String::from(
                        "Found CRLF+LF in header without expected CR first",
                    )));
                }

                (LwspState::LF, InputType::CR) => {
                    // LF after non-breaking character should be followed by another LF, not CR.
                    return Err(EmlError::UnexpectedContent(String::from(
                        "Found LF+CR in header as line delimeter",
                    )));
                }
            }
        }

        self.position = end_position;

        Ok(String::from(
            &self.content[start_position..end_position - 1],
        ))
    }

    fn remove_header_body_separator<T: Iterator<Item = char>>(
        &mut self,
        char_input: &mut Peekable<T>,
    ) -> Result<(), EmlError> {
        // Read off and advance self.position for the null line separating the header from the
        // body. The RFC says CRLF is the line separator, but we're allowing CRCR, LFLF, and
        // CRLFCRLF. Anything else is currently an error
        let mut state = LwspState::ReadingContent;

        while let Some(next_char) = char_input.peek() {
            let ws = EmlParser::next_char_type(*next_char);

            state = match (&state, ws) {
                (LwspState::ReadingContent, InputType::LF) => LwspState::LF,
                (LwspState::ReadingContent, InputType::CR) => LwspState::CR,
                (LwspState::LF, InputType::LF) => break, // acceptable path: LFLF
                (LwspState::CR, InputType::CR) => break, // acceptable path: CRCR
                (LwspState::CR, InputType::LF) => LwspState::CRLF,
                (LwspState::CRLF, InputType::CR) => LwspState::CRLFCR,
                (LwspState::CRLFCR, InputType::LF) => break, // acceptable path: CRLF+CRLF
                (_, _) => {
                    return Err(EmlError::UnexpectedContent(String::from(
                        "Unexpected line ending combination separating the header and body",
                    )))
                }
            };
        }

        // Advance the position as appropriate
        self.position += match &state {
            LwspState::CR | LwspState::LF => 2,
            LwspState::CRLFCR => 4,
            _ => unreachable!(),
        };

        Ok(())
    }

    fn next_char_type(c: char) -> InputType {
        match c {
            '\n' => InputType::LF,
            '\r' => InputType::CR,
            ' ' | '\t' => InputType::WSP,
            // According to RFC0822, linear whitespace is CRLF + (space or tab).
            // There's no clear indication how a form feed (0xC, dec 12) should play into this.
            // Hopefully this isn't an issue, but I am explicitly defining non-CR, non-LF
            // whitespace as being no different than space and tab.
            c if c.is_ascii_whitespace() => InputType::WSP,
            _ => InputType::NonWsp,
        }
    }

    fn parse_body(&mut self) -> Option<String> {
        match self.body_handling {
            BodyHandling::None => None,
            BodyHandling::Preview(bytes) => {
                let bytes_remaining = self.content.len() - self.position;
                let bytes = std::cmp::min(bytes, bytes_remaining);

                //let bytes = if bytes > bytes_remaining { bytes_remaining } else { bytes }

                Some(String::from(
                    &self.content[self.position..self.position + bytes],
                ))
            }
            BodyHandling::All => Some(String::from(&self.content[self.position..])),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_HEADER: &str = r#"Delivered-To: john.public@example.com
Received: by 2002:ac9:700e:0:0:0:0:0 with SMTP id w14csp4493771ocr;
        Mon, 13 Apr 2020 14:04:07 -0700 (PDT)
X-Google-Smtp-Source: APiQypIbRnWumT0t4TOJHlvDOVkxfqZ8A8HBzdR39kgdjVQQfKUsY/DkKFeZI53Ux1Z3reMRqaCl
X-Received: by 2002:a37:aa8e:: with SMTP id t136mr9744838qke.175.1586811847065;
        Mon, 13 Apr 2020 14:04:07 -0700 (PDT)
foo: bar

This is the start of the body
"#;

    #[test]
    fn basic_test() {
        let eml = EmlParser::from_string(TEST_HEADER.to_string())
            .with_body()
            .parse();

        assert!(eml.is_ok());
        let eml = eml.unwrap();

        assert_eq!(5, eml.headers.len());

        let delivered_to: &HeaderField = &eml.headers[0];
        assert_eq!("Delivered-To", delivered_to.name);
        assert_eq!(
            HeaderFieldValue::Unstructured("john.public@example.com".to_string()),
            delivered_to.value
        );

        let received: &HeaderField = &eml.headers[1];
        assert_eq!("Received", received.name);
        assert_eq!(
            HeaderFieldValue::Unstructured(
                r#"by 2002:ac9:700e:0:0:0:0:0 with SMTP id w14csp4493771ocr;
        Mon, 13 Apr 2020 14:04:07 -0700 (PDT)"#
                    .to_string()
            ),
            received.value
        );

        assert!(eml.body.is_some());
        let body = eml.body.unwrap();
        assert_eq!("This is the start of the body\n", body);
    }

    #[test]
    fn basic_test_with_truncated_body() {
        let eml: Eml = EmlParser::from_string(TEST_HEADER.to_string())
            .with_body_preview(15)
            .parse()
            .unwrap(); // Result

        let body = eml.body.unwrap();
        let expected = &"This is the start of the body\n"[0..15];
        assert_eq!(expected, body);
    }

    #[test]
    fn basic_test_with_truncation_gt_body_length() {
        let eml: Eml = EmlParser::from_string(TEST_HEADER.to_string())
            .with_body_preview(150)
            .parse()
            .unwrap(); // Result

        let body = eml.body.unwrap();
        assert_eq!("This is the start of the body\n", body);
    }

    #[test]
    fn parse_emails() {
        let parsed =
            EmlParser::parse_email_address(r#""John Smith" <jsmith@example.com>"#.to_string());

        let jsmith = EmailAddress::NameAndEmailAddress {
            name: "John Smith".to_string(),
            address: "jsmith@example.com".to_string(),
        };
        let expected = HeaderFieldValue::SingleEmailAddress(jsmith);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_and_display_emails() {
        let single = r#""John Q. Public" < john@example.com>, "#.to_string();
        let parsed = EmlParser::parse_email_address(single);

        match &parsed {
            HeaderFieldValue::SingleEmailAddress(EmailAddress::NameAndEmailAddress {
                name,
                address,
            }) => {
                assert_eq!(name, "John Q. Public");
                assert_eq!(address, "john@example.com");
            }
            _ => panic!("Expected SingleEmailAddress, got something else"),
        };

        assert_eq!(parsed.to_string(), r#""John Q. Public" <john@example.com>"#);
    }
}
