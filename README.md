# EmlParser
`EmlParser` is a crate intended to parse `.eml` files. Currently, this crate is very basic, supporting extracting field `(name,value)` pairs from an email header plus the body of the message. Special headers `To`, `From`, and `Subject` are separated out; all others are currently listed in a `Vec<HeaderField>`.

The parsing for this crate attempts to follow [RFC-0822](https://www.ietf.org/rfc/rfc0822.txt), though in practice there seem to be deviations from the RFC as to how to handle newlines. The spec lays out that the body and header are separated by a null line, as delimited by <kbd>CRLF</kbd>. Often, we'll actually see <kdb>\n\n</kbd>, so `EmlParser` allows <kbd>\n\n</kbd>, <kbd>\r\r</kbd>, and <kbd>\r\n\r\n</kbd>.

Note that header fields _are_ able to include newlines in them, defined as [linear whitespace](https://stackoverflow.com/questions/21072713/what-exactly-is-the-linear-whitespace-lws-lwsp).

Finding the separator between the header and body follows the following transition digram:

![Transition diagram for detecting the header/body delimiter](https://raw.githubusercontent.com/aeshirey/EmlParser/master/transition_graph.png)

# Usage
You can use `EmlParser` with a `&str` or a filename:

```rust
let eml: Eml = EmlParser::from_file("Re: hello.eml")?
    .ignore_body()
    .parse()?;

let expected = HeaderFieldValue::SingleEmailAddress(EmailAddress::NameAndEmailAddress {
    name: "Anne Thompson".to_string(),
    address: "anne@example.com".to_string(),
});

assert_eq!(Some(expected), eml.from);
assert_eq!(Some("Re: hello".to_string()), eml.subject);
```
