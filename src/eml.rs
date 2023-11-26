use std::fmt;

#[derive(Debug)]
pub struct HeaderField {
    pub name: String,
    pub value: HeaderFieldValue,
}

#[derive(Debug, std::cmp::PartialEq)]
pub enum EmailAddress {
    AddressOnly { address: String },
    NameAndEmailAddress { name: String, address: String },
}

#[derive(Debug, std::cmp::PartialEq)]
pub enum HeaderFieldValue {
    SingleEmailAddress(EmailAddress),
    MultipleEmailAddresses(Vec<EmailAddress>),
    Unstructured(String),
    Empty,
}

#[derive(Debug, Default)]
pub struct Eml {
    pub headers: Vec<HeaderField>,
    pub from: Option<HeaderFieldValue>,
    pub to: Option<HeaderFieldValue>,
    pub subject: Option<String>,
    pub body: Option<String>,
}

impl fmt::Display for EmailAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EmailAddress::AddressOnly { address } => write!(f, "{address}"),
            EmailAddress::NameAndEmailAddress { name, address } => {
                write!(f, r#""{name}" <{address}>"#)
            }
        }
    }
}

/// Custom formatting for header values.
/// For example, an email may comprise a name and address formatted as: "Name" <email>
impl fmt::Display for HeaderFieldValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HeaderFieldValue::SingleEmailAddress(address) => write!(f, "{address}"),
            HeaderFieldValue::MultipleEmailAddresses(addresses) => {
                // A probably acceptable heurestic for pre-allocating some memory
                let mut combined = String::with_capacity(20 * addresses.len());
                addresses.iter().enumerate().for_each(|(i, a)| {
                    if i > 0 {
                        combined.push_str(", ");
                    }
                    combined.push_str(&a.to_string());
                });
                write!(f, "{combined}")
            }
            HeaderFieldValue::Unstructured(s) => write!(f, "{s}"),
            HeaderFieldValue::Empty => write!(f, ""),
        }
    }
}
