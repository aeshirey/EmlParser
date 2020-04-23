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
    pub to: Option<Vec<HeaderFieldValue>>,
    pub subject: Option<String>,
    pub body: Option<String>,
}
