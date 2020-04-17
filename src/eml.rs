#[derive(Debug)]
pub struct HeaderField {
    pub field_name: String,
    pub field_value: String,
}

#[derive(Debug, Default)]
pub struct Eml {
    pub headers: Vec<HeaderField>,
    pub from: Option<String>,
    pub to: Option<Vec<String>>,
    pub subject: Option<String>,
    pub body: Option<String>,
}
