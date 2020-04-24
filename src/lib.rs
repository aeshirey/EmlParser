// https://www.ietf.org/rfc/rfc0822.txt

mod eml;
mod errors;
mod parser;

pub use eml::*;
pub use parser::EmlParser;
