// https://www.ietf.org/rfc/rfc0822.txt

mod eml;
mod errors;
mod parser;


pub use parser::EmlParser;
pub use eml::Eml;
