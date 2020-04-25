// https://www.ietf.org/rfc/rfc0822.txt

pub mod eml;
pub mod errors;
pub mod parser;

pub use parser::EmlParser;
