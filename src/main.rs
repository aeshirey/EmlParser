mod eml;
mod errors;
mod parser;
use crate::errors::EmlError;
use crate::parser::EmlParser;

fn main() -> Result<(), EmlError> {
    let mut eml = EmlParser::from_file("test_emails/0.eml")?;
    if let Ok(parsed) = eml.parse() {
        println!("{:?}", parsed.to);
    } else {
        println!("Failed to parse");
    }
    println!();

    let mut eml = EmlParser::from_file("test_emails/1.eml")?;
    if let Ok(parsed) = eml.parse() {
        println!("{:?}", parsed.to);
    } else {
        println!("Failed to parse");
    }

    Ok(())
}
