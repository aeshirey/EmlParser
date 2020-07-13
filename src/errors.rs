use std::error;
use std::fmt;
use std::io;

#[derive(Debug)]
pub enum EmlError {
    UnexpectedEndOfStream(String),
    UnexpectedContent(String),
    IoError,
}

impl From<io::Error> for EmlError {
    fn from(_: io::Error) -> Self {
        EmlError::IoError
    }
}

impl fmt::Display for EmlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmlError::UnexpectedEndOfStream(s) => write!(f, "Unexpected end of stream: {}", s),
            EmlError::UnexpectedContent(s) => write!(f, "Unexpected content: {}", s),
            EmlError::IoError => write!(f, "IO error"),
        }
    }
}

impl error::Error for EmlError {}
