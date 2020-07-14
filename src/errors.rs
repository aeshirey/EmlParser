use std::error;
use std::fmt;
use std::io;

#[derive(Debug)]
pub enum EmlError {
    UnexpectedEndOfStream(String),
    UnexpectedContent(String),
    IoError(std::io::Error),
}

impl From<io::Error> for EmlError {
    fn from(inner: io::Error) -> Self {
        EmlError::IoError(inner)
    }
}

impl fmt::Display for EmlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmlError::UnexpectedEndOfStream(s) => write!(f, "Unexpected end of stream: {}", s),
            EmlError::UnexpectedContent(s) => write!(f, "Unexpected content: {}", s),
            EmlError::IoError(inner) => write!(f, "IO error: {}", inner),
        }
    }
}

impl error::Error for EmlError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            EmlError::IoError(inner) => Some(inner),
            _ => None,
        }
    }
}
