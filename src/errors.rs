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
