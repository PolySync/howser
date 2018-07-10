//! Errors and Result Types for validation, reporting, and runtime issues.

extern crate regex;
extern crate termion;
extern crate toml;

use self::regex::Error as RegexError;
use self::termion::color;
use self::termion::style;
use self::toml::de::Error as TomlError;
use data::ContentMatchPair;
use document::{Document, Prescription};
use doogie::errors::DoogieError;
use doogie::Node;
use helpers::cli;
use helpers::cli::ShellText;
use std::error;
use std::fmt;
use std::io::Error as IOError;

/// Crate-wide Result type.
pub type HowserResult<T> = Result<T, HowserError>;
/// Type alias for `Reportable` trait object
pub type ValidationProblem = Box<Reportable>;

/// Error types for use with `HowserResult`.
#[derive(Debug)]
pub enum HowserError {
    DoogieError(DoogieError),
    IOError(IOError),
    Usage(String),
    RuntimeError(String),
    CapabilityError,
    RegexError(RegexError),
    PrescriptionError(SpecWarning),
    TomlError(TomlError),
}

impl error::Error for HowserError {
    fn description(&self) -> &str {
        match self {
            &HowserError::Usage(ref message) => message.as_str(),
            &HowserError::DoogieError(ref error) => error.description(),
            &HowserError::IOError(ref error) => error.description(),
            &HowserError::RuntimeError(ref message) => message,
            &HowserError::CapabilityError => "Capability Error",
            &HowserError::RegexError(ref error) => error.description(),
            &HowserError::PrescriptionError(_) => "Prescription Error",
            &HowserError::TomlError(_) => "TomlError",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match self {
            &HowserError::DoogieError(ref error) => Some(error),
            &HowserError::IOError(ref error) => Some(error),
            &HowserError::RegexError(ref error) => Some(error),
            _ => None,
        }
    }
}

impl fmt::Display for HowserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<IOError> for HowserError {
    fn from(err: IOError) -> Self {
        HowserError::IOError(err)
    }
}

impl From<DoogieError> for HowserError {
    fn from(err: DoogieError) -> Self {
        HowserError::DoogieError(err)
    }
}

impl From<RegexError> for HowserError {
    fn from(err: RegexError) -> Self {
        HowserError::RegexError(err)
    }
}

/// Trait for aggregating validation problems.
impl From<TomlError> for HowserError {
    fn from(err: TomlError) -> Self {
        HowserError::TomlError(err)
    }
}

pub trait Reportable {
    /// Report in standard single line format.
    fn short_msg(&self) -> String;

    /// Report in verbose multiline format.
    fn long_msg(&self) -> String;

    /// Error code
    fn code(&self) -> u32;
}

/// A warning related to `Prescription` specification compliance issues.
#[derive(Debug)]
pub struct SpecWarning {
    line: usize,
    file: String,
    message: String,
}

impl SpecWarning {
    pub fn new(node: &Node, rx: &Document, message: &str) -> HowserResult<Self> {
        let line = Document::get_line_num(&node)?;
        let file = match rx.filename.as_ref() {
            Some(filename) => filename.clone(),
            None => String::new(),
        };

        Ok(SpecWarning {
            line,
            file,
            message: message.to_string(),
        })
    }

    fn type_string() -> String {
        error_type("Rx Specification Error")
    }
}

impl Reportable for SpecWarning {
    fn short_msg(&self) -> String {
        let file_info = file_info(&self.file, self.line);
        let message = format!(
            " :: {}",
            ShellText::MessageColor(Box::new(ShellText::Literal(self.message.clone()))).to_string()
        );
        format!("{} :: {}{}", Self::type_string(), file_info, message)
    }

    fn long_msg(&self) -> String {
        self.short_msg()
    }

    fn code(&self) -> u32 {
        1
    }
}

/// General `Document` validity error.
pub struct DocumentError {
    info: ErrorInfo,
    message: String,
}

impl DocumentError {
    pub fn new(
        doc_node: &Node,
        rx_node: &Node,
        document: &Document,
        rx: &Prescription,
        message: String,
    ) -> HowserResult<Self> {
        Ok(DocumentError {
            info: ErrorInfo::new(rx_node, doc_node, rx, document)?,
            message,
        })
    }

    fn type_string() -> String {
        error_type("Document Error")
    }

    fn message(&self) -> String {
        let message_text = match self.message.is_empty() {
            true => self.message.clone(),
            false => format!(" :: {}", self.message),
        };
        format!(
            "{}{}{}",
            color::Fg(color::Yellow),
            message_text,
            color::Fg(color::Reset)
        )
    }
}

impl Reportable for DocumentError {
    fn short_msg(&self) -> String {
        format!(
            "{}: {} {} {}",
            Self::type_string(),
            self.info.rx_location(),
            self.info.node_location(),
            self.message()
        )
    }

    fn long_msg(&self) -> String {
        let mut message = String::from(self.message());
        message += "\n\n";
        message += &self.info.rx_location();
        message += "\n";
        message += &self.info.rx_snippet();
        message += "\n\n";
        message += &self.info.node_location();
        message += "\n";
        message += &self.info.node_snippet();
        message
    }

    fn code(&self) -> u32 {
        1
    }
}

struct ErrorInfo {
    pub node_file: String,
    pub node_line: usize,
    pub node_type: String,
    pub node_snippet: String,
    pub rx_file: String,
    pub rx_line: usize,
    pub rx_type: String,
    pub rx_snippet: String,
}

impl ErrorInfo {
    fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        doc: &Document,
    ) -> HowserResult<Self> {
        let node_file = doc.filename
            .as_ref()
            .unwrap_or(&"Unknown".to_string())
            .to_string();
        let node_line = Document::get_line_num(doc_node)?;
        let node_type = doc_node.get_cmark_type_string()?;
        let node_snippet = doc_node.render_commonmark();
        let rx_file = rx.document
            .filename
            .as_ref()
            .unwrap_or(&"Unknown".to_string())
            .clone();
        let rx_line = Document::get_line_num(rx_node)?;
        let rx_type = rx_node.get_cmark_type_string()?;
        let rx_snippet = rx_node.render_commonmark();

        Ok(ErrorInfo {
            node_file,
            node_line,
            node_type,
            node_snippet,
            rx_file,
            rx_line,
            rx_type,
            rx_snippet,
        })
    }

    fn rx_location(&self) -> String {
        file_info(&self.rx_file, self.rx_line)
    }

    fn node_location(&self) -> String {
        file_info(&self.node_file, self.node_line)
    }

    fn rx_type(&self) -> String {
        node_type_string(&self.rx_type)
    }

    fn node_type(&self) -> String {
        node_type_string(&self.node_type)
    }

    fn rx_snippet(&self) -> String {
        cli::as_code_lines(&self.rx_snippet, self.rx_line).join("\n")
    }

    fn node_snippet(&self) -> String {
        cli::as_code_lines(&self.node_snippet, self.node_line).join("\n")
    }
}

/// Error resulting from disparate Node types
pub struct TypeMismatchError {
    info: ErrorInfo,
}

impl TypeMismatchError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        doc: &Document,
    ) -> HowserResult<Self> {
        Ok(TypeMismatchError {
            info: ErrorInfo::new(rx_node, doc_node, rx, doc)?,
        })
    }

    fn type_string() -> String {
        error_type("Type Mismatch Error")
    }
}

impl Reportable for TypeMismatchError {
    fn short_msg(&self) -> String {
        format!(
            "{}: {} from {} does not match {} from {}",
            Self::type_string(),
            self.info.rx_type(),
            self.info.rx_location(),
            self.info.node_type(),
            self.info.node_location()
        )
    }

    fn long_msg(&self) -> String {
        let mut message = format!("{}\n\n", Self::type_string());
        message += &self.info.rx_type();
        message += "\n";
        message += &self.info.rx_location();
        message += "\n";
        message += &self.info.rx_snippet();
        message += "\n\n";
        message += &self.info.node_type();
        message += "\n";
        message += &self.info.node_location();
        message += "\n";
        message += &self.info.node_snippet();
        message
    }

    fn code(&self) -> u32 {
        1
    }
}

/// Error resulting from a textual content mismatch
pub struct TextualContentError {
    info: ErrorInfo,
    rx_prompts: Vec<String>,
    doc_matches: Vec<String>,
}

impl TextualContentError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        document: &Document,
        match_pairs: &Vec<ContentMatchPair>,
    ) -> HowserResult<Self> {
        let rx_prompts: Vec<String> = match_pairs
            .iter()
            .map(|pair| {
                let ContentMatchPair(ref content, _) = pair;
                match ContentMatchPair::is_match(pair) {
                    true => ok_text(&content.to_string()),
                    false => error_text(&content.to_string()),
                }
            })
            .collect();
        let doc_matches: Vec<String> = match_pairs
            .iter()
            .map(|pair| {
                let content = match pair {
                    ContentMatchPair(_, Some(ref content)) => content.to_owned(),
                    _ => String::from("<No Match>"),
                };
                match ContentMatchPair::is_match(pair) {
                    true => ok_text(&content),
                    false => error_text(&content),
                }
            })
            .collect();

        Ok(TextualContentError {
            info: ErrorInfo::new(rx_node, doc_node, rx, document)?,
            rx_prompts,
            doc_matches,
        })
    }

    fn type_string() -> String {
        error_type("Textual Content Error")
    }
}

impl Reportable for TextualContentError {
    fn short_msg(&self) -> String {
        format!(
            "{}: at {}, {}",
            Self::type_string(),
            self.info.rx_location(),
            self.info.node_location()
        )
    }

    fn long_msg(&self) -> String {
        let mut message = format!("{}\n\n", Self::type_string());
        message += &format!("{}Prescription : {}", style::Bold, style::Reset);
        message += &self.rx_prompts.join("");
        message += "\n";
        message += &format!("{}Document : {}", style::Bold, style::Reset);
        message += &self.doc_matches.join("");
        message += "\n\n";
        message += &self.info.rx_location();
        message += "\n";
        message += &self.info.rx_snippet();
        message += "\n\n";
        message += &self.info.node_location();
        message += "\n";
        message += &self.info.node_snippet();
        message
    }

    fn code(&self) -> u32 {
        1
    }
}

fn error_type(error_type: &str) -> String {
    format!(
        "{}{}{}",
        color::Fg(color::Red),
        error_type,
        color::Fg(color::Reset),
    )
}

fn file_info(filename: &str, line: usize) -> String {
    format!(
        "{}{} line {}{}",
        style::Underline,
        filename,
        line,
        style::Reset
    )
}

fn node_type_string(node_type: &String) -> String {
    format!("{}{}{}", style::Bold, node_type, style::Reset)
}

fn ok_text(text: &str) -> String {
    format!(
        "{}{}{}",
        color::Fg(color::Green),
        text,
        color::Fg(color::Reset)
    )
}

fn error_text(text: &str) -> String {
    format!(
        "{}{}{}",
        color::Fg(color::Red),
        text,
        color::Fg(color::Reset)
    )
}
