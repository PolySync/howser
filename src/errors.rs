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
use doogie::constants::NodeType;
use doogie::errors::DoogieError;
use doogie::Node;
use helpers::cli;
use helpers::cli::ShellText;
use std::error;
use std::fmt;
use std::io::Error as IOError;
use std::ops::Add;

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
            &HowserError::TomlError(ref err) => "TomlError",
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
    line: u32,
    file: String,
    message: String,
}

impl SpecWarning {
    pub fn new(node: &Node, rx: &Document, message: &str) -> HowserResult<Self> {
        let getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let line = getter.get_start_line()?;
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
}

impl Reportable for SpecWarning {
    fn short_msg(&self) -> String {
        let error_type = ShellText::WarningColor(Box::new(ShellText::Literal(
            "SpecWarning".to_string(),
        ))).to_string();
        let file_info = format!(
            "{} line {}",
            ShellText::Underlined(Box::new(ShellText::Literal(self.file.clone()))).to_string(),
            self.line
        );
        let message = format!(
            " :: {}",
            ShellText::MessageColor(Box::new(ShellText::Literal(self.message.clone()))).to_string()
        );
        format!("{} :: {}{}", error_type, file_info, message)
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
    document_line: u32,
    rx_line: u32,
    document_snippet: String,
    rx_snippet: String,
    message: String,
    document_file: String,
    rx_file: String,
}

impl DocumentError {
    pub fn new(
        doc_node: &Node,
        rx_node: &Node,
        document: &Document,
        rx: &Prescription,
        message: String,
    ) -> HowserResult<Self> {
        let doc_node_getter = doc_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let doc_node_renderer = doc_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let rx_getter = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let rx_renderer = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let document_line = doc_node_getter.get_start_line()?;
        let rx_line = rx_getter.get_start_line()?;
        let document_file = document
            .filename
            .as_ref()
            .unwrap_or(&String::new())
            .to_string();
        let rx_file = rx.document
            .filename
            .as_ref()
            .unwrap_or(&String::new())
            .to_string();
        let document_snippet = doc_node_renderer.render_commonmark();
        let rx_snippet = rx_renderer.render_commonmark();

        Ok(DocumentError {
            document_line,
            rx_line,
            document_snippet,
            rx_snippet,
            message,
            document_file,
            rx_file,
        })
    }
}

impl Reportable for DocumentError {
    fn short_msg(&self) -> String {
        let error_type =
            ShellText::ErrorColor(Box::new(ShellText::Literal("Document Error".to_string())));
        let rx_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.rx_file, self.rx_line
        ))));
        let document_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.document_file, self.document_line
        ))));
        let message_text = match self.message.is_empty() {
            true => self.message.clone(),
            false => format!(" :: {}", self.message),
        };
        let message = ShellText::MessageColor(Box::new(ShellText::Literal(message_text)));
        format!(
            "{} :: {}, {}{}",
            error_type.to_string(),
            rx_info.to_string(),
            document_info.to_string(),
            message.to_string()
        )
    }

    fn long_msg(&self) -> String {
        let mut message_lines = vec![self.short_msg()];

        let mut snippet_lines = vec![
            ShellText::Underlined(Box::new(ShellText::Literal(self.rx_file.to_owned())))
                .to_string(),
        ];
        snippet_lines.append(&mut cli::as_code_lines(
            &self.rx_snippet,
            self.rx_line as usize,
        ));
        message_lines.append(&mut cli::indented_lines(&snippet_lines, 4));

        message_lines.push(String::new());

        let mut snippet_lines = vec![
            ShellText::Underlined(Box::new(ShellText::Literal(self.document_file.to_owned())))
                .to_string(),
        ];
        snippet_lines.append(&mut cli::as_code_lines(
            &self.document_snippet,
            self.document_line as usize,
        ));
        message_lines.append(&mut cli::indented_lines(&snippet_lines, 4));

        message_lines.join("\n")
    }

    fn code(&self) -> u32 {
        1
    }
}

/// Error resulting from disparate Node types
pub struct TypeMismatchError {
    node_type: NodeType,
    rx_type: NodeType,
    rx_file: String,
    doc_file: String,
    rx_line: usize,
    doc_line: usize,
    rx_snippet: String,
    doc_snippet: String,
}

impl TypeMismatchError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        doc: &Document,
    ) -> HowserResult<Self> {
        let node_type = doc_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?;
        let rx_type = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?;
        let rx_file = rx.document
            .filename
            .as_ref()
            .unwrap_or(&"Unknown".to_string())
            .clone();
        let doc_file = doc.filename
            .as_ref()
            .unwrap_or(&"Unknown".to_string())
            .clone();
        let rx_line = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_start_line()? as usize;
        let doc_line = doc_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_start_line()? as usize;
        let doc_snippet = doc_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .render_commonmark();
        let rx_snippet = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .render_commonmark();

        Ok(TypeMismatchError {
            node_type,
            rx_type,
            rx_file,
            doc_file,
            rx_line,
            doc_line,
            rx_snippet,
            doc_snippet,
        })
    }
}

impl Reportable for TypeMismatchError {
    fn short_msg(&self) -> String {
        let error_type = format!(
            "{}Type Mismatch Error{}",
            color::Fg(color::Red),
            color::Fg(color::Reset)
        );
        let rx_info = format!(
            "{}{} line {}{}",
            style::Underline,
            self.rx_file,
            self.rx_line,
            style::Reset
        );
        let doc_info = format!(
            "{}{} line {}{}",
            style::Underline,
            self.doc_file,
            self.doc_line,
            style::Reset
        );
        let node_type = format!("{}{:?}{}", style::Bold, self.node_type, style::Reset);
        let rx_type = format!("{}{:?}{}", style::Bold, self.rx_type, style::Reset);

        format!(
            "{}: {} from {} does not match {} from {}",
            error_type, rx_type, rx_info, node_type, doc_info
        )
    }

    fn long_msg(&self) -> String {
        let rx_info = format!(
            "{}{} line {}{}\n",
            style::Underline,
            self.rx_file,
            self.rx_line,
            style::Reset
        );
        let doc_info = format!(
            "{}{} line {}{}\n",
            style::Underline,
            self.doc_file,
            self.doc_line,
            style::Reset
        );

        let node_type = format!("{}{:?}{}\n", style::Bold, self.node_type, style::Reset);
        let rx_type = format!("{}{:?}{}\n", style::Bold, self.rx_type, style::Reset);

        let mut message = format!(
            "{}Type Mismatch Error{}",
            color::Fg(color::Red),
            color::Fg(color::Reset)
        );
        message += "\n\n";
        message += &rx_info;
        message += &rx_type;
        message += &cli::as_code_lines(&self.rx_snippet, self.rx_line).join("\n");
        message += "\n\n";
        message += &doc_info;
        message += &node_type;
        message += &cli::as_code_lines(&self.doc_snippet, self.doc_line).join("\n");
        message
    }

    fn code(&self) -> u32 {
        1
    }
}

/// Error resulting from a textual content mismatch
pub struct TextualContentError {
    node_type: NodeType,
    rx_file: String,
    rx_line: usize,
    doc_file: String,
    doc_line: usize,
    rx_prompts: Vec<String>,
    doc_matches: Vec<String>,
    doc_snippet: String,
    rx_snippet: String,
}

impl TextualContentError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        document: &Document,
        match_pairs: &Vec<ContentMatchPair>,
    ) -> HowserResult<Self> {
        let node_type = match rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?
        {
            NodeType::CMarkNodeText => match rx_node
                .capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .parent()?
            {
                Some(node) => node.capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .get_type()?,
                None => NodeType::CMarkNodeText,
            },
            nt => nt,
        };
        let rx_line = Document::get_line_num(rx_node)?;
        let rx_file = rx.document
            .filename
            .as_ref()
            .unwrap_or(&String::new())
            .to_string();
        let doc_file = document
            .filename
            .as_ref()
            .unwrap_or(&"Unknown".to_string())
            .to_string();
        let doc_line = Document::get_line_num(doc_node)?;

        let rx_prompts: Vec<String> = match_pairs
            .iter()
            .map(|pair| {
                let content = pair.0.to_string();
                match ContentMatchPair::is_match(pair) {
                    true => format!(
                        "{}{}{}",
                        color::Fg(color::Green),
                        content,
                        color::Fg(color::Reset)
                    ),
                    false => format!(
                        "{}{}{}",
                        color::Fg(color::Red),
                        content,
                        color::Fg(color::Reset)
                    ),
                }
            })
            .collect();
        let doc_matches: Vec<String> = match_pairs
            .iter()
            .map(|pair| {
                let content = pair.1
                    .as_ref()
                    .unwrap_or(&String::from("<No Match>"))
                    .to_owned();
                match ContentMatchPair::is_match(pair) {
                    true => format!(
                        "{}{}{}",
                        color::Fg(color::Green),
                        content,
                        color::Fg(color::Reset)
                    ),
                    false => format!(
                        "{}{}{}",
                        color::Fg(color::Red),
                        content,
                        color::Fg(color::Reset)
                    ),
                }
            })
            .collect();
        let doc_snippet = doc_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .render_commonmark();
        let rx_snippet = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .render_commonmark();

        Ok(TextualContentError {
            node_type,
            rx_file,
            rx_line,
            doc_file,
            doc_line,
            rx_prompts,
            doc_matches,
            rx_snippet,
            doc_snippet,
        })
    }
}

impl Reportable for TextualContentError {
    fn short_msg(&self) -> String {
        let error_type = format!(
            "{}Textual Content Error{}",
            color::Fg(color::Red),
            color::Fg(color::Reset)
        );
        let rx_info = format!(
            "{}{} line {}{}",
            style::Underline,
            self.rx_file,
            self.rx_line,
            style::Reset
        );
        let doc_info = format!(
            "{}{} line {}{}",
            style::Underline,
            self.doc_file,
            self.doc_line,
            style::Reset
        );
        let node_type = format!("{}{:?}{}", style::Bold, self.node_type, style::Reset);

        format!("{}: {} at {}, {}", error_type, node_type, rx_info, doc_info)
    }

    fn long_msg(&self) -> String {
        let mut message = format!(
            "{}Textual Content Error{}\n\n",
            color::Fg(color::Red),
            color::Fg(color::Reset)
        );
        let rx_info = format!(
            "{}{} line {}{}\n",
            style::Underline,
            self.rx_file,
            self.rx_line,
            style::Reset
        );
        let doc_info = format!(
            "{}{} line {}{}\n",
            style::Underline,
            self.doc_file,
            self.doc_line,
            style::Reset
        );

        message += &format!("{}Prescription : {}", style::Bold, style::Reset);
        message += &self.rx_prompts
            .iter()
            .flat_map(|s| s.chars())
            .collect::<String>();
        message += "\n";
        message += &format!("{}Document     : {}", style::Bold, style::Reset);
        message += &self.doc_matches
            .iter()
            .flat_map(|s| s.chars())
            .collect::<String>();
        message += "\n\n";
        message += &rx_info;
        message += &cli::as_code_lines(&self.rx_snippet, self.rx_line).join("\n");
        message += "\n\n";
        message += &doc_info;
        message += &cli::as_code_lines(&self.doc_snippet, self.doc_line).join("\n");
        message
    }

    fn code(&self) -> u32 {
        1
    }
}
