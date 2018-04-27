//! Errors and Result Types for validation, reporting, and runtime issues.

extern crate regex;

use helpers::cli::ShellText;
use helpers::cli;
use self::regex::Error as RegexError;
use std::error;
use std::fmt;
use std::io::Error as IOError;
use doogie::errors::DoogieError;
use doogie::Node;
use document::{Document, Prescription};
use std::ops::Add;
use data::{ContentMatchPair, PromptToken};

/// Crate-wide Result type.
pub type HowserResult<T> = Result<T, HowserError>;
/// Convenience alias for validation reporting.
pub type ValidationProblems = Option<Vec<Box<Reportable>>>;

/// Error types for use with `HowserResult`.
#[derive(Debug)]
pub enum HowserError {
    /// Wrapper for errors originating from the `remarkable` crate.
    RemarkableError(DoogieError),
    IOError(IOError),
    Usage(String),
    RuntimeError(String),
    /// For handling instances where a `Node` does not have the required capabilities.
    CapabilityError,
    RegexError(RegexError),
    PrescriptionError,
}

impl error::Error for HowserError {
    fn description(&self) -> &str {
        match self {
            &HowserError::Usage(ref message) => message.as_str(),
            _ => "General Error",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match self {
            &HowserError::RemarkableError(ref error) => Some(error),
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
        HowserError::RemarkableError(err)
    }
}

impl From<RegexError> for HowserError {
    fn from(err: RegexError) -> Self {
        HowserError::RegexError(err)
    }
}

/// Errors and warnings collected during the validation process.
pub struct ValidationReport {
    pub errors: ValidationProblems,
    pub warnings: ValidationProblems,
}

impl ValidationReport {
    pub fn new(errors: ValidationProblems, warnings: ValidationProblems) -> Self {
        ValidationReport { errors, warnings }
    }
}

impl Add for ValidationReport {
    type Output = Self;

    fn add(self, other: ValidationReport) -> Self {
        let mut aggregated_errors = Vec::new();
        let mut aggregated_warnings = Vec::new();

        if let Some(mut errors) = self.errors {
            aggregated_errors.append(&mut errors);
        }
        if let Some(mut errors) = other.errors {
            aggregated_errors.append(&mut errors);
        }
        if let Some(mut warnings) = self.warnings {
            aggregated_warnings.append(&mut warnings);
        }
        if let Some(mut warnings) = other.warnings {
            aggregated_warnings.append(&mut warnings);
        }

        ValidationReport {
            errors: match aggregated_errors.is_empty() {
                true => None,
                false => Some(aggregated_errors),
            },
            warnings: match aggregated_warnings.is_empty() {
                true => None,
                false => Some(aggregated_warnings),
            },
        }
    }
}

/// Final result of the validation process.
///
/// Contains the final verdict on validity along with the reportable info.
pub struct ValidationResult {
    report: ValidationReport,
    valid: bool,
}

impl ValidationResult {
    pub fn new(report: ValidationReport, valid: bool) -> ValidationResult {
        ValidationResult { report, valid }
    }

    pub fn get_warnings<'a>(&'a self) -> &'a ValidationProblems {
        &self.report.warnings
    }

    pub fn get_errors<'a>(&'a self) -> &'a ValidationProblems {
        &self.report.errors
    }

    pub fn is_valid(&self) -> bool {
        self.valid
    }
}

/// Trait for reporting validation problems.
pub trait Reportable {
    /// Report in standard single line format.
    fn short_msg(&self) -> String;

    /// Report in verbose multiline format.
    fn long_msg(&self) -> String;

    /// Error code
    fn code(&self) -> u32;
}

/// A warning related to `Prescription` specification compliance issues.
pub struct SpecWarning {
    line: u32,
    file: String,
    message: String,
}

impl SpecWarning {
    pub fn new(node: &Node, rx: &Prescription, message: &str) -> HowserResult<Self> {
        let getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let line = getter.get_start_line()?;
        let file = match rx.document.filename.as_ref() {
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
        // Todo -- implement verbose message format.
        self.short_msg()
    }

    fn code(&self) -> u32 {
        // Todo -- contextual error codes.
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

        let _rx_getter = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let _rx_renderer = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        // Todo -- Implement get_base_start_line
        let document_line = doc_node_getter.get_start_line()?;
        let rx_line = doc_node_getter.get_start_line()?;
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
        let rx_snippet = _rx_renderer.render_commonmark();

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
        snippet_lines.append(&mut cli::as_code_lines(&self.rx_snippet, self.rx_line));
        message_lines.append(&mut cli::indented_lines(&snippet_lines, 4));

        message_lines.push(String::new());

        let mut snippet_lines = vec![
            ShellText::Underlined(Box::new(ShellText::Literal(self.document_file.to_owned())))
                .to_string(),
        ];
        snippet_lines.append(&mut cli::as_code_lines(
            &self.document_snippet,
            self.document_line,
        ));
        message_lines.append(&mut cli::indented_lines(&snippet_lines, 4));

        message_lines.join("\n")
    }

    fn code(&self) -> u32 {
        // Todo -- Contextual Error Codes
        1
    }
}

/// Error with `Node` content validity.
pub struct ContentError {
    rx_file: String,
    rx_line: u32,
    document_file: String,
    document_line: u32,
    match_pairs: Vec<ContentMatchPair>,
}

impl ContentError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        document: &Document,
        match_pairs: Vec<ContentMatchPair>,
    ) -> HowserResult<Self> {
        let doc_node_getter = doc_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let _doc_node_renderer = doc_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let _rx_node_getter = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let _rx_node_renderer = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        // Todo -- Implement get_base_start_line
        let document_line = doc_node_getter.get_start_line()?;
        let rx_line = doc_node_getter.get_start_line()?;
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

        Ok(ContentError {
            rx_file: rx_file,
            rx_line: rx_line,
            document_file,
            document_line,
            match_pairs,
        })
    }
}

impl Reportable for ContentError {
    fn short_msg(&self) -> String {
        let error_type =
            ShellText::ErrorColor(Box::new(ShellText::Literal("Content Error".to_string())));
        let rx_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.rx_file, self.rx_line
        ))));
        let document_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.document_file, self.document_line
        ))));
        let mut message = String::new();

        for pair in &self.match_pairs {
            if !ContentMatchPair::is_match(&pair) {
                message = match pair {
                    &ContentMatchPair(PromptToken::Mandatory, None) => {
                        ShellText::MessageColor(Box::new(ShellText::Literal(
                            "No match found for Mandatory Prompt".to_string(),
                        ))).to_string()
                    }
                    &ContentMatchPair(PromptToken::Literal(ref required), None) => {
                        let required_content = ShellText::ErrorColor(Box::new(
                            ShellText::Literal(required.clone()),
                        )).to_string();
                        format!(
                            "The required literal content '{}' was missing.",
                            required_content
                        )
                    }
                    &ContentMatchPair(PromptToken::Literal(ref required), Some(ref found)) => {
                        let required_content = ShellText::ErrorColor(Box::new(
                            ShellText::Literal(required.clone()),
                        )).to_string();
                        let found_content = ShellText::ErrorColor(Box::new(ShellText::Literal(
                            found.clone(),
                        ))).to_string();
                        format!(
                            "The required literal content '{}' does not match '{}'",
                            required_content, found_content
                        )
                    }
                    &ContentMatchPair(PromptToken::None, Some(ref content)) => {
                        let found_content = ShellText::ErrorColor(Box::new(ShellText::Literal(
                            content.clone(),
                        ))).to_string();
                        format!("Superfluous content found: '{}'", found_content)
                    }
                    _ => "Content Mismatch".to_string(),
                };
                break;
            }
        }

        format!(
            "{} :: {}, {} :: {}",
            error_type.to_string(),
            rx_info.to_string(),
            document_info.to_string(),
            message
        )
    }

    fn long_msg(&self) -> String {
        // Todo -- Implement verbose message format.
        self.short_msg()
    }

    fn code(&self) -> u32 {
        // Todo -- contextual error codes.
        1
    }
}

/// Error with hyperlink validity.
pub struct LinkError {
    rx_file: String,
    rx_line: u32,
    document_file: String,
    document_line: u32,
}

impl LinkError {
    pub fn new(
        rx_node: &Node,
        doc_node: &Node,
        rx: &Prescription,
        document: &Document,
    ) -> HowserResult<Self> {
        let _doc_node_getter = doc_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let _doc_node_renderer = doc_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let _rx_node_getter = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let _rx_node_renderer = rx_node
            .capabilities
            .render
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        // Todo -- Implement get_base_start_line
        let document_line = _doc_node_getter.get_start_line()?;
        let rx_line = _doc_node_getter.get_start_line()?;
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

        Ok(LinkError {
            rx_file,
            rx_line,
            document_file,
            document_line,
        })
    }
}

impl Reportable for LinkError {
    fn short_msg(&self) -> String {
        let error_type =
            ShellText::ErrorColor(Box::new(ShellText::Literal("Link Error".to_string())));
        let rx_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.rx_file, self.rx_line
        ))));
        let document_info = ShellText::Underlined(Box::new(ShellText::Literal(format!(
            "{} line {}",
            self.document_file, self.document_line
        ))));
        format!(
            "{} :: {}, {}",
            error_type.to_string(),
            rx_info.to_string(),
            document_info.to_string()
        )
    }

    fn long_msg(&self) -> String {
        // Todo -- implement verbose message format.
        self.short_msg()
    }

    fn code(&self) -> u32 {
        // Todo -- Contextual error codes
        1
    }
}
