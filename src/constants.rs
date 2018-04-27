//! Shared library constants.

pub const MANDATORY_PROMPT: &str = "-!!-";
pub const OPTIONAL_PROMPT: &str = "-??-";
pub const DITTO_TOKEN: &str = "-\"\"-";
pub const U_DITTO_TOKEN: &str = "-\u{201d}\u{201d}-";

pub const CONTENT_PROMPT_PATTERN: &str = "(-(?:!!|\\?\\?)-)";
pub const PROMPT_PATTERN: &str = "(-(?:!!|\\?\\?|\u{201d}\u{201d}|\"\")-)";
