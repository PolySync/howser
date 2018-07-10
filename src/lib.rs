//! Diagnoses for .md files.
//!
//! Howser is for measuring the conformity of documents written in CMark compliant Markdown
//! to templates written according to the Rx specification.

#![feature(splice)]

#[cfg(test)]
#[macro_use]
extern crate proptest;

#[macro_use]
extern crate log;

extern crate doogie;

pub mod document;
pub mod data;
pub mod errors;
pub mod constants;
pub mod validator;
pub mod reporters;
pub mod helpers;
pub mod checker;
