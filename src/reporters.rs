//! Formatters for reporting validation results.

extern crate termion;

use self::termion::color;
use errors::ValidationProblem;

/// Options for configuring a CLI report.
pub enum CLIOption {
    /// The message to be displayed for a valid document.
    SuccessMessage(String),
    /// the message to be displayed for an invalid document
    VerboseMode(bool),
}

/// Returns a textual report of the validation results suitable for display in a CLI environment.
pub fn make_cli_report(issues: &Option<ValidationProblem>, config: Vec<CLIOption>) -> String {
    let mut report: Vec<String> = Vec::new();

    let mut verbose_mode = false;
    let mut success_message = format!(
        "{}{}{}",
        color::Fg(color::Green),
        "Valid",
        color::Fg(color::Reset)
    );

    for option in config {
        match option {
            CLIOption::SuccessMessage(message) => success_message = message,
            CLIOption::VerboseMode(mode) => verbose_mode = mode,
        }
    }

    if let &Some(ref issue) = issues {
        report.push(match verbose_mode {
            true => issue.long_msg(),
            false => issue.short_msg(),
        });
    }

    if issues.is_none() {
        report.push(success_message);
    }

    report.join("\n\n")
}
