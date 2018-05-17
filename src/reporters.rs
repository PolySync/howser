//! Formatters for reporting validation results.

use errors::ValidationResult;
use helpers::cli::ShellText;

/// Options for configuring a CLI report.
pub enum CLIOption {
    /// The message to be displayed for a valid document.
    SuccessMessage(String),
    /// the message to be displayed for an invalid document
    FailureMessage(String),
    /// The CLI is operating in verbose mode.
    VerboseMode(bool),
}

/// Returns a textual report of the validation results suitable for display in a CLI environment.
pub fn make_cli_report(result: &ValidationResult, config: Vec<CLIOption>) -> String {
    let mut report: Vec<String> = Vec::new();

    let mut verbose_mode = false;
    let mut success_message =
        ShellText::OkColor(Box::new(ShellText::Literal("Rx Filled!".to_string()))).to_string();
    let mut failure_message =
        ShellText::ErrorColor(Box::new(ShellText::Literal("Rx Rejected!".to_string()))).to_string();

    for option in config {
        match option {
            CLIOption::SuccessMessage(message) => success_message = message,
            CLIOption::FailureMessage(message) => failure_message = message,
            CLIOption::VerboseMode(mode) => verbose_mode = mode,
        }
    }

    if let &Some(ref issues) = result.get_issues() {
        for issue in issues {
            report.push(match verbose_mode {
                true => issue.long_msg(),
                false => issue.short_msg(),
            });
        }
    }

    report.push(match result.is_valid() {
        true => success_message,
        false => failure_message,
    });

    report.join("\n\n")
}
