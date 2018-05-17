#[macro_use]
extern crate clap;

extern crate doogie;
extern crate env_logger;
extern crate howser;

use clap::{App, Arg, ArgMatches, SubCommand};
use doogie::parse_document;
use howser::document::Document;
use howser::errors::{HowserError, HowserResult, ValidationProblems, ValidationResult};
use howser::helpers::cli::ShellText;
use howser::reporters::{make_cli_report, CLIOption};
use howser::validator::Validator;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    env_logger::init();

    let matches = make_app().get_matches();
    let (report, mut options) = match matches.subcommand() {
        ("check", Some(sub_m)) => {
            let success_msg = ShellText::OkColor(Box::new(ShellText::Literal(
                "Valid Rx".to_string(),
            ))).to_string();
            let failure_msg = ShellText::ErrorColor(Box::new(ShellText::Literal(
                "Invalid Rx".to_string(),
            ))).to_string();
            (
                check(sub_m),
                vec![
                    CLIOption::SuccessMessage(success_msg),
                    CLIOption::FailureMessage(failure_msg),
                ],
            )
        }
        ("validate", Some(sub_m)) => (validate(sub_m), Vec::new()),
        _ => (
            Err(HowserError::Usage(String::from(matches.usage()))),
            Vec::new(),
        ),
    };

    options.push(CLIOption::VerboseMode(matches.is_present("verbose")));
    match report {
        Ok(problems) => {
            let is_valid = problems.is_none();
            let validation_result = ValidationResult::new(problems, is_valid);
            let cli_report = make_cli_report(&validation_result, options);
            println!("{}", cli_report);
            std::process::exit(0);
        }
        Err(e) => {
            println!("{}", e.description());
            std::process::exit(1);
        }
    };
}

fn make_app<'a, 'b>() -> App<'a, 'b> {
    App::new("Howser")
        .about("Document conformity validator for the Rx spec.")
        .version(crate_version!())
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Use verbose (multiline) output for errors and warnings."),
        )
        .subcommand(
            SubCommand::with_name("check")
                .about("Checks that a document intended for use as a prescription conforms to the Rx spec.")
                .arg(
                    Arg::with_name("prescription")
                        .required(true)
                        .help("Prescription file to check.")
                        .takes_value(true)
                        .value_name("PRESCRIPTION"),
                ),
        )
        .subcommand(
            SubCommand::with_name("validate")
                .about("Validate a Markdown document against an Rx Prescription file.")
                .arg(
                    Arg::with_name("prescription")
                        .required(true)
                        .takes_value(true)
                        .value_name("PRESCRIPTION"),
                )
                .arg(
                    Arg::with_name("document")
                        .required(true)
                        .takes_value(true)
                        .value_name("DOCUMENT"),
                ),
        )
}

fn validate(args: &ArgMatches) -> HowserResult<ValidationProblems> {
    let rx_name = args.value_of("prescription");
    let document_name = args.value_of("document");

    if let Some(rx_name) = rx_name {
        if let Some(document_name) = document_name {
            let rx_root = parse_document(&get_file_contents(rx_name)?);
            let doc_root = parse_document(&get_file_contents(document_name)?);
            let rx = Document::new(&rx_root, Some(rx_name.to_string())).into_prescription()?;
            let document = Document::new(&doc_root, Some(document_name.to_string()));
            Validator::new(rx, document).validate()
        } else {
            Err(HowserError::RuntimeError(
                "Document filename could not be parsed from the argument string.".to_string(),
            ))
        }
    } else {
        Err(HowserError::RuntimeError(
            "Prescription filename could not be parsed from the argument string.".to_string(),
        ))
    }
}

fn check(args: &ArgMatches) -> HowserResult<ValidationProblems> {
    if let Some(filename) = args.value_of("prescription") {
        let filename = String::from(filename);
        let rx_root = parse_document(&get_file_contents(&filename)?);
        let document = Document::new(&rx_root, Some(filename));
        match document.into_prescription() {
            Err(HowserError::PrescriptionError(warning)) => Ok(Some(vec![Box::new(warning)])),
            Err(error) => Err(error),
            Ok(_) => Ok(None),
        }
    } else {
        Err(HowserError::RuntimeError(
            "Prescription filename could not be parsed from the argument string.".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::clap::ErrorKind;

    #[test]
    fn test_matches_validate_subcommand() {
        let app = super::make_app();
        let matches =
            app.get_matches_from(vec!["howser", "validate", "some_template", "some_document"]);
        assert_eq!(matches.subcommand_name(), Some("validate"));
    }

    #[test]
    fn test_validate_subcommand_has_prescription() {
        let app = super::make_app();
        let matches =
            app.get_matches_from(vec!["howser", "validate", "some_template", "some_document"]);
        let sub_m = matches.subcommand_matches("validate").unwrap();
        assert_eq!(sub_m.value_of("prescription").unwrap(), "some_template");
    }

    #[test]
    fn test_validate_subcommand_has_document() {
        let app = super::make_app();
        let matches =
            app.get_matches_from(vec!["howser", "validate", "some_template", "some_document"]);
        let sub_m = matches.subcommand_matches("validate").unwrap();
        assert_eq!(sub_m.value_of("document").unwrap(), "some_document");
    }

    #[test]
    fn test_validate_subcommand_requires_enough_args() {
        let app = super::make_app();
        if let Err(e) = app.get_matches_from_safe(vec!["howser", "validate", "some_template"]) {
            assert_eq!(e.kind, ErrorKind::MissingRequiredArgument);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_validate_subcommand_prevents_extra_args() {
        let app = super::make_app();
        if let Err(e) = app.get_matches_from_safe(vec![
            "howser",
            "validate",
            "some_template",
            "some_document",
            "something_extra",
        ]) {
            assert_eq!(e.kind, ErrorKind::UnknownArgument);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_matches_check_subcommand() {
        let app = super::make_app();
        let matches = app.get_matches_from(vec!["howser", "check", "some_template"]);
        assert_eq!(matches.subcommand_name(), Some("check"));
    }

    #[test]
    fn test_check_subcommand_has_prescription() {
        let app = super::make_app();
        let matches = app.get_matches_from(vec!["howser", "check", "some_template"]);
        let sub_m = matches.subcommand_matches("check").unwrap();
        assert_eq!(sub_m.value_of("prescription").unwrap(), "some_template");
    }

    #[test]
    fn test_check_subcommand_requires_enough_args() {
        let app = super::make_app();
        if let Err(e) = app.get_matches_from_safe(vec!["howser", "check"]) {
            assert_eq!(e.kind, ErrorKind::MissingRequiredArgument);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_check_subcommand_prevents_extra_args() {
        let app = super::make_app();
        if let Err(e) =
            app.get_matches_from_safe(vec!["howser", "check", "some_template", "some_document"])
        {
            assert_eq!(e.kind, ErrorKind::UnknownArgument);
        } else {
            panic!();
        }
    }
}

/// Returns the textual content of the indicated file
///
/// # Arguments
/// 'file_name': The name of the file to get.
fn get_file_contents(file_name: &str) -> HowserResult<String> {
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
