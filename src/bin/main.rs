#[macro_use]
extern crate clap;
#[macro_use]
extern crate log;

extern crate doogie;
extern crate env_logger;
extern crate howser;

use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};
use doogie::parse_document;
use howser::document::Document;
use howser::errors::{HowserError, HowserResult, ValidationProblem};
use howser::reporters::{make_cli_report, CLIOption};
use howser::validator::Validator;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::str;

fn main() {
    env_logger::init();

    let app = make_app();
    let matches = app.get_matches();

    let (issues, options) = match matches.subcommand() {
        ("check", Some(sub_m)) => {
            let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
            (check(sub_m), options)
        }
        ("validate", Some(sub_m)) => {
            let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
            if let Some(filemane) = sub_m.value_of("pharmacy") {
                (do_pharmacy_job(&sub_m), options)
            } else {
                (validate(sub_m), options)
            }
        }
        _ => (
            Err(HowserError::Usage(matches.usage().to_string())),
            Vec::new(),
        ),
    };

    match issues {
        Ok(issues) => {
            let cli_report = make_cli_report(&issues, options);
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
        .version_message("Prints version information.")
        .help_message("Prints help information.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .setting(AppSettings::DisableHelpSubcommand)
        .subcommand(
            SubCommand::with_name("check")
                .about("Verifies that an .rx file conforms to the Rx spec.")
                .help_message("Prints help information.")
                .setting(AppSettings::ArgRequiredElseHelp)
                .arg(
                    Arg::with_name("verbose")
                        .short("v")
                        .long("verbose")
                        .help("Use verbose (multiline) output for errors and warnings."),
                )
                .arg(
                    Arg::with_name("prescription")
                        .required(true)
                        .help("Prescription file to check")
                        .takes_value(true)
                        .value_name("PRESCRIPTION"),
                ),
        )
        .subcommand(
            SubCommand::with_name("validate")
                .about("Validate a Markdown document against an .rx Prescription file.")
                .help_message("Prints help information.")
                .setting(AppSettings::ArgRequiredElseHelp)
                .arg(
                    Arg::with_name("pharmacy")
                        .short("-p")
                        .long("pharmacy")
                        .help("Performs validation based on a pharmacy file")
                        .value_name("PHARMACY")
                        .takes_value(true))
                .arg(
                    Arg::with_name("verbose")
                        .short("v")
                        .long("verbose")
                        .help("Use verbose (multiline) output for errors and warnings."),
                )
                .arg(
                    Arg::with_name("prescription")
                        .required_unless("pharmacy")
                        .takes_value(true)
                        .value_name("PRESCRIPTION"),
                )
                .arg(
                    Arg::with_name("document")
                        .required_unless("pharmacy")
                        .takes_value(true)
                        .value_name("DOCUMENT"),
                ),
        )
}

fn validate(args: &ArgMatches) -> HowserResult<Option<ValidationProblem>> {
    let rx_name = args.value_of("prescription");
    let document_name = args.value_of("document");

    if let Some(rx_name) = rx_name {
        if let Some(document_name) = document_name {
            let rx_root = parse_document(&get_file_contents(rx_name)?);
            let doc_root = parse_document(&get_file_contents(document_name)?);
            let rx = Document::new(&rx_root, Some(rx_name.to_string()))?.into_prescription()?;
            let document = Document::new(&doc_root, Some(document_name.to_string()))?;
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

fn check(args: &ArgMatches) -> HowserResult<Option<ValidationProblem>> {
    if let Some(filename) = args.value_of("prescription") {
        let filename = String::from(filename);
        let rx_root = parse_document(&get_file_contents(&filename)?);
        let document = Document::new(&rx_root, Some(filename))?;

        match document.into_prescription() {
            Err(HowserError::PrescriptionError(warning)) => Ok(Some(Box::new(warning))),
            Err(error) => Err(error),
            Ok(_) => Ok(None),
        }
    } else {
        Err(HowserError::RuntimeError(
            "Prescription filename could not be parsed from the argument string.".to_string(),
        ))
    }
}

fn do_pharmacy_job(matches: &ArgMatches) -> (HowserResult<ValidationReport>, Vec<CLIOption>) {
    // get pharmacy file
    // parse rx, doc pairs
    // loop through pairs and return report
    (Ok(ValidationReport::new(None, None)), Vec::new())
}

#[cfg(test)]
mod tests {
    use super::clap::ErrorKind;

    #[test]
    fn test_validate_subcommand() {
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
    fn test_check_subcommand() {
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
