#[macro_use]
extern crate clap;
extern crate doogie;
extern crate env_logger;
extern crate howser;
extern crate toml;

use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};
use doogie::parse_document;
use howser::document::Document;
use howser::errors::{HowserError, HowserResult, ValidationProblem};
use howser::reporters::{make_cli_report, CLIOption};
use howser::validator::Validator;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::str;
use std::collections::BTreeMap;
use toml::Value;

fn main() {
    env_logger::init();

    let app = make_app();
    let matches = app.get_matches();

    if let Err(e) = run(&matches) {
        println!("{}", e.description());
        let mut inner_err = e.cause();
        while let Some(error) = inner_err {
            println!("{}", error.description());
            inner_err = error.cause();
        }
        std::process::exit(1);
    } else {
        std::process::exit(0);
    }
}

fn run(args: &ArgMatches) -> HowserResult<()> {
    let (issues, options) = match args.subcommand() {
        ("check", Some(sub_m)) => {
            let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
            let filename = args
                .value_of("prescription")
                .ok_or(HowserError::RuntimeError(
                    "Error parsing prescription filename.".to_string()))?;

            (check(filename)?, options)
        }
        ("validate", Some(sub_m)) => {
            let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
            let rx_name = sub_m
                .value_of("prescription")
                .ok_or(HowserError::RuntimeError(
                    "Unable to parse the name of the prescription file.".to_string()))?;
            let document_name = sub_m
                .value_of("document")
                .ok_or(HowserError::RuntimeError(
                    "Unable to parse the name of the document file.".to_string()))?;

            (validate(rx_name, document_name)?, options)
        },
        ("pharmacy", Some(sub_m)) => {
            match sub_m.subcommand() {
                ("check", Some(sub_m)) => {
                    let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
                    let fail_early = sub_m.is_present("fail-early");
                    let filename = sub_m
                        .value_of("pharmacy").
                        ok_or(HowserError::RuntimeError(
                            "Pharmacy filename could not be parsed from the argument string.".to_string()))?;
                    let pharmacy = parse_pharmacy_file(filename)?;
                    (check_pharmacy(&pharmacy, fail_early)?, options)
                },
                ("validate", Some(sub_m)) => {
                    let options = vec![CLIOption::VerboseMode(sub_m.is_present("verbose"))];
                    let fail_early = sub_m.is_present("fail-early");
                    let filename = sub_m
                        .value_of("pharmacy").
                        ok_or(HowserError::RuntimeError(
                            "Pharmacy filename could not be parsed from the argument string.".to_string()))?;
                    let pharmacy = parse_pharmacy_file(filename)?;
                    (validate_pharmacy(&pharmacy, fail_early)?, options)
                },
                _ => return Err(HowserError::Usage(args.usage().to_string()))
            }
        }
        _ => return Err(HowserError::Usage(args.usage().to_string()))
    };
    let cli_report = make_cli_report(&issues, &options);

    println!("{}", cli_report);

    Ok(())
}

fn parse_pharmacy_file(filename: &str) -> HowserResult<Pharmacy> {
    let pharmacy_contents = get_file_contents(filename)?;
    parse_pharmacy_string(filename, pharmacy_contents)
}

fn parse_pharmacy_string(filename: &str, pharmacy_file_contents: String) -> HowserResult<Pharmacy> {
    let pharmacy = pharmacy_file_contents.parse::<Value>()?;
    let ref specs = pharmacy["Specs"];
    let prescription_pairs = specs
        .as_table()
        .ok_or(HowserError::RuntimeError(
            format!("Error parsing pharmacy file {}.", filename)))?;
    let mut spec_to_targets:BTreeMap<PathBuf, Vec<PathBuf>> = BTreeMap::new();
    for (key, value) in prescription_pairs {
        let targets:Vec<PathBuf> = match *value {
            Value::String(ref s) => vec![PathBuf::from(s)],
            Value::Array(ref array) => {
                array.iter().map(|a| if let Some(s) = a.as_str() {
                    Ok(PathBuf::from(s))
                } else {
                    Err(HowserError::RuntimeError(
                        format!("Error parsing pharmacy file {}. Target value was not a string or array of strings", filename)))
                })
                    .collect::<Result<Vec<PathBuf>, HowserError>>()?
            },
            _ => return Err(HowserError::RuntimeError(
            format!("Error parsing pharmacy file {}. Target value was not a string or array of strings", filename))),
        };
        spec_to_targets.insert(PathBuf::from(key), targets);
    }
    Ok(Pharmacy { spec_to_targets })
}

/// Named wrapper around the mapping between the file locations
/// for Rx spec files and their associated target documents intended
/// for validation.
#[derive(Clone, Debug, PartialEq)]
struct Pharmacy {
    spec_to_targets: BTreeMap<PathBuf, Vec<PathBuf>>
}

fn make_app<'a, 'b>() -> App<'a, 'b> {
    let check = SubCommand::with_name("check")
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
        );
    let validate = SubCommand::with_name("validate")
        .about("Validates a Markdown document against an .rx Prescription file.")
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
                .required_unless("pharmacy")
                .takes_value(true)
                .value_name("PRESCRIPTION"),
        )
        .arg(
            Arg::with_name("document")
                .required_unless("pharmacy")
                .takes_value(true)
                .value_name("DOCUMENT"),
        );
    let pharmacy_check = SubCommand::with_name("check")
        .about("Verifies that all the .rx files in the pharmacy file conform to the Rx spec.")
        .help_message("Prints help information.")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(
            Arg::with_name("pharmacy")
                .required(true)
                .help("The .toml file containing the documents to process.")
                .takes_value(true)
                .value_name("PHARMACY"),
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Use verbose (multiline) output for errors and warnings."),
        )
        .arg(
            Arg::with_name("fail-early")
                .short("-e")
                .long("fail-early")
                .help("Stop processing and exit after the first error.")
        );
    let pharmacy_validate = SubCommand::with_name("validate")
        .about("Validates all the Markdown document and .rx Prescription file pairs in the pharmacy file.")
        .help_message("Prints help information.")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(
            Arg::with_name("pharmacy")
                .required(true)
                .help("The .toml file containing the documents to process.")
                .value_name("PHARMACY")
        )
        .arg(
            Arg::with_name("fail-early")
                .short("-e")
                .long("fail-early")
                .help("Stop processing and exit after the first error.")
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Use verbose (multiline) output for errors and warnings."),
        );

    App::new("Howser")
        .about("Document conformity validator for the Rx spec.")
        .version(crate_version!())
        .version_message("Prints version information.")
        .help_message("Prints help information.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .setting(AppSettings::DisableHelpSubcommand)
        .subcommand(check.clone())
        .subcommand(validate.clone())
        .subcommand(
            SubCommand::with_name("pharmacy")
                .about("Specifies prescription and document targets from a pharmacy file.")
                .help_message("Prints help information.")
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .setting(AppSettings::DisableHelpSubcommand)
                .setting(AppSettings::VersionlessSubcommands)
                .subcommand(pharmacy_check)
                .subcommand(pharmacy_validate)
        )
}

fn validate<P: AsRef<Path>, Q: AsRef<Path>>(rx_name: P, document_name: Q) -> HowserResult<Vec<ValidationProblem>> {
    let rx_root = parse_document(&get_file_contents(&rx_name)?);
    let doc_root = parse_document(&get_file_contents(&document_name)?);
    let rx = Document::new(&rx_root, rx_name.as_ref().to_str().map(|s| s.to_string()))?.into_prescription()?;
    let document = Document::new(&doc_root, document_name.as_ref().to_str().map(|s| s.to_string()))?;

    Validator::new(rx, document).validate()
}

fn check<P: AsRef<Path>>(filename: P) -> HowserResult<Vec<ValidationProblem>> {
    let rx_root = parse_document(&get_file_contents(&filename)?);
    let document = Document::new(&rx_root, filename.as_ref().to_str().map(|s| s.to_string()))?;

    match document.into_prescription() {
        Err(HowserError::PrescriptionError(warning)) => Ok(vec![Box::new(warning)]),
        Err(error) => Err(error),
        Ok(_) => Ok(Vec::new()),
    }
}

fn check_pharmacy(pharmacy: &Pharmacy, fail_early: bool) -> HowserResult<Vec<ValidationProblem>> {
    let mut report: Vec<ValidationProblem> = Vec::new();

    for rx_file in pharmacy.spec_to_targets.keys() {
        let mut problems = check(&rx_file)?;
        if fail_early && !problems.is_empty() {
            return Ok(problems);
        } else {
            report.append(&mut problems);
        }
    }

    Ok(report)
}

fn validate_pharmacy(pharmacy: &Pharmacy, fail_early: bool) -> HowserResult<Vec<ValidationProblem>> {
    let mut report: Vec<ValidationProblem> = Vec::new();

    for (rx_file, target_docs) in pharmacy.spec_to_targets.iter() {
        for doc_file in target_docs {
            let mut problems = validate(&rx_file, doc_file)?;
            if fail_early && !problems.is_empty() {
                return Ok(problems);
            } else {
                report.append(&mut problems);
            }
        }
    }

    Ok(report)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::path::PathBuf;
    use super::clap::ErrorKind;
    use super::{parse_pharmacy_string, HowserError, Pharmacy};

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
            assert_eq!(e.kind, ErrorKind::MissingArgumentOrSubcommand);
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

    #[test]
    fn test_pharmacy_check_subcommand() {
        let app = super::make_app();
        let pharmacy_file = "pharmacy.toml";
        let matches = app.get_matches_from(vec!["howser", "pharmacy", "check", pharmacy_file]);
        let pharmacy_matches = matches.subcommand_matches("pharmacy").expect("Does not contain pharmacy subcommand");
        let check_matches = pharmacy_matches.subcommand_matches("check").expect("Does not containt check subcommand");
        assert_eq!(check_matches.value_of("pharmacy"), Some(pharmacy_file));
    }

    #[test]
    fn test_pharmacy_validate_subcommand() {
        let app = super::make_app();
        let pharmacy_file = "pharmacy.toml";
        let matches = app.get_matches_from(vec!["howser", "pharmacy", "validate", pharmacy_file]);
        let pharmacy_matches = matches.subcommand_matches("pharmacy").expect("Does not contain pharmacy subcommand");
        let check_matches = pharmacy_matches.subcommand_matches("validate").expect("Does not containt validate subcommand");
        assert_eq!(check_matches.value_of("pharmacy"), Some(pharmacy_file));
    }

    #[test]
    fn parse_pharmacy_allows_empty_table() {
        assert_eq!(Pharmacy { spec_to_targets: BTreeMap::new() },
                   parse_pharmacy_string("test_file", r#"[Specs]"#.to_string())
                       .expect("Should have been able to parse"));
    }

    #[test]
    fn parse_pharmacy_supports_string_values() {
        let mut m = BTreeMap::new();
        m.insert(PathBuf::from("README.rx"), vec![PathBuf::from("README.md")]);
        assert_eq!(Pharmacy { spec_to_targets: m },
                   parse_pharmacy_string("test_file", r#"[Specs]
                   "README.rx" = "README.md""#.to_string())
                       .expect("Should have been able to parse"));
    }

    #[test]
    fn parse_pharmacy_supports_array_of_string_values() {
        let mut m = BTreeMap::new();
        m.insert(PathBuf::from("README.rx"), vec![
            PathBuf::from("README.md"), PathBuf::from("subdir/README.md")]);
        assert_eq!(Pharmacy { spec_to_targets: m },
                   parse_pharmacy_string("test_file", r#"[Specs]
                   "README.rx" = ["README.md", "subdir/README.md"]"#.to_string())
                       .expect("Should have been able to parse"));
    }

    #[test]
    fn parse_pharmacy_disallows_duplicate_keys() {
        let result = parse_pharmacy_string("test_file", r#"[Specs]
            "README.rx" = "README.md"
            "README.rx" = "subdir/README.md""#.to_string());
        match result {
            Err(HowserError::TomlError(_)) => println!("As expected"),
            x @ _ => panic!("Unexpected success or kind of error: {:?}", x),
        }
    }
}

/// Returns the textual content of the indicated file
///
/// # Arguments
/// 'file_name': The name of the file to get.
fn get_file_contents<P: AsRef<Path>>(file_name: P) -> HowserResult<String> {
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
