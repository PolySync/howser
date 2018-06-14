/// Integration test suite for the Pharmacy File Feature
///
/// Excerpts from the requirements doc are used here to give context the the suite and individual
/// tests. See the full requirements in requirements/software.md
///
/// #### Background
///
/// * **Given** that there exists some valid prescription files arbitrarily located within the project
///     * **And** the prescription files are divided into groups "matching" and "mis-matching"
///     * **And** for each of the prescription files in the "match" group there exists a corresponding markdown file somewhere in the project that matches the prescription
///     * **And** for each of the prescription files in the "mis-matching" group there exists a corresponding markdown file somewhere in the project that does not match the prescription
///     * **And** there exists a "pharmacy.toml" file
///     * **And** the pharmacy file contains a toml section labeled "Specs"
///     * **And** for each prescription file there exists a toml key-value pair in the "Specs" section of the pharmacy file with filename as key and markdown filename as value
extern crate toml;
extern crate howser;
extern crate assert_cli;
extern crate tempfile;
extern crate env_logger;

mod fixtures;

use assert_cli::{Assert};
use fixtures::PharmacyFixture;

/// * ##### Getting help info on the pharmacy subcommand
///     * **When** Howser is run without arguments
///     * **Then** the user shall see some help information
///     * **And** the help information shall describe a pharmacy subcommand
#[test]
fn test_pharmacy_subcommand_help() {
    Assert::main_binary()
        .fails()
        .and()
        .stderr().contains("pharmacy")
        .unwrap();
}

/// * ##### Getting help info on the subcommands of the pharmacy subcommand
///     * **When** Howser is run with the "pharmacy" argument
///     * **Then** the user shall see some help information
///     * **And** the help information shall describe a "validate" subcommand
///     * **And** the help information shall describe a "check" subcommand
#[test]
fn test_subcommand_help_of_pharmacy_command() {
    Assert::main_binary()
        .with_args(&["pharmacy"])
        .fails()
        .and()
        .stderr().contains("check       Verifies that all the .rx files in the pharmacy file conform to the Rx spec.")
        .and()
        .stderr().contains("validate    Validates all the Markdown document and .rx Prescription file pairs in the pharmacy file.")
        .unwrap();
}

/// * ##### Getting help info on the pharmacy validate subcommand
///     * **When** Howser is run with the arguments "pharmacy" and "validate"
///     * **Then** the user shall see some help information
///     * **And** the help information shall describe a mandatory "pharmacy" argument
///     * **And** the help information shall describe an optional "--fail-early" option
///     * **And** the help information shall describe an optional "--verbose" option
#[test]
fn test_pharmacy_validate_subcommand_help() {
    Assert::main_binary()
        .with_args(&["pharmacy", "validate"])
        .fails()
        .and()
        .stderr().contains("<PHARMACY>    The .toml file containing the documents to process.")
        .unwrap();
}

/// * ##### Getting help info on the pharmacy check subcommand
///     * **When** Howser is run with the arguments "pharmacy" and "check"
///     * **Then** the user shall see some help information
///     * **And** the help information shall describe a mandatory "pharmacy" argument
///     * **And** the help information shall describe an optional "--fail-early" option
///     * **And** the help information shall describe an optional "--verbose" option
#[test]
fn test_pharmacy_check_subcommand_help() {
    Assert::main_binary()
        .with_args(&["pharmacy", "check"])
        .fails()
        .and()
        .stderr().contains("<PHARMACY>    The .toml file containing the documents to process.")
        .unwrap();
}

/// * ##### Validating documents with a pharmacy file
///     * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
///     * **Then** the user shall only see a validation error message pertaining to each markdown file that do not match the corresponding prescription file
#[test]
fn test_pharmacy_validate_failure() {
    let mut pharmacy = PharmacyFixture::new();
    let failure_count = 10;
    for _ in 0..10 {
        pharmacy.add_matched_spec();
    }
    for _ in 0..failure_count {
        pharmacy.add_mismatched_spec();
    }

    Assert::main_binary()
        .with_args(&["pharmacy", "validate", pharmacy.get_path()])
        .stdout().satisfies(move |out| {
        out.to_string().matches("Error").count() == failure_count
    }, "Wrong number of error messages").unwrap();
}

/// * ##### Fail Early Option when validating
///     * **When** Howser is run with the arguments "pharmacy" and "validate" and "--fail-early" and the pharmacy file path
///     * **Then** the user shall only see a validation error message pertaining to the first prescription from the "mis-matching" group
#[test]
fn test_pharmacy_validate_fail_early() {
    let mut pharmacy = PharmacyFixture::new();
    for _ in 0..10 {
        pharmacy.add_matched_spec();
    }
    for _ in 0..10 {
        pharmacy.add_mismatched_spec();
    }

    Assert::main_binary()
        .with_args(&["pharmacy", "validate", "--fail-early", pharmacy.get_path()])
        .stdout().satisfies(move |out| {
        out.to_string().matches("Error").count() == 1
    }, "Wrong number of error messages").unwrap();
}

/// * ##### Success Message when validating
///     * **Given** that only key-value pairs from the "matching" group are present in the "Specs" section of the "pharmacy.toml" file
///     * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
///     * **Then** the user shall only see "Valid"
#[test]
fn test_pharmacy_validate_success() {
    let mut pharmacy = PharmacyFixture::new();
    for _ in 0..10 {
        pharmacy.add_matched_spec();
    }

    Assert::main_binary()
        .with_args(&["pharmacy", "validate", pharmacy.get_path()])
        .stdout().contains("Valid")
        .unwrap();
}

/// * ##### Missing prescription files when validating
///     * **Given** that some prescription files may be missing
///     * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
///     * **Then** Howser shall exit with a non-zero status code
///     * **And** the user shall see an error message for each missing prescription file
#[test]
fn test_pharmacy_validate_fails_when_missing_prescription() {
    let mut pharmacy = PharmacyFixture::new();
    for _ in 0..10 {
        pharmacy.add_matched_spec();
    }
    pharmacy.add_missing_prescription_spec();

    Assert::main_binary()
        .with_args(&["pharmacy", "validate", pharmacy.get_path()])
        .fails()
        .and()
        .stdout().contains("entity not found")
        .unwrap();
}

/// * ##### Missing markdown files when validating
///     * **Given** that some markdown files may be missing
///     * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
///     * **Then** Howser shall exit with a non-zero status code
///     * **And** the user shall see an error message for each missing markdown file
#[test]
fn test_pharmacy_validate_fails_when_missing_document() {
    let mut pharmacy = PharmacyFixture::new();
    for _ in 0..10 {
        pharmacy.add_matched_spec();
    }
    pharmacy.add_missing_doc_spec();

    Assert::main_binary()
        .with_args(&["pharmacy", "validate", pharmacy.get_path()])
        .fails()
        .and()
        .stdout().contains("entity not found")
        .unwrap();
}
