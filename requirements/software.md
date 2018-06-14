# Software Requirements

* ### Pharmacy Files

    It is likely that a software project has several different engineering
    documents associated with it such as software requirements,
    architecture design, and so forth. Each of these documents may have a
    corresponding Rx file that Howser can use to validate their
    conformance to a prescribed format. In a continuous integration
    environment, it makes sense to have those validations be performed
    automatically as a step in the build process. In order for that to
    occur, there must be a way to specify which documents in the project
    should be checked for conformance and which prescriptions each of
    those documents should be checked against. This is the motivation for
    introducing a `pharmacy.toml` file as a way to specify this configuration
    on a per-project basis. Pharmacy files are written in toml and checked
    into the repository along with the source code. They specify a series
    of key value pairs that correspond to prescription files and the
    documents that are to be checked with them.
    
    **Rx spec** = A DSL for constraining the content of Markdown 	documents

    **prescription file (.rx)** =  A document written using the Rx spec DSL

    **Pharmacy file (.toml)** = A list of documents to validate paired with the 	prescription they’re validated against

    ### Acceptance Requirements

    #### Background
    
    * **Given** that there exists some valid prescription files arbitrarily located within the project
        * **And** the prescription files are divided into groups "matching" and "mis-matching"
        * **And** for each of the prescription files in the "match" group there exists a corresponding markdown file somewhere in the project that matches the prescription
        * **And** for each of the prescription files in the "mis-matching" group there exists a corresponding markdown file somewhere in the project that does not match the prescription
        * **And** there exists a "pharmacy.toml" file 
        * **And** the pharmacy file contains a toml section labeled "Specs"
        * **And** for each prescription file there exists a toml key-value pair in the "Specs" section of the pharmacy file with filename as key and markdown filename as value

    #### Scenarios

    * ##### Getting help info on the pharmacy subcommand
        * **When** Howser is run without arguments
        * **Then** the user shall see some help information
            * **And** the help information shall describe a pharmacy subcommand
            
    * ##### Getting help info on the subcommands of the pharmacy subcommand
        * **When** Howser is run with the "pharmacy" argument
        * **Then** the user shall see some help information
            * **And** the help information shall describe a "validate" subcommand
            * **And** the help information shall describe a "check" subcommand

    * ##### Getting help info on the pharmacy validate subcommand
        * **When** Howser is run with the arguments "pharmacy" and "validate"
        * **Then** the user shall see some help information
            * **And** the help information shall describe a mandatory "pharmacy" argument
            * **And** the help information shall describe an optional "--fail-early" option
            * **And** the help information shall describe an optional "--verbose" option
            
    * ##### Getting help info on the pharmacy check subcommand
        * **When** Howser is run with the arguments "pharmacy" and "check"
        * **Then** the user shall see some help information
            * **And** the help information shall describe a mandatory "pharmacy" argument
            * **And** the help information shall describe an optional "--fail-early" option
            * **And** the help information shall describe an optional "--verbose" option

    * ##### Validating documents with a pharmacy file
        * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
        * **Then** the user shall only see a validation error message pertaining to each markdown file that do not match the corresponding prescription file

    * ##### Fail Early Option when validating
        * **When** Howser is run with the arguments "pharmacy" and "validate" and "--fail-early" and the pharmacy file path
        * **Then** the user shall only see a validation error message pertaining to the first prescription from the "mis-matching" group

    * ##### Success Message when validating
        * **Given** that only key-value pairs from the "matching" group are present in the "Specs" section of the "pharmacy.toml" file
        * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
        * **Then** the user shall only see "Valid"

    * ##### Missing prescription files when validating
        * **Given** that some prescription files may be missing
        * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
        * **Then** Howser shall exit with a non-zero status code
        * **And** the user shall see an error message for each missing prescription file

    * ##### Missing markdown files when validating
        * **Given** that some markdown files may be missing
        * **When** Howser is run with the arguments "pharmacy" and "validate" and the pharmacy file path
        * **Then** Howser shall exit with a non-zero status code
        * **And** the user shall see an error message for each missing markdown file

    ### Safety Requirements

    #### Background

    None

    #### Scenarios

    None