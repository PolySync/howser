<!--
© 2018, PolySync Technologies, Inc., Devin Smith <dsmith@polysync.io>

This file is part of Howser

Howser is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Howser is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Howser.  If not, see <http://www.gnu.org/licenses/>.
-->

# Howser

## Overview

Howser is a command line tool for verifying document conformance via the [rx spec](https://github.com/PolySync/rx).
If your organization frequently produces Markdown documentation, and it would be nice to ensure that all that
documentation contained certain shapes of things without having to go through it all by hand, then this is the tool for
you. For instance, you could use Howser as part of a build system to verify that all your projects have
sensible README files and consistently formatted requirements documentation.

## Getting Started

### Dependencies

* [libcmark](https://github.com/commonmark/cmark#installing)

### Building

Howser can be built using the normal cargo invocation.

* From the project root, run the command
    ```
    $ cargo build
    ```

### Installation

Howser can be installed using cargo.

* From the project root, run the command
    ```
    $ cargo install
    ```

## Usage

Howser is a command line tool with two major functions. Checking a prescription
file for conformity to the Rx spec and Validating a markdown document against
a prescription file. The checking function is applied implicitly as part of
validation. Validations can be batched by using the `--pharmacy` option in the
validation subcommand.

```
$ howser --help

Howser 0.1.0
Document conformity validator for the Rx spec.

USAGE:
    howser <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information.
    -V, --version    Prints version information.

SUBCOMMANDS:
    check       Verifies that an .rx file conforms to the Rx spec.
    pharmacy    Specifies prescription and document targets from a pharmacy file.
    validate    Validates a Markdown document against an .rx Prescription file.
```

*
    ```
    $ howser check --help
    
    howser-check 
    Verifies that an .rx file conforms to the Rx spec.
    
    USAGE:
        howser check [FLAGS] <PRESCRIPTION>
    
    FLAGS:
        -h, --help       Prints help information.
        -v, --verbose    Use verbose (multiline) output for errors and warnings.
    
    ARGS:
        <PRESCRIPTION>    Prescription file to check
    ```
    
    ```
    $ howser validate --help
    
    howser-validate 
    Validates a Markdown document against an .rx Prescription file.
    
    USAGE:
        howser validate [FLAGS] <PRESCRIPTION> <DOCUMENT>
    
    FLAGS:
        -h, --help       Prints help information.
        -v, --verbose    Use verbose (multiline) output for errors and warnings.
    
    ARGS:
        <PRESCRIPTION>    
        <DOCUMENT> 
    ```
    
    ```
    $ howser pharmacy --help
    
    howser-pharmacy 
    Specifies prescription and document targets from a pharmacy file.
    
    USAGE:
        howser pharmacy <SUBCOMMAND>
    
    FLAGS:
        -h, --help    Prints help information.
    
    SUBCOMMANDS:
        check       Verifies that all the .rx files in the pharmacy file conform to the Rx spec.
        validate    Validates all the Markdown document and .rx Prescription file pairs in the pharmacy file.
    ```
    
    ```
    $ howser pharmacy check
    
    howser-pharmacy-check 
    Verifies that all the .rx files in the pharmacy file conform to the Rx spec.
    
    USAGE:
        howser pharmacy check [FLAGS] <PHARMACY>
    
    FLAGS:
        -e, --fail-early    Stop processing and exit after the first error.
        -h, --help          Prints help information.
        -v, --verbose       Use verbose (multiline) output for errors and warnings.
    
    ARGS:
        <PHARMACY>    The .toml file containing the documents to process.
    ```
    
    ```
    $ howser pharmacy validate
    
    howser-pharmacy-validate 
    Validates all the Markdown document and .rx Prescription file pairs in the pharmacy file.
    
    USAGE:
        howser pharmacy validate [FLAGS] <PHARMACY>
    
    FLAGS:
        -e, --fail-early    Stop processing and exit after the first error.
        -h, --help          Prints help information.
        -v, --verbose       Use verbose (multiline) output for errors and warnings.
    
    ARGS:
        <PHARMACY>    The .toml file containing the documents to process.
    ```

### Examples

* Checking valid and invalid prescription files from the examples directory.
    ```
    $ howser check examples/template.rx
    Valid
    ```

    ```
    $ howser check examples/bad_template.rx
    SpecWarning :: examples/bad_template.rx line 1 :: An element with a Ditto prompt must be preceded by an element of the same type.
    ```

* Validating conforming and non-conforming markdown files against the prescription file `wizard.rx` from the examples directory.

    ```
    $ howser validate examples/wizard.rx examples/wizard.md
    Valid
    ```

    ```
    $ howser validate examples/wizard.rx examples/not_the_wizard.md
    Textual Content Error
    
    Prescription : We're off to see -!!-
    Document     : <No Match>e
    
    examples/wizard.rx line 1
    1  We're off to see -\!\!-
    
    examples/not_the_wizard.md line 1
    1  We're off to see
    ```
    
* Use the included Pharmacy file to run a batch of example validation jobs.

    ```
    $ howser pharmacy validate examples/Pharmacy.toml
    
    Valid
    ```
    
* Use the crate's Pharmacy file to validate the README.md file.

    ```
    $ howser pharmacy validate Pharmacy.toml
    
    Valid
    ```    

## Tests

Howser contains both unit tests and property based tests. The unit tests provide coverage for application logic and
certain specific document validation scenarios while the property based tests target broad coverage of
arbitrarily generated validation scenarios.

### Building

The tests are automatically built by cargo when the test runner is invoked. They can also be built on their own.

```
$ cargo build --tests
```

### Running

The tests are run using cargo. Running the tests will trigger their compilation
if an up-to-date build of them has not been previously performed.

```
$ cargo test
```

# License

© 2018, PolySync Technologies, Inc.

* Devin Smith <dsmith@polysync.io>

Please see the [LICENSE](./LICENSE) file for more details
