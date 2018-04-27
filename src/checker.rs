//! Checks a `Prescription` for conformance to the Rx spec.

use document::Prescription;
use doogie::Node;
use data::MatchType;
use errors::{HowserError, HowserResult, SpecWarning, ValidationProblems, ValidationReport};

/// Rx specification rules that are to be applied on a per-node basis.
///
/// Rules can be composed together in a decorator pattern.
///
/// # Examples
/// ```
/// #[doc(hidden)]
/// use howser::checker::SpecRule;
///
/// let rule_set = SpecRule::DittoContent(Some(Box::new(SpecRule::MatchingDittoType(None))));
/// ```
pub enum SpecRule {
    DittoContent(Option<Box<SpecRule>>),
    MatchingDittoType(Option<Box<SpecRule>>),
}

impl SpecRule {
    /// Returns a rule instance that has been composed with a standard set of available rules.
    pub fn standard_set() -> SpecRule {
        SpecRule::DittoContent(Some(Box::new(SpecRule::MatchingDittoType(None))))
    }

    /// Checks the current node for compliance to the specification rule.
    pub fn check(&self, node: &Node, rx: &Prescription) -> HowserResult<ValidationProblems> {
        match self {
            &SpecRule::DittoContent(ref inner_rule) => {
                let mut problems = Vec::new();
                if let &Some(ref rule) = inner_rule {
                    if let Some(more_problems) = rule.check(node, rx)?.as_mut() {
                        problems.append(more_problems);
                    }
                }

                if rx.document.get_match_type(node)? == MatchType::Repeatable {
                    let traverser = node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    if traverser.first_child()?.is_some() {
                        let warning = SpecWarning::new(
                            node,
                            rx,
                            "Content after the ditto token will be ignored.",
                        )?;
                        problems.push(Box::new(warning));
                    }
                }

                match problems.is_empty() {
                    true => Ok(None),
                    false => Ok(Some(problems)),
                }
            }
            &SpecRule::MatchingDittoType(ref inner_rule) => {
                let mut problems = Vec::new();

                if let &Some(ref rule) = inner_rule {
                    if let Some(more_problems) = rule.check(node, rx)?.as_mut() {
                        problems.append(more_problems);
                    }
                }

                let match_type = rx.document.get_match_type(node)?;
                let message = "An element with a Ditto prompt must be preceded by an element of the same type.";
                if match_type == MatchType::Repeatable {
                    let traverser = node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    match traverser.prev_sibling()? {
                        None => problems.push(Box::new(SpecWarning::new(node, rx, message)?)),
                        Some(sibling) => {
                            let getter = node.capabilities
                                .get
                                .as_ref()
                                .ok_or(HowserError::CapabilityError)?;
                            let sibling_getter = sibling
                                .capabilities
                                .get
                                .as_ref()
                                .ok_or(HowserError::CapabilityError)?;
                            if getter.get_type()? != sibling_getter.get_type()? {
                                problems.push(Box::new(SpecWarning::new(node, rx, message)?));
                            }
                        }
                    };
                }

                match problems.is_empty() {
                    true => Ok(None),
                    false => Ok(Some(problems)),
                }
            }
        }
    }
}

/// Checks a `Template` for compliance with a particular set of rules.
pub struct SpecChecker {
    rules: SpecRule,
}

impl SpecChecker {
    /// Returns an instance of SpecChecker configured with the supplied specification rules.
    pub fn new(rules: SpecRule) -> Self {
        SpecChecker { rules }
    }

    /// Checks a `Template` for specification compliance and returns a `ValidationReport` of the results.
    pub fn check(&self, prescription: &Prescription) -> HowserResult<ValidationReport> {
        let traverser = prescription
            .document
            .root
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let problems = self.check_level(traverser.first_child()?, prescription)?;

        Ok(ValidationReport::new(None, problems))
    }

    fn check_level(
        &self,
        node: Option<Node>,
        rx: &Prescription,
    ) -> HowserResult<ValidationProblems> {
        let mut problems = Vec::new();
        let mut sibling = node;

        while let Some(current_node) = sibling {
            let traverser = current_node
                .capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;

            if let Some(more_problems) = self.rules.check(&current_node, rx)?.as_mut() {
                problems.append(more_problems)
            }
            if let Some(more_problems) = self.check_level(traverser.first_child()?, rx)?.as_mut() {
                problems.append(more_problems)
            }

            sibling = traverser.next_sibling()?;
        }

        match problems.is_empty() {
            true => Ok(None),
            false => Ok(Some(problems)),
        }
    }
}
