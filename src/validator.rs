//! For validation of documents.

extern crate env_logger;
extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use constants::{CONTENT_PROMPT_PATTERN, MANDATORY_PROMPT, OPTIONAL_PROMPT};
use data::ElementType;
use data::{ContentMatchPair, MatchType, PromptToken};
use document::{Document, Prescription};
use doogie::Node;
use errors::{
    DocumentError, HowserError, HowserResult, Reportable, TextualContentError, TypeMismatchError,
    ValidationProblem,
};
use std::collections::VecDeque;

/// Arguments for validating mandatory block level elements.
struct MandatoryMatchInput {
    rx: Node,
    node: Option<Node>,
    bookmark: Node,
}

/// Arguments for validating optional block level elements.
struct OptionalMatchInput {
    rx: Node,
    node: Option<Node>,
    bookmark: Option<Node>,
}

/// Type for managing the state of the validation process.
struct MatchState {
    rx: Option<Node>,
    node: Option<Node>,
    bookmark: Option<Node>,
}

/// Return type for a general validation step.
enum MatchResult {
    State(MatchState),
    Error(Box<Reportable>),
}

/// Tracks state in matching algorithms that alternate matching directions.
#[derive(Debug)]
enum MatchDirection {
    Left,
    Right,
}

/// Validates a `Document` against an Rx `Prescription`.
pub struct Validator<'a> {
    prescription: Prescription<'a>,
    document: Document<'a>,
}

impl<'a> Validator<'a> {
    /// Returns a new `Validator`.
    pub fn new(prescription: Prescription<'a>, document: Document<'a>) -> Self {
        Validator {
            prescription: prescription,
            document,
        }
    }

    /// Validates the document against the prescription and returns the results.
    ///
    /// `None` indicates that the document is valid.
    pub fn validate(&self) -> HowserResult<Vec<ValidationProblem>> {
        trace!("validate()");
        let mut problems = Vec::new();
        if let Some(problem) =
            self.validate_sibling_blocks(&self.prescription.document.root, &self.document.root)?
        {
            problems.push(problem);
        }
        Ok(problems)
    }

    /// Validates a set of sibling block elements
    fn validate_sibling_blocks(
        &self,
        parent_rx_node: &Node,
        parent_doc_node: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_sibling_blocks::");
        let mut current_rx = parent_rx_node.first_child()?;
        let mut current_node = parent_doc_node.first_child()?;
        let mut current_bookmark = parent_doc_node.first_child()?;

        while let Some(rx) = current_rx {
            let is_repeatable = match rx.next_sibling()? {
                Some(next_rx) => {
                    self.prescription.document.get_match_type(&next_rx)? == MatchType::Repeatable
                }
                _ => false,
            };

            if is_repeatable {
                match self.consume_repeatable_matches(
                    rx,
                    current_node,
                    current_bookmark,
                    parent_doc_node,
                )? {
                    MatchResult::State(state) => {
                        let MatchState {
                            rx,
                            node,
                            bookmark: new_bookmark,
                        } = state;

                        current_rx = rx;
                        current_node = node;
                        current_bookmark = new_bookmark;
                    }
                    MatchResult::Error(errors) => {
                        return Ok(Some(errors));
                    }
                }
            } else {
                match self.consume_block_match(rx, current_node, current_bookmark, parent_doc_node)?
                {
                    MatchResult::State(state) => {
                        let MatchState {
                            rx,
                            node,
                            bookmark: new_bookmark,
                        } = state;

                        current_rx = rx;
                        current_node = node;
                        current_bookmark = new_bookmark;
                    }
                    MatchResult::Error(errors) => {
                        return Ok(Some(errors));
                    }
                }
            }
        }

        if let Some(extra_node) = current_node {
            debug!("validate_sibling_blocks:: Superfluous Nodes Error");
            let error = DocumentError::new(
                &extra_node,
                parent_rx_node,
                &self.document,
                &self.prescription,
                "Superfluous block content was present.".to_string(),
            )?;
            Ok(Some(Box::new(error)))
        } else {
            info!("validate_sibling_blocks:: Valid!");
            Ok(None)
        }
    }

    /// Performs validation on a set of sibling inline elements.
    ///
    /// Returns `None` if the siblings are valid.
    fn validate_sibling_inlines(
        &self,
        parent_rx: &Node,
        parent_node: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_sibling_inlines::");
        let mut current_rx = parent_rx.first_child()?;
        let mut current_node = parent_node.first_child()?;
        let mut current_bookmark = parent_node.first_child()?;

        while let Some(rx) = current_rx {
            match self.consume_inline_match(rx, current_node, current_bookmark, parent_node)? {
                MatchResult::State(state) => {
                    let MatchState {
                        rx,
                        node,
                        bookmark: new_bookmark,
                    } = state;

                    current_rx = rx;
                    current_node = node;
                    current_bookmark = new_bookmark;
                }
                MatchResult::Error(err) => {
                    return Ok(Some(err));
                }
            }
        }

        if let Some(extra_node) = current_node {
            debug!(
                "validate_sibling_inlines:: Superfluous node error: {:?}",
                extra_node
            );
            let error = DocumentError::new(
                &extra_node,
                parent_rx,
                &self.document,
                &self.prescription,
                "Superfluous inline content was present.".to_string(),
            )?;
            Ok(Some(Box::new(error)))
        } else {
            info!("validate_sibling_inlines:: Valid!");
            Ok(None)
        }
    }

    /// Performs the next validation step for block elements given the inputs and returns an
    /// updated MatchState if successful.
    fn consume_block_match(
        &self,
        rx: Node,
        node: Option<Node>,
        bookmark: Option<Node>,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_block_match::");
        info!("Rx: {}", rx.render_xml());
        if let Some(ref node) = node {
            info!("Doc: {}", node.render_xml());
        }

        match self.prescription.document.get_match_type(&rx)? {
            MatchType::Mandatory => {
                if let Some(bookmark) = bookmark {
                    self.consume_mandatory_block_match(
                        MandatoryMatchInput { rx, node, bookmark },
                        parent_node,
                    )
                } else {
                    debug!("consume_block_match:: Missing Mandatory node and no bookmark");
                    let error = DocumentError::new(
                        parent_node,
                        &rx,
                        &self.document,
                        &self.prescription,
                        "Missing mandatory block node.".to_string(),
                    )?;
                    Ok(MatchResult::Error(Box::new(error)))
                }
            }
            MatchType::Optional => {
                self.consume_optional_block_match(OptionalMatchInput { rx, node, bookmark })
            }
            _ => {
                error!("Unexpected Matchtype encountered in consume_block_match");
                Ok(MatchResult::State(MatchState {
                    rx: Some(rx),
                    node,
                    bookmark,
                }))
            }
        }
    }

    /// Performs the next validation step for inline elements and returns a `MatchState` if
    /// successful.
    fn consume_inline_match(
        &self,
        rx: Node,
        node: Option<Node>,
        bookmark: Option<Node>,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_inline_match::");
        info!("Rx: {}", rx.render_xml());

        if let Some(ref node) = node {
            info!("Doc: {}", node.render_xml());
        }

        match self.prescription.document.get_match_type(&rx)? {
            MatchType::Optional => {
                self.consume_optional_inline_match(OptionalMatchInput { rx, node, bookmark })
            }
            MatchType::Mandatory => {
                if let Some(bookmark) = bookmark {
                    self.consume_mandatory_inline_match(
                        MandatoryMatchInput { rx, node, bookmark },
                        parent_node,
                    )
                } else {
                    debug!("consume_block_match:: Missing Mandatory node and no bookmark");
                    let error = DocumentError::new(
                        parent_node,
                        &rx,
                        &self.document,
                        &self.prescription,
                        "Missing mandatory inline node.".to_string(),
                    )?;
                    Ok(MatchResult::Error(Box::new(error)))
                }
            }
            mt => Err(HowserError::RuntimeError(format!(
                "Inline nodes should not have match type: {:?}",
                mt
            ))),
        }
    }

    /// Performs validation on a repeatable element and returns the result.
    fn consume_repeatable_matches(
        &self,
        rx: Node,
        node: Option<Node>,
        bookmark: Option<Node>,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_repeatable_matches()");
        let out_rx = match rx.next_sibling()? {
            Some(ditto_node) => ditto_node.next_sibling()?,
            _ => None,
        };
        let mut out_node = match node {
            Some(ref node) => Some(node.itself()?),
            None => None,
        };
        let mut out_bookmark = match bookmark {
            Some(ref node) => Some(node.itself()?),
            None => None,
        };
        let mut current_rx = rx.itself()?;
        let mut current_node = match node {
            Some(ref node) => Some(node.itself()?),
            None => None,
        };
        let mut current_bookmark = match bookmark {
            Some(ref node) => Some(node.itself()?),
            None => None,
        };
        let match_type = self.prescription.document.get_match_type(&rx)?;
        let mut matches_consumed: usize = 0;

        loop {
            let current_node_id = match current_node {
                Some(ref node) => node.get_id(),
                _ => 0,
            };
            let match_result =
                self.consume_block_match(current_rx, current_node, current_bookmark, parent_node)?;

            match (match_result, match_type.clone()) {
                (MatchResult::State(state), MatchType::Mandatory) => {
                    let MatchState {
                        rx: _,
                        node: result_node,
                        bookmark: result_bookmark,
                    } = state;

                    matches_consumed += 1;
                    current_node = match result_node {
                        Some(ref node) => Some(node.itself()?),
                        None => None,
                    };
                    current_rx = rx.itself()?;
                    current_bookmark = result_bookmark;
                    out_node = match result_node {
                        Some(ref node) => Some(node.itself()?),
                        None => None,
                    };
                    if matches_consumed == 1 {
                        out_bookmark = match current_bookmark {
                            Some(ref node) => Some(node.itself()?),
                            _ => None,
                        };
                    }
                }
                (MatchResult::State(state), MatchType::Optional) => {
                    let MatchState {
                        rx: _,
                        node: result_node,
                        bookmark: result_bookmark,
                    } = state;

                    current_node = match result_node {
                        Some(ref node) => Some(node.itself()?),
                        None => None,
                    };
                    current_rx = rx.itself()?;
                    current_bookmark = result_bookmark;
                    out_node = match result_node {
                        Some(ref node) => Some(node.itself()?),
                        None => None,
                    };
                    if let Some(ref node) = current_node {
                        if node.get_id() == current_node_id {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                (MatchResult::Error(err), MatchType::Mandatory) => {
                    if matches_consumed == 0 {
                        return Ok(MatchResult::Error(err));
                    } else {
                        break;
                    }
                }
                (MatchResult::Error(_), MatchType::Optional) => {
                    break;
                }
                _ => {
                    error!("consume_repeatable_matches:: Invalid match type encountered");
                    return Err(HowserError::RuntimeError(
                        "Error encountered processing repeatable match. Check the Howser log."
                            .to_string(),
                    ));
                }
            };
        }

        info!("consume_repeatable_matches:: matches found or node was optional");
        Ok(MatchResult::State(MatchState {
            rx: out_rx,
            node: out_node,
            bookmark: out_bookmark,
        }))
    }

    /// Performs validation on a mandatory block element and returns the result.
    fn consume_mandatory_block_match(
        &self,
        input: MandatoryMatchInput,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_mandatory_block_match()");
        let MandatoryMatchInput { rx, node, bookmark } = input;

        if let Some(node) = node {
            match self.check_block_match(&node, &rx)? {
                None => {
                    let end_node = Some(node.itself()?);
                    let next_bookmark = match self.scan_for_block_match(&bookmark, &end_node, &rx)?
                    {
                        Some(node) => node.next_sibling()?,
                        _ => None,
                    };
                    let next_node = node.next_sibling()?;
                    let next_rx = rx.next_sibling()?;
                    info!("consume_mandatory_block_match:: Block matched");
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: next_node,
                        bookmark: next_bookmark,
                    }))
                }
                Some(err) => {
                    let end_node = Some(node.itself()?);
                    if let Some(prev_match) = self.scan_for_block_match(&bookmark, &end_node, &rx)?
                    {
                        let next_bookmark = prev_match.next_sibling()?;
                        let next_node = prev_match.next_sibling()?;
                        let next_rx = rx.next_sibling()?;
                        info!("consume_mandatory_block_match:: Current node mismatch, but match found from bookmark");
                        Ok(MatchResult::State(MatchState {
                            rx: next_rx,
                            node: next_node,
                            bookmark: next_bookmark,
                        }))
                    } else {
                        Ok(MatchResult::Error(err))
                    }
                }
            }
        } else {
            if let Some(prev_match) = self.scan_for_block_match(&bookmark, &None, &rx)? {
                let next_bookmark = prev_match.next_sibling()?;
                let next_node = prev_match.next_sibling()?;
                let next_rx = rx.next_sibling()?;
                info!("consume_mandatory_block_match:: No current node, but match found from bookmark");
                Ok(MatchResult::State(MatchState {
                    rx: next_rx,
                    node: next_node,
                    bookmark: next_bookmark,
                }))
            } else {
                debug!("consume_mandatory_block_match -- No current node and No match");
                let error = DocumentError::new(
                    parent_node,
                    &rx,
                    &self.document,
                    &self.prescription,
                    "Missing mandatory node.".to_string(),
                )?;
                Ok(MatchResult::Error(Box::new(error)))
            }
        }
    }

    /// Performs validation on an optional block element and returns the result.
    fn consume_optional_block_match(&self, input: OptionalMatchInput) -> HowserResult<MatchResult> {
        trace!("consume_optional_block_match()");
        let OptionalMatchInput { rx, node, bookmark } = input;

        if let Some(node) = node {
            match self.check_block_match(&node, &rx)? {
                None => {
                    let next_node = node.next_sibling()?;
                    let next_rx = rx.next_sibling()?;
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: next_node,
                        bookmark: bookmark,
                    }))
                }
                Some(_) => {
                    let next_rx = rx.next_sibling()?;
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: Some(node),
                        bookmark: bookmark,
                    }))
                }
            }
        } else {
            let next_rx = rx.next_sibling()?;
            Ok(MatchResult::State(MatchState {
                rx: next_rx,
                node: node,
                bookmark: bookmark,
            }))
        }
    }

    fn consume_optional_inline_match(
        &self,
        input: OptionalMatchInput,
    ) -> HowserResult<MatchResult> {
        trace!("consume_optional__inline_match");
        let OptionalMatchInput { rx, node, bookmark } = input;

        if let Some(node) = node {
            match self.check_inline_match(&rx, &node)? {
                None => {
                    let next_node = node.next_sibling()?;
                    let next_rx = rx.next_sibling()?;
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: next_node,
                        bookmark: bookmark,
                    }))
                }
                Some(_) => {
                    let next_rx = rx.next_sibling()?;
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: Some(node),
                        bookmark: bookmark,
                    }))
                }
            }
        } else {
            let next_rx = rx.next_sibling()?;
            Ok(MatchResult::State(MatchState {
                rx: next_rx,
                node,
                bookmark: bookmark,
            }))
        }
    }

    /// Performs validation on a mandatory inline element.
    fn consume_mandatory_inline_match(
        &self,
        input: MandatoryMatchInput,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_mandatory_inline_match::");
        let MandatoryMatchInput { rx, node, bookmark } = input;

        if let Some(node) = node {
            match self.check_inline_match(&rx, &node)? {
                None => {
                    let end_node = Some(node.itself()?);
                    let next_bookmark = match self.scan_for_inline_match(&bookmark, &end_node, &rx)?
                    {
                        Some(node) => node.next_sibling()?,
                        None => None,
                    };
                    let next_node = node.next_sibling()?;
                    let next_rx = rx.next_sibling()?;

                    info!("consume_mandatory_inline_match:: Matched!");
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: next_node,
                        bookmark: next_bookmark,
                    }))
                }
                Some(err) => {
                    let end_node = Some(node.itself()?);
                    if let Some(prev_match) = self.scan_for_inline_match(&bookmark, &end_node, &rx)?
                    {
                        let next_bookmark = prev_match.next_sibling()?;
                        let next_node = prev_match.next_sibling()?;
                        let next_rx = rx.next_sibling()?;
                        info!("consume_mandatory_inline_match:: Bookmark Match Found!");
                        Ok(MatchResult::State(MatchState {
                            rx: next_rx,
                            node: next_node,
                            bookmark: next_bookmark,
                        }))
                    } else {
                        Ok(MatchResult::Error(err))
                    }
                }
            }
        } else {
            if let Some(prev_match) = self.scan_for_inline_match(&bookmark, &None, &rx)? {
                let next_bookmark = prev_match.next_sibling()?;
                let next_node = prev_match.next_sibling()?;
                let next_rx = rx.next_sibling()?;
                info!("consume_mandatory_inline_match:: Bookmark Match Found!");
                Ok(MatchResult::State(MatchState {
                    rx: next_rx,
                    node: next_node,
                    bookmark: next_bookmark,
                }))
            } else {
                debug!("consume_mandatory_inline_match:: No current node and No match");
                let error = DocumentError::new(
                    parent_node,
                    &rx,
                    &self.document,
                    &self.prescription,
                    "Missing mandatory node.".to_string(),
                )?;
                Ok(MatchResult::Error(Box::new(error)))
            }
        }
    }

    /// Searchs for a node that will pass validation among the given segment of adjacent sibling
    /// block elements.
    ///
    /// Starts searching at `start_node` and progresses through to `end_node` until a match is
    /// found. Returns the first matched `Node` or `None` if a match could not be found.
    fn scan_for_block_match(
        &self,
        start_node: &Node,
        end_node: &Option<Node>,
        rx: &Node,
    ) -> HowserResult<Option<Node>> {
        trace!("scan_for_block_match()");
        let mut current_node = Some(start_node.itself()?);

        while let Some(node) = current_node {
            if let None = self.check_block_match(&node, rx)? {
                return Ok(Some(node));
            }

            if let &Some(ref stop_node) = end_node {
                let node_id = node.get_id();
                let stop_id = stop_node.get_id();
                if node_id == stop_id {
                    current_node = None;
                } else {
                    current_node = node.next_sibling()?;
                }
            } else {
                current_node = node.next_sibling()?;
            }
        }

        Ok(None)
    }

    /// Searchs for a node that will pass validation among the given segment of adjacent sibling
    /// inline elements.
    ///
    /// Starts searching at `start_node` and progresses through to `end_node` until a match is
    /// found. Returns the first matched `Node` or `None` if a match could not be found.
    fn scan_for_inline_match(
        &self,
        start_node: &Node,
        end_node: &Option<Node>,
        rx: &Node,
    ) -> HowserResult<Option<Node>> {
        trace!("scan_for_inline_match::");
        let mut current_node = Some(start_node.itself()?);

        while let Some(node) = current_node {
            if let None = self.check_inline_match(rx, &node)? {
                return Ok(Some(node));
            }

            if let &Some(ref stop_node) = end_node {
                let node_id = node.get_id();
                let stop_id = stop_node.get_id();

                if node_id == stop_id {
                    current_node = None;
                } else {
                    current_node = node.next_sibling()?;
                }
            } else {
                current_node = node.next_sibling()?;
            }
        }

        Ok(None)
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be block elements.
    fn check_block_match(&self, node: &Node, rx: &Node) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_block_match::");
        match ElementType::determine(rx) {
            ElementType::ContainerBlock => Ok(self.check_container_block_match(&node, &rx)?),
            ElementType::LeafBlock => Ok(self.check_leaf_block_match(&node, &rx)?),
            _ => {
                error!("check_block_match:: called with an invalid element type.");
                Err(HowserError::RuntimeError(
                    "Element type problem -- check the Howser log.".to_string(),
                ))
            }
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// The inputs are assumed to be container blocks.
    fn check_container_block_match(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_container_block_match::");

        if !types_match(node, rx)? {
            debug!("check_container_block_match:: different types -- no match");
            let error = TypeMismatchError::new(rx, node, &self.prescription, &self.document)?;
            return Ok(Some(Box::new(error)));
        }

        let child_validation = self.validate_sibling_blocks(rx, node)?;
        let is_wildcard = self.node_is_wildcard(rx)?;

        match (child_validation, is_wildcard) {
            (Some(err), false) => {
                info!("check_container_block_match:: Children don't match");
                Ok(Some(err))
            }
            _ => {
                info!("check_container_block_match:: Children match");
                Ok(None)
            }
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// The inputs are assumed to be leaf blocks.
    fn check_leaf_block_match(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_leaf_block_match::");
        if !types_match(node, rx)? {
            debug!("check_leaf_block_match:: Types do not match");
            let error = TypeMismatchError::new(rx, node, &self.prescription, &self.document)?;
            return Ok(Some(Box::new(error)));
        }

        let child_validation = self.validate_sibling_inlines(rx, node)?;
        let is_wildcard = self.node_is_wildcard(rx)?;

        match (child_validation, is_wildcard) {
            (Some(errs), false) => {
                info!("check_leaf_block_match:: Children not valid");
                Ok(Some(errs))
            }
            _ => {
                info!("check_leaf_block_match:: Children valid");
                Ok(None)
            }
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be inline elements.
    fn check_inline_match(
        &self,
        rx: &Node,
        node: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_inline_match::");
        match ElementType::determine(rx) {
            ElementType::InlineLeaf => self.check_inline_leaf_match(node, rx),
            ElementType::InlineContainer => self.check_inline_container_match(node, rx),
            _ => {
                error!("check_inline_match:: called with unexpected element type");
                Err(HowserError::RuntimeError(
                    "Invalid element type encountered. Check the log.".to_string(),
                ))
            }
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be container inline elements.
    fn check_inline_container_match(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_inline_container_match::");

        if !types_match(node, rx)? {
            let error = TypeMismatchError::new(rx, node, &self.prescription, &self.document)?;
            return Ok(Some(Box::new(error)));
        }

        match node {
            Node::Link(_) => self.validate_link_node_content(node, rx),
            _ => self.validate_sibling_inlines(rx, node),
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be leaf inline elements.
    fn check_inline_leaf_match(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("check_inline_leaf_match::");

        if !types_match(node, rx)? {
            debug!("check_inline_leaf_match:: Type mismatch");
            let error = TypeMismatchError::new(rx, node, &self.prescription, &self.document)?;

            return Ok(Some(Box::new(error)));
        }

        self.validate_textual_content(node, rx)
    }

    /// Determines if the given prescription `Node` is a wildcard.
    fn node_is_wildcard(&self, rx: &Node) -> HowserResult<bool> {
        trace!("node_is_wildcard()");
        match self.prescription.document.is_wildcard(rx) {
            true => {
                info!("Wildcard!");
                Ok(true)
            }
            false => {
                info!("Not Wildcard");
                Ok(false)
            }
        }
    }

    /// Performs validation of the textual content of `node` against `rx`.
    ///
    /// Returns `None` if the content was valid.
    fn validate_textual_content(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_node_content()");
        match rx {
            Node::Link(_) => self.validate_link_node_content(node, rx),
            _ => self.validate_text_node_content(node, rx),
        }
    }

    /// Performs validation of the textual content of a link type node.
    ///
    /// Returns `None` if valid.
    fn validate_link_node_content(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_link_node_content()");
        match (node, rx) {
            (Node::Link(ref node_link), Node::Link(ref rx_link)) => {
                let node_url = node_link.get_url()?;
                let rx_url = rx_link.get_url()?;
                let url_match_pairs = Self::check_content_match(&node_url, &rx_url)?;

                let node_title = node_link.get_title()?;
                let rx_title = rx_link.get_title()?;
                let title_match_pairs = Self::check_content_match(&node_title, &rx_title)?;

                if ContentMatchPair::contains_mismatch(&url_match_pairs) {
                    debug!("Link destination Error");
                    let error = TextualContentError::new(
                        rx,
                        node,
                        &self.prescription,
                        &self.document,
                        &url_match_pairs,
                    )?;
                    Ok(Some(Box::new(error)))
                } else if ContentMatchPair::contains_mismatch(&title_match_pairs) {
                    debug!("Link title Error");
                    let error = TextualContentError::new(
                        rx,
                        node,
                        &self.prescription,
                        &self.document,
                        &title_match_pairs,
                    )?;
                    Ok(Some(Box::new(error)))
                } else {
                    self.validate_sibling_inlines(rx, node)
                }
            }
            _ => Err(HowserError::RuntimeError(
                "validate_link_node_content called with non-link node".to_string(),
            )),
        }
    }

    /// Performs validation of the textual content of a text node.
    ///
    /// Returns `None` if valid.
    fn validate_text_node_content(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        let (node_content, rx_content) = match (node, rx) {
            (Node::Text(ref node_text), Node::Text(ref rx_text)) => {
                (node_text.get_content()?, rx_text.get_content()?)
            }
            (Node::Code(ref node_code), Node::Code(ref rx_code)) => {
                (node_code.get_content()?, rx_code.get_content()?)
            }
            _ => (String::new(), String::new()),
        };
        let match_pairs = Self::check_content_match(&node_content, &rx_content)?;

        if ContentMatchPair::contains_mismatch(&match_pairs) {
            info!("Content Error");
            return Ok(Some(Box::new(TextualContentError::new(
                rx,
                node,
                &self.prescription,
                &self.document,
                &match_pairs,
            )?)));
        }

        Ok(None)
    }

    /// Determines if the given text is a valid match for the given prescription text.
    fn check_content_match(
        node_content: &String,
        rx_content: &String,
    ) -> HowserResult<Vec<ContentMatchPair>> {
        trace!("match_contents()");
        let mut content_queue = node_content.clone();
        let mut prompts = VecDeque::from(Validator::tokenize_prompts(rx_content)?);
        let mut left_stack = Vec::new();
        let mut right_stack = Vec::new();
        let mut current_direction = MatchDirection::Left;

        while !prompts.is_empty() {
            let (prompt, stack) = match current_direction {
                MatchDirection::Left => (prompts.pop_front().unwrap(), &mut left_stack),
                MatchDirection::Right => (prompts.pop_back().unwrap(), &mut right_stack),
            };
            match prompt {
                PromptToken::Mandatory => {
                    if content_queue.is_empty() {
                        let pair = ContentMatchPair(prompt, None);
                        stack.push(pair);
                    } else {
                        let substitution = match current_direction {
                            MatchDirection::Left => content_queue.remove(0).to_string(),
                            MatchDirection::Right => content_queue.pop().unwrap().to_string(),
                        };
                        let pair = ContentMatchPair(PromptToken::Mandatory, Some(substitution));
                        stack.push(pair);
                    }
                }
                PromptToken::Optional => {
                    let pair = ContentMatchPair(PromptToken::Optional, None);
                    stack.push(pair);
                }
                PromptToken::Literal(ref content) => {
                    let temp_queue = content_queue.clone();
                    let (preface, substitution) = match current_direction {
                        MatchDirection::Left => match temp_queue.find(content) {
                            Some(0) => {
                                let substitution: String =
                                    content_queue.drain(..content.len()).collect();
                                (None, Some(substitution))
                            }
                            Some(n) => {
                                let preface: String = content_queue.drain(..n).collect();
                                let substitution = content_queue.drain(..content.len()).collect();
                                (Some(preface), Some(substitution))
                            }
                            None => (None, None),
                        },
                        MatchDirection::Right => match temp_queue.rfind(content) {
                            Some(n) => {
                                let mut substitution = content_queue.split_off(n);
                                let preface = match substitution.len() > content.len() {
                                    true => Some(substitution.split_off(content.len())),
                                    false => None,
                                };
                                (preface, Some(substitution))
                            }
                            None => (None, None),
                        },
                    };

                    if let Some(substitution) = substitution {
                        if let Some(preface) = preface {
                            match stack.last() {
                                Some(&ContentMatchPair(PromptToken::Literal(_), _)) | None => {
                                    let pair = ContentMatchPair(PromptToken::None, Some(preface));
                                    stack.push(pair);
                                }
                                _ => (),
                            };
                        }
                        let match_pair = ContentMatchPair(
                            PromptToken::Literal(content.to_string()),
                            Some(substitution),
                        );
                        stack.push(match_pair);
                    } else {
                        stack.push(ContentMatchPair(
                            PromptToken::Literal(content.to_string()),
                            None,
                        ));
                    }
                }
                PromptToken::None => {
                    return Err(HowserError::RuntimeError(format!(
                        "Tokenize Prompts should not return a None prompt"
                    )));
                }
            }

            current_direction = match current_direction {
                MatchDirection::Left => MatchDirection::Right,
                MatchDirection::Right => MatchDirection::Left,
            }
        }

        if !content_queue.is_empty() {
            match (left_stack.last(), right_stack.last()) {
                (
                    Some(&ContentMatchPair(PromptToken::Literal(_), _)),
                    Some(&ContentMatchPair(PromptToken::Literal(_), _)),
                ) => {
                    let pair = ContentMatchPair(PromptToken::None, Some(content_queue));
                    left_stack.push(pair);
                }
                _ => (),
            }
        }

        Ok(left_stack
            .into_iter()
            .chain(right_stack.into_iter())
            .collect())
    }

    /// Returns a vector of PromptToken parsed from the given string.
    fn tokenize_prompts(content: &String) -> HowserResult<Vec<PromptToken>> {
        trace!("tokenize_prompts()");
        let prompt_pattern = Regex::new(CONTENT_PROMPT_PATTERN)?;
        let mut tail = content.to_string();
        let mut tokens = Vec::new();

        while !tail.is_empty() {
            let temp_tail = tail.clone();
            if let Some(location) = prompt_pattern.find(&temp_tail) {
                let (matched, remainder) = temp_tail.split_at(location.end());

                if location.start() > 0 {
                    tokens.push(PromptToken::Literal(
                        matched[0..location.start()].to_string(),
                    ));
                }
                let token = match location.as_str() {
                    MANDATORY_PROMPT => PromptToken::Mandatory,
                    OPTIONAL_PROMPT => PromptToken::Optional,
                    _ => PromptToken::None,
                };
                tokens.push(token);
                tail = String::from(remainder);
            } else {
                tokens.push(PromptToken::Literal(tail.clone()));
                tail.clear();
            }
        }
        Ok(tokens)
    }
}

/// Determines if two `Node` are of equivalent type.
///
/// Takes into account heading levels and list types.
pub fn types_match(node: &Node, other: &Node) -> HowserResult<bool> {
    trace!("types_match()");
    let node_type = node.get_cmark_type()?;
    let rx_type = other.get_cmark_type()?;

    if node_type == rx_type {
        match (node, other) {
            (Node::Heading(node_heading), Node::Heading(ref rx_heading)) => {
                let node_level = node_heading.get_level();
                let rx_level = rx_heading.get_level();
                if node_level == rx_level {
                    info!("types_match:: Headings match");
                    return Ok(true);
                } else {
                    debug!("types_match:: Heading level mismatch");
                    return Ok(false);
                }
            }
            (Node::List(ref node_list), Node::List(ref rx_list)) => {
                let node_list_type = node_list.get_list_type()?;
                let rx_list_type = rx_list.get_list_type()?;
                if node_list_type == rx_list_type {
                    info!("types_match:: List types match");
                    return Ok(true);
                } else {
                    debug!("types_match:: List type mismatch");
                    return Ok(false);
                }
            }
            _ => {
                info!("types_match:: Match!");
                return Ok(true);
            }
        }
    }

    info!(
        "types_match:: node: {:?} does not match rx: {:?}",
        node, other
    );
    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::env_logger;
    use super::Validator;
    use data::ContentMatchPair;
    use document::Document;
    use doogie::parse_document;
    use helpers::test::strategies::content;
    use helpers::test::strategies::helpers::*;

    #[test]
    fn test_literal_paragraph_match() {
        let text = "The quick brown fox jumps over the dog.".to_string();
        let rx_root = parse_document(&text);
        let doc_root = parse_document(&text);
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_literal_paragraph_mismatch() {
        let rx_root = parse_document(&"The quick brown fox jumps over the dog.".to_string());
        let doc_root = parse_document(&"The slow brown fox jumps over the dog.".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_literal_mixed_paragraph_match() {
        let text = "*Compile* the code `let a = 12;` using `cargo build`.".to_string();
        let rx_root = parse_document(&text);
        let doc_root = parse_document(&text);
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_literal_mixed_paragraph_mismatch() {
        let rx_root =
            parse_document(&"*Compile* the code `let a = 12;` using `cargo build`.".to_string());
        let doc_root = parse_document(&"*Compile* the code `let a = 12;`.".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_literal_mixed_paragraph_superflous_content() {
        let rx_root = parse_document(&"-!!- (-!!-)[-!!-]".to_string());
        let doc_root = parse_document(&"Joe Schmoe <jschmoe@polysync.io> blargh".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_prompted_text_match() {
        let rx_root = parse_document(&"The quick brown fox -!!- over-??-.".to_string());
        let match_1_root =
            parse_document(&"The quick brown fox jumps overthrows the dog.".to_string());
        let match_2_root = parse_document(&"The quick brown fox slinks over.".to_string());

        let rx_1 = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let rx_2 = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc_1 = Document::new(&match_1_root, None).unwrap();
        let doc_2 = Document::new(&match_2_root, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        let report_1 = validator_1.validate().unwrap();
        let report_2 = validator_2.validate().unwrap();

        assert!(report_1.is_empty());
        assert!(report_2.is_empty());
    }

    #[test]
    fn test_prompted_text_mismatch() {
        let rx_root = parse_document(&"The quick brown fox -!!- over-??-.".to_string());
        let match_root = parse_document(&"The quick brown fox over.".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_optional_inline_prompts_are_optional() {
        let rx_root = parse_document(&"* [foo-!!-](-!!-)-??-**Foo**".to_string());
        let match_root = parse_document(&"* [foobar](Fux)**Foo**".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_literal_code_match() {
        let text = "`let my_num: u32 = 42;`".to_string();
        let rx_root = parse_document(&text);
        let doc_root = parse_document(&text);
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_literal_code_mismatch() {
        let rx_root = parse_document(&"`let my_num: u32 = 42;`".to_string());
        let doc_root = parse_document(&"`let my_num: u32 = 13;`".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_prompted_code_match() {
        let rx_root = parse_document(&"`let -!!- = 42-??-;`".to_string());
        let match_1_root = parse_document(&"`let my_num: u32 = 42;`".to_string());
        let match_2_root = parse_document(&"`let the_answer = 4200;`".to_string());

        let rx_1 = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let rx_2 = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc_1 = Document::new(&match_1_root, None).unwrap();
        let doc_2 = Document::new(&match_2_root, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        let report_1 = validator_1.validate().unwrap();
        let report_2 = validator_2.validate().unwrap();

        assert!(report_1.is_empty());
        assert!(report_2.is_empty());
    }

    #[test]
    fn test_prompted_code_mismatch() {
        let rx_root = parse_document(&"`let -!!- = 42;`".to_string());
        let match_root = parse_document(&"`let = 42;`".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_mandatory_wildcard_paragraph_match() {
        let rx_root = parse_document(&"-!!-".to_string());
        let match_root = parse_document(&"Literally any content here".to_string());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_optional_wildcard_paragraph_match() {
        let first_rx_root = parse_document(&"-??-".to_string());
        let second_rx_root = parse_document(&"-??-".to_string());
        let match_root = parse_document(&"Literally any content here".to_string());
        let empty_match_root = parse_document(&String::new());
        let rx_1 = Document::new(&first_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let rx_2 = Document::new(&second_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let empty_doc = Document::new(&empty_match_root, None).unwrap();
        let validator_1 = Validator::new(rx_1, doc);
        let validator_2 = Validator::new(rx_2, empty_doc);

        assert!(validator_1.validate().unwrap().is_empty());
        assert!(validator_2.validate().unwrap().is_empty());
    }

    #[test]
    fn test_mandatory_wildcard_paragraph_mismatch() {
        let rx_root = parse_document(&"-!!-".to_string());
        let match_root = parse_document(&String::new());
        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(
            !report.is_empty(),
            "The mandatory wildcard paragraph did not fail against empty document."
        );
    }

    #[test]
    fn test_repeatable_mandatory_wildcard_paragraph_match() {
        let rx_text = "\
                       -!!-\n\n\
                       -\"\"-";

        let match_text = "Some random first paragraph\n\nSome random second paragraph";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_repeatable_optional_wildcard_paragraph_match() {
        let rx_text = "\
                       -??-\n\n\
                       -\"\"-";

        let match_text = "Some random first paragraph\n\nSome random second paragraph";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_repeatable_optional_wildcard_paragraph_mismatch() {
        let rx_text = "\
                       -??-\n\n\
                       -\"\"-";

        let match_text = "# A Header\n\nSome content";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_repeatable_wildcard_paragraph_mismatch() {
        let rx_text = "\
                       -!!-\n\n\
                       -\"\"-";

        let match_text =
            "Some random first paragraph\n\nSome random second paragraph\n\n# And a heading";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(!report.is_empty());
    }

    #[test]
    fn test_mandatory_block_level_prompted_paragraph_match() {
        let rx_text = "-!!-\n-!!-my dear-??-";
        let match_text = "Elementary my dear Watson";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc = Document::new(&match_root, None).unwrap();

        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_empty());
    }

    #[test]
    fn test_optional_block_level_prompted_paragraph_match() {
        let rx_text = "-??-\n-!!-my dear-??-";
        let first_match_text = "Elementary my dear Watson";
        let second_match_text = "";

        let first_rx_root = parse_document(&rx_text.to_string());
        let second_rx_root = parse_document(&rx_text.to_string());
        let first_match_root = parse_document(&first_match_text.to_string());
        let second_match_root = parse_document(&second_match_text.to_string());

        let first_rx = Document::new(&first_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let second_rx = Document::new(&second_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let first_doc = Document::new(&first_match_root, None).unwrap();
        let second_doc = Document::new(&second_match_root, None).unwrap();

        let first_validator = Validator::new(first_rx, first_doc);
        let second_validator = Validator::new(second_rx, second_doc);

        assert!(
            first_validator.validate().unwrap().is_empty(),
            "The optional prompted paragraph did not match the given string."
        );
        assert!(
            second_validator.validate().unwrap().is_empty(),
            "The optional prompted paragraph did not match an empty string"
        );
    }

    #[test]
    fn test_mandatory_block_level_prompted_paragraph_mismatch() {
        let rx_text = "-!!-\n-!!-my dear-??-";
        let match_text = "my dear";

        let first_rx_root = parse_document(&rx_text.to_string());
        let second_rx_root = parse_document(&rx_text.to_string());
        let doc_root = parse_document(&match_text.to_string());
        let empty_root = parse_document(&String::new());

        let first_rx = Document::new(&first_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let second_rx = Document::new(&second_rx_root, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let first_doc = Document::new(&doc_root, None).unwrap();
        let second_doc = Document::new(&empty_root, None).unwrap();

        let first_validator = Validator::new(first_rx, first_doc);
        let second_validator = Validator::new(second_rx, second_doc);

        assert!(
            !first_validator.validate().unwrap().is_empty(),
            "The mandatory prompted paragraph did not fail against mismatched text."
        );
        assert!(
            !second_validator.validate().unwrap().is_empty(),
            "The mandatory prompted paragraph did not fail against empty document."
        );
    }

    #[test]
    fn test_optional_repeatable_list_item_match() {
        env_logger::init();
        let rx_text = "* Foo -!!-\n* -??-\n* Bar -!!-\n* -\"\"-";
        let match_text_1 = "* Foo Foo";
        let match_text_2 = "* Foo Foo\n* Bar Bar\n* Bar Baz";

        let rx_root_1 = parse_document(&rx_text.to_string());
        let rx_root_2 = parse_document(&rx_text.to_string());
        let doc_root_1 = parse_document(&match_text_1.to_string());
        let doc_root_2 = parse_document(&match_text_2.to_string());

        let rx_1 = Document::new(&rx_root_1, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let rx_2 = Document::new(&rx_root_2, None)
            .unwrap()
            .into_prescription()
            .unwrap();
        let doc_1 = Document::new(&doc_root_1, None).unwrap();
        let doc_2 = Document::new(&doc_root_2, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        assert!(
            validator_1.validate().unwrap().is_empty(),
            "Absence of optional items caused mismatch"
        );
        assert!(
            validator_2.validate().unwrap().is_empty(),
            "Presence of optional items caused mismatch"
        );
    }

    proptest! {
        #[test]
        /// Tests that some textual content containing Rx tokens is correctly parsed into prompts and literals.
        fn test_tokenize_prompts(ref prompt_seq in content::prompts::prompt_tokens(1..10)) {
            let mut prompt_string = String::new();

            for prompt in prompt_seq {
                prompt_string.push_str(&prompt.to_string());
            }

            let tokens = Validator::tokenize_prompts(&prompt_string).unwrap();
            assert_eq!(&tokens, prompt_seq)
        }

        #[test]
        fn test_content_matches(
            ref matches in content::matches::arb_content_matches(1..10, 1..10)
        ) {
            let (template, document) = serialize_match_seq(matches);

            let match_pairs = Validator::check_content_match(&document, &template).unwrap();

            assert!(! ContentMatchPair::contains_mismatch(&match_pairs));
        }

        #[test]
        fn test_non_matching_content(ref mismatch in content::mismatches::mismatch_pair()) {
            let &(ref prompts, ref content) = mismatch;
            let template = prompts.iter().fold(String::new(), |state, item| {
                state + item
            });

            let match_pairs = Validator::check_content_match(&template, content).unwrap();

            assert!(ContentMatchPair::contains_mismatch(&match_pairs));
        }
    }
}
