//! For validation of documents.

extern crate env_logger;
extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use constants::{CONTENT_PROMPT_PATTERN, MANDATORY_PROMPT, OPTIONAL_PROMPT};
use data::ElementType;
use data::{ContentMatchPair, MatchType, PromptToken};
use document::{Document, Prescription};
use doogie::constants::NodeType;
use doogie::Node;
use errors::{
    ContentError, DocumentError, HowserError, HowserResult, Reportable, SpecWarning,
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
}

/// Return type for validating optional block level elements.
struct OptionalMatchOutput {
    rx: Option<Node>,
    node: Option<Node>,
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
    pub fn validate(&self) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate()");
        self.validate_sibling_blocks(&self.prescription.document.root, &self.document.root)
    }

    /// Validates a set of sibling block elements
    fn validate_sibling_blocks(
        &self,
        parent_rx_node: &Node,
        parent_doc_node: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_sibling_blocks()");
        let parent_rx_traverser = parent_rx_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let parent_node_traverser = parent_doc_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let mut current_rx = parent_rx_traverser.last_child()?;
        let mut current_node = parent_node_traverser.last_child()?;
        let mut current_bookmark = parent_node_traverser.last_child()?;

        while let Some(rx) = current_rx {
            match self.consume_block_match(rx, current_node, current_bookmark, parent_doc_node)? {
                MatchResult::State(state) => {
                    let MatchState {
                        rx,
                        node,
                        bookmark: new_bookmark,
                    } = state;

                    current_rx = rx;
                    current_node = node;
                    current_bookmark = new_bookmark;
                    // update local state
                }
                MatchResult::Error(errors) => {
                    return Ok(Some(errors));
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
            debug!("validate_sibling_blocks:: Blocks were valid");
            Ok(None)
        }
    }

    /// Performs the next validation step given the inputs and returns an updated MatchState if
    /// successful.
    fn consume_block_match(
        &self,
        rx: Node,
        node: Option<Node>,
        bookmark: Option<Node>,
        parent_node: &Node,
    ) -> HowserResult<MatchResult> {
        trace!("consume_block_match()");
        if let Some(ref node) = node {
            info!(
                "{}",
                node
                    .capabilities
                    .render
                    .as_ref()
                    .unwrap()
                    .render_xml()
            );
        }
        info!(
            "{}",
            rx
                .capabilities
                .render
                .as_ref()
                .unwrap()
                .render_xml()
        );

        match self.prescription.document.get_match_type(&rx)? {
            MatchType::Repeatable => {
                self.consume_repeatable_matches(rx, node, bookmark, parent_node)
            }
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
                        "Missing mandatory node.".to_string(),
                    )?;
                    Ok(MatchResult::Error(Box::new(error)))
                }
            }
            MatchType::Optional => {
                let OptionalMatchOutput { rx, node } =
                    self.consume_optional_block_match(OptionalMatchInput { rx, node })?;

                Ok(MatchResult::State(MatchState { rx, node, bookmark }))
            }
            MatchType::None => {
                warn!("Encountered MatchType::None");
                Ok(MatchResult::State(MatchState {
                    rx: Some(rx),
                    node,
                    bookmark,
                }))
            }
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
        if let Some(repeatable_rx) = rx.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.prev_sibling()? {
            let out_rx = repeatable_rx.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.prev_sibling()?;
            let mut out_node = match node {
                Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                None => None
            };
            let mut out_bookmark = match bookmark {
                Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                None => None
            };
            let mut current_rx = repeatable_rx.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?;
            let mut current_node = match node {
                Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                None => None
            };
            let mut current_bookmark = match bookmark {
                Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                None => None
            };
            let match_type = self.prescription.document.get_match_type(&repeatable_rx)?;
            let mut matches_consumed: usize = 0;

            loop {
                let current_node_id = match current_node {
                    Some(ref node) => node.capabilities.get.as_ref().ok_or(HowserError::CapabilityError)?.get_id()?,
                    _ => 0
                };
                let match_state = self.consume_block_match(
                    current_rx,
                    current_node,
                    current_bookmark,
                    parent_node)?;

                match (match_state, match_type.clone()) {
                    (MatchResult::State(state), MatchType::Mandatory) => {
                        let MatchState {
                            rx: _,
                            node: result_node,
                            bookmark: result_bookmark,
                        } = state;

                        matches_consumed += 1;
                        current_node = match result_node {
                            Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                            None => None
                        };
                        current_rx = repeatable_rx.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?;
                        current_bookmark = result_bookmark;
                        out_node = match result_node {
                            Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                            None => None
                        };
                        if matches_consumed == 1 {
                            out_bookmark = match current_bookmark {
                                Some(ref node) => Some(node.capabilities
                                    .traverse
                                    .as_ref()
                                    .ok_or(HowserError::CapabilityError)?
                                    .itself()?),
                                _ => None,
                            };
                        }
                    },
                    (MatchResult::State(state), MatchType::Optional) => {
                        let MatchState {
                            rx: _,
                            node: result_node,
                            bookmark: result_bookmark,
                        } = state;

                        current_node = match result_node {
                            Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                            None => None
                        };
                        current_rx = repeatable_rx.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?;
                        current_bookmark = result_bookmark;
                        out_node = match result_node {
                            Some(ref node) => Some(node.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.itself()?),
                            None => None
                        };
                        if let Some(ref node) = current_node {
                            if node.capabilities.get.as_ref().ok_or(HowserError::CapabilityError)?.get_id()? == current_node_id {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                    (MatchResult::Error(_), _) => {
                        break;
                    },
                    _ => return Err(HowserError::RuntimeError("Invalid pattern encountered in repeatable block match".to_string()))
                };
            }

            match (matches_consumed, match_type) {
                (0, MatchType::Mandatory) => {
                    debug!("consume_repeatable_matches:: zero matches found for mandatory node");
                    let error = DocumentError::new(
                        match node {
                            Some(ref node) => node,
                            None => parent_node
                        },
                        &rx,
                        &self.document,
                        &self.prescription,
                        "Missing mandatory node.".to_string(),
                    )?;
                    Ok(MatchResult::Error(Box::new(error)))
                }
                _ => {
                    info!("consume_repeatable_matches:: matches found or node was optional");
                    Ok(MatchResult::State(MatchState {
                        rx: out_rx,
                        node: out_node,
                        bookmark: out_bookmark,
                    }))
                },
            }
        } else {
            warn!("consume_repeatable_matches:: Rx Error -- No subject of ditto token");
            let error = SpecWarning::new(
                &rx,
                &self.prescription.document,
                "No valid subject for ditto token.",
            )?;
            Ok(MatchResult::Error(Box::new(error)))
        }
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
            if self.block_matches(&node, &rx)? {
                let end_node = Some(node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .itself()?);
                let next_bookmark = match self.scan_for_match(&bookmark, &end_node, &rx)? {
                    Some(node) => node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .prev_sibling()?,
                    _ => None,
                };
                let next_node = node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
                let next_rx = rx.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
                info!("consume_mandatory_block_match:: Block matched");
                Ok(MatchResult::State(MatchState {
                    rx: next_rx,
                    node: next_node,
                    bookmark: next_bookmark,
                }))
            } else {
                let end_node = Some(node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .itself()?);
                if let Some(prev_match) = self.scan_for_match(&bookmark, &end_node, &rx)? {
                    let match_traverser = prev_match
                        .capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    let next_bookmark = match_traverser.prev_sibling()?;
                    let next_node = match_traverser.prev_sibling()?;
                    let next_rx = rx.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .prev_sibling()?;
                    info!("consume_mandatory_block_match:: Current node mismatch, but match found from bookmark");
                    Ok(MatchResult::State(MatchState {
                        rx: next_rx,
                        node: next_node,
                        bookmark: next_bookmark,
                    }))
                } else {
                    debug!("consume_mandatory_block_match:: Bookmark search failed, no match for {:?}", rx);
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
        } else {
            if let Some(prev_match) = self.scan_for_match(&bookmark, &None, &rx)? {
                let match_traverser = prev_match
                    .capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?;
                let next_bookmark = match_traverser.prev_sibling()?;
                let next_node = match_traverser.prev_sibling()?;
                let next_rx = rx.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
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
    fn consume_optional_block_match(
        &self,
        input: OptionalMatchInput,
    ) -> HowserResult<OptionalMatchOutput> {
        trace!("consume_optional_block_match()");
        let OptionalMatchInput { rx, node } = input;

        if let Some(node) = node {
            if self.block_matches(&node, &rx)? {
                let next_node = node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
                let next_rx = rx.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
                Ok(OptionalMatchOutput {
                    rx: next_rx,
                    node: next_node,
                })
            } else {
                let next_rx = rx.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
                Ok(OptionalMatchOutput {
                    rx: next_rx,
                    node: Some(node),
                })
            }
        } else {
            let next_rx = rx.capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .prev_sibling()?;
            Ok(OptionalMatchOutput { rx: next_rx, node })
        }
    }

    /// Searchs for a node that will pass validation among the given segment of adjacent siblings.
    ///
    /// Starts searching at `start_node` and progresses through to `end_node` until a match is
    /// found. Returns the first matched `Node` or `None` if a match could not be found.
    fn scan_for_match(
        &self,
        start_node: &Node,
        end_node: &Option<Node>,
        rx: &Node,
    ) -> HowserResult<Option<Node>> {
        trace!("scan_for_match()");
        let mut current_node = Some(start_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .itself()?);

        while let Some(node) = current_node {
            if self.block_matches(&node, rx)? {
                return Ok(Some(node));
            }

            if let &Some(ref stop_node) = end_node {
                let node_id = node.capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .get_id()?;
                let stop_id = stop_node
                    .capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .get_id()?;
                if node_id == stop_id {
                    current_node = None;
                } else {
                    current_node = node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .prev_sibling()?;
                }
            } else {
                current_node = node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .prev_sibling()?;
            }
        }

        Ok(None)
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be block elements.
    fn block_matches(&self, node: &Node, rx: &Node) -> HowserResult<bool> {
        trace!("block_matches()");
        match ElementType::determine(rx)? {
            ElementType::ContainerBlock => Ok(self.container_block_matches(&node, &rx)?),
            ElementType::LeafBlock => Ok(self.leaf_block_matches(&node, &rx)?),
            _ => Ok(false),
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// The inputs are assumed to be container blocks.
    fn container_block_matches(&self, node: &Node, rx: &Node) -> HowserResult<bool> {
        trace!("container_block_matches()");
        if !types_match(node, rx)? {
            debug!("container_block_matches:: different types -- no match");
            return Ok(false);
        }

        let child_validation = self.validate_sibling_blocks(rx, node)?;
        let is_wildcard = self.node_is_wildcard(rx)?;

        match (child_validation, is_wildcard) {
            (Some(_errs), false) => {
                debug!("container_block_matches:: Children don't match");
                Ok(false)
            },
            _ => {
                debug!("container_block_matches:: Children match");
                Ok(true)
            },
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// The inputs are assumed to be leaf blocks.
    fn leaf_block_matches(&self, node: &Node, rx: &Node) -> HowserResult<bool> {
        trace!("leaf_block_matches()");
        if !types_match(node, rx)? {
            debug!("leaf_block_matches:: Types do not match");
            return Ok(false);
        }

        let child_validation = self.validate_sibling_inlines(rx, node)?;
        let is_wildcard = self.node_is_wildcard(rx)?;

        match (child_validation, is_wildcard) {
            (Some(_errs), false) => {
                debug!("leaf_block_matches:: Children not valid");
                Ok(false)
            },
            _ => {
                debug!("leaf_block_matches:: Children valid");
                Ok(true)
            },
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
        trace!("validate_sibling_inlines()");
        let parent_rx_traverser = parent_rx
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let parent_node_traverser = parent_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let mut current_rx = parent_rx_traverser.first_child()?;
        let mut current_node = parent_node_traverser.first_child()?;

        while let Some(rx) = current_rx {
            if let Some(node) = current_node {
                if self.inline_matches(&rx, &node)? {
                    current_rx = rx.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .next_sibling()?;
                    current_node = node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .next_sibling()?;
                } else {
                    debug!("validate_sibling_inlines:: Inline match Error");
                    let error = DocumentError::new(
                        &node,
                        &rx,
                        &self.document,
                        &self.prescription,
                        "Missing inline node.".to_string(),
                    )?;
                    return Ok(Some(Box::new(error)));
                }
            } else {
                debug!("validate_sibling_inlines:: Missing node Error");
                let error = DocumentError::new(
                    &parent_node,
                    &rx,
                    &self.document,
                    &self.prescription,
                    "Missing inline node.".to_string(),
                )?;
                return Ok(Some(Box::new(error)));
            }
        }

        if let Some(extra_node) = current_node {
            let error = DocumentError::new(
                &extra_node,
                parent_rx,
                &self.document,
                &self.prescription,
                "Superfluous inline content was present.".to_string(),
            )?;
            Ok(Some(Box::new(error)))
        } else {
            Ok(None)
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be inline elements.
    fn inline_matches(&self, rx: &Node, node: &Node) -> HowserResult<bool> {
        trace!("inline_matches()");
        match ElementType::determine(rx)? {
            ElementType::InlineLeaf => self.leaf_inline_matches(node, rx),
            ElementType::InlineContainer => self.container_inline_matches(node, rx),
            _ => Ok(false),
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be container inline elements.
    fn container_inline_matches(&self, node: &Node, rx: &Node) -> HowserResult<bool> {
        trace!("container_inline_matches()");
        if !types_match(node, rx)? {
            return Ok(false);
        }

        let node_type = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?;

        let result = match node_type {
            NodeType::CMarkNodeLink => self.validate_link_node_content(node, rx),
            _ => self.validate_sibling_inlines(rx, node),
        }?;

        match result {
            Some(_errs) => Ok(false),
            None => Ok(true),
        }
    }

    /// Determines if `node` matches `rx`.
    ///
    /// Inputs are assumed to be leaf inline elements.
    fn leaf_inline_matches(&self, node: &Node, rx: &Node) -> HowserResult<bool> {
        trace!("leaf_inline_matches()");
        if !types_match(node, rx)? {
            debug!("Type mismatch");
            return Ok(false);
        }

        match self.validate_node_content(node, rx)? {
            None => Ok(true),
            Some(_) => Ok(false),
        }
    }

    /// Determines if the given prescription `Node` is a wildcard.
    fn node_is_wildcard(&self, rx: &Node) -> HowserResult<bool> {
        trace!("node_is_wildcard()");
        match self.prescription.document.is_wildcard(rx)? {
            true => {
                debug!("Wildcard!");
                Ok(true)
            },
            false => {
                debug!("Not Wildcard");
                Ok(false)
            }
        }
    }

    /// Performs validation of the textual content of `node` against `rx`.
    ///
    /// Returns `None` if the content was valid.
    fn validate_node_content(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_node_content()");
        let rx_getter = rx.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        match rx_getter.get_type()? {
            NodeType::CMarkNodeLink => self.validate_link_node_content(node, rx),
            _ => self.validate_general_node_content(node, rx),
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
        let node_getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let rx_getter = rx.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let node_url = node_getter.get_url()?;
        let rx_url = rx_getter.get_url()?;
        let url_match_pairs = Self::match_contents(&node_url, &rx_url)?;

        let node_title = node_getter.get_title()?;
        let rx_title = rx_getter.get_title()?;
        let title_match_pairs = Self::match_contents(&node_title, &rx_title)?;

        if ContentMatchPair::contains_mismatch(&url_match_pairs) {
            info!("Link destination Error");
            Ok(Some(Box::new(ContentError::new(
                rx,
                node,
                &self.prescription,
                &self.document,
                url_match_pairs.clone(),
            )?)))
        } else if ContentMatchPair::contains_mismatch(&title_match_pairs) {
            info!("Link title Error");
            Ok(Some(Box::new(ContentError::new(
                rx,
                node,
                &self.prescription,
                &self.document,
                title_match_pairs.clone(),
            )?)))
        } else {
            self.validate_sibling_inlines(rx, node)
        }
    }

    /// Performs validation of the textual content of a text node.
    ///
    /// Returns `None` if valid.
    fn validate_general_node_content(
        &self,
        node: &Node,
        rx: &Node,
    ) -> HowserResult<Option<ValidationProblem>> {
        trace!("validate_general_node_content()");
        let rx_getter = rx.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let node_getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let node_content = node_getter.get_content()?;
        let rx_content = rx_getter.get_content()?.to_string();
        let match_pairs = Self::match_contents(&node_content, &rx_content)?;

        if ContentMatchPair::contains_mismatch(&match_pairs) {
            info!("Content Error");
            return Ok(Some(Box::new(ContentError::new(
                rx,
                node,
                &self.prescription,
                &self.document,
                match_pairs,
            )?)));
        }

        Ok(None)
    }

    /// Determines if the given text is a valid match for the given prescription text.
    fn match_contents(
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
    let node_getter = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let rx_getter = other
        .capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;

    let node_type = node_getter.get_type()?;
    let rx_type = rx_getter.get_type()?;

    if node_type == rx_type {
        if node_type == NodeType::CMarkNodeHeading {
            let node_level = node_getter.get_heading_level()?;
            let rx_level = rx_getter.get_heading_level()?;
            if node_level == rx_level {
                info!("types_match:: Headings match");
                return Ok(true);
            } else {
                debug!("types_match:: Heading level mismatch");
                return Ok(false);
            }
        } else if node_type == NodeType::CMarkNodeList {
            let node_list_type = node_getter.get_list_type()?;
            let rx_list_type = rx_getter.get_list_type()?;
            if node_list_type == rx_list_type {
                info!("types_match:: List types match");
                return Ok(true);
            } else {
                debug!("types_match:: List type mismatch");
                return Ok(false);
            }
        } else {
            info!("types_match:: Match!");
            return Ok(true);
        }
    }

    debug!("types_match:: node: {:?} does not match rx: {:?}", node, other);
    Ok(false)
}

#[cfg(test)]
mod tests {
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
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
    }

    #[test]
    fn test_literal_paragraph_mismatch() {
        let rx_root = parse_document(&"The quick brown fox jumps over the dog.".to_string());
        let doc_root = parse_document(&"The slow brown fox jumps over the dog.".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_literal_mixed_paragraph_match() {
        let text = "*Compile* the code `let a = 12;` using `cargo build`.".to_string();
        let rx_root = parse_document(&text);
        let doc_root = parse_document(&text);
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
    }

    #[test]
    fn test_literal_mixed_paragraph_mismatch() {
        let rx_root =
            parse_document(&"*Compile* the code `let a = 12;` using `cargo build`.".to_string());
        let doc_root = parse_document(&"*Compile* the code `let a = 12;`.".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_literal_mixed_paragraph_superflous_content() {
        let rx_root =
            parse_document(&"-!!- (-!!-)[-!!-]".to_string());
        let doc_root = parse_document(&"Joe Schmoe <jschmoe@polysync.io> blargh".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_prompted_text_match() {
        let rx_root = parse_document(&"The quick brown fox -!!- over-??-.".to_string());
        let match_1_root =
            parse_document(&"The quick brown fox jumps overthrows the dog.".to_string());
        let match_2_root = parse_document(&"The quick brown fox slinks over.".to_string());

        let rx_1 = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let rx_2 = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc_1 = Document::new(&match_1_root, None).unwrap();
        let doc_2 = Document::new(&match_2_root, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        let report_1 = validator_1.validate().unwrap();
        let report_2 = validator_2.validate().unwrap();

        assert!(report_1.is_none());
        assert!(report_2.is_none());
    }

    #[test]
    fn test_prompted_text_mismatch() {
        let rx_root = parse_document(&"The quick brown fox -!!- over-??-.".to_string());
        let match_root = parse_document(&"The quick brown fox over.".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_literal_code_match() {
        let text = "`let my_num: u32 = 42;`".to_string();
        let rx_root = parse_document(&text);
        let doc_root = parse_document(&text);
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
    }

    #[test]
    fn test_literal_code_mismatch() {
        let rx_root = parse_document(&"`let my_num: u32 = 42;`".to_string());
        let doc_root = parse_document(&"`let my_num: u32 = 13;`".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&doc_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_prompted_code_match() {
        let rx_root = parse_document(&"`let -!!- = 42-??-;`".to_string());
        let match_1_root = parse_document(&"`let my_num: u32 = 42;`".to_string());
        let match_2_root = parse_document(&"`let the_answer = 4200;`".to_string());

        let rx_1 = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let rx_2 = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc_1 = Document::new(&match_1_root, None).unwrap();
        let doc_2 = Document::new(&match_2_root, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        let report_1 = validator_1.validate().unwrap();
        let report_2 = validator_2.validate().unwrap();

        assert!(report_1.is_none());
        assert!(report_2.is_none());
    }

    #[test]
    fn test_prompted_code_mismatch() {
        let rx_root = parse_document(&"`let -!!- = 42;`".to_string());
        let match_root = parse_document(&"`let = 42;`".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_mandatory_wildcard_paragraph_match() {
        let rx_root = parse_document(&"-!!-".to_string());
        let match_root = parse_document(&"Literally any content here".to_string());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
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

        assert!(validator_1.validate().unwrap().is_none());
        assert!(validator_2.validate().unwrap().is_none());
    }

    #[test]
    fn test_mandatory_wildcard_paragraph_mismatch() {
        let rx_root = parse_document(&"-!!-".to_string());
        let match_root = parse_document(&String::new());
        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(
            report.is_some(),
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

        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
    }

    #[test]
    fn test_repeatable_optional_wildcard_paragraph_match() {
        let rx_text = "\
                       -??-\n\n\
                       -\"\"-";

        let match_text = "Some random first paragraph\n\nSome random second paragraph";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
    }

    #[test]
    fn test_repeatable_optional_wildcard_paragraph_mismatch() {
        let rx_text = "\
                       -??-\n\n\
                       -\"\"-";

        let match_text = "# A Header\n\nSome content";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
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

        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();
        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_some());
    }

    #[test]
    fn test_mandatory_block_level_prompted_paragraph_match() {
        let rx_text = "-!!-\n-!!-my dear-??-";
        let match_text = "Elementary my dear Watson";

        let rx_root = parse_document(&rx_text.to_string());
        let match_root = parse_document(&match_text.to_string());

        let rx = Document::new(&rx_root, None).unwrap().into_prescription().unwrap();
        let doc = Document::new(&match_root, None).unwrap();

        let validator = Validator::new(rx, doc);

        let report = validator.validate().unwrap();

        assert!(report.is_none());
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
            first_validator.validate().unwrap().is_none(),
            "The optional prompted paragraph did not match the given string."
        );
        assert!(
            second_validator.validate().unwrap().is_none(),
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
            first_validator.validate().unwrap().is_some(),
            "The mandatory prompted paragraph did not fail against mismatched text."
        );
        assert!(
            second_validator.validate().unwrap().is_some(),
            "The mandatory prompted paragraph did not fail against empty document."
        );
    }

    #[test]
    fn test_optional_repeatable_list_item_match() {
        let rx_text = "* Foo -!!-\n* -??-\n* Bar -!!-\n* -\"\"-";
        let match_text_1 = "* Foo Foo";
        let match_text_2 = "* Foo Foo\n* Bar Bar\n* Bar Baz";

        let rx_root_1 = parse_document(&rx_text.to_string());
        let rx_root_2 = parse_document(&rx_text.to_string());
        let doc_root_1 = parse_document(&match_text_1.to_string());
        let doc_root_2 = parse_document(&match_text_2.to_string());

        let rx_1 = Document::new(&rx_root_1, None).unwrap().into_prescription().unwrap();
        let rx_2 = Document::new(&rx_root_2, None).unwrap().into_prescription().unwrap();
        let doc_1 = Document::new(&doc_root_1, None).unwrap();
        let doc_2 = Document::new(&doc_root_2, None).unwrap();

        let validator_1 = Validator::new(rx_1, doc_1);
        let validator_2 = Validator::new(rx_2, doc_2);

        assert!(validator_1.validate().unwrap().is_none(), "Absence of optional items caused mismatch");
        assert!(validator_2.validate().unwrap().is_none(), "Presence of optional items caused mismatch");
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

            let match_pairs = Validator::match_contents(&document, &template).unwrap();

            assert!(! ContentMatchPair::contains_mismatch(&match_pairs));
        }

        #[test]
        fn test_non_matching_content(ref mismatch in content::mismatches::mismatch_pair()) {
            let &(ref prompts, ref content) = mismatch;
            let template = prompts.iter().fold(String::new(), |state, item| {
                state + item
            });

            let match_pairs = Validator::match_contents(&template, content).unwrap();

            assert!(ContentMatchPair::contains_mismatch(&match_pairs));
        }
    }
}
