//! Abstractions for CMark Documents and Howser Templates.

extern crate env_logger;
extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use constants::{DITTO_TOKEN, MANDATORY_PROMPT, OPTIONAL_PROMPT, PROMPT_PATTERN};
use data::ElementType;
use data::{MatchType, NodeData};
use doogie::constants::*;
use doogie::Node;
use errors::{HowserError, HowserResult, SpecWarning};
use std::cell::RefCell;
use std::collections::HashMap;
use validator::types_match;

/// Abstraction of the different types of block level Rx Elements and their match context.
///
/// These are used to parse block level annotations. Integrated types are elements whose annotations
/// appear within the same node as the content while discrete types represent elements whose
/// annotation occurs in an adjacent element.
#[derive(Debug)]
enum LookaheadType {
    /// Integrated element with no annotation.
    IntegratedLiteral(Node),
    /// Integrated element with annotation and content.
    IntegratedOccupied(Node, MatchType),
    /// Integrated element with annotation, but no content.
    IntegratedVacant(Node, MatchType),
    /// Discrete element that is an annotation.
    DiscreteAnnotated(Node, MatchType),
    /// Discrete element that is content.
    DiscreteLiteral(Node),
    /// Any element that has a ditto annotation.
    Ditto(Node),
    /// Just list elements.
    List(Node),
    /// Elements that do not support annotation or no element.
    Other(Option<Node>),
}

impl LookaheadType {
    /// Returns a `LookaheadType` that is parsed from the given `Node`.
    fn new(node: Option<Node>) -> HowserResult<Self> {
        if let Some(node) = node {
            let match_type = get_annotation(&node)?;
            match node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .get_type()?
            {
                // List
                NodeType::CMarkNodeList => Ok(LookaheadType::List(node)),
                // Integrated
                NodeType::CMarkNodeParagraph
                | NodeType::CMarkNodeBlockQuote
                | NodeType::CMarkNodeCodeBlock => match match_type {
                    MatchType::Repeatable => Ok(LookaheadType::Ditto(node)),
                    MatchType::None => Ok(LookaheadType::IntegratedLiteral(node)),
                    _ => {
                        if LookaheadType::is_vacant(&node)? {
                            Ok(LookaheadType::IntegratedVacant(node, match_type))
                        } else {
                            Ok(LookaheadType::IntegratedOccupied(node, match_type))
                        }
                    }
                },
                // Discrete
                NodeType::CMarkNodeHeading | NodeType::CMarkNodeItem => match match_type {
                    MatchType::Repeatable => Ok(LookaheadType::Ditto(node)),
                    MatchType::None => Ok(LookaheadType::DiscreteLiteral(node)),
                    _ => Ok(LookaheadType::DiscreteAnnotated(node, match_type)),
                },
                // Other
                _ => Ok(LookaheadType::Other(Some(node))),
            }
        } else {
            Ok(LookaheadType::Other(None))
        }
    }

    /// Determines whether the given integrated node has content other than its annotation.
    fn is_vacant(node: &Node) -> HowserResult<bool> {
        match node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?
        {
            NodeType::CMarkNodeCodeBlock => Ok(node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .get_content()?
                .is_empty()),
            _ => {
                if let Some(annotation) = node.capabilities
                    .traverse
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .first_child()?
                {
                    Ok(annotation
                        .capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .next_sibling()?
                        .is_none())
                } else {
                    Err(HowserError::RuntimeError(
                        "Lookahead Error: Got a match type, but no annotation node was present."
                            .to_string(),
                    ))
                }
            }
        }
    }
}

/// Wrapper for a Markdown document that manages extra metadata about the `Node`s contained within
/// `root`.
pub struct Document<'a> {
    pub root: &'a Node,
    data: RefCell<HashMap<u32, NodeData>>,
    pub filename: Option<String>,
}

impl<'a> Document<'a> {
    /// Returns a new `Document`.
    ///
    /// Note that this is a destructive operation to the root node. It will mutate the internal
    /// structure of the tree. The reason for taking a reference and not ownership is to enable
    /// compatibility with proptest generators.
    pub fn new(root: &'a Node, filename: Option<String>) -> HowserResult<Self> {
        strip_comments(root)?;

        Ok(Document {
            root,
            data: RefCell::new(HashMap::new()),
            filename,
        })
    }

    /// Transform this `Document` instance into a `Prescription`.
    pub fn into_prescription(self) -> HowserResult<Prescription<'a>> {
        trace!("into_prescription");
        process_child_block_elements(&self.root, &self)?;
        Ok(Prescription { document: self })
    }

    /// Traverse the document tree and return the first node encountered of the type specified.
    pub fn first_of_type(&self, node_type: NodeType) -> HowserResult<Option<Node>> {
        let traverser = self.root
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        for (node, _) in traverser.iter() {
            let mut found_type: NodeType;
            {
                let getter = node.capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?;
                found_type = getter.get_type()?;
            }
            if found_type == node_type {
                return Ok(Some(node));
            }
        }

        Ok(None)
    }

    /// Returns the `MatchType` of the specified `Node`.
    pub fn get_match_type(&self, node: &Node) -> HowserResult<MatchType> {
        let getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let id = getter.get_id()?;
        let mut data_store = self.data.borrow_mut();
        let node_data = data_store.entry(id).or_insert(NodeData::new());
        Ok(node_data.match_type.clone())
    }

    /// Set the `MatchType` for a `Node`.
    fn set_match_type(&self, node: &Node, match_type: MatchType) -> Result<&Self, HowserError> {
        let getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let id = getter.get_id()?;
        let mut data = self.data.borrow_mut();
        let node_data = data.entry(id).or_insert(NodeData::new());
        node_data.match_type = match_type;

        Ok(self)
    }

    /// Returns a boolean indicating if this `Node` is considered a wildcard.
    pub fn is_wildcard(&self, rx_node: &Node) -> HowserResult<bool> {
        let getter = rx_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let id = getter.get_id()?;
        let mut data_store = self.data.borrow_mut();
        let node_data = data_store.entry(id).or_insert(NodeData::new());
        Ok(node_data.is_wildcard)
    }

    /// Set the wildcard status for a `Node`.
    fn set_is_wildcard(&self, node: &Node, state: bool) -> Result<&Self, HowserError> {
        if let Some(ref getter) = node.capabilities.get {
            let id = getter.get_id()?;
            let mut data = self.data.borrow_mut();
            let node_data = data.entry(id).or_insert(NodeData::new());
            node_data.is_wildcard = state;
        }

        Ok(self)
    }
}

/// A `Document` that has been parsed into an Rx prescription.
pub struct Prescription<'a> {
    pub document: Document<'a>,
}

/// Process the match types of the children of the given parent.
fn process_child_elements(parent: &Node, document: &Document) -> HowserResult<()> {
    let child = parent
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?;

    if let Some(ref node) = child {
        match ElementType::determine(node)? {
            ElementType::InlineContainer | ElementType::InlineLeaf => {
                process_child_inline_elements(parent, document)
            }
            _ => process_child_block_elements(parent, document),
        }
    } else {
        Ok(())
    }
}

/// Process the match types of a set of inline elements that are children of the given parent.
fn process_child_inline_elements(parent: &Node, document: &Document) -> HowserResult<()> {
    let mut current_child = parent
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?;

    while let Some(node) = current_child {
        let getter = node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        match getter.get_type()? {
            NodeType::CMarkNodeText | NodeType::CMarkNodeCode => {
                if all_content_is_optional(&getter.get_content()?)? {
                    document.set_match_type(&node, MatchType::Optional)?;
                }
            }
            _ => {
                process_child_inline_elements(&node, document)?;
                annotate_circumstantial_node(&node, document)?;
            }
        }

        current_child = node.capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?;
    }

    Ok(())
}

/// Determines if all textual content in the prompted content string is optional.
fn all_content_is_optional(content: &String) -> HowserResult<bool> {
    if content.is_empty() {
        return Ok(true);
    }

    let (match_type, tail) = extract_match_type(content)?;
    Ok(match_type == MatchType::Optional && all_content_is_optional(&tail)?)
}

/// Extract and transform the block-level Rx annotations from the document into metadata.
fn process_child_block_elements(parent: &Node, document: &Document) -> HowserResult<()> {
    trace!("process_child_block_elements::");
    let mut current_child = parent
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?;

    while let Some(l1_node) = current_child {
        let mut l2 = LookaheadType::Other(None);

        if let Some(l2_node) = l1_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?
        {
            if types_match(&l1_node, &l2_node)? {
                l2 = LookaheadType::new(Some(l2_node))?;
            } else {
                l2 = LookaheadType::Other(Some(l2_node));
            }
        }
        let l1 = LookaheadType::new(Some(l1_node))?;

        debug!(
            "process_child_block_elements:: Lookahead Sequence: {:?}, {:?}",
            l1, l2
        );
        match (l1, l2) {
            (
                LookaheadType::DiscreteLiteral(target),
                LookaheadType::Ditto(ditto),
            ) => {
                process_child_elements(&target, document)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::DiscreteLiteral(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::DiscreteAnnotated(annotation, match_type),
                LookaheadType::DiscreteLiteral(target),
            ) => {
                document.set_match_type(&target, match_type)?;
                annotation.capabilities.mutate.as_ref().ok_or(HowserError::CapabilityError)?.unlink();
                current_child = Some(target);
            },
            (
                LookaheadType::DiscreteAnnotated(target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                remove_annotation(&target)?;
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::DiscreteAnnotated(target, match_type),
                _,
            ) => {
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                LookaheadType::Ditto(ditto),
            ) => {
                process_child_elements(&target, document)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedVacant(target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                remove_annotation(&target)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                document.set_is_wildcard(&target, true)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedVacant(target, match_type),
                _,
            ) => {
                remove_annotation(&target)?;
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedOccupied(target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                process_child_elements(&target, document)?;
                remove_annotation(&target)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::IntegratedOccupied(target, match_type),
                _,
            ) => {
                process_child_elements(&target, document)?;
                remove_annotation(&target)?;
                document.set_match_type(&target, match_type)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::List(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                annotate_circumstantial_node(&target, document)?;
                current_child = target.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (LookaheadType::Ditto(node), _) => {
                return Err(HowserError::PrescriptionError(SpecWarning::new(&node, document, "An element with a Ditto prompt must be preceded by an element of the same type.")?))
            },
            _ => return Err(HowserError::RuntimeError("Unexpected Lookahead Encountered".to_string()))
        }
    }

    Ok(())
}

/// Returns the annotation of a block level element.
///
/// Returns None if the element is not annotated.
fn get_annotation(node: &Node) -> HowserResult<MatchType> {
    match node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_type()?
    {
        NodeType::CMarkNodeParagraph => get_paragraph_annotation(node),
        NodeType::CMarkNodeBlockQuote => get_block_quote_annotation(node),
        NodeType::CMarkNodeCodeBlock => get_code_block_annotation(node),
        NodeType::CMarkNodeHeading => get_heading_annotation(node),
        NodeType::CMarkNodeItem => get_item_annotation(node),
        _ => Ok(MatchType::None),
    }
}

/// Strips the annotation from a block level element if one exists.
fn remove_annotation(node: &Node) -> HowserResult<()> {
    match node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_type()?
    {
        NodeType::CMarkNodeParagraph => remove_paragraph_annotation(node),
        NodeType::CMarkNodeBlockQuote => remove_block_quote_annotation(node),
        NodeType::CMarkNodeCodeBlock => remove_code_block_annotation(node),
        NodeType::CMarkNodeHeading => remove_heading_annotation(node),
        NodeType::CMarkNodeItem => remove_item_annotation(node),
        _ => Ok(()),
    }
}

/// Returns the block level annotation of a paragraph node if one exists.
fn get_paragraph_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Some(text_node) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        let softbreak = text_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?;
        if let Some(ref node) = softbreak {
            if node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .get_type()? != NodeType::CMarkNodeSoftbreak
            {
                return Ok(MatchType::None);
            }
        }
        if text_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? != NodeType::CMarkNodeText
        {
            return Ok(MatchType::None);
        }
        let text = text_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_content()?;
        let (token, content) = extract_match_type(&text)?;
        if token != MatchType::None && content.is_empty() {
            Ok(token)
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a paragraph node if one exists.
fn remove_paragraph_annotation(node: &Node) -> HowserResult<()> {
    if let Some(text_node) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        let softbreak = text_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?;
        if let Some(ref node) = softbreak {
            if node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .get_type()? != NodeType::CMarkNodeSoftbreak
            {
                return Ok(());
            }
        }
        if text_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? != NodeType::CMarkNodeText
        {
            return Ok(());
        }
        let text = text_node
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_content()?;
        let (token, content) = extract_match_type(&text)?;
        if token != MatchType::None && content.is_empty() {
            text_node
                .capabilities
                .mutate
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .unlink();

            if let Some(node) = softbreak {
                node.capabilities
                    .mutate
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .unlink();
            }
        }
    }

    Ok(())
}

/// Returns the block-level annotation from a block quote node if one exists.
fn get_block_quote_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Some(paragraph) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if paragraph
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? == NodeType::CMarkNodeParagraph
        {
            get_paragraph_annotation(&paragraph)
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a block quote node if one exists.
fn remove_block_quote_annotation(node: &Node) -> HowserResult<()> {
    if let Some(paragraph) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if paragraph
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? == NodeType::CMarkNodeParagraph
        {
            remove_paragraph_annotation(&paragraph)?;
            if paragraph
                .capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .first_child()?
                .is_none()
            {
                paragraph
                    .capabilities
                    .mutate
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .unlink();
            }
        }
    }

    Ok(())
}

/// Returns the annotation of a code block node if one exists.
fn get_code_block_annotation(node: &Node) -> HowserResult<MatchType> {
    let info = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_fence_info()?;
    let (match_type, _) = extract_match_type(&info)?;
    if match_type != MatchType::None {
        Ok(match_type)
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the annotation from a code block node if one exists.
fn remove_code_block_annotation(node: &Node) -> HowserResult<()> {
    let info = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_fence_info()?;
    let (match_type, content) = extract_match_type(&info)?;
    if match_type != MatchType::None {
        node.capabilities
            .set
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .set_fence_info(&content)?;
    }

    Ok(())
}

/// Returns the block-level annotation of a heading node if one exists.
fn get_heading_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Some(text) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if text.capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?
            .is_none()
        {
            let getter = text.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;
            if getter.get_type()? == NodeType::CMarkNodeText {
                let (match_type, content) = extract_match_type(&getter.get_content()?)?;
                if match_type != MatchType::None && content.is_empty() {
                    Ok(match_type)
                } else {
                    Ok(MatchType::None)
                }
            } else {
                Ok(MatchType::None)
            }
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a heading node if one exists.
fn remove_heading_annotation(node: &Node) -> HowserResult<()> {
    if let Some(text) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if text.capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?
            .is_none()
        {
            let getter = text.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;
            if getter.get_type()? == NodeType::CMarkNodeText {
                let (match_type, content) = extract_match_type(&getter.get_content()?)?;
                if match_type != MatchType::None && content.is_empty() {
                    text.capabilities
                        .mutate
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .unlink();
                }
            }
        }
    }

    Ok(())
}

/// Returns the block-level annotation from a list item node if one exists.
fn get_item_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Some(paragraph) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if paragraph
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? == NodeType::CMarkNodeParagraph
        {
            get_paragraph_annotation(&paragraph)
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a list item node if one exists.
fn remove_item_annotation(node: &Node) -> HowserResult<()> {
    if let Some(paragraph) = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?
    {
        if paragraph
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()? == NodeType::CMarkNodeParagraph
        {
            let token = get_paragraph_annotation(&paragraph)?;
            if token != MatchType::None {
                remove_paragraph_annotation(&paragraph)?;
                paragraph
                    .capabilities
                    .mutate
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .unlink();
            }
        }
    }

    Ok(())
}

/// Sets the match type of a list node based on the match types of its children.
///
/// If any children are marked Mandatory, then the list is also mandatory. Otherwise, it is optional.
/// This is necessary because lists nodes are meta-nodes in cmark and will only appear as containers
/// for list items if they are present.
fn annotate_circumstantial_node(target_node: &Node, document: &Document) -> HowserResult<()> {
    document.set_match_type(target_node, MatchType::Optional)?;
    let traverser = target_node
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let mut child = traverser.first_child()?;
    while let Some(node) = child {
        if document.get_match_type(&node)? == MatchType::Mandatory {
            document.set_match_type(&target_node, MatchType::Mandatory)?;
            return Ok(());
        } else {
            let traverser = node.capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;
            child = traverser.next_sibling()?;
        }
    }

    Ok(())
}

/// Attempts to parse and strip an Rx token from the beginning of a string.
///
/// Returns a tuple with the first element being a `MatchType` corresponding to the parsed token or
/// `MatchType::None` if no token was found. The second element is the original string stripped of
/// its token if found.
fn extract_match_type(content: &String) -> HowserResult<(MatchType, String)> {
    let pattern = Regex::new(PROMPT_PATTERN)?;

    if let Some(location) = pattern.find(content.trim_left()) {
        if location.start() == 0 {
            let prompt = location.as_str();
            let tail = pattern.replace(content, "");
            let match_type = match prompt {
                MANDATORY_PROMPT => MatchType::Mandatory,
                OPTIONAL_PROMPT => MatchType::Optional,
                DITTO_TOKEN => MatchType::Repeatable,
                _ => MatchType::None,
            };

            Ok((match_type, tail.to_string()))
        } else {
            Ok((MatchType::None, content.clone()))
        }
    } else {
        Ok((MatchType::None, content.clone()))
    }
}

/// Strips all html from the document.
fn strip_comments(root: &Node) -> HowserResult<()> {
    for (node, _) in root.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .iter()
    {
        match node.capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .get_type()?
        {
            NodeType::CMarkNodeHtmlInline | NodeType::CMarkNodeHtmlBlock => {
                node.capabilities
                    .mutate
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .unlink();
            }
            _ => (),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::process_child_block_elements;
    use super::Document;
    use data::{MatchType, PromptToken};
    use doogie::parse_document;
    use helpers::test::strategies::cmark::arb_paragraph_match;
    use helpers::test::strategies::helpers::serialize_match_seq;
    use proptest::prelude::*;

    proptest!{
        #[test]
        fn test_block_level_paragraph_annotations_are_processed(
                ref paragraph in arb_paragraph_match(1..10),
                ref prompt in prop_oneof![
                    Just(PromptToken::Mandatory),
                    Just(PromptToken::Optional)]) {
            let (template_content, _) = serialize_match_seq(paragraph);
            let mut test_template = template_content.clone();
            test_template.insert_str(0, &format!("{}\n", prompt.to_string()));
            let root = parse_document(&test_template);
            let document = Document::new(&root, None)?;

            process_child_block_elements(&document.root, &document).unwrap();

            if let Some(paragraph) = document.root.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                let match_type = document.get_match_type(&paragraph).unwrap();
                match prompt {
                    PromptToken::Mandatory => assert_eq!(match_type, MatchType::Mandatory),
                    PromptToken::Optional => assert_eq!(match_type, MatchType::Optional),
                    _ => ()
                }
                assert!(! document.is_wildcard(&paragraph).unwrap());
                if let Some(text) = paragraph.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                    let processed_content = text.capabilities.get.as_ref().unwrap().get_content().unwrap();
                    assert_eq!(processed_content, template_content);
                } else {
                    panic!("No text node found");
                }
            } else {
                panic!("No paragraph node found");
            }
        }

        #[test]
        fn test_wildcard_paragraphs_are_processed(ref prompt in prop_oneof![Just(PromptToken::Mandatory),Just(PromptToken::Optional)]) {
            let template_content = prompt.to_string();
            let root = parse_document(&template_content);
            let document = Document::new(&root, None)?;

            process_child_block_elements(&document.root, &document).unwrap();

            if let Some(paragraph) = document.root.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                let match_type = document.get_match_type(&paragraph).unwrap();
                match prompt {
                    PromptToken::Mandatory => assert_eq!(match_type, MatchType::Mandatory),
                    PromptToken::Optional => assert_eq!(match_type, MatchType::Optional),
                    _ => ()
                }
                assert!(document.is_wildcard(&paragraph).unwrap());
                assert!(paragraph.capabilities.traverse.as_ref().unwrap().first_child().unwrap().is_none());
            } else {
                panic!("No paragraph node found");
            }
        }

        #[test]
        fn test_literal_paragraphs_are_processed(ref paragraph in arb_paragraph_match(2..10)) {
            let (template_content, _) = serialize_match_seq(paragraph);
            let root = parse_document(&template_content);
            let document = Document::new(&root, None)?;

            process_child_block_elements(&root, &document).unwrap();

            if let Some(paragraph) = document.root.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                assert_eq!(document.get_match_type(&paragraph).unwrap(), MatchType::Mandatory);
                assert!(! document.is_wildcard(&paragraph).unwrap());
                if let Some(text) = paragraph.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                    let processed_content = text.capabilities.get.as_ref().unwrap().get_content().unwrap();
                    assert_eq!(processed_content, template_content);
                } else {
                    panic!("No text node found");
                }
            } else {
                panic!("No paragraph node found");
            }
        }

        #[test]
        fn test_literal_repeatable_paragraphs_are_processed(ref paragraph in arb_paragraph_match(2..10)) {
            let (template_content, _) = serialize_match_seq(paragraph);
            let mut test_template = template_content.clone();
            test_template.push_str("\n\n-\"\"-");

            let root = parse_document(&test_template);
            let document = Document::new(&root, None)?;

            process_child_block_elements(&root, &document).unwrap();

            if let Some(paragraph) = document.root.capabilities.traverse.as_ref().unwrap().first_child().unwrap() {
                assert_eq!(document.get_match_type(&paragraph).unwrap(), MatchType::Mandatory);
                assert!(! document.is_wildcard(&paragraph).unwrap());
                let traverser = paragraph.capabilities.traverse.as_ref().unwrap();
                if let Some(text) = traverser.first_child().unwrap() {
                    let processed_content = text.capabilities.get.as_ref().unwrap().get_content().unwrap();
                    assert_eq!(processed_content, template_content);
                } else {
                    panic!("No text node found");
                }
                if let Some(ditto) = traverser.next_sibling().unwrap() {
                    assert_eq!(document.get_match_type(&ditto).unwrap(), MatchType::Repeatable);
                } else {
                    panic!("No Ditto Node found");
                }
            } else {
                panic!("No paragraph node found");
            }
        }
    }
}
