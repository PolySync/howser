//! Abstractions for CMark Documents and Howser Templates.

extern crate env_logger;
extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use self::unicode_segmentation::UnicodeSegmentation;
use constants::{DITTO_TOKEN, MANDATORY_PROMPT, OPTIONAL_PROMPT, PROMPT_PATTERN};
use data::ElementType;
use data::{MatchType, NodeData};
use doogie::constants::*;
use doogie::Node;
use errors::{HowserError, HowserResult, SpecWarning};
use std::cell::RefCell;
use std::collections::HashMap;
use validator::types_match;

#[derive(Debug)]
enum LookaheadType {
    IntegratedLiteral(Node),
    IntegratedOccupied((Node, MatchType)),
    IntegratedVacant((Node, MatchType)),
    DiscreteAnnotated((Node, MatchType)),
    DiscreteLiteral(Node),
    Ditto(Node),
    List(Node),
    Other(Option<Node>),
}

impl LookaheadType {
    fn new(node: Option<Node>) -> HowserResult<Self> {
        if let Some(node) = node {
            let match_type = extract_annotation(&node)?;
            match node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?
                .get_type()?
            {
                NodeType::CMarkNodeList => Ok(LookaheadType::List(node)),
                NodeType::CMarkNodeParagraph
                | NodeType::CMarkNodeBlockQuote
                | NodeType::CMarkNodeCodeBlock => match match_type {
                    MatchType::Repeatable => Ok(LookaheadType::Ditto(node)),
                    MatchType::None => Ok(LookaheadType::IntegratedLiteral(node)),
                    _ => match node.capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?
                        .first_child()?
                    {
                        Some(_) => Ok(LookaheadType::IntegratedOccupied((node, match_type))),
                        None => Ok(LookaheadType::IntegratedVacant((node, match_type))),
                    },
                },
                NodeType::CMarkNodeHeading | NodeType::CMarkNodeItem => match match_type {
                    MatchType::Repeatable => Ok(LookaheadType::Ditto(node)),
                    MatchType::None => Ok(LookaheadType::DiscreteLiteral(node)),
                    _ => Ok(LookaheadType::DiscreteAnnotated((node, match_type))),
                },
                _ => Ok(LookaheadType::Other(Some(node))),
            }
        } else {
            Ok(LookaheadType::Other(None))
        }
    }
}

/// Wrapper for a Markdown document that stores extra metadata.
pub struct Document<'a> {
    pub root: &'a Node,
    data: RefCell<HashMap<u32, NodeData>>,
    pub filename: Option<String>,
}

impl<'a> Document<'a> {
    pub fn new(root: &'a Node, filename: Option<String>) -> Self {
        Document {
            root,
            data: RefCell::new(HashMap::new()),
            filename,
        }
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

    /// Returns the `MatchType` of the specified document node.
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

    /// Returns a boolean indicating if this node is considered a wildcard for matching purposes.
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

    /// Set the wildcard status for a node in this document.
    fn set_is_wildcard(&self, node: &Node, state: bool) -> Result<&Self, HowserError> {
        if let Some(ref getter) = node.capabilities.get {
            let id = getter.get_id()?;
            let mut data = self.data.borrow_mut();
            let node_data = data.entry(id).or_insert(NodeData::new());
            node_data.is_wildcard = state;
        }

        Ok(self)
    }

    /// Set the element-level match type for a node in this document.
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
}

/// A wrapper around the `Document` struct that reflects its status as an Rx prescription.
pub struct Prescription<'a> {
    pub document: Document<'a>,
}

/// A circumstantial node is one that may or may not appear in the document due to its presence
/// only being necessary if an optional child node is present.
pub fn node_type_is_circumstantial(node: &Node) -> HowserResult<bool> {
    let node_type = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_type()?;
    match node_type {
        NodeType::CMarkNodeList => Ok(true),
        _ => Ok(false),
    }
}

pub fn reverse(s: &String) -> String {
    let iter = UnicodeSegmentation::graphemes(s.as_str(), true);
    iter.rev().collect::<String>()
}

/// Extract and transform the block-level Rx prompts from the document content into
/// contextualized metadata on the match types of those elements.
fn process_child_block_elements(parent: &Node, document: &Document) -> HowserResult<()> {
    trace!("process_child_block_elements");
    let mut current_child = parent
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .first_child()?;

    if let Some(ref node) = current_child {
        match ElementType::determine(node)? {
            ElementType::InlineContainer | ElementType::InlineLeaf => {
                debug!("Returning from inline element");
                return Ok(());
            }
            _ => (),
        }
    }

    while let Some(l1_node) = current_child {
        let mut l2 = LookaheadType::Other(None);
        let mut l3 = LookaheadType::Other(None);

        if let Some(l2_node) = l1_node
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?
            .next_sibling()?
        {
            if types_match(&l1_node, &l2_node)? {
                match l1_node
                    .capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .get_type()?
                {
                    NodeType::CMarkNodeHeading | NodeType::CMarkNodeItem => {
                        if let Some(l3_node) = l2_node
                            .capabilities
                            .traverse
                            .as_ref()
                            .ok_or(HowserError::CapabilityError)?
                            .next_sibling()?
                        {
                            if types_match(&l1_node, &l3_node)? {
                                l3 = LookaheadType::new(Some(l3_node))?;
                            } else {
                                l3 = LookaheadType::Other(Some(l3_node));
                            }
                        }
                    }
                    _ => {
                        l3 = LookaheadType::Other(l2_node
                            .capabilities
                            .traverse
                            .as_ref()
                            .ok_or(HowserError::CapabilityError)?
                            .next_sibling()?);
                    }
                }
                l2 = LookaheadType::new(Some(l2_node))?;
            } else {
                l2 = LookaheadType::Other(Some(l2_node));
            }
        }
        let l1 = LookaheadType::new(Some(l1_node))?;

        debug!("Lookahead Sequence: {:?}, {:?}, {:?}", l1, l2, l3);

        match (l1, l2, l3) {
            (
                LookaheadType::DiscreteAnnotated((annotation, match_type)),
                LookaheadType::DiscreteLiteral(target),
                LookaheadType::Ditto(ditto)
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
                annotation.capabilities.mutate.as_ref().ok_or(HowserError::CapabilityError)?.unlink();
            },
            (
                LookaheadType::DiscreteAnnotated((annotation, match_type)),
                LookaheadType::DiscreteLiteral(target),
                LookaheadType::Other(next)
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&target, match_type)?;
                annotation.capabilities.mutate.as_ref().ok_or(HowserError::CapabilityError)?.unlink();
                current_child = next;
            },
            (
                LookaheadType::DiscreteAnnotated((annotation, match_type)),
                LookaheadType::Ditto(ditto),
                LookaheadType::Other(next)
            ) => {
                document.set_match_type(&annotation, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                document.set_is_wildcard(&annotation, true)?;
                current_child = next;
            },
            (
                LookaheadType::DiscreteLiteral(target),
                LookaheadType::Ditto(ditto),
                _
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.capabilities.traverse.as_ref().ok_or(HowserError::CapabilityError)?.next_sibling()?;
            },
            (
                LookaheadType::DiscreteAnnotated((target, match_type)),
                LookaheadType::Other(next),
                _
            ) => {
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = next;
            },
            (
                LookaheadType::DiscreteLiteral(target),
                LookaheadType::Other(next),
                _
            ) => {
                process_child_block_elements(&target, document)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedVacant((target, match_type)),
                LookaheadType::Ditto(ditto),
                LookaheadType::Other(next)
            ) => {
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                document.set_is_wildcard(&target, true)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedOccupied((target, match_type)),
                LookaheadType::Ditto(ditto),
                LookaheadType::Other(next)
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                LookaheadType::Ditto(ditto),
                LookaheadType::Other(next)
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedVacant((target, match_type)),
                LookaheadType::Other(next),
                _
            ) => {
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedOccupied((target, match_type)),
                LookaheadType::Other(next),
                _
            ) => {
                process_child_block_elements(&target, document)?;
                document.set_match_type(&target, match_type)?;
                current_child = next;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                LookaheadType::Other(next),
                _
            ) => {
                process_child_block_elements(&target, document)?;
                current_child = next;
            },
            (
                LookaheadType::List(target),
                LookaheadType::Other(next),
                _
            ) => {
                process_child_block_elements(&target, document)?;
                annotate_list_element(&target, document)?;
                current_child = next;
            },
            (LookaheadType::Ditto(node), _, _) => {
                return Err(HowserError::PrescriptionError(SpecWarning::new(&node, document, "An element with a Ditto prompt must be preceded by an element of the same type.")?))
            },
            _ => return Err(HowserError::RuntimeError("Unexpected Lookahead Encountered".to_string()))
        }
    }

    Ok(())
}

fn extract_annotation(node: &Node) -> HowserResult<MatchType> {
    match node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?
        .get_type()?
    {
        NodeType::CMarkNodeParagraph => extract_paragraph_match_token(node),
        NodeType::CMarkNodeBlockQuote => extract_block_quote_match_token(node),
        NodeType::CMarkNodeCodeBlock => extract_code_block_match_token(node),
        NodeType::CMarkNodeHeading => extract_heading_match_token(node),
        NodeType::CMarkNodeItem => extract_item_match_token(node),
        _ => Ok(MatchType::None),
    }
}

fn extract_paragraph_match_token(node: &Node) -> HowserResult<MatchType> {
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
        match (token == MatchType::None, content.is_empty()) {
            (false, true) => {
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

                Ok(token)
            }
            _ => Ok(MatchType::None),
        }
    } else {
        Ok(MatchType::None)
    }
}

fn extract_block_quote_match_token(node: &Node) -> HowserResult<MatchType> {
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
            let token = extract_paragraph_match_token(&paragraph);
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
            token
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

fn extract_code_block_match_token(node: &Node) -> HowserResult<MatchType> {
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
        Ok(match_type)
    } else {
        Ok(MatchType::None)
    }
}

fn extract_heading_match_token(node: &Node) -> HowserResult<MatchType> {
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

fn extract_item_match_token(node: &Node) -> HowserResult<MatchType> {
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
            let token = extract_paragraph_match_token(&paragraph)?;
            if token != MatchType::None {
                paragraph
                    .capabilities
                    .mutate
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?
                    .unlink();
            }

            Ok(token)
        } else {
            Ok(MatchType::None)
        }
    } else {
        Ok(MatchType::None)
    }
}

/// Sets the match type of a circumstantial node based on the match types of its children.
///
/// If any children are marked Mandatory, then the node is also mandatory. Else, it is optional.
fn annotate_list_element(node: &Node, document: &Document) -> HowserResult<()> {
    document.set_match_type(node, MatchType::Optional)?;
    let traverser = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let mut child = traverser.first_child()?;
    while let Some(node) = child {
        if document.get_match_type(&node)? == MatchType::Mandatory {
            document.set_match_type(&node, MatchType::Mandatory)?;
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

/// Strips a prompt from the beginning of a string and returns the prompt and the stripped string.
///
/// If the string does not begin with a prompt, a blank string and the origininal string are returned.
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
            let document = Document::new(&root, None);

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
            let document = Document::new(&root, None);

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
            let document = Document::new(&root, None);

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
            let document = Document::new(&root, None);

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
