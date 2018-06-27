//! Abstractions for CMark Documents and Howser Templates.

extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use constants::{DITTO_TOKEN, MANDATORY_PROMPT, OPTIONAL_PROMPT, PROMPT_PATTERN};
use data::ElementType;
use data::{MatchType, NodeData};
use doogie::constants::*;
use doogie::{
    Node,
};
use errors::{HowserError, HowserResult, SpecWarning};
use std::cell::RefCell;
use std::collections::HashMap;
use validator::types_match;

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
        for (node, _) in self.root.iter() {
            let found_type = node.get_cmark_type()?;
            if found_type == node_type {
                return Ok(Some(node));
            }
        }

        Ok(None)
    }

    /// Returns the `MatchType` of the specified `Node`.
    pub fn get_match_type(&self, node: &Node) -> HowserResult<MatchType> {
        let id = node.get_id();
        let mut data_store = self.data.borrow_mut();
        let node_data = data_store.entry(id).or_insert(NodeData::new());
        Ok(node_data.match_type.clone())
    }

    /// Set the `MatchType` for a `Node`.
    fn set_match_type(&self, node: &Node, match_type: MatchType) -> Result<&Self, HowserError> {
        trace!("set_match_type()");
        let id = node.get_id();
        let mut data = self.data.borrow_mut();
        let node_data = data.entry(id).or_insert(NodeData::new());
        node_data.match_type = match_type;

        Ok(self)
    }

    /// Returns a boolean indicating if this `Node` is considered a wildcard.
    pub fn is_wildcard(&self, rx_node: &Node) -> HowserResult<bool> {
        let id = rx_node.get_id();
        let mut data_store = self.data.borrow_mut();
        let node_data = data_store.entry(id).or_insert(NodeData::new());
        Ok(node_data.is_wildcard)
    }

    /// Set the wildcard status for a `Node`.
    fn set_is_wildcard(&self, node: &Node, state: bool) -> HowserResult<()> {
        trace!("set_is_wildcard()");
        let id = node.get_id();
        let mut data = self.data.borrow_mut();
        let node_data = data.entry(id).or_insert(NodeData::new());
        node_data.is_wildcard = state;

        Ok(())
    }

    /// Infers the line number of the given node.
    ///
    /// Cmark reports that some inline and nested nodes are at line number zero which is usually
    /// incorrect. This function searches up through the tree ancestry until it finds a parent node
    /// that reports a sensible line number and returns that.
    pub fn get_line_num(node: &Node) -> HowserResult<usize> {
        let parent = node.parent()?;
        let line_num = node.get_start_line();

        match (parent, line_num) {
            (Some(parent), 0) => Document::get_line_num(&parent),
            _ => Ok(line_num as usize),
        }
    }
}

/// A `Document` that has been parsed into an Rx prescription.
pub struct Prescription<'a> {
    pub document: Document<'a>,
}

/// Process the match types of the children of the given parent.
fn process_child_elements(parent: &Node, document: &Document) -> HowserResult<()> {
    let child = parent.first_child()?;

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
    let mut current_child = parent.first_child()?;

    while let Some(node) = current_child {
        match node {
            Node::Text(ref text) => {
                if all_content_is_optional(&text.get_content()?)? {
                    document.set_match_type(&node, MatchType::Optional)?;
                }
            },
            Node::Code(ref code) => {
                if all_content_is_optional(&code.get_content()?)? {
                    document.set_match_type(&node, MatchType::Optional)?;
                }
            },
            _ => {
                process_child_inline_elements(&node, document)?;
                annotate_circumstantial_node(&node, document)?;
            }
        }

        current_child = node.next_sibling()?;
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
    trace!("process_child_block_elements()");
    let mut current_child = parent.first_child()?;

    while let Some(l1_node) = current_child {
        let mut l2 = LookaheadType::Other(None);

        if let Some(l2_node) = l1_node.next_sibling()?
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
                current_child = ditto.next_sibling()?;
            },
            (
                LookaheadType::DiscreteLiteral(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                current_child = target.next_sibling()?;
            },
            (
                LookaheadType::DiscreteAnnotated(mut annotation, match_type),
                LookaheadType::DiscreteLiteral(target),
            ) => {
                document.set_match_type(&target, match_type)?;
                annotation.unlink();
                current_child = Some(target);
            },
            (
                LookaheadType::DiscreteAnnotated(mut target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                remove_annotation(&mut target)?;
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.next_sibling()?;
            },
            (
                LookaheadType::DiscreteAnnotated(target, match_type),
                _,
            ) => {
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = target.next_sibling()?;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                LookaheadType::Ditto(ditto),
            ) => {
                process_child_elements(&target, document)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.next_sibling()?;
            },
            (
                LookaheadType::IntegratedLiteral(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                current_child = target.next_sibling()?;
            },
            (
                LookaheadType::IntegratedVacant(mut target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                remove_annotation(&mut target)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                document.set_is_wildcard(&target, true)?;
                current_child = ditto.next_sibling()?;
            },
            (
                LookaheadType::IntegratedVacant(mut target, match_type),
                _,
            ) => {
                remove_annotation(&mut target)?;
                document.set_match_type(&target, match_type)?;
                document.set_is_wildcard(&target, true)?;
                current_child = target.next_sibling()?;
            },
            (
                LookaheadType::IntegratedOccupied(mut target, match_type),
                LookaheadType::Ditto(ditto),
            ) => {
                process_child_elements(&target, document)?;
                remove_annotation(&mut target)?;
                document.set_match_type(&target, match_type)?;
                document.set_match_type(&ditto, MatchType::Repeatable)?;
                current_child = ditto.next_sibling()?;
            },
            (
                LookaheadType::IntegratedOccupied(mut target, match_type),
                _,
            ) => {
                process_child_elements(&target, document)?;
                remove_annotation(&mut target)?;
                document.set_match_type(&target, match_type)?;
                current_child = target.next_sibling()?;
            },
            (
                LookaheadType::List(target),
                _,
            ) => {
                process_child_elements(&target, document)?;
                annotate_circumstantial_node(&target, document)?;
                current_child = target.next_sibling()?;
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
    trace!("document::get_annotation");
    match node
    {
        Node::Paragraph(_) => get_paragraph_annotation(node),
        Node::BlockQuote(_) => get_block_quote_annotation(node),
        Node::CodeBlock(_) => get_code_block_annotation(node),
        Node::Heading(_) => get_heading_annotation(node),
        Node::Item(_) => get_item_annotation(node),
        _ => Ok(MatchType::None),
    }
}

/// Strips the annotation from a block level element if one exists.
fn remove_annotation(node: &mut Node) -> HowserResult<()> {
    trace!("remove_annotation()");
    match node
    {
        Node::Paragraph(_) => remove_paragraph_annotation(node),
        Node::BlockQuote(_) => remove_block_quote_annotation(node),
        Node::CodeBlock(_) => remove_code_block_annotation(node),
        Node::Heading(_) => remove_heading_annotation(node),
        Node::Item(_) => remove_item_annotation(node),
        _ => Ok(()),
    }
}

/// Returns the block level annotation of a paragraph node if one exists.
fn get_paragraph_annotation(node: &Node) -> HowserResult<MatchType> {
    trace!("get_paragraph_annotation()");
    if let Some(text_node @ Node::Text(_)) = node.first_child()? {
        match text_node.next_sibling()? {
            Some(Node::SoftBreak(_)) | None => {
                if let Node::Text(ref text) = text_node {
                    let content = text.get_content()?;
                    let (token, remainder) = extract_match_type(&content)?;
                    if token != MatchType::None && remainder.is_empty() {
                        return Ok(token);
                    }
                }
            },
            _ => ()
        }
    }

    Ok(MatchType::None)
}

/// Strips the block-level annotation from a paragraph node if one exists.
fn remove_paragraph_annotation(node: &mut Node) -> HowserResult<()> {
    if let Some(mut text_node @ Node::Text(_)) = node.first_child()? {
        let sibling = text_node.next_sibling()?;
        match sibling {
            Some(Node::SoftBreak(_)) | None => {
                let match_result = match text_node {
                    Node::Text(ref text) => extract_match_type(&text.get_content()?)?,
                    _ => (MatchType::None, String::new())
                };
                let (token, remainder) = match_result;
                if token != MatchType::None && remainder.is_empty() {
                    text_node.unlink();
                    if let Some(mut node) = sibling {
                        node.unlink();
                    }
                }
            },
            _ => ()
        }
    }

    Ok(())
}

/// Returns the block-level annotation from a block quote node if one exists.
fn get_block_quote_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Some(paragraph_node @ Node::Paragraph(_)) = node.first_child()?
    {
        get_paragraph_annotation(&paragraph_node)
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a block quote node if one exists.
fn remove_block_quote_annotation(node: &mut Node) -> HowserResult<()> {
    if let Some(Node::Paragraph(_)) = node.first_child()? {
        remove_paragraph_annotation(node)?;
        if node.first_child()?.is_none() {
            node.unlink();
        }
    }

    Ok(())
}

/// Returns the annotation of a code block node if one exists.
fn get_code_block_annotation(node: &Node) -> HowserResult<MatchType> {
    if let Node::CodeBlock(code_block) = node {
        let (match_type, _) = extract_match_type(&code_block.get_fence_info()?)?;
        Ok(match_type)
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the annotation from a code block node if one exists.
fn remove_code_block_annotation(node: &mut Node) -> HowserResult<()> {
    if let Node::CodeBlock(ref mut code_block) = node {
        let (match_type, content) = extract_match_type(&code_block.get_fence_info()?)?;
        if match_type != MatchType::None {
            code_block.set_fence_info(&content)?;
        }
    }

    Ok(())
}

/// Returns the block-level annotation of a heading node if one exists.
fn get_heading_annotation(heading_node: &Node) -> HowserResult<MatchType> {
    if let Some(text_node) = heading_node.first_child()? {
        if let Node::Text(ref text) = text_node {
            if text_node.next_sibling()?.is_none() {
                let (match_type, content) = extract_match_type(&text.get_content()?)?;
                if match_type != MatchType::None && content.is_empty() {
                    return Ok(match_type);
                } else {
                    return Ok(MatchType::None);
                }
            }
        }
    }

    Ok(MatchType::None)
}

/// Strips the block-level annotation from a heading node if one exists.
fn remove_heading_annotation(heading_node: &Node) -> HowserResult<()> {
    if let Some(mut text_node @ Node::Text(_)) = heading_node.first_child()? {
        let match_result = match text_node {
            Node::Text(ref text) => extract_match_type(&text.get_content()?)?,
            _ => (MatchType::None, String::new())
        };
        if text_node.next_sibling()?.is_none() {
            let (match_type, content) = match_result;
            if match_type != MatchType::None && content.is_empty() {
                text_node.unlink();
            }
        }
    }

    Ok(())
}

/// Returns the block-level annotation from a list item node if one exists.
fn get_item_annotation(item_node: &Node) -> HowserResult<MatchType> {
    if let Some(paragraph_node @ Node::Paragraph(_)) = item_node.first_child()? {
        get_paragraph_annotation(&paragraph_node)
    } else {
        Ok(MatchType::None)
    }
}

/// Strips the block-level annotation from a list item node if one exists.
fn remove_item_annotation(item_node: &Node) -> HowserResult<()> {
    if let Some(mut paragraph_node @ Node::Paragraph(_)) = item_node.first_child()? {
        let token = get_paragraph_annotation(&paragraph_node)?;
        if token != MatchType::None {
            remove_paragraph_annotation(&mut paragraph_node)?;
            paragraph_node.unlink();
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
    let mut child = target_node.first_child()?;
    while let Some(node) = child {
        if document.get_match_type(&node)? == MatchType::Mandatory {
            document.set_match_type(&target_node, MatchType::Mandatory)?;
            return Ok(());
        } else {
            child = node.next_sibling()?;
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
    for (mut node, _) in root.iter()
    {
        match node
        {
            Node::HtmlInline(_) | Node::HtmlBlock(_) => {
                node.unlink();
            }
            _ => (),
        }
    }

    Ok(())
}

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
        trace!("LookaheadType::new");
        if let Some(node) = node {
            let match_type = get_annotation(&node)?;
            debug!("LookaheadType::new: MatchType: {:?}", match_type);
            match node {
                    // List
                    Node::List(_) => Ok(LookaheadType::List(node)),
                    // Integrated
                    Node::Paragraph(_)
                    | Node::BlockQuote(_)
                    | Node::CodeBlock(_) => match match_type {
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
                    Node::Heading(_) | Node::Item(_) => match match_type {
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
        match node {
            Node::CodeBlock(ref code_block) => Ok(code_block.get_content()?.is_empty()),
            _ => {
                if let Some(annotation) = node.first_child()?
                    {
                        Ok(annotation.next_sibling()?.is_none())
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

#[cfg(test)]
mod tests {
    use super::process_child_block_elements;
    use super::Document;
    use data::{MatchType, PromptToken};
    use doogie::{
        parse_document,
        Node
    };
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

            if let Some(paragraph_node) = document.root.first_child().unwrap() {
                let match_type = document.get_match_type(&paragraph_node).unwrap();
                match prompt {
                    PromptToken::Mandatory => assert_eq!(match_type, MatchType::Mandatory),
                    PromptToken::Optional => assert_eq!(match_type, MatchType::Optional),
                    _ => ()
                }
                assert!(! document.is_wildcard(&paragraph_node).unwrap());
                if let Some(Node::Text(ref text)) = paragraph_node.first_child().unwrap() {
                    let processed_content = text.get_content().unwrap();
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
            let document = Document::new(&root, None).unwrap();

            process_child_block_elements(&document.root, &document).unwrap();

            if let Some(paragraph @ Node::Paragraph(_)) = document.root.first_child().unwrap() {
                let match_type = document.get_match_type(&paragraph).unwrap();
                match prompt {
                    PromptToken::Mandatory => assert_eq!(match_type, MatchType::Mandatory),
                    PromptToken::Optional => assert_eq!(match_type, MatchType::Optional),
                    _ => ()
                }
                assert!(document.is_wildcard(&paragraph).unwrap());
                assert!(paragraph.first_child().unwrap().is_none());
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

            if let Some(paragraph_node) = document.root.first_child().unwrap() {
                assert_eq!(document.get_match_type(&paragraph_node).unwrap(), MatchType::Mandatory);
                assert!(! document.is_wildcard(&paragraph_node).unwrap());
                if let Some(Node::Text(ref text)) = paragraph_node.first_child().unwrap() {
                    let processed_content = text.get_content().unwrap();
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

            if let Some(paragraph @ Node::Paragraph(_)) = document.root.first_child().expect("No Paragraph Node found") {
                assert_eq!(document.get_match_type(&paragraph).unwrap(), MatchType::Mandatory);
                assert!(! document.is_wildcard(&paragraph).unwrap());

                if let Some(Node::Text(ref text)) = paragraph.first_child().expect("No Text Node found") {
                    let processed_content = text.get_content().unwrap();
                    assert_eq!(processed_content, template_content);
                }

                if let Some(ditto_node @ Node::Paragraph(_)) = paragraph.first_child().unwrap().unwrap().next_sibling().expect("No ditto node found") {
                    assert_eq!(document.get_match_type(&ditto_node).unwrap(), MatchType::Repeatable);
                }
            }
        }
    }
}
