//! Abstractions for CMark Documents and Howser Templates.

extern crate regex;
extern crate unicode_segmentation;

use self::regex::Regex;
use std::collections::HashMap;
use std::cell::RefCell;
use errors::{HowserError, HowserResult};
use data::{MatchType, NodeData};
use doogie::Node;
use doogie::constants::*;

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
        let document = subsume_comments(self)?;
        process_child_prompts(&document.root, &document)?;

        Ok(Prescription { document: document })
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

    // TODO -- Remove this debugging function after no longer needed.
    /// Prints a textual representation of traversing the document tree.
    pub fn output_nodes(&self) {
        println!("------- Begin Structure -----------");
        let traverser = self.root.capabilities.traverse.as_ref().unwrap();

        for (node, e) in traverser.iter() {
            if e == IterEventType::Enter {
                let getter = node.capabilities.get.as_ref().unwrap();
                let found_type = getter.get_type().unwrap();
                let content = getter.get_content().unwrap();
                println!(
                    "Type: {:?}\nContent: {}\nMatch Type: {:?}\n",
                    found_type,
                    content,
                    self.get_match_type(&node).unwrap()
                );
            }
        }
        println!("------- End Structure----------");
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

    /// Set the comment string for a node in this document.
    fn set_comment(&self, node: &Node, comment: &str) -> Result<&Self, HowserError> {
        if let Some(ref getter) = node.capabilities.get {
            let id = getter.get_id()?;
            let mut data = self.data.borrow_mut();
            let node_data = data.entry(id).or_insert(NodeData::new());
            node_data.comment = Some(String::from(comment));
        }

        Ok(self)
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

mod helpers {
    use super::unicode_segmentation::UnicodeSegmentation;
    use super::NodeType;

    /// Sequenceable node types are those which can be used as a pattern in the prescription
    /// that can match against an arbitrary sequence of nodes of the same type in a document.
    pub fn node_type_is_sequencable(node_type: &NodeType) -> bool {
        match *node_type {
            NodeType::CMarkNodeParagraph
            | NodeType::CMarkNodeHeading
            | NodeType::CMarkNodeItem
            | NodeType::CMarkNodeList
            | NodeType::CMarkNodeCodeBlock => true,
            _ => false,
        }
    }

    /// A circumstantial node is one that may or may not appear in the document due to its presence
    /// only being necessary if an optional child node is present.
    pub fn node_type_is_circumstantial(node_type: &NodeType) -> bool {
        match *node_type {
            NodeType::CMarkNodeList => true,
            _ => false,
        }
    }

    pub fn reverse(s: &String) -> String {
        let iter = UnicodeSegmentation::graphemes(s.as_str(), true);
        iter.rev().collect::<String>()
    }
}

/// Walk through the document removing comment nodes and transforming their content into
/// metadata linked to the related node.
fn subsume_comments(document: Document) -> HowserResult<Document> {
    {
        let traverser = document
            .root
            .capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        for (node, event) in traverser.iter() {
            let getter = node.capabilities
                .get
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;

            match (event, getter.get_type()?) {
                (IterEventType::Exit, NodeType::CMarkNodeHtmlInline)
                | (IterEventType::Enter, NodeType::CMarkNodeHtmlInline) => {
                    subsume_comment(&node, &document)?;
                }
                _ => continue,
            }
        }
    }

    Ok(document)
}

/// Remove a comment node from that document and add its content to the metadata of the node
/// it is commenting.
fn subsume_comment(commenter: &Node, document: &Document) -> HowserResult<()> {
    let commenter_traverser = commenter
        .capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;

    if let Some(commentee) = commenter_traverser
        .prev_sibling()?
        .or(commenter_traverser.parent()?)
    {
        let commenter_getter = commenter
            .capabilities
            .get
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let commenter_mutator = commenter
            .capabilities
            .mutate
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;

        let comment = commenter_getter.get_content()?;
        document.set_comment(&commentee, &comment)?;

        commenter_mutator.unlink();
    }

    Ok(())
}

/// Extract and transform the element-level Rx prompts from the document content into
/// contextualized metadata on the match types of those nodes.
fn process_child_prompts(node: &Node, document: &Document) -> HowserResult<()> {
    {
        let traverser = node.capabilities
            .traverse
            .as_ref()
            .ok_or(HowserError::CapabilityError)?;
        let mut child = traverser.first_child()?;

        while let Some(sibling) = child {
            let sibling_trav = sibling
                .capabilities
                .traverse
                .as_ref()
                .ok_or(HowserError::CapabilityError)?;

            let mut node_type: NodeType;
            if let Some(ref getter) = sibling.capabilities.get {
                node_type = getter.get_type()?;
            } else {
                return Err(HowserError::CapabilityError);
            }

            if helpers::node_type_is_sequencable(&node_type)
                && !(node_type == NodeType::CMarkNodeList)
            {
                process_match_token(&sibling, &document)?;
            }

            process_child_prompts(&sibling, document)?;

            if helpers::node_type_is_circumstantial(&node_type) {
                process_circumstantial_node(&sibling, document)?;
            }

            child = sibling_trav.next_sibling()?;
        }
    }

    Ok(())
}

/// Strip the match token from a node and add it as metadata.
fn process_match_token(node: &Node, document: &Document) -> HowserResult<()> {
    let getter = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let mut match_token = String::new();

    if let Some(ref token_node) = get_match_token_node(node)? {
        match_token = subsume_token_node(node, token_node, document)?;
    } else if getter.get_type()? == NodeType::CMarkNodeCodeBlock {
        match_token = subsume_code_fence_token(node, document)?;
    }
    let match_type = match match_token.as_ref() {
        ::constants::MANDATORY_PROMPT => MatchType::Mandatory,
        ::constants::OPTIONAL_PROMPT => MatchType::Optional,
        ::constants::DITTO_TOKEN | ::constants::U_DITTO_TOKEN => MatchType::Repeatable,
        _ => MatchType::Mandatory,
    };
    document.set_match_type(node, match_type)?;

    Ok(())
}

/// Sets the match type of a circumstantial node based on the match types of its children.
///
/// If any children are marked Mandatory, then the node is also mandatory. Else, it is optional.
fn process_circumstantial_node(node: &Node, document: &Document) -> HowserResult<()> {
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

/// Prune a match token node from the tree and add it as metadata to the node it is annotating.
fn subsume_token_node(node: &Node, token_node: &Node, document: &Document) -> HowserResult<String> {
    let traverser = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let token_node_getter = token_node
        .capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let token_node_setter = token_node
        .capabilities
        .set
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let token_node_content = token_node_getter.get_content()?;
    let (match_token, token_node_content) = lstrip_match_prompt(&token_node_content)?;
    token_node_setter.set_content(&token_node_content)?;
    prune_token_node_from(node)?;
    if traverser.first_child()?.is_none() {
        document.set_is_wildcard(node, true)?;
    }

    Ok(match_token)
}

/// Strip the match token from the info string of a code fence node and add it as metadata.
fn subsume_code_fence_token(node: &Node, document: &Document) -> HowserResult<String> {
    let getter = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let setter = node.capabilities
        .set
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;

    let reverse_info = helpers::reverse(&getter.get_fence_info()?);
    let (token, stripped_rev_info) = lstrip_match_prompt(&reverse_info)?;
    setter.set_fence_info(&helpers::reverse(&stripped_rev_info))?;
    if getter.get_content()?.is_empty() {
        document.set_is_wildcard(node, true)?;
    }

    Ok(token)
}

/// Strips a prompt from the beginning of a string and returns the prompt and the stripped string.
///
/// If the string does not begin with a prompt, a blank string and the origininal string are returned.
fn lstrip_match_prompt(content: &String) -> HowserResult<(String, String)> {
    let pattern = Regex::new(::constants::PROMPT_PATTERN)?;

    if let Some(location) = pattern.find(content.trim_left()) {
        if location.start() == 0 {
            let prompt = location.as_str();
            let tail = pattern.replace(content, "");
            Ok((prompt.to_string(), tail.to_string()))
        } else {
            Ok((String::new(), content.clone()))
        }
    } else {
        Ok((String::new(), content.clone()))
    }
}

fn prune_token_node_from(node: &Node) -> HowserResult<()> {
    let getter = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let traverser = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;

    match getter.get_type()? {
        NodeType::CMarkNodeParagraph | NodeType::CMarkNodeHeading => {
            if let Some(meta_node) = traverser.first_child()? {
                let meta_getter = meta_node
                    .capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?;
                let meta_content = meta_getter.get_content()?;
                if meta_getter.get_type()? == NodeType::CMarkNodeText && meta_content.is_empty() {
                    let mutator = meta_node
                        .capabilities
                        .mutate
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    mutator.unlink();
                }
            }
        }
        NodeType::CMarkNodeItem => {
            if let Some(inner_node) = traverser.first_child()? {
                let inner_getter = inner_node
                    .capabilities
                    .get
                    .as_ref()
                    .ok_or(HowserError::CapabilityError)?;
                if inner_getter.get_type()? == NodeType::CMarkNodeParagraph {
                    let inner_traverser = inner_node
                        .capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    if let Some(meta_node) = inner_traverser.first_child()? {
                        let meta_getter = meta_node
                            .capabilities
                            .get
                            .as_ref()
                            .ok_or(HowserError::CapabilityError)?;
                        let meta_content = meta_getter.get_content()?;
                        if meta_getter.get_type()? == NodeType::CMarkNodeText
                            && meta_content.is_empty()
                        {
                            let mutator = meta_node
                                .capabilities
                                .mutate
                                .as_ref()
                                .ok_or(HowserError::CapabilityError)?;
                            mutator.unlink();
                        }
                        if inner_traverser.first_child()?.is_none() {
                            let mutator = inner_node
                                .capabilities
                                .mutate
                                .as_ref()
                                .ok_or(HowserError::CapabilityError)?;
                            mutator.unlink();
                        }
                    }
                }
            }
        }
        _ => (),
    }

    Ok(())
}

/// Find the inner node that contains the match token for an outer node.
fn get_match_token_node(node: &Node) -> HowserResult<Option<Node>> {
    let getter = node.capabilities
        .get
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;
    let traverser = node.capabilities
        .traverse
        .as_ref()
        .ok_or(HowserError::CapabilityError)?;

    if let Some(match_node) = traverser.first_child()? {
        let mut match_node_type = NodeType::CMarkNodeNone;
        if let Some(ref getter) = match_node.capabilities.get {
            match_node_type = getter.get_type()?;
        }
        match getter.get_type()? {
            NodeType::CMarkNodeParagraph | NodeType::CMarkNodeHeading => {
                if match_node_type == NodeType::CMarkNodeText {
                    Ok(Some(match_node))
                } else {
                    Ok(None)
                }
            }
            NodeType::CMarkNodeItem => {
                if match_node_type == NodeType::CMarkNodeParagraph {
                    let paragraph_traverser = match_node
                        .capabilities
                        .traverse
                        .as_ref()
                        .ok_or(HowserError::CapabilityError)?;
                    if let Some(text_node) = paragraph_traverser.first_child()? {
                        let mut node_type = NodeType::CMarkNodeNone;
                        if let Some(ref getter) = text_node.capabilities.get {
                            node_type = getter.get_type()?;
                        }
                        if node_type == NodeType::CMarkNodeText {
                            Ok(Some(text_node))
                        } else {
                            Ok(None)
                        }
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            NodeType::CMarkNodeCodeBlock => Ok(None),
            _ => Err(HowserError::RuntimeError(String::from(
                "Expected sequencable node type.",
            ))),
        }
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use helpers::test::strategies::cmark::arb_paragraph_match;
    use helpers::test::strategies::content::prompts::{content_prompt, literal_prompt};
    use helpers::test::strategies::helpers::serialize_match_seq;
    use data::{MatchType, PromptToken};
    use doogie::constants::NodeType;
    use doogie::parse_document;
    use super::Document;
    use super::{process_child_prompts, subsume_comments};

    proptest!{
        #[test]
        fn test_subsume_comments(ref paragraph in arb_paragraph_match(1..10)) {
            let &(ref matches, ref comment) = paragraph;
            let (rx_content, _) = serialize_match_seq(matches);

            let mut test_content = rx_content.clone();
            if let &Some(ref c) = comment {
                test_content.push_str(&c.to_string());
            }

            let document_root = parse_document(&test_content);

            let document = Document::new(&document_root, None);
            let document = subsume_comments(document).unwrap();

            if let Some(result_paragraph) = document
                .first_of_type(NodeType::CMarkNodeText)
                .unwrap() {
                let getter = result_paragraph.capabilities.get.as_ref().unwrap();
                let result_content = getter.get_content().unwrap();
                assert_eq!(rx_content, result_content);
            } else {
                assert!(false, "Paragraph node not found");
            }
        }

        #[test]
        fn test_subsume_prompts(
                ref paragraph in arb_paragraph_match(1..10),
                ref prompt in prop::option::of(content_prompt()),
                ref literal in literal_prompt(1..10)) {
            let &(ref matches, _) = paragraph;
            let (mut template_content, _) = serialize_match_seq(matches);
            template_content.insert_str(0, &literal.to_string());
            let mut test_template = template_content.clone();
            if let &Some(ref prompt) = prompt {
                test_template.insert_str(0, &prompt.to_string());
            }

            let root = parse_document(&test_template);

            let document = Document::new(&root, None);
            process_child_prompts(&document.root, &document).unwrap();

            if let Some(paragraph_node) = document
                .first_of_type(NodeType::CMarkNodeParagraph)
                .unwrap() {
                let match_type = document.get_match_type(&paragraph_node).unwrap();
                if let Some(text_node) = document.first_of_type(NodeType::CMarkNodeText).unwrap() {
                    let getter = text_node.capabilities.get.as_ref().unwrap();
                    let content = getter.get_content().unwrap();
                    assert_eq!(content, template_content, "Subsumed content(Left) did not match the original content(Right)");
                    match prompt {
                        &Some(PromptToken::Mandatory) | &None => assert_eq!(match_type, MatchType::Mandatory, "Match type should have been mandatory"),
                        &Some(PromptToken::Optional) => assert_eq!(match_type, MatchType::Optional, "Match type should have been optional"),
                        _ => ()
                    }
                } else {
                    panic!("Couldn't find Text Node");
                }
            } else {
                panic!("Couldn't find paragraph node");
            }
        }
    }
}
