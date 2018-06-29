//! Various data types relating to `Document`s, `Template`s, and `Node`s.

use constants::{MANDATORY_PROMPT, OPTIONAL_PROMPT};
use doogie::Node;
use errors::HowserResult;
use std::fmt::{Debug, Error, Formatter};

/// Element-Level match types for `Node`s.
#[derive(PartialEq, Clone, Debug)]
pub enum MatchType {
    None,
    Mandatory,
    Optional,
    Repeatable,
}

/// Supplementary metadata associated with individual `Node`s.
pub struct NodeData {
    pub match_type: MatchType,
    pub comment: Option<String>,
    pub is_wildcard: bool,
}

impl NodeData {
    pub fn new() -> NodeData {
        NodeData {
            /// The `MatchType` of this `Node`.
            match_type: MatchType::Mandatory,
            /// Inline HTML comments are stripped from `Node`s and their contents stored.
            comment: None,
            /// A wildcard `Node` will match any internal content.
            is_wildcard: false,
        }
    }
}

/// Represents a templated prompt for content.
#[derive(PartialEq, Debug, Clone)]
pub enum PromptToken {
    None,
    /// `-!!-` Some arbitrary content must appear here.
    Mandatory,
    /// `-??-` Some arbitrary content may appear here.
    Optional,
    /// The supplied textual content must appear verbatim.
    Literal(String),
}

impl PromptToken {
    pub fn to_string(&self) -> String {
        match self {
            &PromptToken::None => String::new(),
            &PromptToken::Mandatory => MANDATORY_PROMPT.to_string(),
            &PromptToken::Optional => OPTIONAL_PROMPT.to_string(),
            &PromptToken::Literal(ref content) => content.to_string(),
        }
    }
}

/// Abstraction of an inline HTML comment.
#[derive(Debug)]
pub struct Comment(pub String);
impl Comment {
    /// Returns the comment in HTML markup form.
    pub fn to_string(&self) -> String {
        format!("<!-- {} -->", self.0)
    }

    /// Returns the inner text of the comment.
    pub fn content(&self) -> String {
        format!("{}", self.0)
    }
}

/// Represents a pairing of template prompt and document content.
#[derive(Clone)]
pub struct ContentMatchPair(pub PromptToken, pub Option<String>);

impl Debug for ContentMatchPair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "<Token: {:?}, String: {:?}, Match: {}>",
            self.0,
            self.1,
            Self::is_match(self)
        )
    }
}

impl ContentMatchPair {
    /// Determines if this instance represents a valid match of prompt and content.
    pub fn is_match(pair: &Self) -> bool {
        match pair {
            &ContentMatchPair(PromptToken::None, Some(_)) => false,
            &ContentMatchPair(PromptToken::Mandatory, None) => false,
            &ContentMatchPair(PromptToken::Literal(_), None) => false,
            &ContentMatchPair(PromptToken::Literal(ref prompt), Some(ref content)) => {
                prompt == content
            }
            _ => true,
        }
    }

    /// Determines if there is an invalid match within a vector of `ContentMatchPair`.
    pub fn contains_mismatch(pairs: &Vec<ContentMatchPair>) -> bool {
        for pair in pairs {
            if !ContentMatchPair::is_match(&pair) {
                return true;
            }
        }

        false
    }
}

pub enum ElementType {
    ContainerBlock,
    LeafBlock,
    InlineContainer,
    InlineLeaf,
}

impl ElementType {
    pub fn determine(node: &Node) -> HowserResult<Self> {
        match node {
            Node::Document(_) | Node::List(_) | Node::BlockQuote(_) | Node::Item(_) => {
                Ok(ElementType::ContainerBlock)
            }
            Node::Paragraph(_)
            | Node::Heading(_)
            | Node::CodeBlock(_)
            | Node::ThematicBreak(_)
            | Node::HtmlBlock(_)
            | Node::CustomBlock(_) => Ok(ElementType::LeafBlock),
            Node::Emph(_) | Node::Strong(_) | Node::Link(_) | Node::Image(_) => {
                Ok(ElementType::InlineContainer)
            }
            Node::Text(_)
            | Node::SoftBreak(_)
            | Node::LineBreak(_)
            | Node::Code(_)
            | Node::HtmlInline(_)
            | Node::CustomInline(_) => Ok(ElementType::InlineLeaf),
        }
    }
}

#[cfg(test)]
mod tests {
    use data::ContentMatchPair;
    use helpers::test::strategies::content;

    proptest!{
        #[test]
        fn test_match_pair(ref pair in content::matches::arb_content_match(1..10)) {
            assert!(ContentMatchPair::is_match(pair));
        }
    }
}
