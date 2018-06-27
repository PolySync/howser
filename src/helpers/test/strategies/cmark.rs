use super::content::matches::arb_content_matches;
use data::{ContentMatchPair, PromptToken};
use doogie::constants::NodeType;
use doogie::{Node, Text};
use proptest::prelude::*;
use std::ops::Range;
use std::rc::Rc;

#[derive(Debug)]
pub enum MatchType {
    Pair(MatchPair),
    Repeatable(RepeatableMatch),
}
#[derive(Debug)]
pub struct MatchPair {
    rx: Node,
    doc: Node,
}
#[derive(Debug)]
pub struct RepeatableMatch {
    rx: Node,
    ditto: Node,
    matches: Vec<Node>,
}

type MandatoryContainerMatchChild = MatchType;
type LiteralContainerMatchChild = MatchType;
type ArbitraryContainerMatchChild = MatchType;

pub fn arb_paragraph_match(elements: Range<usize>) -> BoxedStrategy<Vec<ContentMatchPair>> {
    _arb_paragraph_match(elements)
}
prop_compose!{
    fn _arb_paragraph_match(elements: Range<usize>)
        (
            match_pairs in arb_content_matches(elements, 1..10)
        ) -> Vec<ContentMatchPair>
    {
        match_pairs
    }
}

pub fn valid_document_match() -> BoxedStrategy<(Node, Node)> {
    _valid_document_match()
}
prop_compose!{
    fn _valid_document_match()
        (block_matches in prop::collection::vec(arb_block_match(), 1..4)) -> (Node, Node)
    {
        let mut template = node_of_type(NodeType::CMarkNodeDocument, None);
        let mut document = node_of_type(NodeType::CMarkNodeDocument, None);

        let mut template_blocks = Vec::new();
        let mut doc_blocks = Vec::new();

        for block in block_matches {
            match block {
                MatchType::Pair(pair) => {
                    template_blocks.push(pair.rx);
                    doc_blocks.push(pair.doc);
                },
                MatchType::Repeatable(mut ditto) => {
                    template_blocks.push(ditto.rx);
                    template_blocks.push(ditto.ditto);
                    doc_blocks.append(&mut ditto.matches);
                }
            }
        }

        append_to(&mut template, template_blocks);
        append_to(&mut document, doc_blocks);

        (template, document)
    }
}

pub fn arb_block_match() -> BoxedStrategy<MatchType> {
    prop_oneof![
        arbitrary_container_block_match() //        leaf_block_match()
    ].boxed()
}

pub fn arbitrary_container_block_match() -> BoxedStrategy<MatchType> {
    container_block_match()
        .prop_map(|(_, _, arbitrary)| arbitrary)
        .boxed()
}

pub fn container_block_match() -> BoxedStrategy<(
    MandatoryContainerMatchChild,
    LiteralContainerMatchChild,
    ArbitraryContainerMatchChild,
)> {
    container_match_children()
        .prop_recursive(4, 32, 8, |children| {
            let mwc_container_match = Rc::new(
                (
                    children.clone().prop_map(|(mandatory, _, _)| mandatory),
                    container_block_type(),
                    prop::collection::vec(
                        children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                        0..8,
                    ),
                ).prop_map(|(first_match, node_type, child_matches)| {
                        let rx_container =
                            node_of_type(node_type.clone(), Some(PromptToken::Mandatory));
                        let mut doc_container = node_of_type(node_type, None);

                        match first_match {
                            MatchType::Pair(match_pair) => {
                                append_to(&mut doc_container, vec![match_pair.doc])
                            }
                            MatchType::Repeatable(ditto_match) => {
                                append_to(&mut doc_container, ditto_match.matches)
                            }
                        }
                        for child in child_matches {
                            match child {
                                MatchType::Pair(pair) => append_to(&mut doc_container, vec![pair.doc]),
                                MatchType::Repeatable(ditto) => {
                                    append_to(&mut doc_container, ditto.matches)
                                }
                            }
                        }

                        MatchType::Pair(MatchPair {
                            rx: rx_container,
                            doc: doc_container,
                        })
                    })
                    .boxed(),
            );

            let mblp_container_match = Rc::new(
                (
                    children.clone().prop_map(|(mandatory, _, _)| mandatory),
                    container_block_type(),
                    prop::collection::vec(
                        children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                        0..8,
                    ),
                ).prop_map(|(first_match, node_type, child_matches)| {
                        let mut rx_container =
                            node_of_type(node_type.clone(), Some(PromptToken::Mandatory));
                        let mut doc_container = node_of_type(node_type, None);

                        match first_match {
                            MatchType::Pair(match_pair) => {
                                append_to(&mut rx_container, vec![match_pair.rx]);
                                append_to(&mut doc_container, vec![match_pair.doc]);
                            }
                            MatchType::Repeatable(ditto_match) => {
                                append_to(&mut rx_container, vec![ditto_match.rx, ditto_match.ditto]);
                                append_to(&mut doc_container, ditto_match.matches);
                            }
                        }

                        for child in child_matches {
                            match child {
                                MatchType::Pair(pair) => {
                                    append_to(&mut rx_container, vec![pair.rx]);
                                    append_to(&mut doc_container, vec![pair.doc]);
                                }
                                MatchType::Repeatable(ditto) => {
                                    append_to(&mut rx_container, vec![ditto.rx, ditto.ditto]);
                                    append_to(&mut doc_container, ditto.matches);
                                }
                            }
                        }

                        MatchType::Pair(MatchPair {
                            rx: rx_container,
                            doc: doc_container,
                        })
                    })
                    .boxed(),
            );

            let lit_container_match = Rc::new(
                (
                    children.clone().prop_map(|(_, literal, _)| literal),
                    container_block_type(),
                    prop::collection::vec(
                        children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                        0..8,
                    ),
                ).prop_map(|(first_match, node_type, child_matches)| {
                        let mut rx_container = node_of_type(node_type.clone(), None);
                        let mut doc_container = node_of_type(node_type, None);

                        match first_match {
                            MatchType::Pair(match_pair) => {
                                append_to(&mut rx_container, vec![match_pair.rx]);
                                append_to(&mut doc_container, vec![match_pair.doc]);
                            }
                            MatchType::Repeatable(ditto_match) => {
                                append_to(&mut rx_container, vec![ditto_match.rx, ditto_match.ditto]);
                                append_to(&mut doc_container, ditto_match.matches);
                            }
                        }

                        for child in child_matches {
                            match child {
                                MatchType::Pair(pair) => {
                                    append_to(&mut rx_container, vec![pair.rx]);
                                    append_to(&mut doc_container, vec![pair.doc])
                                }
                                MatchType::Repeatable(ditto) => {
                                    append_to(&mut rx_container, vec![ditto.rx, ditto.ditto]);
                                    append_to(&mut doc_container, ditto.matches);
                                }
                            }
                        }

                        MatchType::Pair(MatchPair {
                            rx: rx_container,
                            doc: doc_container,
                        })
                    })
                    .boxed(),
            );

            let owc_container_match = Rc::new(
                (
                    children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                    container_block_type(),
                    prop::collection::vec(
                        children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                        0..8,
                    ),
                ).prop_map(|(first_child, node_type, child_matches)| {
                        let rx_container =
                            node_of_type(node_type.clone(), Some(PromptToken::Optional));
                        let mut doc_container = node_of_type(node_type, None);

                        match first_child {
                            MatchType::Pair(match_pair) => {
                                append_to(&mut doc_container, vec![match_pair.doc])
                            }
                            MatchType::Repeatable(ditto_match) => {
                                append_to(&mut doc_container, ditto_match.matches)
                            }
                        }

                        for child in child_matches {
                            match child {
                                MatchType::Pair(pair) => append_to(&mut doc_container, vec![pair.doc]),
                                MatchType::Repeatable(ditto) => {
                                    append_to(&mut doc_container, ditto.matches);
                                }
                            }
                        }

                        MatchType::Pair(MatchPair {
                            rx: rx_container,
                            doc: doc_container,
                        })
                    })
                    .boxed(),
            );

            let oblp_container_match = Rc::new(
                (
                    children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                    container_block_type(),
                    prop::collection::vec(
                        children.clone().prop_map(|(_, _, arbitrary)| arbitrary),
                        0..8,
                    ),
                ).prop_map(|(first_match, node_type, child_matches)| {
                        let mut rx_container =
                            node_of_type(node_type.clone(), Some(PromptToken::Optional));
                        let mut doc_container = node_of_type(node_type, None);

                        match first_match {
                            MatchType::Pair(match_pair) => {
                                append_to(&mut rx_container, vec![match_pair.rx]);
                                append_to(&mut doc_container, vec![match_pair.doc]);
                            }
                            MatchType::Repeatable(ditto_match) => {
                                append_to(&mut rx_container, vec![ditto_match.rx, ditto_match.ditto]);
                                append_to(&mut doc_container, ditto_match.matches);
                            }
                        }
                        for child in child_matches {
                            match child {
                                MatchType::Pair(pair) => {
                                    append_to(&mut rx_container, vec![pair.rx]);
                                    append_to(&mut doc_container, vec![pair.doc]);
                                }
                                MatchType::Repeatable(ditto) => {
                                    append_to(&mut rx_container, vec![ditto.rx, ditto.ditto]);
                                    append_to(&mut doc_container, ditto.matches);
                                }
                            }
                        }

                        MatchType::Pair(MatchPair {
                            rx: rx_container,
                            doc: doc_container,
                        })
                    })
                    .boxed(),
            );

            (
                prop_oneof![mwc_container_match.clone(), mblp_container_match.clone()],
                lit_container_match.clone(),
                prop_oneof![
                    owc_container_match,
                    oblp_container_match,
                    mwc_container_match,
                    mblp_container_match,
                    lit_container_match
                ],
            ).boxed()
        })
        .boxed()
}

/// Returns a generator for the different classes of leaf blocks that can be used to construct a document tree.
///
/// The tuple is constructed as so:
/// (Mandatory Wildcard, Optional Wildcard, Mandatory Block Level Prompt, Optional Block Level Prompt, Literal)
pub fn arb_leaf_block_match() -> BoxedStrategy<MatchType> {
    prop_oneof![
        mwc_leaf_match(),
        owc_leaf_match(),
        mblp_leaf_match(),
        lit_leaf_match()
    ].boxed()
}

pub fn mandatory_container_match_child() -> BoxedStrategy<MatchType> {
    prop_oneof![mwc_leaf_match(), mblp_leaf_match(),].boxed()
}

pub fn lit_container_match_child() -> BoxedStrategy<MatchType> {
    prop_oneof![mwc_leaf_match(), mblp_leaf_match(), lit_leaf_match()].boxed()
}

pub fn container_match_children() -> BoxedStrategy<(MatchType, MatchType, MatchType)> {
    (
        mandatory_container_match_child(),
        lit_container_match_child(),
        arb_leaf_block_match(),
    ).boxed()
}

pub fn mwc_leaf_match() -> BoxedStrategy<MatchType> {
    let inlines_gen = prop::collection::vec(arb_inline(), 1..8);
    let match_pair_gen = leaf_block_type().prop_map(|node_type| {
        (
            node_of_type(node_type.clone(), Some(PromptToken::Mandatory)),
            node_of_type(node_type, None),
        )
    });

    (inlines_gen, match_pair_gen)
        .prop_map(|values| {
            let (inlines, (rx_node, mut doc_node)) = values;
            append_to(&mut doc_node, inlines);

            MatchType::Pair(MatchPair {
                rx: rx_node,
                doc: doc_node,
            })
        })
        .boxed()
}

pub fn owc_leaf_match() -> BoxedStrategy<MatchType> {
    unimplemented!()
}

pub fn mblp_leaf_match() -> BoxedStrategy<MatchType> {
    unimplemented!()
}

pub fn oblp_leaf_match() -> BoxedStrategy<MatchType> {
    unimplemented!()
}

pub fn lit_leaf_match() -> BoxedStrategy<MatchType> {
    unimplemented!()
}

pub fn arb_inline() -> BoxedStrategy<Node> {
    unimplemented!()
}

pub fn leaf_block_type() -> BoxedStrategy<NodeType> {
    prop_oneof![
        Just(NodeType::CMarkNodeHtmlBlock),
        Just(NodeType::CMarkNodeCodeBlock),
        Just(NodeType::CMarkNodeHeading).boxed(),
        Just(NodeType::CMarkNodeParagraph).boxed()
    ].boxed()
}

pub fn container_block_type() -> BoxedStrategy<NodeType> {
    prop_oneof![
        Just(NodeType::CMarkNodeBlockQuote),
        Just(NodeType::CMarkNodeItem)
    ].boxed()
}

fn node_of_type(node_type: NodeType, annotation: Option<PromptToken>) -> Node {
    let mut node = Node::from_type(node_type).unwrap();

    if let Some(token) = annotation {
        match node {
            Node::Heading(_) | Node::Paragraph(_) => {
                append_to(&mut node, vec![text_node(&token.to_string())])
            },
            _ => unimplemented!(),
        }
    }

    node
}

pub fn text_node(content: &String) -> Node {
    let mut node = Text::new();
    node.set_content(content).unwrap();
    Node::Text(node)
}

fn append_to(parent: &mut Node, children: Vec<Node>) {
    for mut child in children {
        parent.append_child(&mut child).unwrap();
    }
}
