use proptest::prelude::*;
use std::ops::Range;
use data::Comment;

/// Some arbitrary CMark-compatible textual content.
pub fn arb_content(words: Range<usize>) -> BoxedStrategy<String> {
    prop::collection::vec("[[:alnum:]]{1,45}", words)
        .prop_map(|v| v.join(" "))
        .boxed()
}

pub fn arb_comment(words: Range<usize>) -> BoxedStrategy<Comment> {
    _arb_comment(words)
}
prop_compose!{
    fn _arb_comment(words: Range<usize>)
        (content in arb_content(words)) -> Comment
    {
        Comment(content)
    }
}

pub mod prompts {
    use proptest::prelude::*;
    use super::arb_content;
    use std::ops::Range;
    use data::PromptToken;

    const PROMPTS: &'static [PromptToken] = &[PromptToken::Mandatory, PromptToken::Optional];

    /// Returns a vector of PromptToken that represents an arbitrary valid prompt sequence.
    pub fn prompt_tokens(segments: Range<usize>) -> BoxedStrategy<Vec<PromptToken>> {
        _prompt_tokens(segments)
    }
    prop_compose!{
        fn _prompt_tokens(segments: Range<usize>)
            (
                head in prop::option::of(content_prompt()),
                segments in prop::collection::vec(prompt_segment(), segments),
                tail in prop::option::of(literal_prompt(1..10))
            ) -> Vec<PromptToken>
        {
            let mut sequence = Vec::new();
            if let Some(head) = head {
                sequence.push(head);
            }
            for segment in segments {
                sequence.append(&mut segment.clone());
            }
            if let Some(tail) = tail {
                sequence.push(tail)
            }

            sequence
        }
    }

    /// Returns a single mandatory or optional PromptToken
    pub fn content_prompt() -> BoxedStrategy<PromptToken> {
        _content_prompt()
    }
    prop_compose!{
        fn _content_prompt()
            (prompt in prop::sample::select(PROMPTS)) -> PromptToken
        {
            prompt
        }
    }

    pub fn block_level_prompt() -> BoxedStrategy<PromptToken> {
        prop_oneof![mandatory_prompt(), optional_prompt()].boxed()
    }

    pub fn mandatory_prompt() -> BoxedStrategy<PromptToken> {
        Just(PromptToken::Mandatory).boxed()
    }

    pub fn optional_prompt() -> BoxedStrategy<PromptToken> {
        Just(PromptToken::Optional).boxed()
    }

    /// Returns a literal PromptToken
    pub fn literal_prompt(word_count: Range<usize>) -> BoxedStrategy<PromptToken> {
        arb_content(word_count)
            .prop_map(PromptToken::Literal)
            .boxed()
    }

    // Todo -- refactor
    /// Returns a content prompt optionally followed by some literal content.
    ///
    /// Used to compose valid prompt sequences.
    prop_compose!{
        fn prompt_segment()
            (
                literal in prop::option::of(literal_prompt(1..10)),
                prompt in content_prompt()
            ) -> Vec<PromptToken>
        {
            let mut segment = Vec::new();
            if let Some(literal) = literal {
                segment.push(literal);
            }
            segment.push(prompt);
            segment
        }
    }
}

pub mod matches {
    use proptest::prelude::*;
    use super::arb_content;
    use data::{ContentMatchPair, PromptToken};
    use std::ops::Range;

    /// Returns a vector of arbitrary ContentMatchPair.
    pub fn arb_content_matches(
        element_count: Range<usize>,
        word_count: Range<usize>,
    ) -> BoxedStrategy<Vec<ContentMatchPair>> {
        _arb_content_matches(element_count, word_count)
    }
    prop_compose!{
        fn _arb_content_matches(element_count: Range<usize>, word_count: Range<usize>)
            (
                matches in prop::collection::vec(arb_content_match(word_count),
                element_count)
            ) -> Vec<ContentMatchPair>
        {
            matches
        }
    }

    /// Returns a single, valid, arbitrary ContentMatchPair.
    pub fn arb_content_match(word_count: Range<usize>) -> BoxedStrategy<ContentMatchPair> {
        prop_oneof![
            mandatory_prompt_match(word_count.clone()),
            optional_prompt_match(word_count.clone()),
            literal_prompt_match(word_count)
        ].boxed()
    }

    /// Returns a ContentMatchPair of some arbitrary mandatory content.
    pub fn mandatory_prompt_match(word_count: Range<usize>) -> BoxedStrategy<ContentMatchPair> {
        _mandatory_prompt_match(word_count)
    }
    prop_compose!{
        fn _mandatory_prompt_match(word_count: Range<usize>)
            (content in arb_content(word_count)) -> ContentMatchPair
        {
            ContentMatchPair(PromptToken::Mandatory, Some(content))
        }
    }

    /// Returns a ContentMatchPair of some arbitrary optional content.
    pub fn optional_prompt_match(word_count: Range<usize>) -> BoxedStrategy<ContentMatchPair> {
        _optional_prompt_match(word_count)
    }
    prop_compose!{
        fn _optional_prompt_match(word_count: Range<usize>)
            (content in prop::option::of(arb_content(word_count))) -> ContentMatchPair
        {
            ContentMatchPair(PromptToken::Optional, content)
        }
    }

    /// Returns a ContentMatchPair for some arbitrary literal content.
    pub fn literal_prompt_match(word_count: Range<usize>) -> BoxedStrategy<ContentMatchPair> {
        _literal_content_match(word_count)
    }
    prop_compose!{
        fn _literal_content_match(word_count: Range<usize>)
            (content in arb_content(word_count)) -> ContentMatchPair
        {
            ContentMatchPair(PromptToken::Literal(content.clone()), Some(content))
        }
    }
}

pub mod mismatches {
    use proptest::prelude::*;
    use super::arb_content;

    pub fn mismatch_pair() -> BoxedStrategy<(Vec<String>, String)> {
        _mismatch_pair()
    }
    prop_compose!{
        fn _mismatch_pair()
            (pair in mismatch_pattern_1().prop_union(mismatch_pattern_2())) -> (Vec<String>, String)
        {
            pair
        }
    }

    prop_compose!{
        fn arb_literal_prompts_and_content()
            (
                literal_prompts in prop::collection::vec(arb_content(1..10), 1..10),
                content in arb_content(10..100)
            ) -> (Vec<String>, String)
        {
            (literal_prompts, content)
        }
    }

    prop_compose!{
        fn literal_prompts_mismatch()
            (
                mismatch_set in arb_literal_prompts_and_content()
                    .prop_filter(
                        "Content contained all prompts".to_owned(),
                        |&(ref prompts, ref content)|
                        {
                            for prompt in prompts {
                                if ! content.contains(prompt) {
                                    return true;
                                }
                            }
                            false
                        }
                    )
            ) -> (Vec<String>, String)
        {
            mismatch_set
        }
    }

    prop_compose!{
        fn mismatch_pattern_1()
            (
                tail in prop::option::of(Just("-!!-".to_string())),
                set in literal_prompts_mismatch()
                    .prop_map(|(mut literal_prompts, content)| {
                        let mut prompts = Vec::new();
                        if let Some(prompt) = literal_prompts.pop() {
                            prompts.push(prompt);
                        }
                        for prompt in literal_prompts {
                            prompts.push("-!!-".to_string());
                            prompts.push(prompt);
                        }
                        (prompts, content)
                    })
            ) -> (Vec<String>, String)
        {
            let (mut prompts, content) = set;
            if let Some(tail) = tail {
                prompts.push(tail);
            }
            (prompts, content)
        }
    }

    prop_compose!{
        fn mismatch_pattern_2()
            (tail in prop::bool::ANY, set in literal_prompts_mismatch()) -> (Vec<String>, String)
        {
            let (mut literal_prompts, content) = set;
            let mut prompts = Vec::new();
            let last_prompt = match tail {
                true => literal_prompts.pop(),
                false => None
            };

            prompts.push("-!!-".to_string());
            for prompt in literal_prompts {
                prompts.push(prompt);
                prompts.push("-!!-".to_string());
            }
            if let Some(prompt) = last_prompt {
                prompts.push(prompt);
            }

            (prompts, content)
        }
    }
}
