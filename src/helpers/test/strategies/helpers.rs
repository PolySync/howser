use data::{ContentMatchPair, PromptToken};

pub fn serialize_match_seq(sequence: &Vec<ContentMatchPair>) -> (String, String) {
    let mut template = String::new();
    let mut document = String::new();
    for &ContentMatchPair(ref prompt, ref content) in sequence {
        template.push_str(&prompt.to_string());
        if let &Some(ref content) = content {
            document.push_str(&content.clone());
        }
    }

    (template, document)
}

pub fn serialize_prompt_seq(prompt_seq: &Vec<PromptToken>) -> String {
    let mut result = String::new();
    for prompt in prompt_seq {
        result.push_str(&prompt.to_string());
    }

    result
}
