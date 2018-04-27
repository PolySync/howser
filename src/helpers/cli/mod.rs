pub enum ShellText {
    Literal(String),
    WarningColor(Box<ShellText>),
    ErrorColor(Box<ShellText>),
    CodeColor(Box<ShellText>),
    MessageColor(Box<ShellText>),
    LineNumColor(Box<ShellText>),
    OkColor(Box<ShellText>),
    Underlined(Box<ShellText>),
    Dim(Box<ShellText>),
}

impl ShellText {
    pub fn to_string(&self) -> String {
        match self {
            &ShellText::Literal(ref text) => text.clone(),
            &ShellText::WarningColor(ref shell_text) => {
                ShellText::formatted("\x1b[33m", shell_text)
            }
            &ShellText::ErrorColor(ref shell_text) => ShellText::formatted("\x1b[31m", shell_text),
            &ShellText::CodeColor(ref shell_text) => ShellText::formatted("\x1b[94m", shell_text),
            &ShellText::MessageColor(ref shell_text) => {
                ShellText::formatted("\x1b[93m", shell_text)
            }
            &ShellText::LineNumColor(ref shell_text) => {
                ShellText::formatted("\x1b[90m", shell_text)
            }
            &ShellText::OkColor(ref shell_text) => ShellText::formatted("\x1b[32m", shell_text),
            &ShellText::Underlined(ref shell_text) => ShellText::formatted("\x1b[4m", shell_text),
            &ShellText::Dim(ref shell_text) => ShellText::formatted("\x1b[2m", shell_text),
        }
    }

    fn formatted(color_code: &str, inner_text: &Box<ShellText>) -> String {
        color_code.to_owned() + &inner_text.to_string() + "\x1b[0m"
    }
}

pub fn as_code_lines(code: &str, start_line: u32) -> Vec<String> {
    code.lines()
        .map(|line| {
            ShellText::CodeColor(Box::new(ShellText::Literal(line.to_string()))).to_string()
        })
        .enumerate()
        .map(|(i, line)| {
            let line_num = ShellText::Dim(Box::new(ShellText::Literal(
                format!("{}    ", i as u32 + start_line)[..4].to_string(),
            )));
            format!("{} {}", line_num.to_string(), line)
        })
        .collect()
}

pub fn indented_lines(lines: &Vec<String>, indent: usize) -> Vec<String> {
    let leading_spaces = vec![" "; indent].join("");
    lines
        .iter()
        .map(|line| format!("{}{}", leading_spaces, line))
        .collect()
}
