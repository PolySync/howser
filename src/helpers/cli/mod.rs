extern crate termion;

use self::termion::color;
use self::termion::style;

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

pub fn as_code_lines(code: &str, start_line: usize) -> Vec<String> {
    code.lines()
        .map(|line| {
            format!(
                "{}{}{}",
                color::Fg(color::LightBlue),
                line,
                color::Fg(color::Reset)
            )
        })
        .enumerate()
        .map(|(i, line)| {
            let mut line_num = format!("{}{}    ", style::Faint, i + start_line);
            line_num.truncate(6);
            line_num += &style::Reset.to_string();
            format!("{} {}", line_num, line)
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
