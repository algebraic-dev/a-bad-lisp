//! A simple parser for the bad lisp language. This is a simple probably bottom up parser.

use std::{iter::Peekable, str::Chars};

use crate::expr::Expr;

/// The state of the parser.
pub struct State<'a> {
    pub stack: Vec<Expr<'a>>,
    pub mark: Vec<usize>,
    pub chars: Peekable<Chars<'a>>,
    pub source: &'a str,
    pub place: usize,
}

impl<'a> State<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            stack: Vec::new(),
            mark: Vec::new(),
            chars: code.chars().peekable(),
            source: code,
            place: 0,
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let char = self.chars.next()?;
        self.place += char.len_utf8();
        Some(char)
    }

    pub fn mark(&mut self) {
        self.mark.push(self.stack.len());
    }

    pub fn parse_rpar(&mut self) -> Result<(), String> {
        if let Some(start) = self.mark.pop() {
            let args = self.stack.split_off(start);
            self.stack.push(Expr::List(args));
            Ok(())
        } else {
            Err("unmatched close parenthesis".to_string())
        }
    }

    pub fn parse_lpar(&mut self) {
        self.mark.push(self.stack.len());
    }

    pub fn parse_num(&mut self, chr: char) {
        let mut num = chr.to_digit(10).unwrap() as u64;
        while self.chars.peek().is_some() && self.chars.peek().unwrap().is_ascii_digit() {
            num *= 10;
            num += self.chars.peek().unwrap().to_digit(10).unwrap() as u64;
            self.next_char();
        }
        self.stack.push(Expr::Num(num))
    }

    pub fn parse_id(&mut self, char: char) {
        let start = self.place - char.len_utf8();

        while self.chars.peek().is_some()
            && !matches!(
                self.chars.peek().unwrap(),
                '(' | ')' | '0'..='9' | ' ' | '\n' | '\r'
            )
        {
            self.next_char();
        }

        let str = &self.source[start..self.place];

        if str.starts_with(':') {
            self.stack.push(Expr::Atom(str.strip_prefix(':').unwrap()))
        } else {
            self.stack.push(Expr::Symbol(str))
        }
    }

    pub fn parse_str(&mut self) -> Result<(), String> {
        let start = self.place;
        while let Some(char) = self.next_char() {
            if char == '"' {
                let str = &self.source[start..self.place - 1];
                self.stack.push(Expr::String(str));
                return Ok(());
            }
        }
        Err("unclosed string".to_string())
    }

    pub fn parse(&mut self) -> Result<Vec<Expr<'a>>, String> {
        while let Some(char) = self.next_char() {
            match char {
                ' ' | '\n' | '\r' => {}
                '(' => self.parse_lpar(),
                ')' => self.parse_rpar()?,
                '"' => self.parse_str()?,
                '0'..='9' => self.parse_num(char),
                _ => self.parse_id(char),
            }
        }

        if !self.mark.is_empty() {
            Err("unclosed parenthesis".to_string())
        } else {
            Ok(std::mem::take(&mut self.stack))
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<Expr>, String> {
    let mut state = State::new(input);
    state.parse()
}
