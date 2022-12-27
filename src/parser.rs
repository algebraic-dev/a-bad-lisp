use std::rc::Rc;

use crate::expr::{Expr, Symbol};

pub fn parse(code: &str) -> Result<Vec<Expr>, String> {
    let mut stack = vec![];
    let mut mark = vec![];

    let mut chars = code.chars().peekable();

    while let Some(char) = chars.next() {
        match char {
            ' ' | '\n' | '\r' => (),
            '(' => mark.push(stack.len()),
            ')' => {
                if let Some(start) = mark.pop() {
                    let args = stack.split_off(start);
                    stack.push(Expr::List(args));
                } else {
                    return Err("unmatched close parenthesis".to_string());
                }
            }
            '0'..='9' => {
                let mut num = char.to_digit(10).unwrap() as u64;
                while chars.peek().is_some() && chars.peek().unwrap().is_ascii_digit() {
                    num *= 10;
                    num += chars.peek().unwrap().to_digit(10).unwrap() as u64;
                    chars.next();
                }
                stack.push(Expr::Num(num))
            }
            char => {
                let mut str = String::new();
                str.push(char);
                while chars.peek().is_some()
                    && !matches!(
                        chars.peek().unwrap(),
                        '(' | ')' | '0'..='9' | ' ' | '\n' | '\r'
                    )
                {
                    str.push(*chars.peek().unwrap());
                    chars.next();
                }
                stack.push(Expr::Atom(Symbol(str)))
            }
        }
    }

    if !mark.is_empty() {
        Err("unclosed parenthesis".to_string())
    } else {
        Ok(stack)
    }
}