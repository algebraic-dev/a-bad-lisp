use std::fmt::Display;

#[derive(Debug)]
pub enum Expr<'a> {
    List(Vec<Expr<'a>>),
    Atom(&'a str),
    String(&'a str),
    Symbol(&'a str),
    Num(u64),
}

impl<'a> Expr<'a> {
    pub fn assert_size(&self, size: usize) -> Option<&Vec<Expr<'a>>> {
        if let Expr::List(list) = self {
            if list.len() == size {
                return Some(list);
            }
        }
        None
    }

    pub fn at_least(&self, size: usize) -> Option<&Vec<Expr<'a>>> {
        if let Expr::List(list) = self {
            if list.len() >= size {
                return Some(list);
            }
        }
        None
    }

    pub fn same_identifier(&self, other: &str) -> bool {
        if let Expr::Atom(atom) = self {
            return atom == &other;
        }
        false
    }

    pub fn as_atom(&self) -> Option<&str> {
        if let Expr::Atom(atom) = self {
            return Some(atom);
        }
        None
    }

    pub fn is_nil(&self) -> bool {
        if let Expr::Atom(atom) = self {
            return atom == &"nil";
        }
        false
    }
}

impl<'a> Expr<'a> {
    fn width(&self) -> usize {
        match self {
            Expr::List(list) => list.iter().map(|expr| expr.width()).sum(),
            Expr::Atom(atom) => atom.len(),
            Expr::String(string) => string.len(),
            Expr::Num(num) => num.to_string().len(),
            Expr::Symbol(symbol) => symbol.len(),
        }
    }

    fn is_quote(&self) -> bool {
        match self {
            Expr::Atom(atom) => *atom == "quote",
            _ => false,
        }
    }

    fn pretty_print(&self, fmt: &mut impl std::fmt::Write, indent: usize) -> std::fmt::Result {
        write!(fmt, "{}", " ".repeat(indent))?;
        match self {
            Expr::List(list) if !list.is_empty() && list[0].is_quote() => {
                write!(fmt, "'")?;
                list[1].pretty_print(fmt, 0)
            }
            Expr::List(list) => {
                write!(fmt, "(")?;
                if !list.is_empty() {
                    let max = self.width() + indent > 40;
                    list[0].pretty_print(fmt, 0)?;
                    for expr in &list[1..] {
                        if max {
                            fmt.write_str("\n")?;
                        } else {
                            write!(fmt, " ")?;
                        }
                        expr.pretty_print(fmt, if max { indent + 1 } else { 0 })?;
                    }
                }
                fmt.write_str(")")
            }
            Expr::Atom(atom) => {
                fmt.write_str(":")?;
                fmt.write_str(atom)
            }
            Expr::String(string) => fmt.write_str(string),
            Expr::Num(num) => fmt.write_str(&num.to_string()),
            Expr::Symbol(symbol) => fmt.write_str(symbol),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.pretty_print(fmt, 0)
    }
}
