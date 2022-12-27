use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(pub String);

pub enum Expr {
    List(Vec<Expr>),
    Atom(Symbol),
    Num(u64),
}