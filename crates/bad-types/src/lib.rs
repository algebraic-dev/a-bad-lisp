use fxhash::FxHashMap;
use std::{cell::RefCell, fmt::Display, rc::Rc};

pub fn assert_size(args: &[Value], size: usize) -> Result<(), String> {
    if args.len() != size {
        return Err(format!(
            "Expected {} args but instead got {}.",
            size,
            args.len()
        ));
    }

    Ok(())
}

pub fn assert_string(arg: Value) -> Result<String, String> {
    match arg.0.as_ref() {
        ValueKind::String(s) => Ok(s.clone()),
        _ => Err("Expected string but instead".to_string()),
    }
}

pub fn assert_number(arg: Value) -> Result<u64, String> {
    match arg.0.as_ref() {
        ValueKind::Number(n) => Ok(*n),
        _ => Err("Expected number but instead".to_string()),
    }
}

pub fn assert_list(arg: Value) -> Result<Vec<Value>, String> {
    match arg.0.as_ref() {
        ValueKind::List(ls) => Ok(ls.clone()),
        _ => Err("Expected list but instead".to_string()),
    }
}

pub fn assert_identifier(arg: Value) -> Result<Symbol, String> {
    match arg.0.as_ref() {
        ValueKind::Symbol(s) => Ok(s.clone()),
        _ => Err("Expected identifier but instead".to_string()),
    }
}

pub fn assert_atom(arg: Value) -> Result<Symbol, String> {
    match arg.0.as_ref() {
        ValueKind::Atom(s) => Ok(s.clone()),
        _ => Err("Expected atom but instead".to_string()),
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub String);

#[derive(Clone)]
pub struct Value(pub Rc<ValueKind>);

pub struct Fun {
    pub params: Vec<Symbol>,
    pub body: Value,
}

pub struct Closure {
    pub env: Env,
    pub fun: Fun,
}

type Prim = fn(&mut Env, &[Value]) -> Result<Value, String>;

pub enum ValueKind {
    Number(u64),
    String(String),
    Symbol(Symbol),
    Atom(Symbol),
    List(Vec<Value>),
    Closure(Closure),
    Primitive(Prim),
}

impl Value {
    pub fn number(num: u64) -> Self {
        Value(Rc::new(ValueKind::Number(num)))
    }

    pub fn string(string: &str) -> Self {
        Value(Rc::new(ValueKind::String(string.to_string())))
    }

    pub fn symbol(symbol: &str) -> Self {
        Value(Rc::new(ValueKind::Symbol(Symbol(symbol.to_string()))))
    }

    pub fn atom(symbol: &str) -> Self {
        Value(Rc::new(ValueKind::Atom(Symbol(symbol.to_string()))))
    }

    pub fn list(list: Vec<Value>) -> Self {
        Value(Rc::new(ValueKind::List(list)))
    }

    pub fn closure(env: Env, fun: Fun) -> Self {
        Value(Rc::new(ValueKind::Closure(Closure { env, fun })))
    }

    pub fn primitive(prim: Prim) -> Self {
        Value(Rc::new(ValueKind::Primitive(prim)))
    }

    pub fn boolean(b: bool) -> Self {
        if b {
            Value::symbol("true")
        } else {
            Value::symbol("false")
        }
    }
}

impl ValueKind {
    fn width(&self) -> usize {
        match self {
            ValueKind::List(list) => list.iter().map(|x| x.0.width()).sum(),
            ValueKind::String(string) => string.len(),
            ValueKind::Number(num) => num.to_string().len(),
            ValueKind::Symbol(symbol) => symbol.0.len(),
            ValueKind::Atom(symbol) => symbol.0.len(),
            ValueKind::Closure(_) => 0,
            ValueKind::Primitive(_) => 0,
        }
    }

    fn pretty_print(&self, fmt: &mut impl std::fmt::Write, indent: usize) -> std::fmt::Result {
        write!(fmt, "{}", " ".repeat(indent))?;
        match self {
            ValueKind::List(list) => {
                write!(fmt, "(")?;
                if !list.is_empty() {
                    let max = self.width() + indent > 40;
                    list[0].0.pretty_print(fmt, 0)?;
                    for expr in &list[1..] {
                        if max {
                            fmt.write_str("\n")?;
                        } else {
                            write!(fmt, " ")?;
                        }
                        expr.0.pretty_print(fmt, if max { indent + 1 } else { 0 })?;
                    }
                }
                fmt.write_str(")")
            }
            ValueKind::String(string) => write!(fmt, "\"{}\"", string),
            ValueKind::Number(num) => write!(fmt, "{}", num),
            ValueKind::Symbol(symbol) => write!(fmt, "{}", symbol.0),
            ValueKind::Atom(symbol) => write!(fmt, ":{}", symbol.0),
            ValueKind::Closure(_) => write!(fmt, "<closure>"),
            ValueKind::Primitive(_) => write!(fmt, "<primitive>"),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.pretty_print(fmt, 0)
    }
}

#[derive(Clone, Default)]
pub struct Frame {
    vars: im::HashMap<Symbol, Value, fxhash::FxBuildHasher>,
}

#[derive(Clone)]
pub struct Env {
    pub frames: im::Vector<Frame>,
    pub place: usize,
    pub global: FxHashMap<String, Value>,
    pub macro_expansion: bool,
    pub expanded: Rc<RefCell<bool>>,
}

impl Env {
    pub fn set(&mut self, name: &Symbol, value: Value) {
        self.global.insert(name.0.clone(), value);
    }
    pub fn new_scope(&mut self) -> &mut Frame {
        self.frames[self.place] = self.frames.last().cloned().unwrap_or_default();
        self.place += 1;
        &mut self.frames[self.place - 1]
    }

    pub fn pop_scope(&mut self) {
        self.place -= 1;
    }

    pub fn front(&mut self) -> Option<&mut Frame> {
        self.frames.get_mut(self.place - 1)
    }

    pub fn push_local(&mut self, name: Symbol, value: Value) {
        match self.front() {
            Some(frame) => {
                frame.vars.insert(name, value);
            }
            None => {
                let frame = self.new_scope();
                frame.vars.insert(Symbol(name.0), value);
            }
        }
    }

    pub fn get(&mut self, name: &Symbol) -> Option<Value> {
        self.front()
            .and_then(|f| f.vars.get(name).cloned())
            .or_else(|| self.global.get(&name.0).cloned())
    }
}
