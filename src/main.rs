use std::{
    collections::{LinkedList, VecDeque},
    fmt::{Debug, Display},
    rc::Rc,
    sync::{Arc, RwLock},
};

use fxhash::FxHashMap;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(String);

#[derive(Clone, Default)]
pub struct Frame {
    vars: FxHashMap<Symbol, Def>,
}

#[derive(Clone, Debug)]
pub struct Def {
    is_macro: bool,
    value: Rc<Value>,
}

impl Def {
    fn new(value: Rc<Value>) -> Def {
        Def {
            is_macro: false,
            value,
        }
    }

    fn imp(value: Rc<Value>) -> Def {
        Def {
            is_macro: true,
            value,
        }
    }
}

#[derive(Clone)]
pub struct Env {
    frames: LinkedList<Frame>,
    global: Arc<RwLock<FxHashMap<String, Def>>>,
}

impl Env {
    fn new_scope(&mut self) {
        self.frames
            .push_back(self.frames.front().cloned().unwrap_or_default());
    }

    pub fn pop_scope(&mut self) {
        self.frames.pop_back();
    }

    fn new_empty_call_frame(&mut self) {
        self.frames.push_back(Default::default());
    }

    fn push_local(&mut self, name: Symbol, value: Rc<Value>, is_macro: bool) {
        match self.frames.front_mut() {
            Some(frame) => {
                frame.vars.insert(name, Def { is_macro, value });
            }
            None => {
                let mut head = self.global.write().unwrap();
                head.insert(name.0, Def { is_macro, value });
            }
        }
    }

    fn find_var(&mut self, name: &Symbol) -> Option<Def> {
        self.frames
            .front_mut()
            .and_then(|f| f.vars.get(name).cloned())
            .or_else(|| self.global.read().unwrap().get(&name.0).cloned())
    }
}

struct Fun {
    params: Vec<Symbol>,
    body: Rc<Value>,
}

struct Closure {
    env: Env,
    fun: Fun,
}

enum Value {
    List(Vec<Rc<Value>>),

    Atom(Symbol),
    Symbol(Symbol),

    Num(u64),

    Closure(Closure),
    Prim(fn(&mut Env, &[Rc<Value>]) -> Result<Rc<Value>, String>),
}

fn eval_fun(env: &mut Env, fun: &Fun, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != fun.params.len() {
        return Err(format!(
            "Expected {} args but instead got {}.",
            fun.params.len(),
            args.len()
        ));
    }

    for (param, arg) in fun.params.iter().zip(args) {
        env.push_local(param.clone(), arg.clone(), false)
    }

    let ret = eval(env, fun.body.clone())?;

    Ok(ret)
}

fn eval_list(env: &mut Env, args: &[Rc<Value>]) -> Result<Vec<Rc<Value>>, String> {
    args.iter()
        .map(|x| eval(env, x.clone()))
        .collect::<Result<Vec<_>, String>>()
}

fn eval(env: &mut Env, body: Rc<Value>) -> Result<Rc<Value>, String> {
    match &*body {
        Value::List(ls) => {
            if ls.is_empty() {
                return Ok(body);
            }

            match &*eval(env, ls[0].clone())? {
                Value::Closure(closure) => {
                    let args = eval_list(env, &ls[1..])?;
                    let mut new_env = closure.env.clone();
                    new_env.new_scope();
                    let ret = eval_fun(&mut new_env, &closure.fun, &args)?;
                    new_env.pop_scope();
                    Ok(ret)
                }
                Value::Prim(prim) => prim(env, &ls[1..]),
                _ => Err(format!("cannot apply as function '{}'", body)),
            }
        }
        Value::Symbol(n) => {
            Ok(Rc::new(Value::Atom(n.clone())))
        }
        Value::Atom(s) => match env.find_var(s) {
            Some(rs) => Ok(rs.value.clone()),
            None => Err(format!("cannot find variable '{}'", s.0)),
        },
        Value::Num(n) => Ok(Rc::new(Value::Num(*n))),
        _ => Ok(body.clone()),
    }
}

fn macro_expand(expanded: &mut bool, env: &mut Env, body: Rc<Value>) -> Result<Rc<Value>, String> {
    match &*body {
        Value::List(ls) => {
            if !ls.is_empty() {
                match &*macro_expand(expanded, env, ls[0].clone())? {
                    Value::Closure(closure) => {
                        let mut new_env = closure.env.clone();
                        new_env.new_scope();
                        let ret = eval_fun(&mut new_env, &closure.fun, &ls[1..])?;
                        new_env.pop_scope();
                        *expanded = true;
                        Ok(ret)
                    }
                    Value::Prim(prim) => {
                        *expanded = true;
                        prim(env, &ls[1..])
                    }
                    _ => {
                        let args = ls
                            .iter()
                            .map(|x| macro_expand(expanded, env, x.clone()))
                            .collect::<Result<Vec<_>, String>>()?;
                        Ok(Rc::new(Value::List(args)))
                    }
                }
            } else {
                Ok(body.clone())
            }
        }
        Value::Atom(s) => match env.find_var(s) {
            Some(def) if def.is_macro => {
                macro_expand(expanded, env, def.value.clone())
            }
            _ => Ok(body.clone()),
        },
        _ => Ok(body.clone()),
    }
}

// Parser

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(ls) => {
                if ls.len() > 0 {
                    write!(f, "(")?;
                    write!(f, "{}", ls[0])?;
                    for arg in &ls[1..] {
                        write!(f, " {}", arg)?;
                    }
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
            Value::Atom(s) => write!(f, "{}", s.0),
            Value::Num(n) => write!(f, "{}", n),
            Value::Closure(_) => write!(f, "#(closure)"),
            Value::Prim(_) => write!(f, "#(prim)"),
            Value::Symbol(m) => write!(f, "'{}", m.0),
        }
    }
}

fn parse(code: &str) -> Result<Vec<Rc<Value>>, String> {
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
                    stack.push(Rc::new(Value::List(args)));
                } else {
                    return Err("unmatched close parenthesis".to_string());
                }
            }
            '0'..='9' => {
                let mut num = char.to_digit(10).unwrap() as u64;
                while chars.peek().is_some() && chars.peek().unwrap().is_digit(10) {
                    num *= 10;
                    num += chars.peek().unwrap().to_digit(10).unwrap() as u64;
                    chars.next();
                }
                stack.push(Rc::new(Value::Num(num)))
            }
            ':' => {
                let mut str = String::new();
                while chars.peek().is_some()
                    && !matches!(
                        chars.peek().unwrap(),
                        '(' | ')' | '0'..='9' | ' ' | '\n' | '\r'
                    )
                {
                    str.push(*chars.peek().unwrap());
                    chars.next();
                }
                stack.push(Rc::new(Value::Symbol(Symbol(str))))
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
                stack.push(Rc::new(Value::Atom(Symbol(str))))
            }
        }
    }

    if !mark.is_empty() {
        println!("unclosed parenthesis")
    }

    Ok(stack)
}

fn empty_context() -> FxHashMap<String, Def> {
    fn lambda(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 2 {
            return Err(format!("Expected 2 args but instead got {}!", args.len()));
        }

        let mut params = vec![];

        if let Value::List(names) = &*args[0] {
            for name in names {
                if let Value::Atom(atom) = &**name {
                    params.push(atom.clone());
                } else {
                    return Err("expected an identifier".to_string());
                }
            }

            let fun = Fun {
                params,
                body: args[1].clone(),
            };

            return Ok(Rc::new(Value::Closure(Closure {
                env: env.clone(),
                fun,
            })));
        } else {
            return Err("expected an identifier".to_string());
        }
    }

    fn set_ctx(env: &mut Env, args: &[Rc<Value>], is_macro: bool) -> Result<Rc<Value>, String> {
        if args.len() != 2 {
            return Err(format!("Expected 2 args but instead got {} ", args.len()));
        }

        let value = eval(env, args[1].clone())?;

        match &*args[0] {
            Value::Atom(name) => {
                env.global
                    .write()
                    .unwrap()
                    .insert(name.0.clone(), Def { value, is_macro });

                Ok(Rc::new(Value::List(vec![])))
            }
            _ => Err("expected an identifier".to_string()),
        }
    }

    fn let_decl(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 3 {
            return Err(format!("Expected 3 args but instead got {} ", args.len()));
        }

        let value = eval(env, args[1].clone())?;

        match &*args[0] {
            Value::Atom(name) => {
                env.push_local(name.clone(), value, false);
                eval(env, args[2].clone())
            }
            _ => Err("expected an identifier".to_string()),
        }
    }

    fn print(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        println!("{}", eval(env, args[0].clone())?);

        Ok(Rc::new(Value::List(vec![])))
    }

    fn add(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        let args = eval_list(env, args)?;

        let mut result = 0;

        for arg in args {
            match &*arg {
                Value::Num(n) => {
                    result += n;
                }
                _ => return Err(format!("expected a number '{}'", arg)),
            }
        }

        Ok(Rc::new(Value::Num(result)))
    }

    fn list(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        let args = eval_list(env, args)?;
        Ok(Rc::new(Value::List(args)))
    }

    fn evaluate(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        let res = eval(env, args[0].clone())?;
        eval(env, res)
    }

    fn quote(_: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        fn quoted(arg: Rc<Value>) -> Rc<Value> {
            match &*arg {
                Value::List(ls) => Rc::new(Value::List(ls.iter().cloned().map(quoted).collect())),
                Value::Atom(n) => Rc::new(Value::Symbol(n.clone())),
                _ => arg.clone()
            }
        }

        Ok(quoted(args[0].clone()))
    }

    fn is_nil_sttm(val: &Value) -> bool {
        match val {
            Value::List(n) => n.is_empty(),
            _ => false,
        }
    }

    fn if_(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 3 {
            return Err(format!("Expected 3 arg but instead got {} ", args.len()));
        }

        let cond = eval(env, args[0].clone())?;

        if !is_nil_sttm(&cond) {
            eval(env, args[1].clone())
        } else {
            eval(env, args[2].clone())
        }
    }

    fn cmp(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::List(n), Value::List(m)) => n.iter().zip(m.iter()).all(|f| cmp(f.0, f.1)),
            (Value::Atom(n), Value::Atom(m)) => n.0 == m.0,
            (Value::Symbol(n), Value::Symbol(m)) => n.0 == m.0,
            (Value::Num(n), Value::Num(m)) => n == m,
            _ => false,
        }
    }

    fn eq(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 2 {
            return Err(format!("Expected 2 arg but instead got {} ", args.len()));
        }

        let args = eval_list(env, args)?;

        if cmp(&args[0], &args[1]) {
            return Ok(Rc::new(Value::Atom(Symbol("true".to_string()))));
        } else {
            return Ok(Rc::new(Value::List(vec![])));
        }
    }

    fn cons(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 2 {
            return Err(format!("Expected 2 arg but instead got {} ", args.len()));
        }

        let args = eval_list(env, args)?;

        match &*args[1] {
            Value::List(ls) => {
                let mut args = vec![args[0].clone()];
                for arg in ls {
                    args.push(arg.clone());
                }
                Ok(Rc::new(Value::List(args)))
            }
            _ => Ok(Rc::new(Value::List(vec![args[0].clone(), args[1].clone()]))),
        }
    }

    fn is_nil(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        if let Value::List(ls) = &*eval(env, args[0].clone())? {
            if ls.is_empty() {
                return Ok(Rc::new(Value::Atom(Symbol("true".to_string()))));
            }
        }

        Ok(Rc::new(Value::List(vec![])))
    }

    fn is_cons(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        if let Value::List(ls) = &*eval(env, args[0].clone())? {
            if !ls.is_empty() {
                return Ok(Rc::new(Value::Atom(Symbol("true".to_string()))));
            }
        }

        Ok(Rc::new(Value::List(vec![])))
    }

    fn head(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        match &*eval(env, args[0].clone())? {
            Value::List(ls) if !ls.is_empty() => Ok(ls[0].clone()),
            _ => Ok(Rc::new(Value::List(vec![]))),
        }
    }

    fn tail(env: &mut Env, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 arg but instead got {} ", args.len()));
        }

        let res = eval(env, args[0].clone())?;

        match &*res {
            Value::List(ls) => Ok(Rc::new(Value::List(Vec::from(&ls[1..])))),
            _ => Ok(Rc::new(Value::List(vec![]))),
        }
    }

    let mut globals = FxHashMap::default();

    globals.insert(
        "set*".to_string(),
        Def::new(Rc::new(Value::Prim(|env, ctx| set_ctx(env, ctx, false)))),
    );
    globals.insert(
        "setm*".to_string(),
        Def::new(Rc::new(Value::Prim(|env, ctx| set_ctx(env, ctx, true)))),
    );
    globals.insert("print".to_string(), Def::new(Rc::new(Value::Prim(print))));
    globals.insert("+".to_string(), Def::new(Rc::new(Value::Prim(add))));
    globals.insert("list".to_string(), Def::new(Rc::new(Value::Prim(list))));
    globals.insert("eval".to_string(), Def::new(Rc::new(Value::Prim(evaluate))));
    globals.insert("quote".to_string(), Def::imp(Rc::new(Value::Prim(quote))));
    globals.insert("lambda".to_string(), Def::new(Rc::new(Value::Prim(lambda))));

    globals.insert("if".to_string(), Def::new(Rc::new(Value::Prim(if_))));
    globals.insert("eq".to_string(), Def::new(Rc::new(Value::Prim(eq))));

    globals.insert("is-nil".to_string(), Def::new(Rc::new(Value::Prim(is_nil))));
    globals.insert(
        "is-cons".to_string(),
        Def::new(Rc::new(Value::Prim(is_cons))),
    );
    globals.insert("head".to_string(), Def::new(Rc::new(Value::Prim(head))));
    globals.insert("tail".to_string(), Def::new(Rc::new(Value::Prim(tail))));
    globals.insert("cons".to_string(), Def::new(Rc::new(Value::Prim(cons))));

    globals.insert("let".to_string(), Def::new(Rc::new(Value::Prim(let_decl))));

    globals
}

fn main() -> Result<(), String> {
    let entry = parse(
        "
        (set* map (lambda (f expr)
            (if (is-cons expr)
                (cons (f (head expr)) (map f (tail expr)))
                expr)))

        (setm* quasi-quote (lambda (expr)
            (if (is-cons expr)
                (if (eq :unquote (head expr))
                    (head (tail expr))
                    (cons :list (map quasi-quote expr)))
                (list :quote expr))))

        (setm* def-macro (lambda (name args body)
            (quasi-quote
                (setm* (unquote name) (lambda (unquote args) (unquote body))))))

        (def-macro defn (name args body)
            (quasi-quote
                (set* (unquote name) (lambda (unquote args) (unquote body)))))

        (defn pudim (a b c) (+ a b c))

        (print (pudim 1 2 3))
        ",
    )?;

    let mut env = Env {
        frames: Default::default(),
        global: Arc::new(RwLock::new(empty_context())),
    };

    for entry in entry {
        let mut expanded_form = entry;

        loop {
            let mut expanded = false;
            expanded_form = macro_expand(&mut expanded, &mut env, expanded_form)?;
            if !expanded {
                break;
            }
        }

        eval(&mut env, expanded_form)?;
    }

    Ok(())
}