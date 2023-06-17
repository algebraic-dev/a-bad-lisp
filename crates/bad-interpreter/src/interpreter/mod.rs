use std::rc::Rc;

use bad_types::{Env, Fun, Symbol, Value, ValueKind};

use crate::expr::Expr;

pub mod prim;

type Result<T> = std::result::Result<T, String>;

impl<'a> From<&Expr<'a>> for Value {
    fn from(value: &Expr<'a>) -> Self {
        let kind = match value {
            Expr::List(list) => {
                let mut values = Vec::new();
                for expr in list {
                    values.push(expr.into());
                }
                ValueKind::List(values)
            }
            Expr::Symbol(atom) => ValueKind::Symbol(Symbol(atom.to_string())),
            Expr::Atom(symbol) => ValueKind::Atom(Symbol(symbol.to_string())),
            Expr::String(string) => ValueKind::String(string.to_string()),
            Expr::Num(num) => ValueKind::Number(*num),
        };
        Value(Rc::new(kind))
    }
}

fn eval_list(env: &mut Env, args: &[Value]) -> Result<Vec<Value>> {
    args.iter()
        .map(|x| x.clone().eval(env))
        .collect::<Result<Vec<_>>>()
}

pub trait Eval {
    type Output;

    fn eval(self, env: &mut Env) -> Result<Self::Output>;
}

pub struct Redex<'a>(&'a Fun, &'a [Value]);

impl Eval for Redex<'_> {
    type Output = Value;

    fn eval(self, env: &mut Env) -> Result<Value> {
        let Redex(fun, args) = self;

        if args.len() != fun.params.len() {
            return Err(format!(
                "Expected {} args but instead got {}.",
                fun.params.len(),
                args.len()
            ));
        }

        for (param, arg) in fun.params.iter().zip(args) {
            env.push_local(param.clone(), arg.clone())
        }

        fun.body.clone().eval(env)
    }
}
impl Eval for Value {
    type Output = Value;

    fn eval(self, env: &mut Env) -> Result<Value> {
        pub fn run_as_function(
            env: &mut Env,
            expr: &Value,
            args: &[Value],
            expand: bool,
        ) -> Result<Option<Value>> {
            let head = expr.clone().eval(env)?;

            match &*head.0 {
                ValueKind::Primitive(prim) => {
                    if expand {
                        env.expanded.replace(true);
                    }
                    prim(env, args).map(Some)
                }
                ValueKind::Closure(closure) => {
                    if expand {
                        env.expanded.replace(true);
                    }
                    let args = eval_list(env, args)?;
                    let mut new_env = closure.env.clone();
                    let redex = Redex(&closure.fun, &args);
                    new_env.new_scope();
                    let ret = redex.eval(&mut new_env)?;
                    new_env.pop_scope();
                    Ok(Some(ret))
                }
                ValueKind::Atom(s) if !env.macro_expansion => {
                    Ok(Some(Value(Rc::new(ValueKind::Symbol(s.clone())))))
                }
                expr if !env.macro_expansion => Err(format!("cannot apply '{expr}' as function")),
                _ => Ok(None),
            }
        }

        match &*self.0 {
            ValueKind::Symbol(_) => {
                let call = env.get(&Symbol("call".to_string())).unwrap();
                Ok(run_as_function(env, &call, &[self.clone()], false)?.unwrap())
            }
            ValueKind::List(list) if !list.is_empty() => {
                let head = list[0].clone().eval(env)?;
                let result = run_as_function(env, &head, &list[1..], true)?;
                match result {
                    Some(value) => Ok(value),
                    None => Ok(self.clone()),
                }
            }
            _ => Ok(self),
        }
    }
}

pub fn eval(env: &mut Env, expr: &Expr<'_>) -> Result<Value> {
    env.macro_expansion = true;
    env.expanded.replace(true);

    let mut value: Value = expr.into();

    while env.expanded.take() {
        env.expanded.replace(false);
        value = value.eval(env)?;
    }

    env.macro_expansion = false;

    value.eval(env)
}
