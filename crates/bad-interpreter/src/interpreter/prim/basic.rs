use crate::{define, interpreter::Eval};
use bad_types::*;

pub fn cmp(left: Value, right: Value) -> bool {
    match (&*left.0, &*right.0) {
        (ValueKind::Symbol(s1), ValueKind::Symbol(s2)) => s1 == s2,
        (ValueKind::Atom(s1), ValueKind::Atom(s2)) => s1 == s2,
        (ValueKind::Number(s1), ValueKind::Number(s2)) => s1 == s2,
        (ValueKind::String(s1), ValueKind::String(s2)) => s1 == s2,
        _ => std::ptr::eq(left.0.as_ref(), right.0.as_ref()),
    }
}

pub fn to_bool(left: Value) -> bool {
    match &*left.0 {
        ValueKind::Atom(b) if b.0 == "false" => false,
        ValueKind::List(x) if x.is_empty() => false,
        ValueKind::Number(0) => false,
        _ => true,
    }
}

pub fn register(env: &mut Env) {
    define!(env, "set", |env, args| {
        assert_size(args, 2)?;
        let name = assert_identifier(args[0].clone())?;
        let value = args[1].clone().eval(env)?;
        env.set(&name, value);
        Ok(Value::list(vec![]))
    });

    define!(env, "list", |env, args| {
        Ok(Value::list(
            args.iter()
                .map(|x| x.clone().eval(env))
                .collect::<Result<_, _>>()?,
        ))
    });

    define!(env, "eval", |env, args| {
        assert_size(args, 1)?;
        let expr = args[0].clone().eval(env)?;
        expr.eval(env)
    });

    define!(env, "quote", |_, args| {
        assert_size(args, 1)?;
        Ok(args[0].clone())
    });

    define!(env, "lambda", |env, args| {
        assert_size(args, 2)?;

        let params = match &*args[0].0 {
            ValueKind::List(ls) => ls
                .iter()
                .map(|x| assert_identifier(x.clone()))
                .collect::<Result<Vec<Symbol>, _>>()?,
            _ => return Err("expected list".to_string()),
        };

        let body = args[1].clone();

        Ok(Value::closure(env.clone(), Fun { params, body }))
    });

    define!(env, "eq", |env, args| {
        assert_size(args, 2)?;

        let left = args[0].clone().eval(&mut env.clone())?;
        let right = args[1].clone().eval(&mut env.clone())?;

        Ok(Value::boolean(cmp(left, right)))
    });

    define!(env, "is-nil", |env, args| {
        assert_size(args, 1)?;

        let arg = args[0].clone().eval(&mut env.clone())?;

        match &*arg.0 {
            ValueKind::Atom(s) if s.0 == "nil" => Ok(Value::boolean(true)),
            ValueKind::List(ls) if ls.is_empty() => Ok(Value::boolean(true)),
            _ => Ok(Value::boolean(false)),
        }
    });

    define!(env, "print", |env, args| {
        for arg in args {
            let arg = arg.clone().eval(&mut env.clone())?.0;

            match &*arg {
                ValueKind::String(s) => print!("{}", s),
                _ => print!("{}", arg),
            }
        }

        println!();
        Ok(Value::list(vec![]))
    });

    define!(env, "if", |env, args| {
        assert_size(args, 3)?;

        let cond = args[0].clone().eval(&mut env.clone())?;

        if to_bool(cond) {
            args[1].clone().eval(env)
        } else {
            args[2].clone().eval(env)
        }
    });

    define!(env, "block", |env, args| {
        let mut result = Value::atom("nil");

        env.new_scope();

        for arg in args {
            result = arg.clone().eval(env)?;
        }

        env.pop_scope();

        Ok(result)
    });

    define!(env, "let", |env, args| {
        assert_size(args, 2)?;

        let name = assert_identifier(args[0].clone())?;
        let value = args[1].clone().eval(env)?;

        env.push_local(name, value);

        Ok(Value::list(vec![]))
    });

    define!(env, "for", |env, args| {
        assert_size(args, 3)?;
        // (for x iterator body)
        let name = assert_identifier(args[0].clone())?;
        let iterator = args[1].clone().eval(env)?;
        let body = args[2].clone();
        let mut result = Value::atom("nil");

        env.new_scope();

        match &*iterator.0 {
            ValueKind::List(ls) => {
                for item in ls {
                    env.push_local(name.clone(), item.clone());
                    result = body.clone().eval(env)?;
                }
            }
            ValueKind::String(str) => {
                for item in str.chars() {
                    env.push_local(name.clone(), Value::string(&item.to_string()));
                    result = body.clone().eval(env)?;
                }
            }
            _ => return Err("expected list".to_string()),
        }

        env.pop_scope();

        Ok(result)
    });
}
