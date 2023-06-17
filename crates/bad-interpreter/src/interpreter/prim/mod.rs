use bad_types::*;

pub mod basic;
pub mod convert;
pub mod list;
pub mod math;
pub mod str;

#[macro_export]
macro_rules! define {
    ($env:expr, $function:ident) => {
        $env.global.insert(
            stringify!($function).to_string(),
            Value(std::rc::Rc::new(ValueKind::Primitive($function))),
        );
    };

    ($env:expr, $name: literal, $function:expr) => {
        $env.global.insert(
            $name.to_string(),
            Value(std::rc::Rc::new(ValueKind::Primitive($function))),
        );
    };

    ($env:expr, $name: literal, $meta:expr, $function:expr) => {
        $env.global.insert(
            $name.to_string(),
            Value(std::rc::Rc::new(ValueKind::List(vec![
                $meta,
                Value::primitive($function),
            ]))),
        );
    };
}

// Primitive definitions

pub fn call(env: &mut Env, args: &[Value]) -> Result<Value, String> {
    assert_size(args, 1)?;

    let n = assert_identifier(args[0].clone())?;
    let a = env.get(&n).ok_or_else(|| format!("{} not found", n.0))?;

    match &*a.0 {
        ValueKind::List(ls) if ls.len() == 2 => {
            let name = assert_atom(ls[0].clone())?;
            if name.0 == "macro" && env.macro_expansion {
                return Ok(ls[1].clone());
            }
        }
        _ => (),
    }

    if env.macro_expansion {
        Ok(args[0].clone())
    } else {
        Ok(a)
    }
}

pub fn default_env() -> Env {
    let mut frames = im::Vector::default();

    for _ in 0..100 {
        frames.push_back(Frame::default())
    }

    let mut env = Env {
        frames,
        place: 1,
        global: Default::default(),
        macro_expansion: Default::default(),
        expanded: Default::default(),
    };

    define!(env, call);

    basic::register(&mut env);
    math::register(&mut env);
    str::register(&mut env);
    convert::register(&mut env);
    list::register(&mut env);

    env
}
