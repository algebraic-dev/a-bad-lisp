use crate::{define, interpreter::eval_list, Eval};
use bad_types::*;

pub fn register(env: &mut Env) {
    define!(env, "+", |env, args| {
        let args = eval_list(env, args)?;

        let mut result = 0;
        for arg in args {
            result += assert_number(arg.clone())?;
        }
        Ok(Value::number(result))
    });

    define!(env, "-", |env, args| {
        let args = eval_list(env, args)?;

        let mut result = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            result -= assert_number(arg.clone())?;
        }
        Ok(Value::number(result))
    });

    define!(env, "*", |env, args| {
        let args = eval_list(env, args)?;

        let mut result = 1;
        for arg in args {
            result *= assert_number(arg.clone())?;
        }
        Ok(Value::number(result))
    });

    define!(env, "/", |env, args| {
        let args = eval_list(env, args)?;

        let mut result = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            result /= assert_number(arg.clone())?;
        }
        Ok(Value::number(result))
    });

    define!(env, "%", |env, args| {
        let args = eval_list(env, args)?;

        let mut result = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            result %= assert_number(arg.clone())?;
        }
        Ok(Value::number(result))
    });

    define!(env, ">", |env, args| {
        let args = eval_list(env, args)?;

        let mut last = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            let next = assert_number(arg.clone())?;
            if last <= next {
                return Ok(Value::boolean(false));
            }
            last = next;
        }
        Ok(Value::boolean(true))
    });

    define!(env, "<", |env, args| {
        let args = eval_list(env, args)?;

        let mut last = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            let next = assert_number(arg.clone())?;
            if last >= next {
                return Ok(Value::boolean(false));
            }
            last = next;
        }
        Ok(Value::boolean(true))
    });

    define!(env, ">=", |env, args| {
        let args = eval_list(env, args)?;

        let mut last = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            let next = assert_number(arg.clone())?;
            if last < next {
                return Ok(Value::boolean(false));
            }
            last = next;
        }
        Ok(Value::boolean(true))
    });

    define!(env, "<=", |env, args| {
        let args = eval_list(env, args)?;

        let mut last = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            let next = assert_number(arg.clone())?;
            if last > next {
                return Ok(Value::boolean(false));
            }
            last = next;
        }
        Ok(Value::boolean(true))
    });

    define!(env, "=", |env, args| {
        let args = eval_list(env, args)?;

        let mut last = assert_number(args[0].clone())?;
        for arg in args.iter().skip(1) {
            let next = assert_number(arg.clone())?;
            if last != next {
                return Ok(Value::boolean(false));
            }
            last = next;
        }
        Ok(Value::boolean(true))
    });
}
