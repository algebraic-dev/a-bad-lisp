use crate::{define, interpreter::eval_list};

use bad_types::*;

pub fn register(env: &mut Env) {
    define!(env, "list/len", |env, args| {
        assert_size(args, 1)?;
        let args = eval_list(env, args)?;
        Ok(Value::number(args.len() as u64))
    });

    define!(env, "list/get", |env, args| {
        assert_size(args, 2)?;
        let args = eval_list(env, args)?;
        let index = assert_number(args[1].clone())? as usize;
        let list = assert_list(args[0].clone())?;
        Ok(list[index].clone())
    });

    define!(env, "list/set", |env, args| {
        assert_size(args, 3)?;
        let args = eval_list(env, args)?;
        let index = assert_number(args[1].clone())? as usize;
        let mut list = assert_list(args[0].clone())?;
        list[index] = args[2].clone();
        Ok(Value::list(list))
    });

    define!(env, "list/append", |env, args| {
        assert_size(args, 2)?;
        let args = eval_list(env, args)?;
        let mut list = assert_list(args[0].clone())?;
        list.push(args[1].clone());
        Ok(Value::list(list))
    });

    define!(env, "list/concat", |env, args| {
        let args = eval_list(env, args)?;
        let mut result = vec![];
        for arg in args {
            result.append(&mut assert_list(arg)?);
        }
        Ok(Value::list(result))
    });

    define!(env, "list/slice", |env, args| {
        let args = eval_list(env, args)?;
        assert_size(&args, 3)?;
        let list = assert_list(args[0].clone())?;
        let start = assert_number(args[1].clone())? as usize;
        let end = assert_number(args[2].clone())? as usize;
        Ok(Value::list(list[start..end].to_vec()))
    });
}
