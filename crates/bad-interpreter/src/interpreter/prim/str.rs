use crate::interpreter::Eval;
use crate::{define, interpreter::eval_list};

use bad_types::*;

pub fn register(env: &mut Env) {
    define!(env, "str/concat", |env, args| {
        let args = eval_list(env, args)?;
        let mut result = String::new();
        for arg in args {
            result += &assert_string(arg.clone())?;
        }
        Ok(Value::string(&result))
    });

    define!(env, "str/len", |env, args| {
        let args = &eval_list(env, args)?;
        assert_size(args, 1)?;
        Ok(Value::number(assert_string(args[0].clone())?.len() as u64))
    });

    define!(env, "str/slice", |env, args| {
        let args = &eval_list(env, args)?;
        assert_size(args, 3)?;
        let s = assert_string(args[0].clone())?;
        let start = assert_number(args[1].clone())? as usize;
        let end = assert_number(args[2].clone())? as usize;
        Ok(Value::string(&s[start..end]))
    });

    define!(env, "str/starts-with", |env, args| {
        let args = &eval_list(env, args)?;
        assert_size(args, 2)?;
        let s = assert_string(args[0].clone())?;
        let prefix = assert_string(args[1].clone())?;
        Ok(Value::boolean(s.starts_with(&prefix)))
    });

    define!(env, "str/ends-with", |env, args| {
        let args = &eval_list(env, args)?;
        assert_size(args, 2)?;
        let s = assert_string(args[0].clone())?;
        let suffix = assert_string(args[1].clone())?;
        Ok(Value::boolean(s.ends_with(&suffix)))
    });

    define!(env, "str/contains", |env, args| {
        let args = &eval_list(env, args)?;
        assert_size(args, 2)?;
        let s = assert_string(args[0].clone())?;
        let substring = assert_string(args[1].clone())?;
        Ok(Value::boolean(s.contains(&substring)))
    });

    define!(env, "str/to-list", |env, args| {
        assert_size(args, 1)?;
        let s = assert_string(args[0].clone().eval(env)?)?;
        let mut result = vec![];
        for c in s.chars() {
            result.push(Value::string(&c.to_string()));
        }
        Ok(Value::list(result))
    });

    define!(env, "str/from-list", |env, args| {
        assert_size(args, 1)?;
        let arg = args[0].clone().eval(env)?;
        let chars = assert_list(arg)?;
        let mut result = String::new();
        for c in chars {
            result += &assert_string(c)?;
        }
        Ok(Value::string(&result))
    });

    define!(env, "str/get", |env, args| {
        assert_size(args, 2)?;
        let args = eval_list(env, args)?;
        let index = assert_number(args[1].clone())? as usize;
        let s = assert_string(args[0].clone())?;
        Ok(Value::string(&s[index..index + 1]))
    });

    define!(env, "str/replace", |env, args| {
        assert_size(args, 3)?;
        let args = eval_list(env, args)?;
        let s = assert_string(args[0].clone())?;
        let from = assert_string(args[1].clone())?;
        let to = assert_string(args[2].clone())?;
        Ok(Value::string(&s.replace(&from, &to)))
    });
}
