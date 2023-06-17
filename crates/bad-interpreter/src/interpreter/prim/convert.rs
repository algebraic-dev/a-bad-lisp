use crate::{define, Eval};
use bad_types::*;

pub fn register(env: &mut Env) {
    define!(env, "to-str", |env, args| {
        assert_size(args, 1)?;
        Ok(Value::string(&args[0].clone().eval(env)?.0.to_string()))
    });
}
