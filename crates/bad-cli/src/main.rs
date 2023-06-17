use std::env;

use bad_interpreter::{eval, parse, prim::default_env};

pub fn run() -> Result<(), String> {
    let mut env = default_env();
    let silent = env::var("AION_SILENT").is_ok();

    for source in env::args().skip(1) {
        for expr in parse(&source)? {
            let evaluated = eval(&mut env, &expr)?;
            if !silent {
                println!("{}", evaluated.0);
            }
        }
    }

    Ok(())
}

pub fn main() {
    if let Err(e) = run() {
        eprintln!("[err] {}", e);
        std::process::exit(1);
    }
}
