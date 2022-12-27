pub mod interpreter;
pub mod bytecode;

pub mod expr;
pub mod parser;

pub fn main() -> Result<(), String> {
    interpreter::run(
        "
        (set* map (lambda (f expr)
            (if (is-cons expr)
                (cons (f (head expr)) (map f (tail expr)))
                expr)))

        (setm* quasi-quote (lambda (expr)
            (if (is-cons expr)
                (if (eq (quote unquote) (head expr))
                    (head (tail expr))
                    (cons (quote list) (map quasi-quote expr)))
                (list (quote quote) expr))))

        (setm* def-macro (lambda (name args body)
            (quasi-quote
                (setm* (unquote name) (lambda (unquote args) (unquote body))))))

        (def-macro defn (name args body)
            (quasi-quote
                (set* (unquote name) (lambda (unquote args) (unquote body)))))

        (defn fib (num)
            (if (< num 2)
                num
                (+ (fib (- num 1)) (fib (- num 2)))))

        (print (fib 10))
        ",
    )?;

    Ok(())
}
