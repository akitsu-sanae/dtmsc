#![feature(box_patterns)]
#![feature(slice_patterns)]
#![feature(bind_by_move_pattern_guards)]

#[macro_use]
extern crate lazy_static;
extern crate peg;

mod ast;
mod ast_printer;
mod ast_util;
mod parser;
mod reduce;

fn main() {
    use std::io::Read;
    let filename = ::std::env::args().nth(1).expect("filename required");
    let mut f = ::std::fs::File::open(&filename).expect("cannot open file");
    let mut buf = String::new();
    f.read_to_string(&mut buf).expect("cannot read file");

    match parser::term(&buf) {
        Ok(mut term) => {
            use std::io::{stdout, BufWriter, Write};
            let stdout = stdout();
            let mut out = BufWriter::new(stdout.lock());
            writeln!(out, "input term: {}", term).unwrap();
            while !reduce::is_value(&term, &vec![]) {
                term = match reduce::reduce(term) {
                    Ok(term) => term,
                    Err(msg) => {
                        eprintln!("{}", msg);
                        break;
                    }
                };
                writeln!(out, "=> {}", term).unwrap();
            }
        }
        Err(err) => {
            eprintln!(
                "\x1B[31mparsing error\x1B[0m at {}:{}:{}",
                filename, err.location.line, err.location.column
            );
            eprintln!("  expected: [{}]", {
                let mut tokens = err.expected.tokens();
                let head = tokens.next().unwrap().to_string();
                tokens.fold(head, |acc, expected| format!("{}, {}", acc, expected))
            });
            eprintln!(
                "  found   : \"{}\"",
                parser::pick_token(&buf[err.location.offset..])
            );
        }
    }
}
