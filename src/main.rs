#![feature(box_patterns)]
#![feature(exclusive_range_pattern)]

#[macro_use]
extern crate lazy_static;
extern crate peg;

mod ast;
mod reduce;
mod type_check;
mod util;

fn main() {
    use std::io::Read;
    let filename = ::std::env::args().nth(1).expect("filename required");
    let mut f = ::std::fs::File::open(&filename).expect("cannot open file");
    let mut buf = String::new();
    f.read_to_string(&mut buf).expect("cannot read file");

    match ast::parser::term(&buf) {
        Ok(mut term) => {
            use std::io::{stdout, BufWriter, Write};
            let stdout = stdout();
            let mut out = BufWriter::new(stdout.lock());
            writeln!(out, "input term: {}", term).unwrap();
            match term.type_check(&vec![]) {
                Ok(ty) => {
                    eprintln!("given term has `{}` type", ty);
                    while !term.is_value_at(&vec![]) {
                        term = match term.reduce() {
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
                    eprintln!("{}", err);
                }
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
                ast::parser::pick_token(&buf[err.location.offset..])
            );
        }
    }
}
