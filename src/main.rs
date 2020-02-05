#![feature(box_patterns)]
#![feature(slice_patterns)]

extern crate peg;

mod ast;
mod parser;
mod reduce;

fn main() {
    use std::io::Read;
    let filename = ::std::env::args().nth(1).expect("filename required");
    let mut f = ::std::fs::File::open(filename).expect("cannot open file");
    let mut buf = String::new();
    f.read_to_string(&mut buf).expect("cannot read file");

    match parser::term(&buf) {
        Ok(mut term) => {
            println!("input term: {:?}", term);
            while !reduce::is_value(&term, vec![]) {
                term = reduce::reduce(term);
                println!("=> {:?}", term);
            }
        }
        Err(err) => eprintln!("{:?}", err),
    }
}
