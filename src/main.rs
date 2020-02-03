extern crate peg;

mod ast;
mod parser;

fn main() {
    println!("{:?}", parser::term("123"));
}
