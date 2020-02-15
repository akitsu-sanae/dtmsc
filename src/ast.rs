pub mod parser;
pub mod printer;
pub mod subst;
pub mod util;

use std::hash::Hash;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TermVar(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageVar(pub String);

pub type Stage = Vec<StageVar>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    Int(i32),
    Add,
    Sub,
    Mult,
    Div,

    Nil,
    Cons,
    Head,
    Tail,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Const(Literal),
    Var(TermVar),
    Lam(TermVar, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Code(StageVar, Box<Term>),
    Escape(StageVar, Box<Term>),
    StageLam(StageVar, Box<Term>),
    StageApp(Box<Term>, Stage),
    CSP(StageVar, Box<Term>),

    Let(TermVar, Box<Type>, Box<Term>, Box<Term>),
    Fix(TermVar, Box<Type>, Box<Term>),
    Ifz(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    ProperType,
    TypeOperation(TermVar, Box<Type>, Box<Kind>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Vector,
    DepFun(TermVar, Box<Type>, Box<Type>),
    App(Box<Type>, Box<Term>),
    Code(StageVar, Box<Type>),
    StageBind(StageVar, Box<Type>),
}
