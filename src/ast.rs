#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TermVar(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageVar(pub String);

pub type Stage = Vec<StageVar>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Int(i32),
    BinOp(BinOp, Box<Term>, Box<Term>),
    Var(TermVar),
    Lam(TermVar, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Code(StageVar, Box<Term>),
    Escape(StageVar, Box<Term>),
    StageLam(StageVar, Box<Term>),
    StageApp(Box<Term>, Stage),
    CSP(StageVar, Box<Term>),
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
