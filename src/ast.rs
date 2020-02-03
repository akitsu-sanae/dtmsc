#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TermVar(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageVar(pub String);

pub type Stage = Vec<StageVar>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Int(i32),
    Var(TermVar),
    Lam(TermVar, Box<Term>),
    App(Box<Term>, Box<Term>),
    Code(StageVar, Box<Term>),
    Escape(StageVar, Box<Term>),
    StageLam(StageVar, Box<Term>),
    StageApp(Box<Term>, Stage),
    CSP(StageVar, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    NotImplemented,
}
