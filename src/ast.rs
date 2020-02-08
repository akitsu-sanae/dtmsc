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

impl Term {
    pub fn subst_term(self, x: TermVar, t: Term) -> Term {
        match self {
            Term::Var(x_) if x == x_ => t,
            Term::BinOp(op, box t1, box t2) => Term::BinOp(
                op,
                Box::new(t1.subst_term(x.clone(), t.clone())),
                Box::new(t2.subst_term(x, t)),
            ),
            Term::Lam(x_, box ty, box t_) if x != x_ => {
                Term::Lam(x_, Box::new(ty), Box::new(t_.subst_term(x, t)))
            }
            Term::App(box t1, box t2) => Term::App(
                Box::new(t1.subst_term(x.clone(), t.clone())),
                Box::new(t2.subst_term(x, t)),
            ),
            Term::Code(stage_var, box t_) => Term::Code(stage_var, Box::new(t_.subst_term(x, t))),
            Term::Escape(stage_var, box t_) => {
                Term::Escape(stage_var, Box::new(t_.subst_term(x, t)))
            }
            Term::StageLam(stage_var, box t_) => {
                Term::StageLam(stage_var, Box::new(t_.subst_term(x, t)))
            }
            Term::StageApp(box t_, stage) => Term::StageApp(Box::new(t_.subst_term(x, t)), stage),
            Term::CSP(stage_var, box t_) => Term::CSP(stage_var, Box::new(t_.subst_term(x, t))),
            _ => self,
        }
    }
    pub fn subst_stage(self, a: StageVar, A: Stage) -> Term {
        match self {
            Term::BinOp(op, box t1, box t2) => Term::BinOp(
                op,
                Box::new(t1.subst_stage(a.clone(), A.clone())),
                Box::new(t2.subst_stage(a, A)),
            ),
            Term::Lam(x, box ty, box t) => {
                Term::Lam(x, Box::new(ty), Box::new(t.subst_stage(a, A)))
            }
            Term::App(box t1, box t2) => Term::App(
                Box::new(t1.subst_stage(a.clone(), A.clone())),
                Box::new(t2.subst_stage(a, A)),
            ),
            Term::Code(stage_var, box t) => Term::Code(stage_var, Box::new(t.subst_stage(a, A))),
            Term::Escape(stage_var, box t) => {
                Term::Escape(stage_var, Box::new(t.subst_stage(a, A)))
            }
            Term::StageLam(stage_var, box t_) => {
                Term::StageLam(stage_var, Box::new(t_.subst_stage(a, A)))
            }
            Term::StageApp(box t, stage) => Term::StageApp(Box::new(t.subst_stage(a, A)), stage),
            Term::CSP(stage_var, box t) => Term::CSP(stage_var, Box::new(t.subst_stage(a, A))),
            _ => self,
        }
    }
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
