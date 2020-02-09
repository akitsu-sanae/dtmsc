use crate::ast::*;
use std::fmt;

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Int(ref n) => write!(f, "{}", n),
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mult => write!(f, "*"),
            Div => write!(f, "/"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Term::*;
        match self {
            Const(lit) => write!(f, "{}", lit),
            Var(TermVar(ref ident)) => write!(f, "{}", ident),
            Lam(TermVar(ref ident), box ref ty, box ref t) => {
                write!(f, "(lam {}: {}. {})", ident, ty, t)
            }
            App(box ref t1, box ref t2) => write!(f, "({} {})", t1, t2),
            Code(StageVar(ref ident), box ref t) => write!(f, "|>{}. {}", ident, t),
            Escape(StageVar(ref ident), box ref t) => write!(f, "<|{}. {}", ident, t),
            StageLam(StageVar(ref ident), box ref t) => write!(f, "(LAM {}. {})", ident, t),
            StageApp(box ref t, ref stage) => {
                let mut stage_iter = stage.iter();
                let stage = if let Some(StageVar(ref head)) = stage_iter.next() {
                    stage_iter.fold(head.to_string(), |acc, StageVar(ref stage_var)| {
                        format!("{}, {}", acc, stage_var)
                    })
                } else {
                    String::new()
                };
                write!(f, "({}@{{{}}})", t, stage)
            }
            CSP(StageVar(ref ident), box ref t) => write!(f, "%{}. {}", ident, t),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Vector => write!(f, "vector"),
            DepFun(TermVar(ref ident), box ref ty1, box ref ty2) => {
                if ident.contains("dummy") {
                    write!(f, "{} -> {}", ty1, ty2)
                } else {
                    write!(f, "Pi {}: {}. {}", ident, ty1, ty2)
                }
            }
            App(box ref ty, box ref t) => write!(f, "{} {}", ty, t),
            Code(StageVar(ref ident), box ref ty) => write!(f, "|>{}. {}", ident, ty),
            StageBind(StageVar(ref ident), box ref ty) => write!(f, "forall {}. {}", ident, ty),
        }
    }
}
