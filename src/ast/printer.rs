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

            Vector(ref v) => {
                let mut head = v.clone();
                let v = head.split_off(1);
                if head.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(f, "[{}", head.pop().unwrap())?;
                    for t in v.iter() {
                        write!(f, ", {}", t)?;
                    }
                    write!(f, "]")
                }
            }
            Nil => write!(f, "nil"),
            Cons => write!(f, "cons"),
            Head => write!(f, "head"),
            Tail => write!(f, "tail"),
        }
    }
}

struct TermWithIndent<'a>(&'a Term, usize);

impl<'a> fmt::Display for TermWithIndent<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Term::*;
        let TermWithIndent(ref term, indent) = self;
        match term {
            Const(lit) => write!(f, "{}", lit),
            Var(TermVar(ref ident)) => write!(f, "{}", ident),
            Lam(TermVar(ref ident), box ref ty, box ref t) => write!(
                f,
                "(lam\n{3:indent$}{}: {}.\n{3:indent$}{})",
                ident,
                ty,
                TermWithIndent(t, indent + 2),
                "",
                indent = indent + 2,
            ),
            App(box ref t1, box ref t2) => write!(
                f,
                "({} {})",
                TermWithIndent(t1, indent + 2),
                TermWithIndent(t2, indent + 2),
            ),
            Code(StageVar(ref ident), box ref t) => {
                write!(f, "(|>{}. {})", ident, TermWithIndent(t, indent + 2))
            }
            Escape(StageVar(ref ident), box ref t) => {
                write!(f, "(<|{}. {})", ident, TermWithIndent(t, indent + 2))
            }
            StageLam(StageVar(ref ident), box ref t) => write!(
                f,
                "(LAM {}.\n{2:indent$}{})",
                ident,
                TermWithIndent(t, indent + 2),
                "",
                indent = indent + 2
            ),
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
            CSP(StageVar(ref ident), box ref t) => write!(f, "(%{}. {})", ident, t),

            Let(TermVar(ref x), box ref ty, box ref t1, box ref t2) => write!(
                f,
                "let(\n{4:indent$}{}: {},\n{4:indent$}{},\n{4:indent$}{})",
                x,
                ty,
                TermWithIndent(t1, indent + 2),
                TermWithIndent(t2, indent + 2),
                "",
                indent = indent + 2,
            ),
            Fix(TermVar(ref x), box ref ty, box ref t) => write!(
                f,
                "(fix\n{3:indent$}{}: {}.\n{3:indent$}{})",
                x,
                ty,
                TermWithIndent(t, indent + 2),
                "",
                indent = indent + 2
            ),
            Ifz(box ref cond, box ref t1, box ref t2) => write!(
                f,
                "ifz(\n{3:indent$}{},\n{3:indent$}{},\n{3:indent$}{})",
                TermWithIndent(cond, indent + 2),
                TermWithIndent(t1, indent + 2),
                TermWithIndent(t2, indent + 2),
                "",
                indent = indent + 2
            ),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        TermWithIndent(self, 0).fmt(f)
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
