use crate::ast::*;

#[test]
fn is_value_at_test() {
    use Term::*;
    assert!(Const(Literal::Add).is_value_at(&vec![]));
    assert!(Lam(
        TermVar("x".to_string()),
        Box::new(Type::Int),
        Box::new(Const(Literal::Int(42)))
    )
    .is_value_at(&vec![]));
    // assert!(Const(Literal::Add).is_value_at(&vec![]));
}

impl Term {
    pub fn is_value_at(&self, stage: &Stage) -> bool {
        use Term::*;
        if stage.is_empty() {
            match self {
                Const(Literal::Vector(ts)) => ts.iter().all(|t| t.is_value_at(stage)),
                Const(_) | Lam(_, _, _) => true,
                Code(ref alpha, box ref v) => v.is_value_at(&vec![alpha.clone()]),
                StageLam(_, box ref v) => v.is_value_at(&vec![]),
                _ => false,
            }
        } else {
            match self {
                Const(_) | Var(_) => true,
                Lam(_, _, box ref v) => v.is_value_at(stage),
                App(box ref v1, box ref v2) => v1.is_value_at(stage) && v2.is_value_at(stage),
                Code(ref alpha, box ref v) => {
                    let mut stage = stage.clone();
                    stage.push(alpha.clone());
                    v.is_value_at(&stage)
                }
                StageLam(_, box ref v) => v.is_value_at(stage),
                StageApp(box ref v, _) => v.is_value_at(stage),
                Escape(ref alpha, box ref v) if stage.len() != 1 => {
                    let mut stage = stage.clone();
                    if let Some(alpha_) = stage.pop() {
                        alpha == &alpha_ && v.is_value_at(&stage)
                    } else {
                        false
                    }
                }
                CSP(ref alpha, box ref v) => {
                    let mut stage = stage.clone();
                    if let Some(alpha_) = stage.pop() {
                        alpha == &alpha_ && v.is_value_at(&stage)
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
    }
}
