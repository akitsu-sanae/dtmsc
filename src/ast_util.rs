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

    pub fn subst_term(self, x: TermVar, t: Term) -> Term {
        match self {
            Term::Var(x_) if x == x_ => t,
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
    pub fn subst_stage(self, a: StageVar, stage: Stage) -> Term {
        use Term::*;
        match self {
            Lam(x, box ty, box t) => Lam(x, Box::new(ty), Box::new(t.subst_stage(a, stage))),
            App(box t1, box t2) => App(
                Box::new(t1.subst_stage(a.clone(), stage.clone())),
                Box::new(t2.subst_stage(a, stage)),
            ),
            Code(stage_var, box t) if stage_var == a => {
                let t = t.subst_stage(a, stage.clone());
                stage
                    .into_iter()
                    .rev()
                    .fold(t, |acc, stage_var| Code(stage_var, Box::new(acc)))
            }
            Code(stage_var, box t) => Code(stage_var, Box::new(t.subst_stage(a, stage))),
            Escape(stage_var, box t) if stage_var == a => {
                let t = t.subst_stage(a, stage.clone());
                stage
                    .into_iter()
                    .fold(t, |acc, stage_var| Escape(stage_var, Box::new(acc)))
            }
            Escape(stage_var, box t) => Escape(stage_var, Box::new(t.subst_stage(a, stage))),
            StageLam(stage_var, box t_) if stage_var != a => {
                StageLam(stage_var, Box::new(t_.subst_stage(a, stage)))
            }
            StageApp(box t, stage_) => StageApp(Box::new(t.subst_stage(a, stage)), stage_),
            CSP(stage_var, box t) if stage_var == a => {
                let t = t.subst_stage(a, stage.clone());
                stage
                    .into_iter()
                    .fold(t, |acc, stage_var| Escape(stage_var, Box::new(acc)))
            }
            CSP(stage_var, box t) => CSP(stage_var, Box::new(t.subst_stage(a, stage))),
            _ => self,
        }
    }
}
