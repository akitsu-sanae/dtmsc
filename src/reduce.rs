use crate::ast::*;

impl Term {
    pub fn is_value_at(&self, stage: &Stage) -> bool {
        use Term::*;
        let mut stage = stage.clone();
        if stage.is_empty() {
            match self {
                Const(_) | Lam(_, _, _) => true,
                Code(ref stage_var, box ref t) => t.is_value_at(&vec![stage_var.clone()]),
                StageLam(_, box ref t) => t.is_value_at(&vec![]),
                _ => false,
            }
        } else {
            match self {
                Const(_) => true,
                Lam(_, _, box ref t) => t.is_value_at(&stage),
                App(box ref t1, box ref t2) => t1.is_value_at(&stage) && t2.is_value_at(&stage),
                Code(ref stage_var, box ref t) => {
                    stage.push(stage_var.clone());
                    t.is_value_at(&stage)
                }
                StageLam(_, box ref t) => t.is_value_at(&stage),
                StageApp(box ref t, _) => t.is_value_at(&stage),
                Escape(ref stage_var, box ref t) | CSP(ref stage_var, box ref t) => {
                    if !stage.is_empty() {
                        let stage_ = &stage[..(stage.len() - 1)];
                        let alpha = &stage[stage.len() - 1];
                        alpha == stage_var && t.is_value_at(&stage_.to_vec())
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
    }

    pub fn reduce(self) -> Result<Term, String> {
        use Term::*;
        reduce_context(self, vec![], None, |term, hole_stage| {
            let no_reduction_err = Err(format!("no reduction for {}", term));
            match term {
                App(
                    box App(box Const(op), box Const(Literal::Int(n1))),
                    box Const(Literal::Int(n2)),
                ) if hole_stage.is_none() => Ok(Const(Literal::Int(match op {
                    Literal::Add => n1 + n2,
                    Literal::Sub => n1 - n2,
                    Literal::Mult => n1 * n2,
                    Literal::Div => n1 / n2,
                    _ => return no_reduction_err,
                }))),
                App(box Lam(x, _, t), box v) if hole_stage.is_none() && v.is_value_at(&vec![]) => {
                    Ok(t.subst_term(x, v))
                }
                StageApp(box StageLam(alpha, box v), stage)
                    if hole_stage.is_none() && v.is_value_at(&vec![]) =>
                {
                    Ok(v.subst_stage(alpha, stage))
                }
                Escape(stage_var1, box Code(stage_var2, box v))
                    if hole_stage.is_some()
                        && hole_stage.clone().unwrap() == stage_var1
                        && stage_var1 == stage_var2
                        && v.is_value_at(&vec![stage_var1.clone()]) =>
                {
                    Ok(v)
                }
                _ => no_reduction_err,
            }
        })
    }
}

fn option_to_vec<T>(x: Option<T>) -> Vec<T> {
    if let Some(x) = x {
        vec![x]
    } else {
        vec![]
    }
}

fn reduce_context(
    term: Term,
    stage: Stage,
    hole_stage: Option<StageVar>,
    rule: fn(Term, Option<StageVar>) -> Result<Term, String>,
) -> Result<Term, String> {
    use Term::*;
    if stage.is_empty() && hole_stage.is_none() {
        rule(term, hole_stage)
    } else if stage.is_empty() {
        match term {
            App(box v, box t) if v.is_value_at(&vec![]) => Ok(App(
                Box::new(v),
                Box::new(reduce_context(t, vec![], hole_stage, rule)?),
            )),
            App(box t1, box t2) => Ok(App(
                Box::new(reduce_context(t1, vec![], hole_stage, rule)?),
                Box::new(t2),
            )),
            Code(alpha, box t) => Ok(Code(
                alpha.clone(),
                Box::new(reduce_context(t, vec![alpha], hole_stage, rule)?),
            )),
            StageLam(alpha, box t) => Ok(StageLam(
                alpha.clone(),
                Box::new(reduce_context(t, vec![], hole_stage, rule)?),
            )),
            StageApp(box t, stage) => Ok(StageApp(
                Box::new(reduce_context(t, vec![], hole_stage, rule)?),
                stage,
            )),
            _ => unreachable!(),
        }
    } else if stage == option_to_vec(hole_stage.clone()) {
        rule(term, hole_stage)
    } else {
        match term {
            Lam(x, ty, box t) => Ok(Lam(
                x,
                ty,
                Box::new(reduce_context(t, stage, hole_stage, rule)?),
            )),
            _ => unreachable!(),
        }
    }
}
