use crate::ast::*;

pub fn is_value(term: &Term, stage: &Stage) -> bool {
    use Term::*;
    let mut stage = stage.clone();
    if stage.is_empty() {
        match term {
            Const(_) | Lam(_, _, _) => true,
            Code(ref stage_var, box ref t) => is_value(t, &vec![stage_var.clone()]),
            StageLam(_, box ref t) => is_value(t, &vec![]),
            _ => false,
        }
    } else {
        match term {
            Const(_) => true,
            Lam(_, _, box ref t) => is_value(t, &stage),
            App(box ref t1, box ref t2) => is_value(t1, &stage) && is_value(t2, &stage),
            Code(ref stage_var, box ref t) => {
                stage.push(stage_var.clone());
                is_value(t, &stage)
            }
            StageLam(_, box ref t) => is_value(t, &stage),
            StageApp(box ref t, _) => is_value(t, &stage),
            Escape(ref stage_var, box ref t) => {
                if let [stage_.., alpha] = stage.as_slice() {
                    let stage_ = stage_.to_vec();
                    alpha == stage_var && is_value(t, &stage_.to_vec())
                } else {
                    false
                }
            }
            CSP(ref stage_var, box ref t) => {
                if let [stage_.., alpha] = stage.as_slice() {
                    alpha == stage_var && is_value(t, &stage_.to_vec())
                } else {
                    false
                }
            }
            _ => false,
        }
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
            App(box v, box t) if is_value(&v, &vec![]) => Ok(App(
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

pub fn reduce(term: Term) -> Result<Term, String> {
    use Term::*;
    reduce_context(term, vec![], None, |term, hole_stage| {
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
            App(box Lam(x, _, t), box v) if hole_stage.is_none() && is_value(&v, &vec![]) => {
                Ok(t.subst_term(x, v))
            }
            StageApp(box StageLam(alpha, box v), stage)
                if hole_stage.is_none() && is_value(&v, &vec![]) =>
            {
                Ok(v.subst_stage(alpha, stage))
            }
            Escape(stage_var1, box Code(stage_var2, box v))
                if hole_stage.is_some()
                    && hole_stage.clone().unwrap() == stage_var1
                    && stage_var1 == stage_var2
                    && is_value(&v, &vec![stage_var1.clone()]) =>
            {
                Ok(v)
            }
            _ => no_reduction_err,
        }
    })
}
