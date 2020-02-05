use crate::ast::*;

pub fn is_value(term: &Term, mut stage: Stage) -> bool {
    if stage.is_empty() {
        match term {
            Term::Int(_) | Term::Lam(_, _, _) => true,
            Term::Code(ref stage_var, box ref t) => is_value(t, vec![stage_var.clone()]),
            Term::StageLam(_, box ref t) => is_value(t, vec![]),
            _ => false,
        }
    } else {
        match term {
            Term::Var(_) => true,
            Term::Lam(_, _, box ref t) => is_value(t, stage),
            Term::App(box ref t1, box ref t2) => is_value(t1, stage.clone()) && is_value(t2, stage),
            Term::Code(ref stage_var, box ref t) => {
                stage.push(stage_var.clone());
                is_value(t, stage)
            }
            Term::StageLam(_, box ref t) => is_value(t, stage),
            Term::StageApp(box ref t, _) => is_value(t, stage),
            Term::Escape(ref stage_var, box ref t) => {
                if let [stage_.., alpha] = stage.as_slice() {
                    alpha == stage_var && is_value(t, stage_.to_vec())
                } else {
                    false
                }
            }
            Term::CSP(ref stage_var, box ref t) => {
                if let [stage_.., alpha] = stage.as_slice() {
                    alpha == stage_var && is_value(t, stage_.to_vec())
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

fn reduce_rule(term: Term, A: Stage, B: Option<StageVar>) -> Term {
    match term {
        Term::App(box Term::Lam(x, _, box t1), box t2) if B.is_none() && is_value(&t2, vec![]) => {
            t1.subst_term(x, t2)
        }
        Term::StageApp(box Term::StageLam(a, box t), stage)
            if B.is_none() && is_value(&t, vec![]) =>
        {
            t.subst_stage(a, A)
        }
        Term::Escape(a, box Term::Code(a_, box t)) if a == a_ && is_value(&t, vec![a.clone()]) => t,
        _ => term,
    }
}

pub fn reduce(mut _term: Term, _A: Stage, _B: Option<StageVar>) -> Term {
    unimplemented!()
}
