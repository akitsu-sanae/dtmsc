use crate::ast::*;

pub fn is_value(term: &Term, stage: &Stage) -> bool {
    let mut stage = stage.clone();
    if stage.is_empty() {
        match term {
            Term::Int(_) | Term::Lam(_, _, _) => true,
            Term::Code(ref stage_var, box ref t) => is_value(t, &vec![stage_var.clone()]),
            Term::StageLam(_, box ref t) => is_value(t, &vec![]),
            _ => false,
        }
    } else {
        match term {
            Term::Int(_) | Term::Var(_) => true,
            Term::Lam(_, _, box ref t) => is_value(t, &stage),
            Term::App(box ref t1, box ref t2) => is_value(t1, &stage) && is_value(t2, &stage),
            Term::Code(ref stage_var, box ref t) => {
                stage.push(stage_var.clone());
                is_value(t, &stage)
            }
            Term::StageLam(_, box ref t) => is_value(t, &stage),
            Term::StageApp(box ref t, _) => is_value(t, &stage),
            Term::Escape(ref stage_var, box ref t) => {
                if let [stage_.., alpha] = stage.as_slice() {
                    alpha == stage_var && is_value(t, &stage_.to_vec())
                } else {
                    false
                }
            }
            Term::CSP(ref stage_var, box ref t) => {
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

fn calc_binop(op: BinOp, lhs: i32, rhs: i32) -> i32 {
    use BinOp::*;
    match op {
        Add => lhs + rhs,
        Sub => lhs - rhs,
        Mult => lhs * rhs,
        Div => lhs / rhs,
    }
}

pub fn reduce(term: Term, A: Stage, B: Option<StageVar>) -> Result<Term, String> {
    use Term::*;
    let no_reduction_err = Err(format!("no reduction for {} under {:?}", term, A));
    match term {
        BinOp(op, box t1, box t2) if is_value(&t1, &A) && is_value(&t2, &A) => match (t1, t2) {
            (Int(lhs), Int(rhs)) => Ok(Int(calc_binop(op, lhs, rhs))),
            (t1, t2) => Err(format!(
                "cannot apply `{}` binary operator for {} and {}",
                op, t1, t2
            )),
        },
        BinOp(op, box t1, box t2) if is_value(&t1, &A) => {
            Ok(BinOp(op, Box::new(t1), Box::new(reduce(t2, A, B)?)))
        }
        BinOp(op, box t1, box t2) => Ok(BinOp(op, Box::new(reduce(t1, A, B)?), Box::new(t2))),

        App(box Lam(x, _, box t1), box t2) if B.is_none() && is_value(&t2, &vec![]) => {
            Ok(t1.subst_term(x, t2))
        }
        App(box t1, box t2) if is_value(&t1, &A) => {
            Ok(App(Box::new(t1), Box::new(reduce(t2, A, B)?)))
        }
        App(box t1, box t2) => Ok(App(Box::new(reduce(t1, A, B)?), Box::new(t2))),

        StageApp(box StageLam(alpha, box t), stage) if B.is_none() && is_value(&t, &vec![]) => {
            Ok(t.subst_stage(alpha, stage))
        }
        StageApp(box t, stage) => Ok(StageApp(Box::new(reduce(t, A, B)?), stage)),

        Escape(stage_var1, box Code(stage_var2, box t)) =>
            // if B.is_some()
            //    && B.clone().unwrap() == stage_var1
            //    && stage_var1 == stage_var2
            //    && is_value(&t, &vec![B.clone().unwrap()]) =>
        {
            Ok(t)
        }
        Escape(stage_var1, box t) => {
            if let [stage_.., alpha] = A.as_slice() {
                if alpha == &stage_var1 {
                    Ok(Escape(stage_var1, Box::new(reduce(t, stage_.to_vec(), B)?)))
                } else {
                    no_reduction_err
                }
            } else {
                no_reduction_err
            }
        }

        StageLam(stage_var, box t) => Ok(StageLam(stage_var, Box::new(reduce(t, A, B)?))),
        Code(stage_var, box t) => {
            let mut stage = A;
            stage.push(stage_var.clone());
            Ok(Code(stage_var, Box::new(reduce(t, stage, B)?)))
        }
        CSP(stage_var, box t) => {
            if let [stage_.., alpha] = A.as_slice() {
                if alpha == &stage_var {
                    Ok(CSP(stage_var, Box::new(reduce(t, stage_.to_vec(), B)?)))
                } else {
                    no_reduction_err
                }
            } else {
                no_reduction_err
            }
        }

        t if is_value(&t, &A) => Err(format!("{} is already value", t)),
        _ => no_reduction_err,
    }
}
