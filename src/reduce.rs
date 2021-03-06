use crate::ast::*;

impl Term {
    pub fn reduce(self) -> Result<Term, String> {
        use Term::*;
        reduce_context(self, &vec![], |term| {
            let no_reduction_err = Err(format!("no reduction for {}", term));
            match term {
                App(box Lam(x, _, t), box v) if v.is_value_at(&vec![]) => Ok(t.subst_term(x, v)),
                StageApp(box StageLam(alpha, box v), stage) if v.is_value_at(&vec![]) => {
                    Ok(v.subst_stage(alpha, stage))
                }
                Escape(stage_var1, box Code(stage_var2, box v))
                    if stage_var1 == stage_var2 && v.is_value_at(&vec![stage_var1.clone()]) =>
                {
                    Ok(v)
                }
                _ => no_reduction_err,
            }
        })
    }
}

fn reduce_context(
    term: Term,
    stage: &Stage,
    rule: fn(Term) -> Result<Term, String>,
) -> Result<Term, String> {
    use Term::*;
    let no_reduction_err = Err(format!("no reduction for {} at {:?}", term, stage));
    if stage.is_empty() {
        match term {
            /*
             builtin constants
            */
            App(
                box App(box Const(op), box Const(Literal::Int(n1))),
                box Const(Literal::Int(n2)),
            ) => Ok(Const(Literal::Int(match op {
                Literal::Add => n1 + n2,
                Literal::Sub => n1 - n2,
                Literal::Mult => n1 * n2,
                Literal::Div => n1 / n2,
                _ => return no_reduction_err,
            }))),

            App(box t1, box t2) if !t1.is_value_at(&vec![]) => Ok(App(
                Box::new(reduce_context(t1, &vec![], rule)?),
                Box::new(t2),
            )),
            App(box v, box t) if v.is_value_at(&vec![]) && !t.is_value_at(&vec![]) => Ok(App(
                Box::new(v),
                Box::new(reduce_context(t, &vec![], rule)?),
            )),
            App(box v1, box v2) if v1.is_value_at(&vec![]) && v2.is_value_at(&vec![]) => {
                rule(App(Box::new(v1), Box::new(v2)))
            }
            Code(alpha, box t) if !t.is_value_at(&vec![alpha.clone()]) => Ok(Code(
                alpha.clone(),
                Box::new(reduce_context(t, &vec![alpha.clone()], rule)?),
            )),
            StageLam(alpha, box t) if !t.is_value_at(&vec![]) => {
                Ok(StageLam(alpha, Box::new(reduce_context(t, &vec![], rule)?)))
            }
            StageApp(box t, stage) if !t.is_value_at(&vec![]) => {
                Ok(StageApp(Box::new(reduce_context(t, &vec![], rule)?), stage))
            }
            StageApp(box StageLam(alpha, box t), stage) => {
                rule(StageApp(Box::new(StageLam(alpha, Box::new(t))), stage))
            }

            Let(x, box ty, box t1, box t2) => reduce_context(
                App(Box::new(Lam(x, Box::new(ty), Box::new(t2))), Box::new(t1)),
                stage,
                rule,
            ),
            Fix(x, box ty, box t) => {
                let fix = Fix(x.clone(), Box::new(ty), Box::new(t.clone()));
                Ok(t.subst_term(x, fix))
            }
            Ifz(box cond, box t1, box t2) if cond.is_value_at(&vec![]) => {
                if let Term::Const(Literal::Int(n)) = cond {
                    if n == 0 {
                        Ok(t1)
                    } else {
                        Ok(t2)
                    }
                } else {
                    no_reduction_err
                }
            }
            Ifz(box cond, box t1, box t2) => Ok(Ifz(
                Box::new(reduce_context(cond, stage, rule)?),
                Box::new(t1),
                Box::new(t2),
            )),
            Const(Literal::Vector(ts)) => {
                let res: Result<(Vec<Term>, bool), String> =
                    ts.into_iter()
                        .try_fold((vec![], false), |(mut acc, is_reduced), t| {
                            if !is_reduced && !t.is_value_at(&vec![]) {
                                let t = reduce_context(t, stage, rule)?;
                                acc.push(t);
                                Ok((acc, true))
                            } else {
                                acc.push(t);
                                Ok((acc, is_reduced))
                            }
                        });
                let (ts, is_reduced) = res?;
                if is_reduced {
                    Ok(Const(Literal::Vector(ts)))
                } else {
                    no_reduction_err
                }
            }
            _ => no_reduction_err,
        }
    } else {
        match term {
            Lam(x, ty, box t) if !t.is_value_at(stage) => {
                Ok(Lam(x, ty, Box::new(reduce_context(t, stage, rule)?)))
            }
            App(box t1, box t2) if !t1.is_value_at(stage) => Ok(App(
                Box::new(reduce_context(t1, &vec![], rule)?),
                Box::new(t2),
            )),
            App(box v, box t) if v.is_value_at(stage) && !t.is_value_at(stage) => {
                Ok(App(Box::new(v), Box::new(reduce_context(t, stage, rule)?)))
            }
            App(box v1, box v2) if v1.is_value_at(stage) && v2.is_value_at(stage) => {
                rule(App(Box::new(v1), Box::new(v2)))
            }
            Escape(alpha, box Code(alpha_, box v))
                if alpha == alpha_ && v.is_value_at(&vec![alpha.clone()]) =>
            {
                rule(Escape(alpha, Box::new(Code(alpha_, Box::new(v)))))
            }
            Code(alpha, box t) => {
                let mut stage = stage.clone();
                stage.push(alpha.clone());
                if !t.is_value_at(&stage) {
                    rule(Code(alpha, Box::new(reduce_context(t, &stage, rule)?)))
                } else {
                    no_reduction_err
                }
            }
            Escape(alpha, box t) => {
                let mut stage = stage.clone();
                if let Some(alpha_) = stage.pop() {
                    if alpha == alpha_ {
                        Ok(Escape(alpha, Box::new(reduce_context(t, &stage, rule)?)))
                    } else {
                        no_reduction_err
                    }
                } else {
                    no_reduction_err
                }
            }
            StageLam(alpha, box t) if !t.is_value_at(stage) => {
                Ok(StageLam(alpha, Box::new(reduce_context(t, stage, rule)?)))
            }
            StageApp(box t, stage) if !t.is_value_at(&stage) => {
                Ok(StageApp(Box::new(reduce_context(t, &stage, rule)?), stage))
            }
            StageApp(box StageLam(alpha, box t), stage) => {
                rule(StageApp(Box::new(StageLam(alpha, Box::new(t))), stage))
            }
            CSP(alpha, box t) => {
                let mut stage = stage.clone();
                if let Some(alpha_) = stage.pop() {
                    if alpha == alpha_ && !t.is_value_at(&stage) {
                        Ok(CSP(alpha, Box::new(reduce_context(t, &stage, rule)?)))
                    } else {
                        no_reduction_err
                    }
                } else {
                    no_reduction_err
                }
            }

            Let(x, box ty, box t1, box t2) => reduce_context(
                App(Box::new(Lam(x, Box::new(ty), Box::new(t2))), Box::new(t1)),
                stage,
                rule,
            ),
            Fix(x, box ty, box t) => {
                let fix = Fix(x.clone(), Box::new(ty), Box::new(t.clone()));
                Ok(t.subst_term(x, fix))
            }
            Ifz(box cond, box t1, box t2) if cond.is_value_at(stage) => {
                if let Term::Const(Literal::Int(n)) = cond {
                    if n == 0 {
                        Ok(t1)
                    } else {
                        Ok(t2)
                    }
                } else {
                    no_reduction_err
                }
            }
            Ifz(box cond, box t1, box t2) => Ok(Ifz(
                Box::new(reduce_context(cond, stage, rule)?),
                Box::new(t1),
                Box::new(t2),
            )),

            Const(Literal::Vector(ts)) => {
                let res: Result<(Vec<Term>, bool), String> =
                    ts.into_iter()
                        .try_fold((vec![], false), |(mut acc, is_reduced), t| {
                            if !is_reduced && !t.is_value_at(stage) {
                                let t = reduce_context(t, stage, rule)?;
                                acc.push(t);
                                Ok((acc, true))
                            } else {
                                acc.push(t);
                                Ok((acc, is_reduced))
                            }
                        });
                let (ts, is_reduced) = res?;
                if is_reduced {
                    Ok(Const(Literal::Vector(ts)))
                } else {
                    no_reduction_err
                }
            }

            _ => no_reduction_err,
        }
    }
}
