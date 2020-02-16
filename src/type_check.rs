use crate::{ast::*, util::*};
use std::collections::HashMap;

fn type_check_impl(
    term: &Term,
    stage: &Stage,
    env: &HashMap<TermVar, (Type, Stage)>,
) -> Result<Type, String> {
    use Term::*;
    match term {
        Const(Literal::Int(_)) => Ok(Type::Int),
        Const(_) => {
            let ty1 = TermVar(fresh_ident());
            let ty2 = TermVar(fresh_ident());
            Ok(Type::DepFun(
                ty1,
                Box::new(Type::Int),
                Box::new(Type::DepFun(ty2, Box::new(Type::Int), Box::new(Type::Int))),
            ))
        }
        Var(ref var) => {
            if let Some(&(ref ty, ref stage_)) = env.get(var) {
                if stage == stage_ {
                    Ok(ty.clone())
                } else {
                    Err(format!("{:?} is not at {:?}. but {:?}", var, stage, stage_))
                }
            } else {
                Err(format!("no found variable: {:?}", var))
            }
        }
        Lam(ref var, box ref arg_ty, box ref t) => {
            let mut env = env.clone();
            env.insert(var.clone(), (arg_ty.clone(), stage.clone()));
            let ret_ty = type_check_impl(t, stage, &env)?;
            Ok(Type::DepFun(
                var.clone(),
                Box::new(arg_ty.clone()),
                Box::new(ret_ty.clone()),
            ))
        }
        App(box ref t1, box ref t2) => {
            if let Type::DepFun(ref x, box ref param_ty, box ref body_ty) =
                type_check_impl(t1, stage, env)?
            {
                let arg_ty = type_check_impl(t2, stage, env)?;
                if &arg_ty == param_ty
                    || input_yes_or_no(&format!("are {} and {} same? (y/n): ", arg_ty, param_ty))
                {
                    Ok(body_ty.clone().subst_term(x.clone(), t2.clone()))
                } else {
                    Err(format!("{} and {} must match", arg_ty, param_ty))
                }
            } else {
                Err(format!("{} must have function type", t1))
            }
        }
        Code(ref alpha, box ref t) => {
            let mut stage = stage.clone();
            stage.push(alpha.clone());
            let ty = type_check_impl(t, &stage, env)?;
            Ok(Type::Code(alpha.clone(), Box::new(ty)))
        }
        Escape(ref alpha, box ref t) => {
            let mut stage = stage.clone();
            if let Some(alpha_) = stage.pop() {
                if alpha == &alpha_ {
                    if let Type::Code(alpha_, box ty) = type_check_impl(t, &stage, env)? {
                        if alpha == &alpha_ {
                            Ok(ty)
                        } else {
                            Err(format!("{:?} and {:?} must match", alpha, alpha_))
                        }
                    } else {
                        Err(format!("cannot escape non-code term: {}", t))
                    }
                } else {
                    Err(format!("{:?} and {:?} must match", alpha, alpha_))
                }
            } else {
                Err(format!("stage for escape cannot be empty"))
            }
        }
        StageLam(ref alpha, box ref t) => {
            let ty = type_check_impl(t, stage, env)?;
            Ok(Type::StageBind(alpha.clone(), Box::new(ty)))
        }
        StageApp(box ref t, ref stage_) => {
            if let Type::StageBind(alpha, ty) = type_check_impl(t, stage, env)? {
                Ok(ty.subst_stage(alpha, stage_.clone()))
            } else {
                Err(format!("cannot stage-apply for non-stage-lambda term"))
            }
        }
        CSP(ref alpha, box ref t) => {
            let mut stage = stage.clone();
            if let Some(alpha_) = stage.pop() {
                if alpha == &alpha_ {
                    type_check_impl(t, &stage, env)
                } else {
                    Err(format!("{:?} and {:?} must match", alpha, alpha_))
                }
            } else {
                Err(format!("stage for CSP cannot be empty"))
            }
        }

        Let(ref x, box ref ty, box ref t1, box ref t2) => type_check_impl(
            &App(
                Box::new(Lam(x.clone(), Box::new(ty.clone()), Box::new(t2.clone()))),
                Box::new(t1.clone()),
            ),
            stage,
            env,
        ),

        Fix(ref x, box ref ty, box ref t) => {
            let mut env = env.clone();
            env.insert(x.clone(), (ty.clone(), stage.clone()));
            let ret_ty = type_check_impl(t, stage, &env)?;
            if input_yes_or_no(&format!("are {} and {} same? (y/n): ", ty, ret_ty)) {
                Ok(ret_ty)
            } else {
                Err(format!("{} and {} must match", ty, ret_ty))
            }
        }
        Ifz(box ref cond, box ref t1, box ref t2) => {
            let cond_ty = type_check_impl(cond, stage, env)?;
            if Type::Int == cond_ty {
                let ty1 = type_check_impl(t1, stage, env)?;
                let ty2 = type_check_impl(t2, stage, env)?;
                if input_yes_or_no(&format!("are {} and {} same? (y/n): ", ty1, ty2)) {
                    Ok(ty1)
                } else {
                    Err(format!("{} and {} must match", ty1, ty2))
                }
            } else {
                Err(format!("condition must be integer, but {}", cond_ty))
            }
        }
    }
}

impl Term {
    pub fn type_check(&self, stage: &Stage) -> Result<Type, String> {
        type_check_impl(self, stage, &HashMap::new())
    }
}
