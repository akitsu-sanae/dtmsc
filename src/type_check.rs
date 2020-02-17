use crate::{ast::*, util::*};
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeCheckError {
    NotMatch(Type, Type, Stage),
    StageNotMatch(StageVar, StageVar, Stage),
    NotFoundVar(TermVar, HashMap<TermVar, (Type, Stage)>, Stage),
    MustBe(Term, Type, Stage, String),
    CannotBeEmptyStage,
    Other(String), // remove later
}

use std::fmt;
impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TypeCheckError::*;
        match self {
            NotMatch(ty1, ty2, stage) => {
                write!(f, "{} and {} must match under {:?}", ty1, ty2, stage)
            }
            StageNotMatch(StageVar(alpha1), StageVar(alpha2), stage) => write!(
                f,
                "{} and {} must be same under {:?}",
                alpha1, alpha2, stage
            ),
            NotFoundVar(TermVar(ident), env, stage) => write!(
                f,
                "variable {} not found in {:?} under {:?}",
                ident, env, stage
            ),
            MustBe(term, ty, stage, desc) => write!(
                f,
                "{} must be {}, but in {} under {:?}",
                term, desc, ty, stage
            ),
            CannotBeEmptyStage => write!(f, "cannot be empty stage"), // TODO
            Other(msg) => write!(f, "{}", msg),
        }
    }
}

use std::error::Error;

impl Error for TypeCheckError {
    fn description(&self) -> &str {
        "error in type checking"
    }
}

fn type_check_impl(
    term: &Term,
    stage: &Stage,
    env: &HashMap<TermVar, (Type, Stage)>,
) -> Result<Type, TypeCheckError> {
    use Term::*;
    use TypeCheckError::*;
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
                    Err(Other(format!(
                        "{:?} is not at {:?}, but {:?}",
                        var, stage, stage_
                    ))) // TODO
                }
            } else {
                Err(NotFoundVar(var.clone(), env.clone(), stage.clone()))
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
            let ty = type_check_impl(t1, stage, env)?;
            if let Type::DepFun(ref x, box ref param_ty, box ref body_ty) = ty {
                let arg_ty = type_check_impl(t2, stage, env)?;
                if &arg_ty == param_ty
                    || input_yes_or_no(&format!(
                        "are\n  {} and\n  {}\nsame? (y/n): ",
                        arg_ty, param_ty
                    ))
                {
                    Ok(body_ty.clone().subst_term(x.clone(), t2.clone()))
                } else {
                    Err(NotMatch(arg_ty, param_ty.clone(), stage.clone()))
                }
            } else {
                Err(MustBe(
                    t1.clone(),
                    ty,
                    stage.clone(),
                    "function".to_string(),
                ))
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
                    let ty = type_check_impl(t, &stage, env)?;
                    if let Type::Code(alpha_, box ty) = ty {
                        if alpha == &alpha_ {
                            Ok(ty)
                        } else {
                            Err(StageNotMatch(alpha.clone(), alpha_, stage.clone()))
                        }
                    } else {
                        Err(MustBe(t.clone(), ty, stage.clone(), "code".to_string()))
                    }
                } else {
                    Err(StageNotMatch(alpha.clone(), alpha_, stage.clone()))
                }
            } else {
                Err(CannotBeEmptyStage)
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
                Err(Other(format!(
                    "cannot stage-apply for non-stage-lambda term"
                ))) // TODO
            }
        }
        CSP(ref alpha, box ref t) => {
            let mut stage = stage.clone();
            if let Some(alpha_) = stage.pop() {
                if alpha == &alpha_ {
                    type_check_impl(t, &stage, env)
                } else {
                    Err(StageNotMatch(alpha.clone(), alpha_, stage.clone()))
                }
            } else {
                Err(Other(format!("stage for CSP cannot be empty"))) // TODO
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
            if input_yes_or_no(&format!("are\n  {} and\n  {}\nsame? (y/n): ", ty, ret_ty)) {
                Ok(ret_ty)
            } else {
                Err(NotMatch(ty.clone(), ret_ty, stage.clone()))
            }
        }
        Ifz(box ref cond, box ref t1, box ref t2) => {
            let cond_ty = type_check_impl(cond, stage, env)?;
            if Type::Int == cond_ty {
                let ty1 = type_check_impl(t1, stage, env)?;
                let ty2 = type_check_impl(t2, stage, env)?;
                if input_yes_or_no(&format!("are\n  {} and\n  {}\nsame? (y/n): ", ty1, ty2)) {
                    Ok(ty1)
                } else {
                    Err(NotMatch(ty1, ty2, stage.clone()))
                }
            } else {
                Err(MustBe(
                    cond.clone(),
                    cond_ty,
                    stage.clone(),
                    "int".to_string(),
                ))
            }
        }
    }
}

impl Term {
    pub fn type_check(&self, stage: &Stage) -> Result<Type, TypeCheckError> {
        type_check_impl(self, stage, &HashMap::new())
    }
}
