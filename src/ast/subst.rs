use crate::ast::*;

impl Term {
    pub fn subst_term(self, x: TermVar, t: Term) -> Term {
        match self {
            Term::Var(x_) if x == x_ => t,
            Term::Lam(x_, box ty, box t_) if x != x_ => Term::Lam(
                x_,
                Box::new(ty.subst_term(x.clone(), t.clone())),
                Box::new(t_.subst_term(x, t)),
            ),
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

impl Type {
    pub fn subst_term(self, x: TermVar, t: Term) -> Type {
        use Type::*;
        match self {
            Int => Int,
            Vector => Vector,
            DepFun(x_, box ty1, box ty2) if x != x_ => DepFun(
                x_,
                Box::new(ty1.subst_term(x.clone(), t.clone())),
                Box::new(ty2.subst_term(x, t)),
            ),
            DepFun(x_, box ty1, box ty2) => {
                DepFun(x_.clone(), Box::new(ty1.clone()), Box::new(ty2.clone()))
            }
            App(box ty, box arg) => App(
                Box::new(ty.subst_term(x.clone(), t.clone())),
                Box::new(arg.subst_term(x, t)),
            ),
            Code(alpha, box ty) => Code(alpha, Box::new(ty.subst_term(x, t))),
            StageBind(alpha, box ty) => StageBind(alpha, Box::new(ty.subst_term(x, t))),
        }
    }

    pub fn subst_stage(self, alpha: StageVar, stage: Stage) -> Type {
        use Type::*;
        match self {
            Int => Int,
            Vector => Vector,
            DepFun(x_, box ty1, box ty2) => DepFun(
                x_,
                Box::new(ty1.subst_stage(alpha.clone(), stage.clone())),
                Box::new(ty2.subst_stage(alpha, stage)),
            ),
            App(box ty, box arg) => App(
                Box::new(ty.subst_stage(alpha.clone(), stage.clone())),
                Box::new(arg.subst_stage(alpha, stage)),
            ),
            Code(alpha_, box ty) if alpha == alpha_ => {
                let ty = ty.subst_stage(alpha, stage.clone());
                stage
                    .into_iter()
                    .rev()
                    .fold(ty, |acc, stage_var| Code(stage_var, Box::new(acc)))
            }
            Code(alpha, box ty) => Code(alpha.clone(), Box::new(ty.subst_stage(alpha, stage))),
            StageBind(alpha, box ty) => {
                StageBind(alpha.clone(), Box::new(ty.subst_stage(alpha, stage)))
            }
        }
    }
}
