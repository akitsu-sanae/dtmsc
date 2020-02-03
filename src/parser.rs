use crate::ast::*;
use peg;

peg::parser!(grammar parser() for str {

pub rule term() -> Term
    = apply_term()

rule apply_term() -> Term
    = t1:factor_term() t2:apply_term() { Term::App(Box::new(t1), Box::new(t2)) }
    / t1:factor_term() AT() stage:stage() { Term::StageApp(Box::new(t1), stage) }
    / factor_term()

rule factor_term() -> Term
    = LAM() ident:ident() DOT() t:term() { Term::Lam(TermVar(ident), Box::new(t)) }
    / CODE() ident:ident() DOT() t:term() { Term::Code(StageVar(ident), Box::new(t)) }
    / ESCAPE() ident:ident() DOT() t:term() { Term::Escape(StageVar(ident), Box::new(t)) }
    / STAGE_LAM() ident:ident() DOT() t:term() { Term::StageLam(StageVar(ident), Box::new(t)) }
    / CSP() ident:ident() DOT() t:term() {Term::CSP(StageVar(ident), Box::new(t))}
    / n:number() { Term::Int(n) }
    / ident:ident() { Term::Var(TermVar(ident)) }
    / paren_term()

rule paren_term() -> Term
  = LPAREN() t:term() RPAREN() { t }

rule stage() -> Vec<StageVar>
  = LPAREN() head:ident() tail:(COMMA() i:ident() {i})* RPAREN() {
      let mut stage: Vec<_> = tail.into_iter().map(|ident| StageVar(ident)).collect();
      stage.insert(0, StageVar(head));
      stage
  }
  / LPAREN() RPAREN() { vec![] }

rule number() -> i32
    = n:$(['0'..='9']+) __ { n.parse().unwrap() }

rule ident() -> String
    = s:$(quiet!{['a'..='z'|'A'..='Z']['a'..='z'|'A'..='Z'|'0'..='9'|'_']*}) __ { s.to_string() }
    / expected!("identifier")

rule __() = [' '|'\t'|'\r'|'\n']*

rule LAM() = "lam" __
rule DOT() = "." __
rule CODE() = "|>" __
rule ESCAPE() = "<|" __
rule STAGE_LAM() = "LAM" __
rule CSP() = "%" __
rule AT() = "@" __
rule LPAREN() = "(" __
rule RPAREN() = ")" __
rule COMMA() = "," __
});

pub use parser::*;

#[test]
fn term_test() {
    assert_eq!(
        term("lam x. 42"),
        Ok(Term::Lam(TermVar("x".to_string()), Box::new(Term::Int(42))))
    );
    assert_eq!(
        term("|>a. 42"),
        Ok(Term::Code(
            StageVar("a".to_string()),
            Box::new(Term::Int(42))
        ))
    );
    assert_eq!(
        term("<|a. 42"),
        Ok(Term::Escape(
            StageVar("a".to_string()),
            Box::new(Term::Int(42))
        ))
    );
    assert_eq!(
        term("LAM a. 42"),
        Ok(Term::StageLam(
            StageVar("a".to_string()),
            Box::new(Term::Int(42))
        ))
    );
    assert_eq!(
        term("%a. 42"),
        Ok(Term::CSP(
            StageVar("a".to_string()),
            Box::new(Term::Int(42))
        ))
    );
    assert_eq!(term("123"), Ok(Term::Int(123)));
    assert_eq!(
        term("f 42"),
        Ok(Term::App(
            Box::new(Term::Var(TermVar("f".to_string()))),
            Box::new(Term::Int(42))
        ))
    );
}
