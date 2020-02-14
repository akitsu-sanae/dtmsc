use crate::{ast::*, util::fresh_ident};
use peg;

pub fn pick_token(buf: &str) -> String {
    let buf = buf.trim_start();
    let single_char_tokens = vec!['.', '%', '@', '(', ')', '{', '}', ',', '*', ':'];
    let double_char_tokens = ["|>", "<|", "->"];
    let mut chars = buf.chars();
    if let Some(c) = chars.next() {
        if c.is_alphabetic() {
            // identifier
            let mut ident = c.to_string();
            while let Some(c) = chars.next() {
                if c.is_alphanumeric() || c == '_' {
                    ident.push(c);
                } else {
                    break;
                }
            }
            ident
        } else if single_char_tokens.contains(&c) {
            c.to_string()
        } else if let Some(c2) = chars.next() {
            let token = format!("{}{}", c, c2);
            if double_char_tokens.contains(&token.as_str()) {
                token
            } else {
                format!("<unknown token starting with {}>", c)
            }
        } else {
            format!("<unknown token starting with {}>", c)
        }
    } else {
        "<eof>".to_string()
    }
}

peg::parser!(grammar rules() for str {

pub rule term() -> Term
    = apply_stage_term()

rule apply_stage_term() -> Term
    = t:apply_term() stages:(AT() stage:stage() {stage})* {
        stages.into_iter().fold(t, |acc, stage| Term::StageApp(Box::new(acc), stage))
    }

rule apply_term() -> Term
    = ts:factor_term()+ {
        let mut ts = ts;
        let head = ts.remove(0);
        ts.into_iter().fold(head, |acc, t| Term::App(Box::new(acc), Box::new(t)))
    }

rule factor_term() -> Term
    = LAM() ident:ident() COLON() ty:type_() DOT() t:term() { Term::Lam(TermVar(ident), Box::new(ty), Box::new(t)) }
    / CODE() ident:ident() DOT() t:term() { Term::Code(StageVar(ident), Box::new(t)) }
    / ESCAPE() ident:ident() DOT() t:term() { Term::Escape(StageVar(ident), Box::new(t)) }
    / STAGE_LAM() ident:ident() DOT() t:term() { Term::StageLam(StageVar(ident), Box::new(t)) }
    / CSP() ident:ident() DOT() t:term() {Term::CSP(StageVar(ident), Box::new(t))}
    / n:number() { Term::Const(Literal::Int(n)) }
    / PLUS() { Term::Const(Literal::Add) }
    / MINUS() { Term::Const(Literal::Sub) }
    / AST() { Term::Const(Literal::Mult) }
    / SLASH() { Term::Const(Literal::Div) }
    / ident:ident() { Term::Var(TermVar(ident)) }
    / paren_term()

rule paren_term() -> Term
  = LPAREN() t:term() RPAREN() { t }

pub rule kind() -> Kind
  = AST() {  Kind::ProperType }
  / PI() ident:ident() COLON() ty:type_() DOT() kind:kind() {
      Kind::TypeOperation(TermVar(ident), Box::new(ty), Box::new(kind))
  }

pub rule type_() -> Type
  = arrow_type()

pub rule arrow_type() -> Type
  = from:app_type() ARROW() to:arrow_type() {
    Type::DepFun(TermVar(fresh_ident()), Box::new(from), Box::new(to))
  }
  / app_type()


rule app_type() -> Type
  = ty:factor_type() LBRACKET() t:term() RBRACKET() { Type::App(Box::new(ty), Box::new(t)) }
  / factor_type()

rule factor_type() -> Type
  = INT() { Type::Int }
  / VECTOR() { Type::Vector }
  / CODE() ident:ident() DOT() ty:type_() { Type::Code(StageVar(ident), Box::new(ty)) }
  / FORALL() ident:ident() DOT() ty:type_() { Type::StageBind(StageVar(ident), Box::new(ty)) }
  / PI() ident:ident() COLON() ty1:type_() DOT() ty2:type_() {
      Type::DepFun(TermVar(ident), Box::new(ty1), Box::new(ty2))
  }

rule stage() -> Vec<StageVar>
  = LBRACE() head:ident() tail:(COMMA() i:ident() {i})* RBRACE() {
      let mut stage: Vec<_> = tail.into_iter().map(|ident| StageVar(ident)).collect();
      stage.insert(0, StageVar(head));
      stage
  }
  / LBRACE() RBRACE() { vec![] }

rule number() -> i32
    = n:$(['0'..='9']+) __ { n.parse().unwrap() }

rule ident() -> String
    = s:$(quiet!{['a'..='z'|'A'..='Z']['a'..='z'|'A'..='Z'|'0'..='9'|'_']*}) __ { s.to_string() }
    / expected!("<identifier>")

rule __() = [' '|'\t'|'\r'|'\n']*

rule INT() = "int" __
rule VECTOR() = "vector" __
rule FORALL() = "forall" __
rule LAM() = "lam" __
rule STAGE_LAM() = "LAM" __
rule PI() = "Pi" __

rule PLUS() = "+" __
rule MINUS() = "-" __
rule AST() = "*" __
rule SLASH() = "/" __

rule DOT() = "." __
rule COMMA() = "," __
rule COLON() = ":" __
rule ARROW() = "->" __
rule CODE() = "|>" __
rule ESCAPE() = "<|" __
rule CSP() = "%" __
rule AT() = "@" __

rule LPAREN() = "(" __
rule RPAREN() = ")" __
rule LBRACE() = "{" __
rule RBRACE() = "}" __
rule LBRACKET() = "[" __
rule RBRACKET() = "]" __
});

pub use rules::*;

#[test]
fn term_test() {
    assert_eq!(
        term("lam x: int. 42"),
        Ok(Term::Lam(
            TermVar("x".to_string()),
            Box::new(Type::Int),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(
        term("|>a. 42"),
        Ok(Term::Code(
            StageVar("a".to_string()),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(
        term("<|a. 42"),
        Ok(Term::Escape(
            StageVar("a".to_string()),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(
        term("LAM a. 42"),
        Ok(Term::StageLam(
            StageVar("a".to_string()),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(
        term("%a. 42"),
        Ok(Term::CSP(
            StageVar("a".to_string()),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(term("123"), Ok(Term::Const(Literal::Int(123))));
    assert_eq!(
        term("f 42"),
        Ok(Term::App(
            Box::new(Term::Var(TermVar("f".to_string()))),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
}

#[test]
fn kind_test() {
    assert_eq!(kind("*"), Ok(Kind::ProperType));

    assert_eq!(
        kind("Pi x:int. *"),
        Ok(Kind::TypeOperation(
            TermVar("x".to_string()),
            Box::new(Type::Int),
            Box::new(Kind::ProperType)
        ))
    );

    assert_eq!(
        kind("Pi x:int. Pi y:int. *"),
        Ok(Kind::TypeOperation(
            TermVar("x".to_string()),
            Box::new(Type::Int),
            Box::new(Kind::TypeOperation(
                TermVar("y".to_string()),
                Box::new(Type::Int),
                Box::new(Kind::ProperType)
            ))
        ))
    );
}

#[test]
fn type_test() {
    assert_eq!(type_("int"), Ok(Type::Int));
    assert_eq!(type_("vector"), Ok(Type::Vector));
    assert_eq!(
        type_("Pi x:int. int"),
        Ok(Type::DepFun(
            TermVar("x".to_string()),
            Box::new(Type::Int),
            Box::new(Type::Int)
        ))
    );
    assert_eq!(
        type_("vector[42]"),
        Ok(Type::App(
            Box::new(Type::Vector),
            Box::new(Term::Const(Literal::Int(42)))
        ))
    );
    assert_eq!(
        type_("|>a. int"),
        Ok(Type::Code(StageVar("a".to_string()), Box::new(Type::Int)))
    );
    assert_eq!(
        type_("forall a. int"),
        Ok(Type::StageBind(
            StageVar("a".to_string()),
            Box::new(Type::Int)
        ))
    );

    assert_eq!(
        type_("Pi x:vector[n]. int"),
        Ok(Type::DepFun(
            TermVar("x".to_string()),
            Box::new(Type::App(
                Box::new(Type::Vector),
                Box::new(Term::Var(TermVar("n".to_string())))
            )),
            Box::new(Type::Int)
        ))
    );
    assert_eq!(
        type_("int -> int"),
        Ok(Type::DepFun(
            TermVar("<dummy0>".to_string()),
            Box::new(Type::Int),
            Box::new(Type::Int)
        ))
    );
}
