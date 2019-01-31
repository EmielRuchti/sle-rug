module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  =  Str Id ":" Type
  | Str Id ":" Type "=" Expr
  | block: "{" Question* "}"
  | "if" "(" Expr ")" Question
  | "if" "(" Expr ")" Question "else" Question
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Bool
  | Int
  | Str
  | "(" Expr ")"
  | "!" Expr
   >
  left (
      mul: Expr "*" !>> [*=] Expr
    | div: Expr "/" !>> [/=] Expr
  )
  >
  left (
      add: Expr "+" !>> [+=]  Expr
    | sub: Expr "-" !>> [\-=] Expr
  )
  >
  non-assoc (
      lt: Expr "\<" Expr
    | leq: Expr "\<=" Expr
    | gt: Expr "\>" Expr
    | geq: Expr "\>=" Expr
  )
  >
  right (
      equ: Expr "==" !>> [=] Expr 
    | neq: Expr "!=" !>> [=] Expr
  )
  > left and: Expr "&&" Expr
  > left or: Expr "||" Expr
  ;
  
syntax Type = "integer" | "boolean";  
  
lexical Str
  = [\"][a-zA-Z0-9_?:\t-\n\r\ ]*[\"] 
  ;
 
lexical Int = [0-9]+;

lexical Bool = "true" | "false" ;

