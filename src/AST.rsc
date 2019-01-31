module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str caption, str qname, AType type1)
  | question(str caption, str qname, AType type1, AExpr expr1)
  | block(list[AQuestion] questions)
  | ifthen(AExpr expr1, AQuestion question1)
  | ifelse(AExpr expr1, AQuestion question1, AQuestion question2)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | nat (int nat)
  | bol (bool bol)
  | brc (AExpr exp)
  | neg (AExpr exp)
  | mul (AExpr lhs, AExpr rhs)
  | div (AExpr lhs, AExpr rhs)
  | add (AExpr lhs, AExpr rhs)
  | sub (AExpr lhs, AExpr rhs)
  | lt (AExpr lhs, AExpr rhs)
  | leq (AExpr lhs, AExpr rhs)
  | gt (AExpr lhs, AExpr rhs)
  | geq (AExpr lhs, AExpr rhs)
  | equ (AExpr lhs, AExpr rhs)
  | neq (AExpr lhs, AExpr rhs)
  | and (AExpr lhs, AExpr rhs)
  | or (AExpr lhs, AExpr rhs)
  ;

data AType(loc src = |tmp:///|)
	= integer()
	| boolean()
	;
