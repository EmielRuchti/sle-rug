module CST2AST

import Syntax;
import AST;
import Boolean;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return cst2ast(f); 
}

AForm cst2ast(f:(Form)`form <Id x> { <Question* qs> }`) {
  return form("<x>", [cst2ast(q) | Question q <- qs], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
    switch (q) {
    	case (Question)`<Str x> <Id y> : <Type t>`: return question("<x>", "<y>", cst2ast(t), src=q@\loc);
    	case (Question)`<Str x> <Id y> : <Type t> = <Expr e>`: return question("<x>","<y>",cst2ast(t),cst2ast(e), src=q@\loc);
    	case (Question)`{<Question* qs> }` : return block([cst2ast(qt) | Question qt <- qs], src=q@\loc);
    	case (Question)`if (<Expr e>) <Question q>`: return ifthen(cst2ast(e),cst2ast(q), src=q@\loc);
    	case (Question)`if (<Expr e>) <Question q1> else <Question q2>`: return ifelse(cst2ast(e),cst2ast(q1),cst2ast(q2), src=q@\loc);

    	default: throw "Unhandled question: <q>";
	}
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref("<x>", src=x@\loc);
    case (Expr)`<Int x>`: return nat(toInt("<x>"), src=x@\loc);
    case (Expr)`<Bool x>`: return bol(fromString("<x>"), src=x@\loc);
    case (Expr)`( <Expr exp> )` : return brc(cst2ast(exp),src=e@\loc);
    case (Expr)`!<Expr exp>`: return neg(cst2ast(exp),src=e@\loc);
    case (Expr)`<Expr lhs> * <Expr rhs>`: return mul(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> / <Expr rhs>`: return div(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> + <Expr rhs>`: return add(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> - <Expr rhs>`: return sub(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \< <Expr rhs>`: return lt(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \<= <Expr rhs>`: return leq(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \> <Expr rhs>`: return gt(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \>= <Expr rhs>`: return geq(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> == <Expr rhs>`: return equ(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> != <Expr rhs>`: return neq(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> && <Expr rhs>`: return and(cst2ast(lhs),cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> || <Expr rhs>`: return or(cst2ast(lhs),cst2ast(rhs), src=e@\loc);

    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
  	case (Type)`boolean`: return boolean(src=t@\loc);
  	case (Type)`integer`: return integer(src=t@\loc);
  }
}
