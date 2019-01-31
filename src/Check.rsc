module Check

import AST;
import Resolve;
import Message; // see standard library
import Set;
import IO;
import List;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv e = {};
  Type t;
  for(/q:question(_,_,AType at) := f) {
     t = typeOf(at);
   	 e += {<q.src,q.qname,q.caption,t>};
  }
    for(/q:question(_,_,AType at,_) := f) {
     t = typeOf(at);
   	 e += {<q.src,q.qname,q.caption,t>};
  }

  return e; 
   
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  return ({} | check(q,tenv,useDef)| AQuestion q <- f.questions); 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};
	switch (q) {
	 case question(str caption,str name, AType t, src = loc u):
       msgs += { error("Declared questions have same name but different types", u) | size([y,w | <x,y,z,w> <-tenv,y==name,w!=typeOf(t)])>0}
       + { warning("Duplicate labels",u)| (size([z | <x,y,z,w> <-tenv,z==caption])>1)};
	 case question(str caption,str name, AType t,AExpr e,src = loc u): 
	   msgs += { error("Declared questions have same name but different types", u) | size([y,w | <x,y,z,w> <-tenv,y==name,w!=typeOf(t)])>0} 
	   + { error("Declared type computed questions should match the type of expression",u) | typeOf(t) != typeOf(e,tenv,useDef)}
	   + { warning("Duplicate labels",u)| (size([z | <x,y,z,w> <-tenv,z==caption])>1)}
	   + check(e,tenv,useDef);
	 case ifthen(AExpr e, AQuestion, src = loc u): 
	   msgs += {error ("Condition is not boolean",u) | typeOf(e,tenv,useDef) != tbool()}
	   + check(e,tenv,useDef); 
	 case ifelse(AExpr e, AQuestion question1, AQuestion question2, src = loc u):
	    msgs += {error ("Condition is not boolean",u) | typeOf(e,tenv,useDef) != tbool()}
	    + check(e,tenv,useDef);
	 }
  return msgs;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
    case brc(AExpr exp, src = loc u):
      msgs += check(exp,tenv,useDef);
    case mul(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types",u) | typeOf(lhs,tenv,useDef) != tint() || typeOf(rhs,tenv,useDef) != tint()};
    case div(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != tint() || typeOf(rhs,tenv,useDef) != tint()};
    case add(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != tint() || typeOf(rhs,tenv,useDef) != tint()};
    case sub(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != tint() || typeOf(rhs,tenv,useDef) != tint()}; 
    case lt(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case leq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};   
    case gt(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case geq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case equ(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case neq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case and(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};
    case or(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Invalid operand types", u) | typeOf(lhs,tenv,useDef) != typeOf(rhs,tenv,useDef)};                         
  }
  
  return msgs; 
}

Type typeOf(AType at) {
  Type t;
  switch(at) {
      case boolean(src = loc u): t = tbool();
      case integer(src = loc u): t = tint();
  }
  return t;
}


Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case brc(AExpr exp, src = loc u):
     return typeOf(exp, tenv,useDef);
    case exp(AExpr lhs, AExpr rhs, src = loc u): 
      if (typeOf(lhs,tenv,useDef) == typeOf(rhs,tenv,useDef)) {
        return typeOf(lhs,tenv,useDef);
      }  
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

