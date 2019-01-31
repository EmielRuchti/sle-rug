module Eval

import AST;
import Resolve;
import IO;
import util::Math;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);


Value valueOf(AType at) {
  Value v;
  switch(at) {
      case boolean(src = loc u): v = vbool(false);
      case integer(src = loc u): v = vint(0);
  }
  return v;
}
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv e = ();
  for(/q:question(_,str x,AType at) := f) {
    e += (x:valueOf(at));
  }
  for(/q:question(_,str x,AType at,_) := f) {
    e += (x:valueOf(at));
  }
  return e;
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for(/AQuestion q := f) {
  	venv = eval(q,inp,venv);
  }
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {

  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  
  switch(q) {
   case question(str caption,str name, AType t): if(inp.question == name) { venv[name] = inp.\value; }
   case question(str caption,str name, AType t,AExpr e):  venv[name] = eval(e,venv);
   case ifthen(AExpr e, AQuestion question1): if(eval(e,venv) == vbool(true)) { return eval(question1,inp,venv); }
   case ifelse(AExpr e, AQuestion question1, AQuestion question2): return (eval(e,venv) == vbool(true)) ? eval(question1,inp,venv) : eval(question2,inp,venv);
  }
  return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(str x): return venv[x];
    case nat(int nat): return vint(nat);
    case bol(bool bol): return vbool(bol);
    case brc(AExpr exp): return eval(exp,venv);
    case neg(AExpr exp): return vbool(!eval(exp,venv).b);
    case mul(AExpr lhs, AExpr rhs): return vint(eval(lhs,venv).n * eval(rhs,venv).n);
    case div(AExpr lhs, AExpr rhs): return vint(eval(lhs,venv).n / eval(rhs,venv).n);
    case add(AExpr lhs, AExpr rhs): return vint((eval(lhs,venv).n) + eval(rhs,venv).n);
    case sub(AExpr lhs, AExpr rhs): return vint(eval(lhs,venv).n - eval(rhs,venv).n);
    case lt(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n < eval(rhs,venv).n);
    case leq(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n <= eval(rhs,venv).n);
    case gt(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n > eval(rhs,venv).n);
    case geq(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n >= eval(rhs,venv).n);
    case equ(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n == eval(rhs,venv).n);
    case neq(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).n != eval(rhs,venv).n);
    case and(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).b && eval(rhs,venv).b);
    case or(AExpr lhs, AExpr rhs): return vbool(eval(lhs,venv).b || eval(rhs,venv).b);
    
    default: throw "Unsupported expression <e>";
  }
}