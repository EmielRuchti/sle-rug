module Transform

import Resolve;
import AST;
import List;
import IO;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; if (a) { if (b) { q1: "" int; } q2: "" int; }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  list[AQuestion] flat = [];
  flat = flatten(f.questions,[]);
  return form(f.name,flat); 
}

list[AQuestion] flatten(list[AQuestion] qs, list[AExpr] exprs) {

  list[AQuestion] flat = [];
  
   for (AQuestion q <- qs) {
    switch(q) {
      case question(str caption, str qname, AType type1): 
      {
        AExpr al;
        if (isEmpty(exprs)) {
         al = bol(true);
        } else {
         al = head(exprs);
         for(AExpr e <- exprs) {
           if (e!=head(exprs)) {
           	al = and(al,e);
           }
         }
        }
        flat += ifthen(al,q);
      }
      case question(str caption, str qname, AType type1,AExpr expr1): 
      {
        AExpr al;
        if (isEmpty(exprs)) {
         al = bol(true);
        } else {
         al = head(exprs);
         for(AExpr e <- exprs) {
           if (e!=head(exprs)) {
           	al = and(al,e);
           }
         }
        }
        flat += ifthen(al,q);
      }
      case block(list[AQuestion] questions):
      {
        flat += flatten(questions,exprs);
      }
      case ifthen(AExpr expr1, AQuestion question1):
      {
      flat += flatten([question1],exprs+expr1);
      }
      case ifelse(AExpr expr1, AQuestion question1, AQuestion question2):
      {
      flat += flatten([question1],exprs+expr1) + flatten([question2], exprs + neg(expr1));
      }
    }
  }
  
  return flat;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 * Bonus: do it on concrete syntax trees.
 */
 
 AForm rename(AForm f, loc useOrDef, str newName, UseDef useDef) {
   
   set[loc] reNaming = {useOrDef} + {l | <useOrDef, loc l> <-useDef} + {l | <loc l, useOrDef> <- useDef};

   return visit(f){
     case q0: question(str caption,str name,AType t) => question(caption,newName,t) when (q0.src) in reNaming
     case q1: question(str caption,str name,AType t,AExpr e) => question(caption,newName,t,e) when (q1.src) in reNaming
     case q2: ref(str name) => ref(newName) when (q2.src) in reNaming
   }; 
} 
 
 
 

