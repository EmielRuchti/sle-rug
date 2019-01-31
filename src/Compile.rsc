module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
	writeFile(f.src[extension="js"].top, form2js(f));
	writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
	return html(
    	head(
    		title("QL"), 
    		script(src("https://cdn.jsdelivr.net/npm/vue/dist/vue.js")),
      		script(src(f.src[extension="js"].file))
      	),
    	body(
      		div(id("Form"),
        		div([question2html(q) | AQuestion q <- f.questions])
      		)      	
    	)
  	);
}


HTML5Node question2html(AQuestion q) {
  switch(q) {
    case question(str caption, str qname, AType type1):
      return div(
        label(\for("<qname>"), caption),
        input(name(qname), html5attr("v-model",qname), type2html(q.type1))
      );
    case question(str caption, str qname, AType type1, AExpr expression):
      return div(
        label(\for("<qname>"), caption),
        input(name(qname), html5attr("v-model",qname), type2html(q.type1), readonly([]))
      );
    case block(list[AQuestion] qs):
      return div([question2html(q2) | AQuestion q2 <- qs]);
    case ifelse(AExpr expr1, AQuestion question1, AQuestion question2):
      return 
        div(html5attr("v-if",sourceLocationToIdentifier(expr1.src)), question2html(question1), question2html(question2));
    case ifthen(AExpr expr1, AQuestion question1):
      return 
        div(html5attr("v-if",sourceLocationToIdentifier(expr1.src)), question2html(question1));
    default: throw "Unsupported question type encountered";
  }
}

HTML5Attr type2html(boolean()) = \type("checkbox");
HTML5Attr type2html(integer()) = \type("number");

str sourceLocationToIdentifier(loc source)
  =  "expr_<source.offset>_<source.length>_<source.begin.line>_<source.begin.column>_<source.end.line>_<source.end.column>";


str form2js(AForm f) {
	return "var app = new Vue({
    	'  el: \'#app\',
        '  data: {
        '    <for (/AQuestion q := f) {><if (q has qname && !(q has expr1)) {>
        '    <q.qname>: <type2js(q.type1)>,<}><}>
        '  },
        '  computed: {
        '    <for (/AQuestion q := f) {><if (q has qname && q has expr1) {>
        '    <q.qname>:  function() {
        '      return <expr2js(q.expr1)>;
        '    },<}><}>
        '    <for (/AQuestion q := f) {><if (q has expr1 && q has question2) {>
        '    <sourceLocationToIdentifier(q.expr1.src)>:  function() {
        '      return <expr2js(q.expr1)>;
        '    }<}><}>
        '  }
        '});
        ";
}

str expr2js(AExpr ex) {
  switch(ex) {
    case ref(str name):
      return "this.<name>";
   	case bol(bool bol):
	  return "<bol>";
   	case nat(int nat):
   	  return "<nat>";
   	case brc(AExpr ex2):
   	  return "(" + expr2js(ex2) + ")";
   	case neg(AExpr ex2):
   	  return "!" + expr2js(ex2);  
    case mul(AExpr ex1, AExpr ex2): 
   	  return expr2js(ex1) + "*" + expr2js(ex2);
    case div(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "/" + expr2js(ex2);
    case add(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "+" + expr2js(ex2);
    case sub(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "-" + expr2js(ex2);
    case lt(AExpr ex1, AExpr ex2):       
      return expr2js(ex1) + "\<" + expr2js(ex2);
    case leq(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "\<=" + expr2js(ex2);
    case gt(AExpr ex1, AExpr ex2):
       return expr2js(ex1) + "\>" + expr2js(ex2);
    case geq(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "\>=" + expr2js(ex2);
    case equ(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "==" + expr2js(ex2);
    case neq(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "!=" + expr2js(ex2);
    case and(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "&&" + expr2js(ex2);
    case or(AExpr ex1, AExpr ex2): 
      return expr2js(ex1) + "||" + expr2js(ex2);
    default: throw "Unsupported expression <ex>";
  }
}

str type2js(boolean()) = "false";
str type2js(integer()) = "0";
