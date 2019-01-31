module Test

import ParseTree;
import AST;
import CST2AST;
import Syntax;
import Check;
import Eval;
import Resolve;
import Compile;
import Transform;

Form PT = parse(#Form,|project://QL/examples/tax.myql|);
AForm AT = cst2ast(PT);