module Plugin

import Syntax;
import AST;
import CST2AST;
import Resolve;
import Check;
import Message;
import ParseTree;

set[Message] main() {
 start[Form] pp = (parse(#start[Form], |file:///home/s2063751/Downloads/sle-rug-master/examples/empty.myql|));
 AForm ast = cst2ast(pp);
 UseDef useDef = resolve(ast);
 set[Message] msgs = check(ast, collect(ast), useDef);
 return msgs;
}