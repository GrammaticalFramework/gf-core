pgf = require("pgf")

console.log(pgf);
console.log(new pgf.ExprFun("f"));
console.log(new pgf.ExprApp(new pgf.ExprFun("f"),new pgf.ExprFun("x")));
console.log(pgf.readExpr("g x 315 \"aaa\""));
