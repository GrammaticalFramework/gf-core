const pgf = require('bindings')('pgf')
// console.log('pgf', pgf)
console.log(pgf.readPGF('/Users/john/repositories/gf-typescript/test/grammars/Zero.pgf'));
module.exports = pgf

/*
let gr = pgf.readPGF('App12.pgf')
let eng = gr.languages['AppEng']
let i = eng.parse('this is a small theatre', pgf.readType('NP'))
let pr = i.next() // pr.tree, pr.prob

let e = pgf.readExpr("AdjCN (PositA red_A) (UseN theatre_N)")
eng.linearize(e) // : string
eng.linearizeAll(e) // : string[]
eng.tabularLinearize(e) // : {[key]:string : string}
*/
