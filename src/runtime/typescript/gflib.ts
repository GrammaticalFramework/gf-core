/**
 * gflib.ts
 *
 * by John J. Camilleri
 *
 * A port of the pure JavaScript runtime (/src/runtime/javascript/gflib.js) into TypeScript
 */

// Note: the String prototype is extended with:
// String.prototype.tag = "";
// String.prototype.setTag = function (tag) { this.tag = tag; };

/**
 * A GF grammar is one abstract and multiple concretes
 */
export class GFGrammar {
  public abstract: GFAbstract
  public concretes: {[key: string]: GFConcrete}

  public constructor(abstract: GFAbstract, concretes: {[key: string]: GFConcrete}) {
    this.abstract = abstract
    this.concretes = concretes
  }

  public translate(
    input: string,
    fromLang: string,
    toLang: string
  ): {[key: string]: {[key: string]: string}} {
    var outputs = {}
    var fromConcs = this.concretes
    if (fromLang) {
      fromConcs = {}
      fromConcs[fromLang] = this.concretes[fromLang]
    }
    var toConcs = this.concretes
    if (toLang) {
      toConcs = {}
      toConcs[toLang] = this.concretes[toLang]
    }
    for (var c1 in fromConcs) {
      var concrete = this.concretes[c1]
      var trees = concrete.parseString(input, this.abstract.startcat)
      if (trees.length > 0) {
        outputs[c1] = []
        for (var i in trees) {
          outputs[c1][i] = new Object()
          for (var c2 in toConcs) {
            outputs[c1][i][c2] = this.concretes[c2].linearize(trees[i])
          }
        }
      }
    }
    return outputs
  }
}

/**
 * Abstract Syntax Tree
 */
export class Fun {
  public name: string
  public args: Fun[]
  public type?: string

  public constructor(name: string, ...args: Fun[]) {
    this.name = name
    this.args = []
    for (var i = 1; i < args.length; i++) {
      this.args[i-1] = args[i]
    }
  }

  public print(): string {
    return this.show(0)
  }

  public show(prec: number): string {
    if (this.isMeta()) {
      if (isUndefined(this.type)) {
        return '?'
      } else {
        var s = '?:' + this.type
        if (prec > 0) {
          s = '(' + s + ')'
        }
        return s
      }
    } else {
      var s = this.name
      var cs = this.args
      for (var i in cs) {
        s += ' ' + (isUndefined(cs[i]) ? 'undefined' : cs[i].show(1))
      }
      if (prec > 0 && cs.length > 0) {
        s = '(' + s + ')'
      }
      return s
    }
  }

  public getArg(i: number): Fun {
    return this.args[i]
  }

  public setArg(i: number, c: Fun): void {
    this.args[i] = c
  }

  public isMeta(): boolean {
    return this.name == '?'
  }

  public isComplete(): boolean {
    if (this.isMeta()) {
      return false
    } else {
      for (var i in this.args) {
        if (!this.args[i].isComplete()) {
          return false
        }
      }
      return true
    }
  }

  public isLiteral(): boolean {
    return (/^[\"\-\d]/).test(this.name)
  }

  public isString(): boolean {
    return (/^\".*\"$/).test(this.name)
  }

  public isInt(): boolean {
    return (/^\-?\d+$/).test(this.name)
  }

  public isFloat(): boolean {
    return (/^\-?\d*(\.\d*)?$/).test(this.name) && this.name != '.' && this.name != '-.'
  }

  public isEqual(obj: Fun): boolean {
    if (this.name != obj.name)
      return false

    for (var i in this.args) {
      if (!this.args[i].isEqual(obj.args[i]))
        return false
    }

    return true
  }
}

/**
 * Abstract syntax
 */
class GFAbstract {
  public startcat: string
  private types: {[key: string]: Type} // key is function name

  public constructor(startcat: string, types: {[key: string]: Type}) {
    this.startcat = startcat
    this.types = types
  }

  public addType(fun: string, args: string[], cat: string): void {
    this.types[fun] = new Type(args, cat)
  }

  public getArgs(fun: string): string[] {
    return this.types[fun].args
  }

  public getCat(fun: string): string {
    return this.types[fun].cat
  }

  private annotate(tree: Fun, type: string): Fun {
    if (tree.name == '?') {
      tree.type = type
    } else {
      var typ = this.types[tree.name]
      for (var i in tree.args) {
        this.annotate(tree.args[i], typ.args[i])
      }
    }
    return tree
  }

  public handleLiterals(tree: Fun, type: string): Fun {
    if (tree.name != '?') {
      if (type === 'String' || type === 'Int' || type === 'Float') {
        tree.name = type + '_Literal_' + tree.name
      } else {
        var typ = this.types[tree.name]
        for (var i in tree.args) {
          this.handleLiterals(tree.args[i], typ.args[i])
        }
      }
    }
    return tree
  }

  // Hack to get around the fact that our SISR doesn't build real Fun objects.
  public copyTree(x: Fun): Fun {
    var t = new Fun(x.name)
    if (!isUndefined(x.type)) {
      t.type = x.type
    }
    var cs = x.args
    if (!isUndefined(cs)) {
      for (let i = 0; i < cs.length; i++) {
        t.setArg(i, this.copyTree(cs[i]))
      }
    }
    return t
  }

  public parseTree(str: string, type: string): Fun {
    return this.annotate(this.parseTree_(str.match(/[\w\'\.\"]+|\(|\)|\?|\:/g), 0), type)
  }

  private parseTree_(tokens: string[], prec: number): Fun {
    if (tokens.length == 0 || tokens[0] == ')') {
      return null
    }
    var t = tokens.shift()
    if (t == '(') {
      var tree = this.parseTree_(tokens, 0)
      tokens.shift()
      return tree
    } else if (t == '?') {
      var tree = this.parseTree_(tokens, 0)
      return new Fun('?')
    } else {
      var tree = new Fun(t)
      if (prec == 0) {
        let c: Fun
        let i: number
        for (i = 0; (c = this.parseTree_(tokens, 1)) !== null; i++) {
          tree.setArg(i,c)
        }
      }
      return tree
    }
  }
}

/**
 * Type
 */
class Type {
  public args: string[]
  public cat: string

  public constructor(args: string[], cat: string) {
    this.args = args
    this.cat = cat
  }
}

type ApplyOrCoerce = Apply | Coerce

/**
 * Concrete syntax
 */
class GFConcrete {
  private flags: {[key: string]: string}
  // private productions: {[key: number]: ApplyOrCoerce[]}
  private functions: CncFun[]
  private sequences: Sym[][]
  private startCats: {[key: string]: {s: number; e: number}}
  private totalFIds: number
  private pproductions: {[key: number]: ApplyOrCoerce[]}
  private lproductions: {[key: string]: {fid: FId; fun: CncFun}[]}

  public constructor(
    flags: {[key: string]: string},
    productions: {[key: number]: ApplyOrCoerce[]},
    functions: CncFun[],
    sequences: Sym[][],
    startCats: {[key: string]: {s: number; e: number}},
    totalFIds: number
  ) {
    this.flags       = flags
    // this.productions = productions
    this.functions   = functions
    this.sequences   = sequences
    this.startCats   = startCats
    this.totalFIds   = totalFIds

    this.pproductions = productions
    this.lproductions = {}

    for (let fid0 in productions) {
      let fid: number = parseInt(fid0)
      for (let i in productions[fid]) {
        var rule = productions[fid][i]

        if (rule.id === 'Apply') {
          rule = rule as Apply
          var fun: CncFun = this.functions[rule.fun as FId]
          var lproductions = this.lproductions

          rule.fun = fun

          var register = function (args: PArg[], key: string, i: number): void {
            if (i < args.length) {
              var c   = 0
              var arg = args[i].fid

              for (var k in productions[arg]) {
                var rule = productions[arg][k]
                if (rule.id === 'Coerce') {
                  rule = rule as Coerce
                  register(args, key + '_' + rule.arg, i+1)
                  c++
                }
              }

              if (c == 0) {
                register(args, key + '_' + arg, i+1)
              }
            } else {
              var set = lproductions[key]
              if (set == null) {
                set = []
                lproductions[key] = set
              }
              set.push({fun: fun, fid: fid})
            }
          }
          register(rule.args, rule.fun.name, 0)
        }
      }
    }

    for (var fun of functions) {
      for (var j in fun.lins) {
        fun.lins[j] = sequences[fun.lins[j] as number]
      }
    }

  }

  private linearizeSyms(tree: Fun, tag: string): {fid: FId; table: any[][]}[] {
    var res = []

    if (tree.isString()) {
      var sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -1, table: [[sym]]})
    } else if (tree.isInt()) {
      var sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -2, table: [[sym]]})
    } else if (tree.isFloat()) {
      var sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -3, table: [[sym]]})
    } else if (tree.isMeta()) {
      // TODO: Use lindef here
      var cat = this.startCats[tree.type]

      var sym = new SymKS(tree.name)
      sym.tag = tag

      for (var fid = cat.s; fid <= cat.e; fid++) {
        res.push({fid: fid, table: [[sym]]})
      }
    } else {
      var cs = []
      for (var i in tree.args) {
        // TODO: we should handle the case for nondeterministic linearization
        cs.push(this.linearizeSyms(tree.args[i],tag + '-' + i)[0])
      }
      var key = tree.name
      for (var i in cs) {
        key = key + '_' + cs[i].fid
      }

      for (var i in this.lproductions[key]) {
        var rule = this.lproductions[key][i]
        var row  = {
          fid: rule.fid,
          table: []
        }
        for (var j in rule.fun.lins) {
          var lin  = rule.fun.lins[j]
          var toks = []
          row.table[j] = toks

          for (var k in lin) {
            var sym = lin[k]
            switch (sym.id) {
              case 'Arg':
              case 'Lit':
                var ts = cs[sym.i].table[sym.label]
                for (var l in ts) {
                  toks.push(ts[l])
                }
                break
              case 'KS':
              case 'KP':
                toks.push(this.tagIt(sym,tag))
                break
            }
          }
        }
        res.push(row)
      }
    }

    return res
  }

  private syms2toks(syms: Sym[]): string[] {
    var ts = []
    for (var i in syms) {
      var sym = syms[i]
      switch (sym.id) {
        case 'KS':
          sym = sym as SymKS
          for (var j in sym.tokens) {
            ts.push(this.tagIt(sym.tokens[j],sym.tag))
          }
          break
        case 'KP':
          sym = sym as SymKP
          for (var j in sym.tokens) {
            ts.push(this.tagIt(sym.tokens[j],sym.tag))
          }
          break
      }
    }
    return ts
  }

  public linearizeAll(tree: Fun): string[] {
    return this.linearizeSyms(tree,'0').map(function(r): string {
      return this.unlex(this.syms2toks(r.table[0]))
    })
  }

  public linearize(tree: Fun): string {
    var res = this.linearizeSyms(tree,'0')
    return this.unlex(this.syms2toks(res[0].table[0]))
  }

  public tagAndLinearize(tree: Fun): string[] {
    var res = this.linearizeSyms(tree,'0')
    return this.syms2toks(res[0].table[0])
  }

  private unlex(ts: string[]): string {
    if (ts.length == 0) {
      return ''
    }

    var noSpaceAfter = /^[\(\-\[]/
    var noSpaceBefore = /^[\.\,\?\!\)\:\;\-\]]/

    var s = ''
    for (var i = 0; i < ts.length; i++) {
      var t = ts[i]
      var after = i < ts.length-1 ? ts[i+1] : null
      s += t
      // TODO handle pre construct
      if (after != null && !t.match(noSpaceAfter)
            && !after.match(noSpaceBefore)) {
        s += ' '
      }
    }
    return s
  }

  private tagIt(obj: any, tag: string): any {
    if (isString(obj)) {
      var o = new String(obj)
      o.setTag(tag)
      return o
    } else {
      var me = arguments.callee
      if (arguments.length == 2) {
        me.prototype = obj
        var o = new me()
        o.tag = tag
        return o
      }
    }
  }

  // public showRules(): string {
  //   var ruleStr = []
  //   ruleStr.push('')
  //   for (var i = 0, j = this.rules.length; i < j; i++) {
  //     ruleStr.push(this.rules[i].show())
  //   }
  //   return ruleStr.join('')
  // }

  private tokenize(string: string): string[] {
    var inToken = false
    var start: number, end: number
    var tokens = []

    for (var i = 0; i < string.length; i++) {
      if (string.charAt(i) == ' '       // space
       || string.charAt(i) == '\f'      // form feed
       || string.charAt(i) == '\n'      // newline
       || string.charAt(i) == '\r'      // return
       || string.charAt(i) == '\t'      // horizontal tab
       || string.charAt(i) == '\v'      // vertical tab
       || string.charAt(i) == String.fromCharCode(160) // &nbsp;
      ) {
        if (inToken) {
          end = i-1
          inToken = false
          tokens.push(string.substr(start,end-start+1))
        }
      } else {
        if (!inToken) {
          start = i
          inToken = true
        }
      }
    }

    if (inToken) {
      end = i-1
      inToken = false
      tokens.push(string.substr(start,end-start+1))
    }

    return tokens
  }

  public parseString(string: string, cat: string): Fun[] {
    var tokens = this.tokenize(string)

    var ps = new ParseState(this, cat)
    for (var i in tokens) {
      if (!ps.next(tokens[i]))
        return []
    }
    return ps.extractTrees()
  }

  public complete(
    input: string,
    cat: string
  ): {consumed: string[]; suggestions: string[]} {
    // Parameter defaults
    if (input == null) input = ''
    // if (cat == null) cat = grammar.abstract.startcat

    // Tokenise input string & remove empty tokens
    let tokens = input.trim().split(' ')
    for (var i = tokens.length - 1; i >= 0; i--) {
      if (tokens[i] == '') {
        tokens.splice(i, 1)
      }
    }

    // Capture last token as it may be partial
    let current = tokens.pop()
    if (current == null) current = ''

    // Init parse state objects.
    // ps2 is used for testing whether the final token is parsable or not.
    var ps = new ParseState(this, cat)
    var ps2 = new ParseState(this, cat)

    // Iterate over tokens, feed one by one to parser
    for (var i = 0; i < tokens.length ; i++) {
      if (!ps.next(tokens[i])) {
        return { 'consumed': [], 'suggestions': [] } // Incorrect parse, nothing to suggest
      }
      ps2.next(tokens[i]) // also consume token in ps2
    }

    // Attempt to also parse current, knowing it may be incomplete
    if (ps2.next(current)) {
      ps.next(current)
      tokens.push(current)
      current = ''
    }

    // Parse is successful so far, now get suggestions
    var acc = ps.complete(current)

    // Format into just a list of strings & return
    // (I know the multiple nesting looks horrible)
    var suggs = []
    if (acc.value) {
      // Iterate over all acc.value[]
      for (var v = 0; v < acc.value.length; v++) {
        // Iterate over all acc.value[].seq[]
        for (var s = 0; s < acc.value[v].seq.length; s++) {
          if (acc.value[v].seq[s].tokens == null) continue
          // Iterate over all acc.value[].seq[].tokens
          for (var t = 0; t < acc.value[v].seq[s].tokens.length; t++) {
            suggs.push( acc.value[v].seq[s].tokens[t] )
          }
        }
      }
    }

    // Note: return used tokens too
    return { 'consumed' : tokens, 'suggestions' : suggs }
  }
}

/**
 * Function ID
 */
type FId = number

/**
 * Apply
 */
class Apply {
  public id: string
  private fun: FId | CncFun
  private args: PArg[]

  public constructor(fun: FId, args: PArg[]) {
    this.id   = 'Apply' // TODO use enum
    this.fun  = fun
    this.args = args
  }

  public show(cat: string): string {
    var recStr = []
    recStr.push(cat, ' -> ', (this.fun as CncFun).name, ' [', this.args, ']')
    return recStr.join('')
  }

  public isEqual(obj: Apply): boolean {
    if (this.id != obj.id || this.fun != obj.fun || this.args.length != obj.args.length)
      return false

    for (var i in this.args) {
      if (this.args[i] != obj.args[i])
        return false
    }

    return true
  }
}

/**
 * Coerce
 */
class Coerce {
  public id: string
  private arg: FId

  public constructor(arg: FId) {
    this.id = 'Coerce' // TODO use enum
    this.arg = arg
  }

  public show(cat: string): string {
    var recStr = []
    recStr.push(cat, ' -> _ [', this.arg, ']')
    return recStr.join('')
  }
}

/**
 * PArg
 */
class PArg {
  private fid: FId
  private hypos: FId[]

  public constructor(...hypos: FId[]) {
    this.fid = hypos[hypos.length-1]
    if (hypos.length > 1)
      this.hypos = hypos.slice(0, hypos.length-1)
  }
}

/**
 * Const
 */
class Const {
  private id: string
  private lit: Fun
  private toks: string[]

  public constructor(lit: Fun, toks: string[]) {
    this.id   = 'Const'
    this.lit  = lit
    this.toks = toks
  }

  public show(cat: string): string {
    var recStr = []
    recStr.push(cat, ' -> ', this.lit.print())
    return recStr.join('')
  }

  public isEqual(obj: Const): boolean {
    if (this.id != obj.id || this.lit.isEqual(obj.lit) || this.toks.length != obj.toks.length)
      return false

    for (var i in this.toks) {
      if (this.toks[i] != obj.toks[i])
        return false
    }

    return true
  }
}

/**
 * CncFun
 */
class CncFun {
  public name: string
  private lins: number[] | Sym[][]

  public constructor(name: string, lins: FId[]) {
    this.name = name
    this.lins = lins
  }
}

/**
 * Sym: Definition of symbols present in linearization records
 */
type Sym = SymCat | SymKS | SymKP | SymLit

/**
 * SymCat: Object to represent argument projections in grammar rules
 */
class SymCat {
  public id: string
  private i: number
  private label: number

  public constructor(i: number, label: number) {
    this.id = 'Arg'
    this.i = i
    this.label = label
  }

  public getArgNum(): number {
    return this.i
  }

  public show(): string {
    var argStr = []
    argStr.push(this.i, this.label)
    return argStr.join('.')
  }
}

/**
 * SymKS: Object to represent terminals in grammar rules
 */
class SymKS {
  public id: string
  public tokens: string[]

  public constructor(...tokens: string[]) {
    this.id = 'KS'
    this.tokens = tokens
  }

  public show(): string {
    var terminalStr = []
    terminalStr.push('"', this.tokens, '"')
    return terminalStr.join('')
  }
}

/**
 * SymKP: Object to represent pre in grammar rules
 */
class SymKP {
  public id: string
  public tokens: string[]
  private alts: Alt[]

  public constructor(tokens: string[], alts: Alt[]) {
    this.id = 'KP'
    this.tokens = tokens
    this.alts = alts
  }

  public show(): string {
    var terminalStr = []
    terminalStr.push('"', this.tokens, '"')
    return terminalStr.join('')
  }
}

/**
 * Alt
 */
class Alt {
  private tokens: string[]
  private prefixes: string[]

  public constructor(tokens: string[], prefixes: string[]) {
    this.tokens   = tokens
    this.prefixes = prefixes
  }
}

/**
 * SymLit: Object to represent pre in grammar rules
 */
class SymLit {
  public id: string
  private i: number
  private label: number

  public constructor(i: number, label: number) {
    this.id = 'Lit'
    this.i = i
    this.label = label
  }

  public getId(): string {
    return this.id
  }

  public show(): string {
    var argStr = []
    argStr.push(this.i, this.label)
    return argStr.join('.')
  }
}

/**
 * Trie
 */
class Trie<T> {
  private value: T[]
  private items: Trie<T>[]

  public constructor() {
    this.value = null
    this.items = []
  }

  private insertChain(keys: string[], obj: T[]): void {
    var node = this
    for (var i in keys) {
      var nnode = node.items[keys[i]]
      if (nnode == null) {
        nnode = new Trie()
        node.items[keys[i]] = nnode
      }
      node = nnode
    }
    node.value = obj
  }

  private insertChain1(keys: string[], obj: T): void {
    var node = this
    for (var i in keys) {
      var nnode = node.items[keys[i]]
      if (nnode == null) {
        nnode = new Trie()
        node.items[keys[i]] = nnode
      }
      node = nnode
    }
    if (node.value == null)
      node.value = [obj]
    else
      node.value.push(obj)
  }

  private lookup(key: string): T {
    return this.items[key]
  }

  private isEmpty(): boolean {
    if (this.value != null)
      return false

    for (let _ in this.items) {
      return false
    }

    return true
  }
}

/**
 * ParseState
 */
declare class ParseState {
  concrete: GFConcrete
  startCat: string
  items: Trie
  chart: Chart

  constructor(concrete: GFConcrete, startCat: string)

  next(token: string): boolean
  complete(correntToken: string): Trie
  extractTrees(): any[]
  process(
    agenda: ActiveItem[],
    literalCallback: (fid: FId) => any,
    tokenCallback: (tokens: string[], item: any) => any
  ): void
}

/**
 * Chart
 */
declare class Chart {
  active: {[key: number]: ActiveItem} // key: FId
  actives: {[key: number]: ActiveItem}[] // key: FId
  passive: {[key: string]: FId}
  forest: {[key: number]: ApplyOrCoerce[]} // key: FId
  nextId: number
  offset: number

  constructor(concrete: GFConcrete)

  lookupAC(fid: FId, label: number): ActiveItem[]
  lookupACo(offset: number, fid: FId, label: number): ActiveItem[]

  labelsAC(fid: FId): ActiveItem
  insertAC(fid: FId, label: number, items: any[]): void

  lookupPC(fid: FId, label: number, offset: number): FId
  insertPC(fid1: FId, label: number, offset: number, fid2: FId): void
  shift(): void
  expandForest(fid: FId): Apply[]
}

/**
 * ActiveItem
 */
class ActiveItem {
  public offset: number
  public dot: number
  public fun: CncFun
  public seq: Sym[]
  public args: PArg[]
  public fid: FId
  public lbl: number

  public constructor(
    offset: number,
    dot: number,
    fun: CncFun,
    seq: Sym[],
    args: PArg[],
    fid: FId,
    lbl: number
  ) {
    this.offset = offset
    this.dot   = dot
    this.fun   = fun
    this.seq   = seq
    this.args  = args
    this.fid   = fid
    this.lbl   = lbl
  }

  public isEqual(obj: ActiveItem): boolean {
    return (this.offset== obj.offset &&
            this.dot   == obj.dot &&
            this.fun   == obj.fun &&
            this.seq   == obj.seq &&
            this.args  == obj.args &&
            this.fid   == obj.fid &&
            this.lbl   == obj.lbl)
  }

  public shiftOverArg(i: number, fid: FId): ActiveItem {
    var nargs = []
    for (var k in this.args) {
      nargs[k] = this.args[k]
    }
    nargs[i] = new PArg(fid)
    return new ActiveItem(this.offset,this.dot+1,this.fun,this.seq,nargs,this.fid,this.lbl)
  }

  public shiftOverTokn(): ActiveItem {
    return new ActiveItem(this.offset,this.dot+1,this.fun,this.seq,this.args,this.fid,this.lbl)
  }
}

/**
 * Utilities
 */

/* from Remedial JavaScript by Douglas Crockford, http://javascript.crockford.com/remedial.html */
function isString(a: any): boolean {
  return typeof a === 'string'
}
function isArray(a: any): boolean {
  return a && typeof a === 'object' && a.constructor === Array
}
function isUndefined(a: any): boolean {
  return typeof a === 'undefined'
}
function isBoolean(a: any): boolean {
  return typeof a === 'boolean'
}
function isNumber(a: any): boolean {
  return typeof a === 'number' && isFinite(a)
}
function isFunction(a: any): boolean {
  return typeof a === 'function'
}
function dumpObject (obj: any): string {
  if (isUndefined(obj)) {
    return 'undefined'
  } else if (isString(obj)) {
    return '"' + obj.toString() + '"' // FIXME: escape
  } else if (isBoolean(obj) || isNumber(obj)) {
    return obj.toString()
  } else if (isArray(obj)) {
    var x = '['
    for (let i = 0; i < obj.length; i++) {
      x += dumpObject(obj[i])
      if (i < obj.length-1) {
        x += ','
      }
    }
    return x + ']'
  } else {
    var x = '{'
    for (let y in obj) {
      x += y + '=' + dumpObject(obj[y]) + ';'
    }
    return x + '}'
  }
}
