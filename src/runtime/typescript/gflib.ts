/**
 * gflib.ts
 *
 * by John J. Camilleri
 *
 * A port of the pure JavaScript runtime (/src/runtime/javascript/gflib.js) into TypeScript
 */

/**
 * A GF grammar is one abstract and multiple concretes
 */
class GFGrammar { // eslint-disable-line @typescript-eslint/no-unused-vars
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
  ): {[key: string]: {[key: string]: string}[]} {
    let outputs: {[key: string]: {[key: string]: string}[]} = {}
    let fromConcs = this.concretes
    if (fromLang) {
      fromConcs = {}
      fromConcs[fromLang] = this.concretes[fromLang]
    }
    let toConcs = this.concretes
    if (toLang) {
      toConcs = {}
      toConcs[toLang] = this.concretes[toLang]
    }
    for (let c1 in fromConcs) {
      let concrete = this.concretes[c1]
      let trees = concrete.parseString(input, this.abstract.startcat)
      if (trees.length > 0) {
        outputs[c1] = []
        for (let i in trees) {
          outputs[c1][i] = {}
          for (let c2 in toConcs) {
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
class Fun {
  public name: string
  public args: Fun[]
  public type?: string

  public constructor(name: string, ...args: Fun[]) {
    this.name = name
    this.args = []
    for (let i = 1; i < args.length; i++) {
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
        let s = '?:' + this.type
        if (prec > 0) {
          s = '(' + s + ')'
        }
        return s
      }
    } else {
      let s = this.name
      let cs = this.args
      for (let i in cs) {
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
      for (let i in this.args) {
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

    for (let i in this.args) {
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
      let typ = this.types[tree.name]
      for (let i in tree.args) {
        this.annotate(tree.args[i], typ.args[i])
      }
    }
    return tree
  }

  public handleLiterals(tree: Fun, type: string): Fun {
    if (tree.name != '?') {
      if (type == 'String' || type == 'Int' || type == 'Float') {
        tree.name = type + '_Literal_' + tree.name
      } else {
        let typ = this.types[tree.name]
        for (let i in tree.args) {
          this.handleLiterals(tree.args[i], typ.args[i])
        }
      }
    }
    return tree
  }

  // Hack to get around the fact that our SISR doesn't build real Fun objects.
  public copyTree(x: Fun): Fun {
    let t = new Fun(x.name)
    if (!isUndefined(x.type)) {
      t.type = x.type
    }
    let cs = x.args
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
    let t = tokens.shift()
    if (t == '(') {
      let tree = this.parseTree_(tokens, 0)
      tokens.shift()
      return tree
    } else if (t == '?') {
      // let tree = this.parseTree_(tokens, 0)
      return new Fun('?')
    } else {
      let tree = new Fun(t)
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

/**
 * Concrete syntax
 */
class GFConcrete {
  public flags: {[key: string]: string}
  // private productions: {[key: number]: Rule[]}
  private functions: CncFun[]
  // private sequences: Sym[][]
  public startCats: {[key: string]: {s: number; e: number}}
  public totalFIds: number
  public pproductions: {[key: number]: Rule[]}
  private lproductions: {[key: string]: {fid: FId; fun: CncFun}[]}

  public constructor(
    flags: {[key: string]: string},
    productions: {[key: number]: Rule[]},
    functions: CncFun[],
    sequences: Sym[][],
    startCats: {[key: string]: {s: number; e: number}},
    totalFIds: number
  ) {
    this.flags       = flags
    // this.productions = productions
    this.functions   = functions
    // this.sequences   = sequences
    this.startCats   = startCats
    this.totalFIds   = totalFIds

    this.pproductions = productions
    this.lproductions = {}

    for (let fid0 in productions) {
      let fid: number = parseInt(fid0)
      for (let i in productions[fid]) {
        let rule = productions[fid][i]

        if (rule.id == 'Apply') {
          rule = rule as Apply
          let fun: CncFun = this.functions[rule.fun as FId]
          let lproductions = this.lproductions

          rule.fun = fun

          let register = function (args: PArg[], key: string, i: number): void {
            if (i < args.length) {
              let c   = 0
              let arg = args[i].fid

              for (let k in productions[arg]) {
                let rule = productions[arg][k]
                if (rule.id == 'Coerce') {
                  rule = rule as Coerce
                  register(args, key + '_' + rule.arg, i+1)
                  c++
                }
              }

              if (c == 0) {
                register(args, key + '_' + arg, i+1)
              }
            } else {
              let set = lproductions[key]
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

    for (let fun of functions) {
      for (let j in fun.lins) {
        fun.lins[j] = sequences[fun.lins[j] as number]
      }
    }

  }

  private linearizeSyms(tree: Fun, tag: string): {fid: FId; table: Sym[][]}[] {
    let res = []

    if (tree.isString()) {
      let sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -1, table: [[sym]]})
    } else if (tree.isInt()) {
      let sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -2, table: [[sym]]})
    } else if (tree.isFloat()) {
      let sym = new SymKS(tree.name)
      sym.tag = tag
      res.push({fid: -3, table: [[sym]]})
    } else if (tree.isMeta()) {
      // TODO: Use lindef here
      let cat = this.startCats[tree.type]

      let sym = new SymKS(tree.name)
      sym.tag = tag

      for (let fid = cat.s; fid <= cat.e; fid++) {
        res.push({fid: fid, table: [[sym]]})
      }
    } else {
      let cs: {fid: FId; table: Sym[][]}[] = []
      for (let i in tree.args) {
        // TODO: we should handle the case for nondeterministic linearization
        cs.push(this.linearizeSyms(tree.args[i],tag + '-' + i)[0])
      }
      let key = tree.name
      for (let i in cs) {
        key = key + '_' + cs[i].fid
      }

      for (let i in this.lproductions[key]) {
        let rule = this.lproductions[key][i]
        let row: {fid: FId; table: Sym[][]} = {
          fid: rule.fid,
          table: []
        }
        for (let j in rule.fun.lins) {
          let lin = rule.fun.lins[j] as Sym[]
          let toks: Sym[] = []
          row.table[j] = toks

          lin.forEach((sym0: Sym): void => {
            switch (sym0.id) {
              case 'Arg':
              case 'Lit': {
                let sym = sym0 as SymCat | SymLit
                let ts = cs[sym.i].table[sym.label]
                for (let l in ts) {
                  toks.push(ts[l])
                }
                break
              }
              case 'KS':
              case 'KP': {
                let sym = sym0 as SymKS | SymKP
                toks.push(sym.tagWith(tag))
                break
              }
            }
          })
        }
        res.push(row)
      }
    }

    return res
  }

  private syms2toks(syms: Sym[]): string[] {
    let ts: string[] = []
    for (let i = 0; i < syms.length; i++) {
      let sym0 = syms[i]
      switch (sym0.id) {
        case 'KS': {
          let sym = sym0 as SymKS
          for (let j in sym.tokens) {
            ts.push(sym.tokens[j].tagWith(sym.tag))
          }
          break
        }
        case 'KP': {
          let sym = sym0 as SymKP
          let addedAlt = false
          if (i < syms.length-1) {
            let nextSym = syms[i+1]
            if (nextSym.id == 'KS') {
              let nextToken = (nextSym as SymKS).tokens[0]
              sym.alts.forEach((alt: Alt): void => {
                // consider alts here (for handling pre)
                if (alt.prefixes.some((p: string): boolean => nextToken.startsWith(p))) {
                  alt.tokens.forEach((symks: SymKS): void => {
                    symks.tokens.forEach((t: string): void => {
                      ts.push(t.tagWith(sym.tag))
                    })
                  })
                  addedAlt = true
                  return
                }
              })
            }
          }
          if (addedAlt) break
          // Fall through here when no alts (or none apply)
          sym.tokens.forEach((symks: SymKS): void => {
            symks.tokens.forEach((t: string): void => {
              ts.push(t.tagWith(sym.tag))
            })
          })
          break
        }
      }
    }
    return ts
  }

  public linearizeAll(tree: Fun): string[] {
    return this.linearizeSyms(tree,'0').map((r): string => {
      return this.unlex(this.syms2toks(r.table[0]))
    })
  }

  public linearize(tree: Fun): string {
    let res = this.linearizeSyms(tree,'0')
    return this.unlex(this.syms2toks(res[0].table[0]))
  }

  public tagAndLinearize(tree: Fun): string[] {
    let res = this.linearizeSyms(tree,'0')
    return this.syms2toks(res[0].table[0])
  }

  private unlex(ts: string[]): string {
    if (ts.length == 0) {
      return ''
    }

    let noSpaceAfter = /^[\(\-\[]/
    let noSpaceBefore = /^[\.\,\?\!\)\:\;\-\]]/

    let s = ''
    for (let i = 0; i < ts.length; i++) {
      let t: string = ts[i]
      let after: string | null = i < ts.length-1 ? ts[i+1] : null
      s += t
      if (after != null
       && !t.match(noSpaceAfter)
       && !after.match(noSpaceBefore)
      ) {
        s += ' '
      }
    }
    return s
  }

  // private tagIt(obj: Taggable, tag: string): Taggable {
  //   if (isString(obj)) {
  //     let o = new String(obj)
  //     o.setTag(tag)
  //     return o
  //   } else {
  //     let me = arguments.callee
  //     if (arguments.length == 2) {
  //       me.prototype = obj
  //       let o = new me()
  //       o.tag = tag
  //       return o
  //     }
  //   }
  // }

  // public showRules(): string {
  //   let ruleStr = []
  //   ruleStr.push('')
  //   for (let i = 0, j = this.rules.length; i < j; i++) {
  //     ruleStr.push(this.rules[i].show())
  //   }
  //   return ruleStr.join('')
  // }

  private tokenize(string: string): string[] {
    let inToken = false
    let start: number, end: number
    let tokens = []

    let i: number
    for (i = 0; i < string.length; i++) {
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
    let tokens = this.tokenize(string)

    let ps = new ParseState(this, cat)
    for (let i in tokens) {
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
    for (let i = tokens.length - 1; i >= 0; i--) {
      if (tokens[i] == '') {
        tokens.splice(i, 1)
      }
    }

    // Capture last token as it may be partial
    let current = tokens.pop()
    if (current == null) current = ''

    // Init parse state objects.
    // ps2 is used for testing whether the final token is parsable or not.
    let ps = new ParseState(this, cat)
    let ps2 = new ParseState(this, cat)

    // Iterate over tokens, feed one by one to parser
    for (let i = 0; i < tokens.length ; i++) {
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
    let acc = ps.complete(current)

    // Format into just a list of strings & return
    let suggs: string[] = []
    if (acc.value) {
      acc.value.forEach((a: ActiveItem): void =>{
        a.seq.forEach((s: SymKS | SymKP): void => {
          if (s.tokens == null) return
          switch (s.id) {
            case 'KS': {
              (s as SymKS).tokens.forEach((t: string): void => {
                suggs.push(t)
              })
              break
            }
            case 'KP': {
              (s as SymKP).tokens.forEach((symks: SymKS): void => {
                symks.tokens.forEach((t: string): void => {
                  suggs.push(t)
                })
              })
              break
            }
          }
        })
      })
    }

    // Note: return used tokens too
    return { 'consumed' : tokens, 'suggestions' : suggs }
  }
}

/**
 * A type which can be tagged
 */
interface Taggable {
  tag?: string;
  tagWith: (tag: string) => Taggable;
}

/**
 * Strings can also be tagged in the same way
 */
interface String {
  tag?: string;
  tagWith: (tag: string) => string;
}
String.prototype.tagWith = function (tag: string): string {
  // returns a copy
  let s2 = this
  s2.tag = tag
  return s2
}

/**
 * Function ID
 */
type FId = number

/**
 * Rule
 */
type Rule = Apply | Coerce | Const

/**
 * Apply
 */
class Apply {
  public id: string
  public fun: FId | CncFun
  public args: PArg[]

  public constructor(fun: FId | CncFun, args: PArg[]) {
    this.id   = 'Apply'
    this.fun  = fun
    this.args = args
  }

  public show(cat: string): string {
    let recStr = []
    recStr.push(cat, ' -> ', (this.fun as CncFun).name, ' [', this.args, ']')
    return recStr.join('')
  }

  public isEqual(obj: Apply): boolean {
    if (this.id != obj.id || this.fun != obj.fun || this.args.length != obj.args.length)
      return false

    for (let i in this.args) {
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
  public arg: FId

  public constructor(arg: FId) {
    this.id = 'Coerce'
    this.arg = arg
  }

  public show(cat: string): string {
    let recStr = []
    recStr.push(cat, ' -> _ [', this.arg, ']')
    return recStr.join('')
  }
}

/**
 * PArg
 */
class PArg {
  public fid: FId
  public hypos: FId[]

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
  public id: string
  public lit: Fun
  public toks: string[]

  public constructor(lit: Fun, toks: string[]) {
    this.id   = 'Const'
    this.lit  = lit
    this.toks = toks
  }

  public show(cat: string): string {
    let recStr = []
    recStr.push(cat, ' -> ', this.lit.print())
    return recStr.join('')
  }

  public isEqual(obj: Const): boolean {
    if (this.id != obj.id || this.lit.isEqual(obj.lit) || this.toks.length != obj.toks.length)
      return false

    for (let i in this.toks) {
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
  public lins: number[] | Sym[][]

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
  public i: number
  public label: number

  public constructor(i: number, label: number) {
    this.id = 'Arg'
    this.i = i
    this.label = label
  }

  public show(): string {
    let argStr = []
    argStr.push(this.i, this.label)
    return argStr.join('.')
  }
}

/**
 * SymKS: Object to represent terminals in grammar rules
 */
class SymKS implements Taggable {
  public id: string
  public tokens: string[]
  public tag?: string

  public constructor(...tokens: string[]) {
    this.id = 'KS'
    this.tokens = tokens
  }

  public show(): string {
    let terminalStr = []
    terminalStr.push('"', this.tokens, '"')
    return terminalStr.join('')
  }

  public tagWith(tag: string): SymKS {
    let s = new SymKS()
    s.tokens = [...this.tokens] // copy array
    s.tag = tag
    return s
  }
}

/**
 * SymKP: Object to represent pre in grammar rules
 */
class SymKP implements Taggable {
  public id: string
  public tokens: SymKS[]
  public alts: Alt[]
  public tag?: string

  public constructor(tokens: SymKS[], alts: Alt[]) {
    this.id = 'KP'
    this.tokens = tokens
    this.alts = alts
  }

  public show(): string {
    let terminalStr = []
    terminalStr.push('"', this.tokens, '"')
    return terminalStr.join('')
  }

  public tagWith(tag: string): SymKP {
    let s = new SymKP([...this.tokens], [...this.alts]) // copy arguments
    s.tag = tag
    return s
  }
}

/**
 * Alt
 */
class Alt {
  public tokens: SymKS[]
  public prefixes: string[]

  public constructor(tokens: SymKS[], prefixes: string[]) {
    this.tokens   = tokens
    this.prefixes = prefixes
  }
}

/**
 * SymLit: Object to represent pre in grammar rules
 */
class SymLit {
  public id: string
  public i: number
  public label: number

  public constructor(i: number, label: number) {
    this.id = 'Lit'
    this.i = i
    this.label = label
  }

  public getId(): string {
    return this.id
  }

  public show(): string {
    let argStr = []
    argStr.push(this.i, this.label)
    return argStr.join('.')
  }
}

/**
 * Trie
 */
class Trie<T> {
  public value: T[]
  private items: {[key: string]: Trie<T>}

  public constructor() {
    this.value = null
    this.items = {}
  }

  public insertChain(keys: string[], obj: T[]): void {
    let node: Trie<T> = this
    keys.forEach((key: string): void => {
      let nnode = node.items[key]
      if (nnode == null) {
        nnode = new Trie()
        node.items[key] = nnode
      }
      node = nnode
    })
    node.value = obj
  }

  public insertChain1(keys: string[], obj: T): void {
    let node: Trie<T> = this
    keys.forEach((key: string): void => {
      let nnode = node.items[key]
      if (nnode == null) {
        nnode = new Trie()
        node.items[key] = nnode
      }
      node = nnode
    })
    if (node.value == null)
      node.value = [obj]
    else
      node.value.push(obj)
  }

  public lookup(key: string): Trie<T> {
    return this.items[key]
  }

  public isEmpty(): boolean {
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
class ParseState {
  private concrete: GFConcrete
  private startCat: string
  private items: Trie<ActiveItem>
  private chart: Chart

  public constructor(concrete: GFConcrete, startCat: string) {
    this.concrete = concrete
    this.startCat = startCat
    this.items = new Trie()
    this.chart = new Chart(concrete)

    let items = []

    let fids = concrete.startCats[startCat]
    if (fids != null) {
      let fid: FId
      for (fid = fids.s; fid <= fids.e; fid++) {
        let exProds = this.chart.expandForest(fid)
        for (let j in exProds) {
          let rule = exProds[j] as Apply
          let fun  = rule.fun as CncFun
          for (let lbl in fun.lins) {
            items.push(new ActiveItem(
              0,
              0,
              rule.fun as CncFun,
              fun.lins[lbl] as Sym[],
              rule.args,
              fid,
              parseInt(lbl))
            )
          }
        }
      }
    }

    this.items.insertChain([], items)
  }

  public next(token: string): boolean {
    let acc = this.items.lookup(token)
    if (acc == null) {
      acc = new Trie()
    }
    this.process(
      this.items.value,
      function (fid: FId): Const | null {
        switch (fid) {
          // String
          case -1:
            return new Const(new Fun('"'+token+'"'), [token])
          // Integer
          case -2: {
            let x = parseInt(token,10)
            if (token == '0' || (x != 0 && !isNaN(x)))
              return new Const(new Fun(token), [token])
            else
              return null
          }
          // Float
          case -3: {
            let x = parseFloat(token)
            if (token == '0' || token == '0.0' || (x != 0 && !isNaN(x)))
              return new Const(new Fun(token), [token])
            else
              return null
          }
        }

        return null
      },
      function (tokens: string[], item: ActiveItem): void {
        if (tokens[0] == token) {
          let tokens1 = []
          for (let i = 1; i < tokens.length; i++) {
            tokens1[i-1] = tokens[i]
          }
          acc.insertChain1(tokens1, item)
        }
      }
    )

    this.items = acc
    this.chart.shift()

    return !this.items.isEmpty()
  }

  /**
   * For a ParseState and a partial input, return all possible completions
   * Based closely on ParseState.next()
   * currentToken could be empty or a partial string
   */
  public complete(currentToken: string): Trie<ActiveItem> {

    // Initialise accumulator for suggestions
    let acc = this.items.lookup(currentToken)
    if (acc == null)
      acc = new Trie()

    this.process(
      // Items
      this.items.value,

      // Deal with literal categories
      function (_fid: FId): null {
        // Always return null, as suggested by Krasimir
        return null
      },

      // Takes an array of tokens and populates the accumulator
      function (tokens: string[], item: ActiveItem): void {
        if (currentToken == '' || tokens[0].indexOf(currentToken) == 0) { //if begins with...
          let tokens1 = []
          for (let i = 1; i < tokens.length; i++) {
            tokens1[i-1] = tokens[i]
          }
          acc.insertChain1(tokens1, item)
        }
      }
    )

    // Return matches
    return acc
  }

  public extractTrees(): Fun[] {
    this.process(
      this.items.value,
      function (_fid: FId): null {
        return null
      },
      function (_tokens: string[], _item: ActiveItem): void {
      }
    )

    let totalFIds = this.concrete.totalFIds
    let forest    = this.chart.forest

    function go(fid: FId): Fun[] {
      if (fid < totalFIds) {
        return [new Fun('?')]
      } else {
        let trees: Fun[] = []

        let rules = forest[fid] // could be undefined
        for (let j in rules) {
          let rule: Rule = rules[j]
          if (rule.id == 'Const') {
            trees.push((rule as Const).lit)
          } else {
            rule = rule as Apply
            let arg_ix: number[] = []
            let arg_ts: Fun[][] = []
            for (let k in rule.args) {
              arg_ix[k] = 0
              arg_ts[k] = go(rule.args[k].fid)
            }

            while (true) {
              let t = new Fun((rule.fun as CncFun).name)
              for (let k = 0; k < arg_ts.length; k++) {
                t.setArg(k,arg_ts[k][arg_ix[k]])
              }
              trees.push(t)

              let i = 0
              while (i < arg_ts.length) {
                arg_ix[i]++
                if (arg_ix[i] < arg_ts[i].length)
                  break

                arg_ix[i] = 0
                i++
              }

              if (i >= arg_ts.length)
                break
            }
          }
        }

        return trees
      }
    }

    let trees = []
    let fids = this.concrete.startCats[this.startCat]
    if (fids != null) {
      for (let fid0 = fids.s; fid0 <= fids.e; fid0++) {

        let labels: {[key: number]: boolean} = {}
        let rules = this.chart.expandForest(fid0)
        rules.forEach((rule): void => {
          for (let lbl in (rule.fun as CncFun).lins) {
            labels[lbl] = true
          }
        })

        for (let lbl0 in labels) {
          let lbl: number = parseInt(lbl0)
          let fid = this.chart.lookupPC(fid0, lbl, 0)
          let arg_ts = go(fid)
          for (let i in arg_ts) {
            let isMember = false
            for (let j in trees) {
              if (arg_ts[i].isEqual(trees[j])) {
                isMember = true
                break
              }
            }

            if (!isMember)
              trees.push(arg_ts[i])
          }
        }
      }
    }

    return trees
  }

  private process(
    agenda: ActiveItem[],
    literalCallback: (fid: FId) => Const | null, // this is right
    tokenCallback: (tokens: string[], item: ActiveItem) => void
  ): void {
    if (agenda != null) {
      while (agenda.length > 0) {
        let item = agenda.pop()
        let lin = item.seq

        if (item.dot < lin.length) {
          let sym0 = lin[item.dot]
          switch (sym0.id) {
            case 'Arg': {
              let sym = sym0 as SymCat
              let fid = item.args[sym.i].fid
              let label = sym.label

              let items = this.chart.lookupAC(fid,label)
              if (items == null) {
                let rules = this.chart.expandForest(fid)
                for (let j in rules) {
                  let rule = rules[j] as Apply
                  agenda.push(new ActiveItem(
                    this.chart.offset,
                    0,
                    rule.fun as CncFun,
                    ((rule.fun as CncFun).lins as Sym[][])[label],
                    rule.args,
                    fid,
                    label)
                  )
                }
                this.chart.insertAC(fid,label,[item])
              } else {
                let isMember = false
                for (let j in items) {
                  if (items[j].isEqual(item)) {
                    isMember = true
                    break
                  }
                }

                if (!isMember) {
                  items.push(item)

                  let fid2 = this.chart.lookupPC(fid,label,this.chart.offset)
                  if (fid2 != null) {
                    agenda.push(item.shiftOverArg(sym.i,fid2))
                  }
                }
              }
              break
            }
            case 'KS': {
              let sym = sym0 as SymKS
              tokenCallback(sym.tokens, item.shiftOverTokn())
              break
            }
            case 'KP': {
              let sym = sym0 as SymKP
              let pitem = item.shiftOverTokn()
              sym.tokens.forEach((symks: SymKS): void => { // TODO not sure if this is right
                tokenCallback(symks.tokens, pitem)
              })
              sym.alts.forEach((alt: Alt): void => {
                // tokenCallback(alt.tokens, pitem)
                alt.tokens.forEach((symks: SymKS): void => { // TODO not sure if this is right
                  tokenCallback(symks.tokens, pitem)
                })
              })
              break
            }
            case 'Lit': {
              let sym = sym0 as SymLit
              let fid = item.args[sym.i].fid
              let rules = this.chart.forest[fid]
              if (rules != null) {
                tokenCallback((rules[0] as Const).toks, item.shiftOverTokn())
              } else {
                let rule = literalCallback(fid)
                if (rule != null) {
                  fid = this.chart.nextId++
                  this.chart.forest[fid] = [rule]
                  tokenCallback(rule.toks, item.shiftOverArg(sym.i, fid))
                }
              }
              break
            }
          }
        } else {
          let fid = this.chart.lookupPC(item.fid,item.lbl,item.offset)
          if (fid == null) {
            fid = this.chart.nextId++

            let items = this.chart.lookupACo(item.offset,item.fid,item.lbl)
            if (items != null) {
              items.forEach((pitem: ActiveItem): void => {
                let i = (pitem.seq[pitem.dot] as SymCat).i
                agenda.push(pitem.shiftOverArg(i,fid))
              })
            }

            this.chart.insertPC(item.fid,item.lbl,item.offset,fid)
            this.chart.forest[fid] = [new Apply(item.fun,item.args)]
          } else {
            let labels = this.chart.labelsAC(fid)
            if (labels != null) {
              for (let lbl in labels) {
                agenda.push(new ActiveItem(
                  this.chart.offset,
                  0,
                  item.fun,
                  item.fun.lins[lbl] as Sym[],
                  item.args,
                  fid,
                  parseInt(lbl))
                )
              }
            }

            let rules = this.chart.forest[fid]
            let rule  = new Apply(item.fun,item.args)

            let isMember = false
            rules.forEach((rule1): void => {
              if ((rule1 as Apply).isEqual(rule)) // TODO might need to check if Coerce here
                isMember = true
            })

            if (!isMember)
              rules.push(rule)
          }
        }
      }
    }
  }
}

/**
 * Map of label to list of ActiveItems
 */
interface ActiveItemMap {[key: number]: ActiveItem[]}

/**
 * Chart
 */
class Chart {
  // private active: {[key: number]: ActiveItem} // key: FId
  private active: {[key: number]: ActiveItemMap} // key: FId
  private actives: {[key: number]: ActiveItemMap}[] // key: FId
  private passive: {[key: string]: FId}
  public forest: {[key: number]: Rule[]} // key: FId
  public nextId: number
  public offset: number

  public constructor(concrete: GFConcrete) {
    this.active = {}
    this.actives = []
    this.passive = {}
    this.forest = {}
    this.nextId = concrete.totalFIds
    this.offset = 0

    for (let fid in concrete.pproductions) {
      this.forest[fid] = concrete.pproductions[fid]
    }
  }

  public lookupAC(fid: FId, label: number): ActiveItem[] | null {
    let tmp = this.active[fid]
    if (tmp == null)
      return null
    return tmp[label]
  }

  public lookupACo(offset: number, fid: FId, label: number): ActiveItem[] | null {
    let tmp: ActiveItemMap

    if (offset == this.offset)
      tmp = this.active[fid]
    else
      tmp = this.actives[offset][fid]

    if (tmp == null)
      return null

    return tmp[label]
  }

  public labelsAC(fid: FId): ActiveItemMap {
    return this.active[fid]
  }

  public insertAC(fid: FId, label: number, items: ActiveItem[]): void {
    let tmp: ActiveItemMap = this.active[fid]
    if (tmp == null) {
      tmp = {}
      this.active[fid] = tmp
    }
    tmp[label] = items
  }

  public lookupPC(fid: FId, label: number, offset: number): FId {
    let key = fid+'.'+label+'-'+offset
    return this.passive[key]
  }

  public insertPC(fid1: FId, label: number, offset: number, fid2: FId): void {
    let key = fid1+'.'+label+'-'+offset
    this.passive[key] = fid2
  }

  public shift(): void {
    this.actives.push(this.active)
    this.active  = {}
    this.passive = {}
    this.offset++
  }

  public expandForest(fid: FId): Apply[] {
    let rules: Apply[] = []
    let forest = this.forest

    let go = function (rules0: Rule[]): void {
      for (let i in rules0) {
        let rule = rules0[i]
        switch (rule.id) {
          case 'Apply':
            rules.push(rule as Apply)
            break
          case 'Coerce':
            go(forest[(rule as Coerce).arg])
            break
        }
      }
    }

    go(this.forest[fid])
    return rules
  }
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
    let nargs: PArg[] = []
    for (let k in this.args) {
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

/* eslint-disable @typescript-eslint/no-explicit-any */

/* from Remedial JavaScript by Douglas Crockford, http://javascript.crockford.com/remedial.html */
// function isString(a: any): boolean {
//   return typeof a == 'string' || a instanceof String
// }
// function isArray(a: any): boolean {
//   return a && typeof a == 'object' && a.constructor == Array
// }
function isUndefined(a: any): boolean {
  return typeof a == 'undefined'
}
// function isBoolean(a: any): boolean {
//   return typeof a == 'boolean'
// }
// function isNumber(a: any): boolean {
//   return typeof a == 'number' && isFinite(a)
// }
// function isFunction(a: any): boolean {
//   return typeof a == 'function'
// }
// function dumpObject (obj: any): string {
//   if (isUndefined(obj)) {
//     return 'undefined'
//   } else if (isString(obj)) {
//     return '"' + obj.toString() + '"' // FIXME: escape
//   } else if (isBoolean(obj) || isNumber(obj)) {
//     return obj.toString()
//   } else if (isArray(obj)) {
//     let x = '['
//     for (let i = 0; i < obj.length; i++) {
//       x += dumpObject(obj[i])
//       if (i < obj.length-1) {
//         x += ','
//       }
//     }
//     return x + ']'
//   } else {
//     let x = '{'
//     for (let y in obj) {
//       x += y + '=' + dumpObject(obj[y]) + ';'
//     }
//     return x + '}'
//   }
// }

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith#Polyfill
// if (!String.prototype.startsWith) {
interface String {
  startsWith: (search: string, pos?: number) => boolean;
}
Object.defineProperty(String.prototype, 'startsWith', {
  value: function(search: string, pos?: number): boolean {
    pos = !pos || pos < 0 ? 0 : +pos
    return this.substring(pos, pos + search.length) === search
  }
})
// }
