export class Type {
  hypos: Hypo[]
  name: string
  exprs: Expr[]

  constructor (hs: Hypo[], n: string, es: Expr[]) {
    this.hypos = hs
    this.name = n
    this.exprs = es
  }
}

export class Hypo {
  bind_type!: boolean
  var!: string
  type!: Type
}

export class Expr {
  // TODO overload
  constructor (n?: number) {
    if (n != null) {
      return new ExprLit(n)
    }
  }

  toString (): string {
    // TODO call showExpr
    return this.toString()
  }
}

export class ExprAbs extends Expr {
  bind_type: boolean
  name: string
  body: Expr

  constructor (bt: boolean, n: string, b: Expr) {
    super()
    this.bind_type = bt
    this.name = n
    this.body = b
  }
}

export class ExprApp extends Expr {
  fun: Expr
  arg: Expr

  constructor (f: Expr, a: Expr) {
    super()
    this.fun = f
    this.arg = a
  }
}

export class ExprLit extends Expr {
  lit: Literal

  constructor (l: Literal | number | bigint | string) {
    super()
    if (l instanceof Literal) this.lit = l
    else this.lit = new Literal(l)
  }

  toString (): string {
    return this.lit.toString()
  }
}

export class ExprMeta extends Expr {
  id: number

  constructor (i: number) {
    super()
    this.id = i
  }
}

export class ExprFun extends Expr {
  name: string

  constructor (n: string) {
    super()
    this.name = n
  }
}

export class ExprVar extends Expr {
  var: number

  constructor (v: number) {
    super()
    this.var = v
  }
}

export class ExprTyped extends Expr {
  expr: Expr
  type: Type

  constructor (e: Expr, t: Type) {
    super()
    this.expr = e
    this.type = t
  }
}

export class ExprImplArg extends Expr {
  expr: Expr

  constructor (e: Expr) {
    super()
    this.expr = e
  }
}

export class Literal {
  val: number | bigint | string

  constructor (v: number | bigint | string) {
    this.val = v
  }

  toString (): string {
    return this.val.toString()
  }
}
