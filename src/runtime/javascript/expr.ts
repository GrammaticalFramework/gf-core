export class Type {
  hypos!: Hypo[]
  name!: string
  exprs!: Expr[]
}

export class Hypo {
  bind_type!: boolean
  var!: string
  type!: Type
}

export class Expr {
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

}
export class ExprApp extends Expr {

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

}
export class ExprFun extends Expr {

}
export class ExprVar extends Expr {

}
export class ExprTyped extends Expr {

}
export class ExprImplArg extends Expr {

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
