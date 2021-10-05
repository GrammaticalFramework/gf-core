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
  dummy!: string
}

export class ExprAbs extends Expr {

}
export class ExprApp extends Expr {

}
export class ExprLit extends Expr {

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
