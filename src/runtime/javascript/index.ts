/* eslint-disable @typescript-eslint/naming-convention */

import errno from './errno'
import {
  Type, mkType,
  Hypo, mkHypo, mkDepHypo, mkImplHypo,
  Expr,
  ExprAbs,
  ExprApp,
  ExprLit,
  ExprMeta,
  ExprFun,
  ExprVar,
  ExprTyped,
  ExprImplArg,
  Literal
} from './expr'
import {
  voidPtr,
  PgfRevisionPtr,
  PgfItorPtr,
  PgfTextPtr,
  PgfExn,
  PgfExnPtr,

  PgfText_AsString,
  PgfText_FromString,

  runtime,
  unmarshaller
} from './ffi'

import ffi from 'ffi-napi'
import ref, { Pointer } from 'ref-napi'
import { StructObject } from 'ref-struct-di'

// ----------------------------------------------------------------------------
// TypeScript Types

interface PgfExnType {
  type: number
  code: number
  msg: string | null
}

class PGFError extends Error {
  constructor (message: string) {
    super(message)
    // https://stackoverflow.com/a/65243177/98600
    Object.setPrototypeOf(this, PGFError.prototype)
  }
}

// ----------------------------------------------------------------------------
// Helpers

function handleError (err: StructObject<PgfExnType>): void {
  switch (err.type) {
    // PGF_EXN_NONE
    case 0: return

    // PGF_EXN_SYSTEM_ERROR
    case 1: {
      const desc = errno.lookup(err.code)
      throw new Error(`${desc as string}: ${err.msg as string}`)
    }
    // PGF_EXN_PGF_ERROR
    case 2:
      throw new PGFError(err.msg as string)

    // PGF_EXN_OTHER_ERROR
    case 3:
      throw new Error(err.msg as string)

    default:
      throw new Error(`unknown error type: ${err.type}`)
  }
}

// ----------------------------------------------------------------------------
// PGF grammar object

class PGFGrammar {
  readonly db: Pointer<any>
  readonly revision: Pointer<any>

  constructor (db: Pointer<any>, revision: Pointer<any>) {
    this.db = db
    this.revision = revision
  }

  // NB the library user is responsible for calling this
  release (): void {
    runtime.pgf_free_revision(this.db, this.revision.deref())
  }

  getAbstractName (): string {
    const err = new PgfExn()
    const txt = runtime.pgf_abstract_name(this.db, this.revision.deref(), err.ref())
    handleError(err)
    return PgfText_AsString(txt)
  }

  getCategories (): string[] {
    const cats: string[] = []
    const callback = ffi.Callback(ref.types.void, [PgfItorPtr, PgfTextPtr, voidPtr, PgfExnPtr],
      function (self: Pointer<any>, key: Pointer<any>, value: Pointer<any>, err: Pointer<any>) {
        const k = PgfText_AsString(key)
        cats.push(k)
      })
    const err = new PgfExn()
    runtime.pgf_iter_categories(this.db, this.revision.deref(), callback.ref() as Pointer<void>, err.ref())
    handleError(err)
    return cats
  }

  getFunctions (): string[] {
    const funs: string[] = []
    const callback = ffi.Callback(ref.types.void, [PgfItorPtr, PgfTextPtr, voidPtr, PgfExnPtr],
      function (self: Pointer<any>, key: Pointer<any>, value: Pointer<any>, err: Pointer<any>) {
        const k = PgfText_AsString(key)
        funs.push(k)
      })
    const err = new PgfExn()
    runtime.pgf_iter_functions(this.db, this.revision.deref(), callback.ref() as Pointer<void>, err.ref())
    handleError(err)
    return funs
  }

  categoryProbability (cat: string): number | undefined {
    const catname = PgfText_FromString(cat)
    const err = new PgfExn()
    const prob = runtime.pgf_category_prob(this.db, this.revision.deref(), catname, err.ref())
    handleError(err)
    if (prob === Infinity) {
      return undefined
    } else {
      return prob
    }
  }

  functionProbability (fun: string): number | undefined {
    const funname = PgfText_FromString(fun)
    const err = new PgfExn()
    const prob = runtime.pgf_function_prob(this.db, this.revision.deref(), funname, err.ref())
    handleError(err)
    if (prob === Infinity) {
      return undefined
    } else {
      return prob
    }
  }

  functionIsConstructor (fun: string): boolean {
    const funname = PgfText_FromString(fun)
    const err = new PgfExn()
    const isCon = runtime.pgf_function_is_constructor(this.db, this.revision.deref(), funname, err.ref())
    handleError(err)
    return Boolean(isCon)
  }

  functionsByCategory (cat: string): string[] {
    const catname = PgfText_FromString(cat)
    const funs: string[] = []
    const callback = ffi.Callback(ref.types.void, [PgfItorPtr, PgfTextPtr, voidPtr, PgfExnPtr],
      function (self: Pointer<any>, key: Pointer<any>, value: Pointer<any>, err: Pointer<any>) {
        const k = PgfText_AsString(key)
        funs.push(k)
      })
    const err = new PgfExn()
    runtime.pgf_iter_functions_by_cat(this.db, this.revision.deref(), catname, callback.ref() as Pointer<void>, err.ref())
    handleError(err)
    return funs
  }
}

// ----------------------------------------------------------------------------
// PGF module functions

function readPGF (path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn()
  const db = runtime.pgf_read_pgf(path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function bootNGF (pgf_path: string, ngf_path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn()
  const db = runtime.pgf_boot_ngf(pgf_path, ngf_path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function readNGF (path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn()
  const db = runtime.pgf_read_ngf(path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function newNGF (abstract_name: string, path?: string): PGFGrammar {
  const absname = PgfText_FromString(abstract_name)
  const fpath = path != null ? path : null
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn()
  const db = runtime.pgf_new_ngf(absname, fpath, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function readType (str: string): Type {
  const txt = PgfText_FromString(str)
  const type = runtime.pgf_read_type(txt, unmarshaller.ref())
  if (ref.isNull(type)) {
    throw new PGFError('unable to parse type')
  }
  return ref.readObject(type) as Type
}

function readExpr (str: string): Expr {
  const txt = PgfText_FromString(str)
  const expr = runtime.pgf_read_expr(txt, unmarshaller.ref())
  if (ref.isNull(expr)) {
    throw new PGFError('unable to parse expression')
  }
  return ref.readObject(expr) as Expr
}

function showType (context: string[], type: Type): string {
  return 'TODO'
}

// ----------------------------------------------------------------------------
// Exposed library API

export {
  PGFGrammar,
  PGFError,
  readPGF,
  bootNGF,
  readNGF,
  newNGF,

  readType,
  readExpr,
  showType,

  Type,
  mkType,
  Hypo,
  mkHypo,
  mkDepHypo,
  mkImplHypo,
  Expr,
  ExprAbs,
  ExprApp,
  ExprLit,
  ExprMeta,
  ExprFun,
  ExprVar,
  ExprTyped,
  ExprImplArg,
  Literal
}
export default {
  PGFGrammar,
  PGFError,
  readPGF,
  bootNGF,
  readNGF,
  newNGF,

  readType,
  readExpr,
  showType,

  Type,
  mkType,
  Hypo,
  mkHypo,
  mkDepHypo,
  mkImplHypo,
  Expr,
  ExprAbs,
  ExprApp,
  ExprLit,
  ExprMeta,
  ExprFun,
  ExprVar,
  ExprTyped,
  ExprImplArg,
  Literal
}
