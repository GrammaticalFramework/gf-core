/* eslint-disable @typescript-eslint/naming-convention */
import {
  Type,
  Hypo,
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
import { BE, WORDSIZE_BITS, WORDSIZE_BYTES, LINT_BASE } from './constants'

import ffi from 'ffi-napi'
import ref, { Pointer } from 'ref-napi'
import ref_struct from 'ref-struct-di'
const Struct = ref_struct(ref)

// ----------------------------------------------------------------------------
// FFI "Types"

export const voidPtr = ref.refType(ref.types.void)
const prob_t = ref.types.float
const size_t = ref.types.size_t

const PgfDB = ref.types.void
const PgfDBPtr = ref.refType(PgfDB)

const PgfRevision = ref.refType(ref.types.void)
export const PgfRevisionPtr = ref.refType(PgfRevision)

const PgfProbsCallback = ref.refType(ref.types.void)
export const PgfProbsCallbackPtr = ref.refType(PgfProbsCallback)

export const PgfExn = Struct({
  type: ref.types.int,
  code: ref.types.int,
  msg: ref.types.CString
})
export const PgfExnPtr = ref.refType(PgfExn)

const PgfText = Struct({
  size: ref.types.size_t,
  text: ref.types.char // char[]
})
export const PgfTextPtr = ref.refType(PgfText)

export const PgfItorPtr = ref.refType(ref.types.void)

const PgfType = ref.refType(ref.types.Object)

const PgfTypeHypo = Struct({
  bind_type: ref.types.int,
  cid: PgfTextPtr,
  type: PgfType
})

export const PgfExpr = ref.refType(ref.types.Object)
export const PgfLiteral = ref.refType(ref.types.Object)

// export const PgfTypePtr = ref.refType(PgfType)
export const PgfTypeHypoPtr = ref.refType(PgfTypeHypo)
export const PgfExprPtr = ref.refType(PgfExpr)
// export const PgfLiteralPtr = ref.refType(PgfLiteral)

const PgfMetaId = ref.types.int

const PgfPrintContext = ref.types.void // TODO
const PgfPrintContextPtr = ref.refType(PgfPrintContext)

const PgfUnmarshallerVtbl = Struct({
  eabs: ref.refType(ref.types.void),
  eapp: ref.refType(ref.types.void),
  elit: ref.refType(ref.types.void),
  emeta: ref.refType(ref.types.void),
  efun: ref.refType(ref.types.void),
  evar: ref.refType(ref.types.void),
  etyped: ref.refType(ref.types.void),
  eimplarg: ref.refType(ref.types.void),
  lint: ref.refType(ref.types.void),
  lflt: ref.refType(ref.types.void),
  lstr: ref.refType(ref.types.void),
  dtyp: ref.refType(ref.types.void),
  free_ref: ref.refType(ref.types.void)
})
const PgfUnmarshaller = Struct({
  vtbl: ref.refType(PgfUnmarshallerVtbl)
})
const PgfUnmarshallerPtr = ref.refType(PgfUnmarshaller)

const PgfMarshallerVtbl = Struct({
  match_lit: ref.refType(ref.types.void),
  match_expr: ref.refType(ref.types.void),
  match_type: ref.refType(ref.types.void)
})
const PgfMarshaller = Struct({
  vtbl: ref.refType(PgfMarshallerVtbl)
})
const PgfMarshallerPtr = ref.refType(PgfMarshaller)

// ----------------------------------------------------------------------------
// FFI

export const runtime = ffi.Library('libpgf', {
  pgf_read_pgf: [PgfDBPtr, [ref.types.CString, PgfRevisionPtr, PgfProbsCallbackPtr | null, PgfExnPtr]],
  pgf_boot_ngf: [PgfDBPtr, [ref.types.CString, ref.types.CString, PgfRevisionPtr, PgfProbsCallbackPtr | null, PgfExnPtr]],
  pgf_read_ngf: [PgfDBPtr, [ref.types.CString, PgfRevisionPtr, PgfExnPtr]],
  pgf_new_ngf: [PgfDBPtr, [PgfTextPtr, ref.types.CString, PgfRevisionPtr, PgfExnPtr]],
  pgf_write_pgf: [ref.types.void, [ref.types.CString, PgfDBPtr, PgfRevision, PgfExnPtr]],

  pgf_free_revision: [ref.types.void, [PgfDBPtr, PgfRevision]],

  pgf_abstract_name: [PgfTextPtr, [PgfDBPtr, PgfRevision, PgfExnPtr]],
  pgf_iter_categories: [ref.types.void, [PgfDBPtr, PgfRevision, PgfItorPtr, PgfExnPtr]],
  pgf_start_cat: [PgfType, [PgfDBPtr, PgfRevision, PgfUnmarshallerPtr, PgfExnPtr]],
  pgf_category_context: [PgfTypeHypoPtr, [PgfDBPtr, PgfRevision, PgfTextPtr, ref.refType(ref.types.size_t), PgfUnmarshallerPtr, PgfExnPtr]],
  pgf_category_prob: [prob_t, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],
  pgf_iter_functions: [ref.types.void, [PgfDBPtr, PgfRevision, PgfItorPtr, PgfExnPtr]],
  pgf_iter_functions_by_cat: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfItorPtr, PgfExnPtr]],
  pgf_function_type: [PgfType, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr]],
  pgf_function_is_constructor: [ref.types.int, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],
  pgf_function_prob: [prob_t, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],
  pgf_print_expr: [PgfTextPtr, [PgfExpr, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr]],
  pgf_read_expr: [PgfExpr, [PgfTextPtr, PgfUnmarshallerPtr]],
  pgf_read_expr_ex: [PgfExpr, [PgfTextPtr, ref.refType(ref.types.CString), PgfUnmarshallerPtr]],
  pgf_expr_prob: [prob_t, [PgfDBPtr, PgfRevision, PgfExpr, PgfMarshallerPtr, PgfExnPtr]],
  pgf_print_type: [PgfTextPtr, [PgfType, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr]],
  pgf_print_context: [PgfTextPtr, [size_t, PgfTypeHypoPtr, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr]],
  pgf_read_type: [PgfType, [PgfTextPtr, PgfUnmarshallerPtr]],

  pgf_start_transaction: [PgfRevision, [PgfDBPtr, PgfExnPtr]],
  pgf_commit_transaction: [ref.types.void, [PgfDBPtr, PgfRevision, PgfExnPtr]],
  pgf_rollback_transaction: [ref.types.void, [PgfDBPtr, PgfRevision]],
  pgf_checkout_revision: [PgfRevision, [PgfDBPtr, PgfExnPtr]],
  pgf_create_function: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfType, size_t, prob_t, PgfMarshallerPtr, PgfExnPtr]],
  pgf_drop_function: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],
  pgf_create_category: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, size_t, PgfTypeHypoPtr, prob_t, PgfMarshallerPtr, PgfExnPtr]],
  pgf_drop_category: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],

  pgf_get_global_flag: [PgfLiteral, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr]],
  pgf_set_global_flag: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfLiteral, PgfMarshallerPtr, PgfExnPtr]],
  pgf_get_abstract_flag: [PgfLiteral, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr]],
  pgf_set_abstract_flag: [ref.types.void, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfLiteral, PgfMarshallerPtr, PgfExnPtr]]
})

// ----------------------------------------------------------------------------
// Conversion helpers

export function PgfText_AsString (txtPtr: Pointer<any>): string {
  const txtSize = txtPtr.deref().size
  const charPtr = ref.reinterpret(txtPtr, txtSize, ref.types.size_t.size)
  return charPtr.toString('utf8')
}

export function PgfText_FromString (str: string): Pointer<any> {
  const strbuf = Buffer.from(str, 'utf8')
  const size = strbuf.length // size in bytes, not chars
  const sizebuf = ref.alloc(ref.types.size_t, size)
  const txtbuf = Buffer.alloc(ref.types.size_t.size + size + 1)
  sizebuf.copy(txtbuf, 0, 0)
  strbuf.copy(txtbuf, ref.types.size_t.size, 0)
  return txtbuf as Pointer<void>
}

// ----------------------------------------------------------------------------
// Un/marshalling

const eabs = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, ref.types.bool, PgfTextPtr, PgfExpr],
  function (self, btype: boolean, name: Pointer<any>, body: Pointer<Expr>): Pointer<ExprAbs> {
    const jsname = PgfText_AsString(name)
    const obj = new ExprAbs(btype, jsname, body.deref())
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprAbs>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const eapp = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfExpr, PgfExpr],
  function (self, fun: Pointer<Expr>, arg: Pointer<Type>): Pointer<ExprApp> {
    const obj = new ExprApp(fun.deref(), arg.deref())
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprApp>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const elit = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfLiteral],
  function (self, lit: Pointer<Literal>): Pointer<ExprLit> {
    const litObj = lit.deref()
    const obj = new ExprLit(litObj)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprLit>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const emeta = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfMetaId],
  function (self, meta: number): Pointer<ExprMeta> {
    const obj = new ExprMeta(meta)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprMeta>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const efun = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfTextPtr],
  function (self, name: Pointer<any>): Pointer<ExprFun> {
    const jsname = PgfText_AsString(name)
    const obj = new ExprFun(jsname)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprFun>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const evar = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, ref.types.int],
  function (self, index: number): Pointer<ExprVar> {
    const obj = new ExprVar(index)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprVar>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const etyped = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfExpr, PgfType],
  function (self, expr: Pointer<Expr>, type: Pointer<Type>): Pointer<ExprTyped> {
    const obj = new ExprTyped(expr.deref(), type.deref())
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprTyped>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const eimplarg = ffi.Callback(PgfExpr, [PgfUnmarshallerPtr, PgfExpr],
  function (self, expr: Pointer<Expr>): Pointer<ExprImplArg> {
    const obj = new ExprImplArg(expr.deref())
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprImplArg>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const lint = ffi.Callback(PgfLiteral, [PgfUnmarshallerPtr, ref.types.size_t, ref.refType(ref.types.int)],
  function (self, size: number, val: Pointer<number>): Pointer<Literal> {
    let jsval: number | bigint = 0
    if (size === 1) {
      jsval = val.deref()
    } else if (size > 1) {
      jsval = BigInt(val.deref())
      const pos = jsval >= 0
      const vals = ref.reinterpret(val, size * WORDSIZE_BYTES)
      for (let n = 1; n < size; n++) {
        let thisval
        switch (WORDSIZE_BITS) {
          case 64:
            thisval = BigInt(BE ? vals.readUInt64BE(n * WORDSIZE_BYTES) : vals.readUInt64LE(n * WORDSIZE_BYTES))
            break
          case 32:
            thisval = BigInt(BE ? vals.readUInt32BE(n * WORDSIZE_BYTES) : vals.readUInt32LE(n * WORDSIZE_BYTES))
            break
          case 16:
            thisval = BigInt(BE ? vals.readUInt16BE(n * WORDSIZE_BYTES) : vals.readUInt16LE(n * WORDSIZE_BYTES))
            break
          case 8:
            thisval = BigInt(vals.readUInt8(n * WORDSIZE_BYTES))
            break
        }
        if (thisval == null) {
          throw Error(`Unsupported word size: ${WORDSIZE_BITS}`)
        }
        jsval = (jsval * LINT_BASE) + (pos ? thisval : -thisval)
      }
    }

    const obj = new Literal(jsval)
    const buf = ref.alloc(ref.types.Object) as Pointer<Literal>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const lflt = ffi.Callback(PgfLiteral, [PgfUnmarshallerPtr, ref.types.double],
  function (self, val: number): Pointer<Literal> {
    const obj = new Literal(val)
    const buf = ref.alloc(ref.types.Object) as Pointer<Literal>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const lstr = ffi.Callback(PgfLiteral, [PgfUnmarshallerPtr, PgfTextPtr],
  function (self, val: Pointer<any>): Pointer<Literal> {
    const jsval = PgfText_AsString(val)
    const obj = new Literal(jsval)
    const buf = ref.alloc(ref.types.Object) as Pointer<Literal>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const dtyp = ffi.Callback(PgfType, [PgfUnmarshallerPtr, ref.types.int, PgfTypeHypoPtr, PgfTextPtr, ref.types.int, PgfExprPtr],
  function (self, n_hypos: number, hypos: Pointer<any>, cat: Pointer<any>, n_exprs: number, exprs: Pointer<any>): Pointer<Type> {
    const jshypos: Hypo[] = []
    if (n_hypos > 0) {
      const hyposArr = hypos.reinterpret(PgfTypeHypo.size * n_hypos, 0)
      for (let i = 0; i < n_hypos; i++) {
        const hypo = ref.get(hyposArr, PgfTypeHypo.size * i, PgfTypeHypo)
        const h = new Hypo(hypo.bind_type, PgfText_AsString(hypo.cid), hypo.type.deref() as Type)
        jshypos.push(h)
      }
    }

    const jsname = PgfText_AsString(cat)

    const jsexprs: Expr[] = []
    if (n_exprs > 0) {
      const exprsArr = exprs.reinterpret(PgfExpr.size * n_exprs, 0)
      for (let i = 0; i < n_exprs; i++) {
        const expr = ref.get(exprsArr, PgfExpr.size * i, PgfExpr)
        jsexprs.push(expr.deref() as Expr)
      }
    }

    const obj = new Type(jshypos, jsname, jsexprs)
    const buf = ref.alloc(ref.types.Object) as Pointer<Type>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const free_ref = ffi.Callback(ref.types.void, [PgfUnmarshallerPtr, ref.refType(ref.types.Object)],
  function (self, x: Pointer<any>): void {
  })

const un_vtbl = new PgfUnmarshallerVtbl({
  eabs: eabs as any,
  eapp: eapp as any,
  elit: elit as any,
  emeta: emeta as any,
  efun: efun as any,
  evar: evar as any,
  etyped: etyped as any,
  eimplarg: eimplarg as any,
  lint: lint as any,
  lflt: lflt as any,
  lstr: lstr as any,
  dtyp: dtyp as any,
  free_ref: free_ref as any
})

export const unmarshaller = new PgfUnmarshaller({ vtbl: un_vtbl.ref() })

const match_lit = ffi.Callback(ref.types.Object, [PgfMarshallerPtr, PgfUnmarshallerPtr, PgfLiteral],
  function (self, u: any, lit: any): Pointer<any> {
    return 0 as any
  })

const match_expr = ffi.Callback(ref.types.Object, [PgfMarshallerPtr, PgfUnmarshallerPtr, PgfExpr],
  function (self, u: any, expr: any): Pointer<any> {
    return 0 as any
  })

const match_type = ffi.Callback(ref.types.Object, [PgfMarshallerPtr, PgfUnmarshallerPtr, PgfType],
  function (self, u: any, type: any): Pointer<any> {
    return 0 as any
  })

const vtbl = new PgfMarshallerVtbl({
  match_lit: match_lit as any,
  match_expr: match_expr as any,
  match_type: match_type as any
})

export const marshaller = new PgfMarshaller({ vtbl: vtbl.ref() })
