/* eslint-disable @typescript-eslint/naming-convention */
import {
  Type,
  // Hypo,
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

import os from 'os'
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

const PgfType = ref.types.void // TODO
// const PgfTypePtr = ref.refType(PgfType)

const PgfTypeHypo = ref.types.void // TODO
const PgfTypeHypoPtr = ref.refType(PgfTypeHypo)

export const PgfExpr = ref.refType(ref.types.Object)
export const PgfLiteral = ref.refType(ref.types.Object)

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
  pgf_read_pgf: [PgfDBPtr, [ref.types.CString, PgfRevisionPtr, PgfExnPtr]],
  pgf_boot_ngf: [PgfDBPtr, [ref.types.CString, ref.types.CString, PgfRevisionPtr, PgfExnPtr]],
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

  pgf_clone_revision: [PgfRevision, [PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr]],
  pgf_commit_revision: [ref.types.void, [PgfDBPtr, PgfRevision, PgfExnPtr]],
  pgf_checkout_revision: [PgfRevision, [PgfDBPtr, PgfTextPtr, PgfExnPtr]],
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

const eabs = ffi.Callback(PgfExpr, [PgfUnmarshaller, ref.types.bool, PgfTextPtr, PgfExpr],
  function (self, btype: boolean, name: Pointer<any>, body: Pointer<any>): Pointer<ExprAbs> {
    return 0 as any
  })

const eapp = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfExpr, PgfExpr],
  function (self, fun: Pointer<any>, arg: Pointer<any>): Pointer<ExprApp> {
    return 0 as any
  })

const elit = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfLiteral],
  function (self, lit: Pointer<any>): Pointer<ExprLit> {
    const litObj = lit.deref() as Literal
    const obj = new ExprLit(litObj)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprLit>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const emeta = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfMetaId],
  function (self, meta: Pointer<any>): Pointer<ExprMeta> {
    return 0 as any
  })

const efun = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfTextPtr],
  function (self, name: Pointer<any>): Pointer<ExprFun> {
    return 0 as any
  })

const evar = ffi.Callback(PgfExpr, [PgfUnmarshaller, ref.types.int],
  function (self, index: number): Pointer<ExprVar> {
    return 0 as any
  })

const etyped = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfExpr, PgfType],
  function (self, expr: Pointer<any>, type: Pointer<any>): Pointer<ExprTyped> {
    return 0 as any
  })

const eimplarg = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfExpr],
  function (self, expr: Pointer<any>): Pointer<ExprImplArg> {
    return 0 as any
  })

// TODO get from platform/runtime
const WORDSIZE_BITS = 64
const WORDSIZE_BYTES = WORDSIZE_BITS / 8
const LINT_BASE = BigInt('10000000000000000000')

const lint = ffi.Callback(PgfLiteral, [PgfUnmarshaller, ref.types.size_t, ref.refType(ref.types.int)],
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
            thisval = BigInt((os.endianness() === 'LE') ? vals.readUInt64LE(n * WORDSIZE_BYTES) : vals.readUInt64BE(n * WORDSIZE_BYTES))
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

const lflt = ffi.Callback(PgfLiteral, [PgfUnmarshaller, ref.types.double],
  function (self, val: number): Pointer<Literal> {
    const obj = new Literal(val)
    const buf = ref.alloc(ref.types.Object) as Pointer<Literal>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const lstr = ffi.Callback(PgfLiteral, [PgfUnmarshaller, PgfTextPtr],
  function (self, val: Pointer<any>): Pointer<Literal> {
    const jsval = PgfText_AsString(val)
    const obj = new Literal(jsval)
    const buf = ref.alloc(ref.types.Object) as Pointer<Literal>
    ref.writeObject(buf, 0, obj)
    return buf
  })

const dtyp = ffi.Callback(PgfType, [PgfUnmarshaller, ref.types.int, PgfTypeHypoPtr, PgfTextPtr, ref.types.int, ref.refType(PgfExpr)],
  function (self, n_hypos: number, hypos: Pointer<any>, cat: Pointer<any>, n_exprs: number, exprs: Pointer<any>): Pointer<Type> {
    return 0 as any
  })

const free_ref = ffi.Callback(ref.types.void, [PgfUnmarshaller, ref.refType(ref.types.Object)],
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

const match_lit = ffi.Callback(ref.types.Object, [PgfMarshaller, PgfUnmarshaller, PgfLiteral],
  function (self, u: any, lit: any): Pointer<any> {
    return 0 as any
  })

const match_expr = ffi.Callback(ref.types.Object, [PgfMarshaller, PgfUnmarshaller, PgfExpr],
  function (self, u: any, expr: any): Pointer<any> {
    return 0 as any
  })

const match_type = ffi.Callback(ref.types.Object, [PgfMarshaller, PgfUnmarshaller, PgfType],
  function (self, u: any, type: any): Pointer<any> {
    return 0 as any
  })

const vtbl = new PgfMarshallerVtbl({
  match_lit: match_lit as any,
  match_expr: match_expr as any,
  match_type: match_type as any
})

export const marshaller = new PgfMarshaller({ vtbl: vtbl.ref() })
