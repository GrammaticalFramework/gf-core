/* eslint-disable @typescript-eslint/naming-convention */
import { ExprLit, Literal } from './expr'

import os from 'os'
import ffi from 'ffi-napi'
import ref, { Pointer } from 'ref-napi'
import ref_struct from 'ref-struct-di'
const Struct = ref_struct(ref)

export const PgfExpr = ref.refType(ref.types.Object)
export const PgfLiteral = ref.refType(ref.types.Object)

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
export const PgfUnmarshaller = Struct({
  vtbl: ref.refType(PgfUnmarshallerVtbl)
})

const elit = ffi.Callback(PgfExpr, [PgfUnmarshaller, PgfLiteral],
  function (self, lit: Pointer<any>): Pointer<ExprLit> {
    const litObj = lit.deref() as Literal
    const obj = new ExprLit(litObj)
    const buf = ref.alloc(ref.types.Object) as Pointer<ExprLit>
    ref.writeObject(buf, 0, obj)
    return buf
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

const free_ref = ffi.Callback(ref.types.void, [PgfUnmarshaller, ref.refType(ref.types.void)],
  function (self, x: any): void {
  })

const vtbl = new PgfUnmarshallerVtbl({
  eabs: ref.alloc('void').ref(),
  eapp: ref.alloc('void').ref(),
  elit: elit as any,
  emeta: ref.alloc('void').ref(),
  efun: ref.alloc('void').ref(),
  evar: ref.alloc('void').ref(),
  etyped: ref.alloc('void').ref(),
  eimplarg: ref.alloc('void').ref(),
  lint: lint as any,
  lflt: ref.alloc('void').ref(),
  lstr: ref.alloc('void').ref(),
  dtyp: ref.alloc('void').ref(),
  free_ref: free_ref as any
})

export const unmarshaller = new PgfUnmarshaller({ vtbl: vtbl.ref() })
export const marshaller = {}
