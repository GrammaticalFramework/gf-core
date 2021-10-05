/* eslint-disable @typescript-eslint/naming-convention */

import ffi from 'ffi-napi'
import ref, { Pointer } from 'ref-napi'
import ref_struct from 'ref-struct-di'
const Struct = ref_struct(ref)

export const PgfExpr = ref.refType(ref.types.void)
export const PgfLiteral = ref.refType(ref.types.void)

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
  function (self, lit: Pointer<any>): any { // eslint-disable-line @typescript-eslint/no-unused-vars
    console.log('myelit')
    console.log('  lit', lit)
    return lit.deref()
  })

const lint = ffi.Callback(PgfLiteral, [PgfUnmarshaller, ref.types.size_t, ref.refType(ref.types.uint)],
  function (self, size: number, val: Pointer<number>): any { // eslint-disable-line @typescript-eslint/no-unused-vars
    console.log('mylint')
    console.log('  size', size)
    console.log('  val', val.deref())
    // const x: number = val.deref()
    return val
  })

const free_ref = ffi.Callback(ref.types.void, [PgfUnmarshaller, ref.refType(ref.types.void)],
  function (self, x: any): void { // eslint-disable-line @typescript-eslint/no-unused-vars
    console.log('free_ref')
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
