import errno from './errno'
import ffi from 'ffi-napi'
import ref, { Pointer } from 'ref-napi'
import ref_struct, { StructObject } from 'ref-struct-di'
const Struct = ref_struct(ref)

// ----------------------------------------------------------------------------
// FFI "Types"

const voidPtr = ref.refType(ref.types.void)
const prob_t = ref.types.float
const size_t = ref.types.size_t

const PgfDB = ref.types.void
const PgfDBPtr = ref.refType(PgfDB)

const PgfRevision = ref.refType(ref.types.void)
const PgfRevisionPtr = ref.refType(PgfRevision)

const PgfExn = Struct({
  type: ref.types.int,
  code: ref.types.int,
  msg: ref.types.CString
})
const PgfExnPtr = ref.refType(PgfExn)

const PgfText = Struct({
  size: ref.types.size_t,
  text: ref.types.char // char[]
})
const PgfTextPtr = ref.refType(PgfText)

const PgfItorPtr = ref.refType(ref.types.void)

const PgfType = ref.types.void // TODO
// const PgfTypePtr = ref.refType(PgfType)

const PgfTypeHypo = ref.types.void // TODO
const PgfTypeHypoPtr = ref.refType(PgfTypeHypo)

const PgfExpr = ref.types.void // TODO
const PgfLiteral = ref.types.void // TODO

const PgfPrintContext = ref.types.void // TODO
const PgfPrintContextPtr = ref.refType(PgfPrintContext)

const PgfUnmarshaller = ref.types.void // TODO
const PgfUnmarshallerPtr = ref.refType(PgfUnmarshaller)

const PgfMarshaller = ref.types.void // TODO
const PgfMarshallerPtr = ref.refType(PgfMarshaller)


// ----------------------------------------------------------------------------
// TypeScript Types

interface PgfExnType {
  type: number
  code: number
  msg: string | null
}

class PGFError extends Error {
  constructor(message: string) {
    super(message)
    // https://stackoverflow.com/a/65243177/98600
    Object.setPrototypeOf(this, PGFError.prototype)
  }
}

// ----------------------------------------------------------------------------
// FFI

const runtime = ffi.Library('libpgf', {
  'pgf_read_pgf': [ PgfDBPtr, [ ref.types.CString, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_boot_ngf': [ PgfDBPtr, [ ref.types.CString, ref.types.CString, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_read_ngf': [ PgfDBPtr, [ ref.types.CString, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_new_ngf': [ PgfDBPtr, [ PgfTextPtr, ref.types.CString, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_write_pgf': [ ref.types.void, [ ref.types.CString, PgfDBPtr, PgfRevision, PgfExnPtr ] ],

  'pgf_free_revision': [ ref.types.void, [ PgfDBPtr, PgfRevision ] ],

  'pgf_abstract_name': [ PgfTextPtr, [ PgfDBPtr, PgfRevision, PgfExnPtr ] ],
  'pgf_iter_categories': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfItorPtr, PgfExnPtr ] ],
  'pgf_start_cat': [ PgfType, [ PgfDBPtr, PgfRevision, PgfUnmarshallerPtr, PgfExnPtr ] ],
  'pgf_category_context': [ PgfTypeHypoPtr, [ PgfDBPtr, PgfRevision, PgfTextPtr, ref.refType(ref.types.size_t), PgfUnmarshallerPtr, PgfExnPtr ] ],
  'pgf_category_prob': [ prob_t, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr ] ],
  'pgf_iter_functions': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfItorPtr, PgfExnPtr ] ],
  'pgf_iter_functions_by_cat': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfItorPtr, PgfExnPtr ] ],
  'pgf_function_type': [ PgfType, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr ] ],
  'pgf_function_is_constructor': [ ref.types.int, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr ] ],
  'pgf_function_prob': [ prob_t, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr ] ],
  'pgf_print_expr': [ PgfTextPtr, [ PgfExpr, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr ] ],
  'pgf_read_expr': [ PgfExpr, [ PgfTextPtr, PgfUnmarshallerPtr ] ],
  'pgf_read_expr_ex': [ PgfExpr, [ PgfTextPtr, ref.refType(ref.types.CString), PgfUnmarshallerPtr ] ],
  'pgf_expr_prob': [ prob_t, [ PgfDBPtr, PgfRevision, PgfExpr, PgfMarshallerPtr, PgfExnPtr ] ],
  'pgf_print_type': [ PgfTextPtr, [ PgfType, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr ] ],
  'pgf_print_context': [ PgfTextPtr, [ size_t, PgfTypeHypoPtr, PgfPrintContextPtr, ref.types.int, PgfMarshallerPtr ] ],
  'pgf_read_type': [ PgfType, [ PgfTextPtr, PgfUnmarshallerPtr ] ],

  'pgf_clone_revision': [ PgfRevision, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr ] ],
  'pgf_commit_revision': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfExnPtr ] ],
  'pgf_checkout_revision': [ PgfRevision, [ PgfDBPtr, PgfTextPtr, PgfExnPtr ] ],
  'pgf_create_function': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfType, size_t, prob_t, PgfMarshallerPtr, PgfExnPtr ] ],
  'pgf_drop_function': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr,  PgfExnPtr ] ],
  'pgf_create_category': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, size_t, PgfTypeHypoPtr, prob_t, PgfMarshallerPtr, PgfExnPtr ] ],
  'pgf_drop_category': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfExnPtr ] ],

  'pgf_get_global_flag': [ PgfLiteral, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr ] ],
  'pgf_set_global_flag': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfLiteral, PgfMarshallerPtr, PgfExnPtr ] ],
  'pgf_get_abstract_flag': [ PgfLiteral, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfUnmarshallerPtr, PgfExnPtr ] ],
  'pgf_set_abstract_flag': [ ref.types.void, [ PgfDBPtr, PgfRevision, PgfTextPtr, PgfLiteral, PgfMarshallerPtr, PgfExnPtr ] ],
})

// ----------------------------------------------------------------------------
// Helpers

function handleError (err: StructObject<PgfExnType>): void {
  switch (err.type) {
    // PGF_EXN_NONE
    case 0: return

    // PGF_EXN_SYSTEM_ERROR
    case 1: {
      const desc = errno.lookup(err.code)
      throw new Error(`${desc}: ${err.msg}`)
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

function PgfText_AsString (txtPtr: Pointer<any>): string {
  const txtSize = txtPtr.deref().size
  const charPtr = ref.reinterpret(txtPtr, txtSize, ref.types.size_t.size)
  return charPtr.toString('utf8')
}

function PgfText_FromString (str: string): Pointer<any> {
  const strbuf = Buffer.from(str, 'utf8')
  const size = strbuf.length // size in bytes, not chars
  const sizebuf = ref.alloc(ref.types.size_t, size)
  const txtbuf = Buffer.alloc(ref.types.size_t.size + size + 1)
  sizebuf.copy(txtbuf, 0, 0)
  strbuf.copy(txtbuf, ref.types.size_t.size, 0)
  return txtbuf as Pointer<void>
}

// ----------------------------------------------------------------------------
// PGF grammar object

export class PGFGrammar {
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
    const err = new PgfExn
    const txt = runtime.pgf_abstract_name(this.db, this.revision.deref(), err.ref())
    handleError(err)
    return PgfText_AsString(txt)
  }

  getCategories (): string[] {
    const cats: string[] = []
    const callback = ffi.Callback(ref.types.void, [ PgfItorPtr, PgfTextPtr, voidPtr, PgfExnPtr ],
      function (self: Pointer<any>, key: Pointer<any>, value: Pointer<any>, err: Pointer<any>) { // eslint-disable-line @typescript-eslint/no-unused-vars
        const k = PgfText_AsString(key)
        cats.push(k)
    })
    const err = new PgfExn
    runtime.pgf_iter_categories(this.db, this.revision.deref(), callback.ref() as Pointer<void>, err.ref())
    handleError(err)
    return cats
  }

  getFunctions (): string[] {
    const funs: string[] = []
    const callback = ffi.Callback(ref.types.void, [ PgfItorPtr, PgfTextPtr, voidPtr, PgfExnPtr ],
      function (self: Pointer<any>, key: Pointer<any>, value: Pointer<any>, err: Pointer<any>) { // eslint-disable-line @typescript-eslint/no-unused-vars
        const k = PgfText_AsString(key)
        funs.push(k)
    })
    const err = new PgfExn
    runtime.pgf_iter_functions(this.db, this.revision.deref(), callback.ref() as Pointer<void>, err.ref())
    handleError(err)
    return funs
  }

}

// ----------------------------------------------------------------------------
// PGF module functions

function readPGF (path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn
  const db = runtime.pgf_read_pgf(path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function bootNGF (pgf_path: string, ngf_path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn
  const db = runtime.pgf_boot_ngf(pgf_path, ngf_path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function readNGF (path: string): PGFGrammar {
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn
  const db = runtime.pgf_read_ngf(path, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

function newNGF (abstract_name: string, path?: string): PGFGrammar {
  const absname = PgfText_FromString(abstract_name)
  const fpath = path != null ? path : null
  const rev = ref.alloc(PgfRevisionPtr)
  const err = new PgfExn
  const db = runtime.pgf_new_ngf(absname, fpath, rev, err.ref())
  handleError(err)
  return new PGFGrammar(db, rev)
}

// ----------------------------------------------------------------------------
// Exposed library API

export default {
  PGFError,
  readPGF,
  bootNGF,
  readNGF,
  newNGF
}
