import errno from './errno'
import ffi from 'ffi'
import { deref } from 'ref'
import ref from 'ref'
import Struct from 'ref-struct'

// ----------------------------------------------------------------------------
// FFI "Types"

const PgfDB = ref.types.int
const PgfDBPtr = ref.refType(PgfDB)
const PgfRevision = ref.types.int
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

// ----------------------------------------------------------------------------
// TypeScript Types

interface Pointer extends Buffer {
  deref(): any
}

class PGFError extends Error {
  constructor(message: string) {
    super(message)
  }
}

// ----------------------------------------------------------------------------
// FFI

const runtime = ffi.Library('libpgf', {
  'pgf_read_pgf': [ PgfDBPtr, [ ref.types.CString, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_write_pgf': [ ref.types.void, [ ref.types.CString, PgfDBPtr, PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_abstract_name': [ PgfTextPtr, [ PgfDBPtr, PgfRevision, PgfExnPtr ] ],
})

// ----------------------------------------------------------------------------
// Helpers

function handleError (errPtr: Pointer): void {
  const err = errPtr.deref()
  switch (err.type) {
    // PGF_EXN_NONE
    case 0: return

    // PGF_EXN_SYSTEM_ERROR
    case 1:
      const desc = errno.lookup(err.code)
      throw new Error(`${desc}: ${err.msg}`)

    // PGF_EXN_PGF_ERROR
    case 2:
      throw new PGFError(err.msg)

    // PGF_EXN_OTHER_ERROR
    case 3:
      throw new Error(err.msg)

    default:
      throw new Error(`unknown error type: ${err.type}`)
  }
}

function PgfText_AsString (txtPtr: Pointer) {
  const txtSize = txtPtr.deref().size
  const charPtr = ref.reinterpret(txtPtr, txtSize, ref.types.size_t.size)
  return charPtr.toString('utf8')
}

// ----------------------------------------------------------------------------
// PGF grammar object

class PGF {
  db: Pointer
  revision: number

  constructor () {
    this.db = ref.NULL_POINTER as Pointer
    this.revision = 0
  }

  getAbstractName () {
    const err = ref.alloc(PgfExn) as Pointer
    const txt = runtime.pgf_abstract_name(this.db, this.revision, err)
    handleError(err)
    return PgfText_AsString(txt)
  }

}

// ----------------------------------------------------------------------------
// PGF module functions

function readPGF (path: string): PGF {
  const rev = ref.alloc(PgfRevision) as Pointer
  const err = ref.alloc(PgfExn) as Pointer
  const db = runtime.pgf_read_pgf(path, rev, err)
  handleError(err)

  const state = new PGF()
  state.db = db
  state.revision = rev.deref()
  return state
}

// ----------------------------------------------------------------------------
// Exposed library API

export default {
  readPGF
}
