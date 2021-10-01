import { deref } from 'ref'
import * as ref from 'ref'
import * as ffi from 'ffi'

const PgfDB = ref.types.int
const PgfDBPtr = ref.refType(PgfDB)
const PgfRevision = ref.types.int
const PgfRevisionPtr = ref.refType(PgfRevision)
const PgfExn = ref.types.int
const PgfExnPtr = ref.refType(PgfExn)
// const stringPtr = ref.refType(ref.types.CString)

const PGF = ffi.Library('libpgf', {
  'pgf_read_pgf': [ PgfDBPtr, [ 'string', PgfRevisionPtr, PgfExnPtr ] ],
  'pgf_write_pgf': [ 'void', [ 'string', PgfDBPtr, PgfRevisionPtr, PgfExnPtr ] ],
})

interface Pointer extends Buffer {
  deref(): any
}

interface State {
  db: Pointer,
  revision: number
}
const state: State = {
  db: ref.NULL_POINTER as Pointer,
  revision: 0
}

export default {
  readPGF: function (path: string) {
    const rev = ref.alloc(PgfRevision) as Pointer
    const err = ref.alloc(PgfExn) as Pointer
    const db = PGF.pgf_read_pgf(path, rev, err)
    if (err.deref() === 0) {
      state.db = db
      state.revision = rev.deref()
      console.log(`ok`)
    } else {
      console.error(`error ${err.deref()}`)
    }
  }
}
