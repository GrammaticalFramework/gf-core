#include <HsFFI.h>
#include <pgf/pgf.h>
#include <stdlib.h>
#include "PGF2/FFI_stub.h"

static
PgfMarshallerVtbl haskell_marshaller_vtbl = {
    (void*)haskell_match_lit,
    (void*)haskell_match_expr,
    (void*)haskell_match_type
};

PgfMarshaller haskell_marshaller = {
    &haskell_marshaller_vtbl
};

static
void haskell_free_ref(PgfUnmarshaller *self, uintptr_t ref)
{
    hs_free_stable_ptr((HsStablePtr) ref);
}

static
PgfUnmarshallerVtbl haskell_unmarshaller_vtbl = {
    (void*)haskell_eabs,
    (void*)haskell_eapp,
    (void*)haskell_elit,
    (void*)haskell_emeta,
    (void*)haskell_efun,
    (void*)haskell_evar,
    (void*)haskell_etyped,
    (void*)haskell_eimplarg,
    (void*)haskell_lint,
    (void*)haskell_lflt,
    (void*)haskell_lstr,
    (void*)haskell_dtyp,
    haskell_free_ref
};

PgfUnmarshaller haskell_unmarshaller = {
    &haskell_unmarshaller_vtbl
};
