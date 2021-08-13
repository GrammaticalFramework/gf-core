#include <HsFFI.h>
#include <pgf/pgf.h>
#include <stdlib.h>

void hs_free_unmarshaller(PgfUnmarshaller *unmarshaller)
{
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->eabs);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->eapp);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->elit);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->emeta);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->efun);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->evar);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->etyped);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->eimplarg);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->lint);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->lflt);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->lstr);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->vtbl->dtyp);
    free(unmarshaller->vtbl);
    free(unmarshaller);
}

void hs_free_reference(PgfUnmarshaller *unmarshaller, uintptr_t ref)
{
    hs_free_stable_ptr((HsStablePtr) ref);
}
