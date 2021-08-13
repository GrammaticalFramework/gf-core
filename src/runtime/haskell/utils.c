#include <HsFFI.h>
#include <pgf/pgf.h>
#include <stdlib.h>

void hs_free_unmarshaller(PgfUnmarshaller *unmarshaller)
{
    hs_free_fun_ptr((HsFunPtr) unmarshaller->eabs);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->eapp);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->elit);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->emeta);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->efun);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->evar);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->etyped);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->eimplarg);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->lint);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->lflt);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->lstr);
    hs_free_fun_ptr((HsFunPtr) unmarshaller->dtyp);
    free(unmarshaller);
}
