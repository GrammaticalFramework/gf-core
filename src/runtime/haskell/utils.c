#include <HsFFI.h>
#include <pgf/pgf.h>

void hs_free_unmarshaller(PgfUnmarshaller *unmarshaller)
{
    hs_free_fun_ptr(unmarshaller->eabs);
    hs_free_fun_ptr(unmarshaller->eapp);
    hs_free_fun_ptr(unmarshaller->elit);
    hs_free_fun_ptr(unmarshaller->emeta);
    hs_free_fun_ptr(unmarshaller->efun);
    hs_free_fun_ptr(unmarshaller->evar);
    hs_free_fun_ptr(unmarshaller->etyped);
    hs_free_fun_ptr(unmarshaller->eimplarg);
    hs_free_fun_ptr(unmarshaller->lint);
    hs_free_fun_ptr(unmarshaller->lflt);
    hs_free_fun_ptr(unmarshaller->lstr);
    hs_free_fun_ptr(unmarshaller->dtyp);
    free(unmarshaller);
}
