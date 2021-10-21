#include "data.h"

void PgfFlag::release(ref<PgfFlag> flag)
{
    pgf_literal_free(flag->value);
}

void PgfAbsFun::release(ref<PgfAbsFun> absfun)
{
    pgf_type_free(absfun->type);

    if (absfun->defns != 0) {
        for (size_t i = 0; i < absfun->defns->len; i++) {
            ref<PgfEquation> eq = *vector_elem(absfun->defns, i);
            pgf_expr_free(eq->body);

            for (size_t j = 0; j < eq->patts.len; j++) {
                PgfPatt patt = *vector_elem(ref<PgfVector<PgfPatt>>::from_ptr(&eq->patts), j);
                pgf_patt_free(patt);
            }
        }

        PgfDB::free(absfun->defns);
    }
}

void PgfAbsCat::release(ref<PgfAbsCat> abscat)
{
    pgf_context_free(abscat->context);
}

void PgfPGF::release(ref<PgfPGF> pgf)
{
    namespace_release(pgf->gflags);
    PgfDB::free(pgf->abstract.name);
    namespace_release(pgf->abstract.aflags);
    namespace_release(pgf->abstract.funs);
    namespace_release(pgf->abstract.cats);
    namespace_release(pgf->concretes);
}

void PgfConcr::release(ref<PgfConcr> concr)
{
    namespace_release(concr->cflags);
}
