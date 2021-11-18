#include "data.h"

void PgfFlag::release(ref<PgfFlag> flag)
{
    pgf_literal_free(flag->value);
}

void PgfAbsFun::release(ref<PgfAbsFun> absfun)
{
    pgf_type_free(absfun->type);

    if (absfun->bytecode != 0) {
        PgfDB::free(absfun->bytecode);
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
    namespace_release(concr->lins);
    namespace_release(concr->lincats);
    namespace_release(concr->printnames);
}

void PgfConcrLincat::release(ref<PgfConcrLincat> lincat)
{
    for (size_t i = 0; i < lincat->fields->len; i++) {
        PgfDB::free(*vector_elem(lincat->fields, i));
    }

    PgfDB::free(lincat->fields);
}

void PgfConcrLin::release(ref<PgfConcrLin> lin)
{
    for (size_t i = 0; i < lin->args->len; i++) {
        PgfDB::free(vector_elem(lin->args, i)->param);
    }
    PgfDB::free(lin->args);

    for (size_t i = 0; i < lin->res->len; i++) {
        PgfDB::free(*vector_elem(lin->res, i));
    }
    PgfDB::free(lin->res);

    for (size_t i = 0; i < lin->seqs->len; i++) {
        ref<Vector<PgfSymbol>> syms = *vector_elem(lin->seqs, i);
        for (size_t j = 0; j < syms->len; j++) {
            PgfSymbol sym = *vector_elem(syms, i);
            PgfDB::free(ref<void>::untagged(sym));
        }
        PgfDB::free(syms);
    }
    PgfDB::free(lin->seqs);
}

void PgfConcrPrintname::release(ref<PgfConcrPrintname> printname)
{
    PgfDB::free(printname->printname);
}
