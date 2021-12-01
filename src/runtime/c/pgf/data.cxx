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

PGF_INTERNAL
void pgf_symbol_free(PgfSymbol sym)
{
    switch (ref<PgfSymbol>::get_tag(sym)) {
    case PgfSymbolKP::tag: {
        auto sym_kp = ref<PgfSymbolKP>::untagged(sym);
        pgf_symbols_free(sym_kp->default_form);
        for (size_t i = 0; i < sym_kp->alts.len; i++) {
            pgf_symbols_free(sym_kp->alts.data[i].form);
            for (size_t j = 0; j < sym_kp->alts.data[i].prefixes->len; j++) {
                ref<PgfText> prefix = *vector_elem(sym_kp->alts.data[i].prefixes, j);
                PgfDB::free(prefix);
            }
        }
        PgfDB::free(sym_kp);
        break;
    }
    case PgfSymbolBIND::tag:
    case PgfSymbolSOFTBIND::tag:
    case PgfSymbolNE::tag:
    case PgfSymbolSOFTSPACE::tag:
    case PgfSymbolCAPIT::tag:
    case PgfSymbolALLCAPIT::tag:
        break;
    default:
        PgfDB::free(ref<void>::untagged(sym));
    }
}

PGF_INTERNAL
void pgf_symbols_free(ref<Vector<PgfSymbol>> syms)
{
    for (size_t i = 0; i < syms->len; i++) {
        PgfSymbol sym = *vector_elem(syms, i);
        pgf_symbol_free(sym);
    }
    PgfDB::free(syms);
}

void PgfConcrLin::release(ref<PgfConcrLin> lin)
{
    for (size_t i = 0; i < lin->args->len; i++) {
        PgfDB::free(vector_elem(lin->args, i)->param);
    }
    PgfDB::free(lin->args);

    for (size_t i = 0; i < lin->res->len; i++) {
        ref<PgfPResult> res = *vector_elem(lin->res, i);
        if (res->vars != 0)
            PgfDB::free(res->vars);
        PgfDB::free(res);
    }
    PgfDB::free(lin->res);

    for (size_t i = 0; i < lin->seqs->len; i++) {
        ref<Vector<PgfSymbol>> syms = *vector_elem(lin->seqs, i);
        for (size_t j = 0; j < syms->len; j++) {
            PgfSymbol sym = *vector_elem(syms, j);
            pgf_symbol_free(sym);
        }
        PgfDB::free(syms);
    }
    PgfDB::free(lin->seqs);
}

void PgfConcrPrintname::release(ref<PgfConcrPrintname> printname)
{
    PgfDB::free(printname->printname);
}
