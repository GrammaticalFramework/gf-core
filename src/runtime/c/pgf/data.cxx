#include "data.h"

void PgfFlag::release(ref<PgfFlag> flag)
{
    pgf_literal_release(flag->value);
    PgfDB::free(flag, flag->name.size+1);
}

void PgfAbsFun::release(ref<PgfAbsFun> absfun)
{
    pgf_type_release(absfun->type);

    if (absfun->bytecode != 0) {
        PgfDB::free(absfun->bytecode);
    }

    PgfDB::free(absfun, absfun->name.size+1);
}

void PgfAbsCat::release(ref<PgfAbsCat> abscat)
{
    pgf_context_release(abscat->context);
    PgfDB::free(abscat, abscat->name.size+1);
}

void PgfPGF::release(ref<PgfPGF> pgf)
{
    namespace_release(pgf->gflags);
    text_db_release(pgf->abstract.name);
    namespace_release(pgf->abstract.aflags);
    namespace_release(pgf->abstract.funs);
    namespace_release(pgf->abstract.cats);
    namespace_release(pgf->concretes);
    PgfDB::free(pgf);
}

void PgfConcr::release(ref<PgfConcr> concr)
{
    namespace_release(concr->cflags);
    namespace_release(concr->lins);
    namespace_release(concr->lincats);
    phrasetable_release(concr->phrasetable);
    namespace_release(concr->printnames);
    PgfDB::free(concr, concr->name.size+1);
}

void PgfConcrLincat::release(ref<PgfConcrLincat> lincat)
{
    for (size_t i = 0; i < lincat->fields->len; i++) {
        text_db_release(*vector_elem(lincat->fields, i));
    }
    Vector<ref<PgfText>>::release(lincat->fields);

	for (size_t i = 0; i < lincat->args->len; i++) {
        PgfLParam::release(vector_elem(lincat->args, i)->param);
    }
    Vector<PgfPArg>::release(lincat->args);

    for (size_t i = 0; i < lincat->res->len; i++) {
        PgfPResult::release(*vector_elem(lincat->res, i));
    }
    Vector<ref<PgfPResult>>::release(lincat->res);

    Vector<ref<PgfSequence>>::release(lincat->seqs);

    PgfDB::free(lincat, lincat->name.size+1);
}

void PgfLParam::release(ref<PgfLParam> param)
{
    PgfDB::free(param, param->n_terms*sizeof(param->terms[0]));
}

void PgfPResult::release(ref<PgfPResult> res)
{
    if (res->vars != 0)
        Vector<PgfVariableRange>::release(res->vars);
    PgfDB::free(res, res->param.n_terms*sizeof(res->param.terms[0]));
}

void PgfSequence::release(ref<PgfSequence> seq)
{
	for (size_t i = 0; i < seq->syms.len; i++) {
        PgfSymbol sym = *vector_elem(&seq->syms, i);

        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolCat::tag: {
            auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
            PgfDB::free(sym_cat, sym_cat->r.n_terms*sizeof(sym_cat->r.terms[0]));
            break;
        }
        case PgfSymbolLit::tag: {
            auto sym_lit = ref<PgfSymbolLit>::untagged(sym);
            PgfDB::free(sym_lit, sym_lit->r.n_terms*sizeof(sym_lit->r.terms[0]));
            break;
        }
        case PgfSymbolVar::tag:
            PgfDB::free(ref<PgfSymbolVar>::untagged(sym));
            break;
        case PgfSymbolKS::tag: {
            auto sym_ks = ref<PgfSymbolKS>::untagged(sym);
            PgfDB::free(sym_ks, sym_ks->token.size+1);
            break;
        }
        case PgfSymbolKP::tag: {
            auto sym_kp = ref<PgfSymbolKP>::untagged(sym);
            PgfSequence::release(sym_kp->default_form);
            for (size_t i = 0; i < sym_kp->alts.len; i++) {
                PgfSequence::release(sym_kp->alts.data[i].form);
                for (size_t j = 0; j < sym_kp->alts.data[i].prefixes->len; j++) {
                    text_db_release(*vector_elem(sym_kp->alts.data[i].prefixes, j));
                }
            }
            PgfDB::free(sym_kp, sym_kp->alts.len*sizeof(PgfAlternative));
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
            throw pgf_error("Unknown symbol tag");
        }
    }
    PgfDB::free(seq,seq->syms.len*sizeof(PgfSymbol));
}

void PgfConcrLin::release(ref<PgfConcrLin> lin)
{
    for (size_t i = 0; i < lin->args->len; i++) {
        PgfLParam::release(vector_elem(lin->args, i)->param);
    }
    Vector<PgfPArg>::release(lin->args);

    for (size_t i = 0; i < lin->res->len; i++) {
        PgfPResult::release(*vector_elem(lin->res, i));
    }
    Vector<ref<PgfPResult>>::release(lin->res);

    Vector<ref<PgfSequence>>::release(lin->seqs);

    PgfDB::free(lin, lin->name.size+1);
}

void PgfConcrPrintname::release(ref<PgfConcrPrintname> printname)
{
    text_db_release(printname->printname);
    PgfDB::free(printname, printname->name.size+1);
}
