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
    probspace_release(pgf->abstract.funs_by_cat);
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
    for (ref<PgfText> field : lincat->fields) {
        text_db_release(field);
    }
    vector<ref<PgfText>>::release(lincat->fields);

	for (size_t i = 0; i < lincat->args.size(); i++) {
        PgfLParam::release(lincat->args[i].param);
    }
    vector<PgfPArg>::release(lincat->args);

    for (ref<PgfPResult> res : lincat->res) {
        PgfPResult::release(res);
    }
    vector<ref<PgfPResult>>::release(lincat->res);

    vector<ref<PgfSequence>>::release(lincat->seqs);

    PgfDB::free(lincat, lincat->name.size+1);
}

void PgfLParam::release(ref<PgfLParam> param)
{
    PgfDB::free(param, param->n_terms*sizeof(param->terms[0]));
}

void PgfPResult::release(ref<PgfPResult> res)
{
    if (res->vars != 0)
        vector<PgfVariableRange>::release(res->vars);
    PgfDB::free(res, res->param.n_terms*sizeof(res->param.terms[0]));
}

void PgfSequence::release(ref<PgfSequence> seq)
{
	for (PgfSymbol sym : seq->syms) {
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
            for (size_t i = 0; i < sym_kp->alts.size(); i++) {
                PgfSequence::release(sym_kp->alts[i].form);
                for (size_t j = 0; j < sym_kp->alts[i].prefixes.size(); j++) {
                    text_db_release(sym_kp->alts[i].prefixes[j]);
                }
            }
            inline_vector<PgfAlternative>::release(&PgfSymbolKP::alts, sym_kp);
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
    inline_vector<PgfSymbol>::release(&PgfSequence::syms, seq);
}

void PgfConcrLin::release(ref<PgfConcrLin> lin)
{
    for (size_t i = 0; i < lin->args.size(); i++) {
        PgfLParam::release(lin->args[i].param);
    }
    vector<PgfPArg>::release(lin->args);

    for (ref<PgfPResult> res : lin->res) {
        PgfPResult::release(res);
    }
    vector<ref<PgfPResult>>::release(lin->res);

    vector<ref<PgfSequence>>::release(lin->seqs);

    PgfDB::free(lin, lin->name.size+1);
}

void PgfConcrPrintname::release(ref<PgfConcrPrintname> printname)
{
    text_db_release(printname->printname);
    PgfDB::free(printname, printname->name.size+1);
}
