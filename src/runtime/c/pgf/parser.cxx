#include "data.h"
#include "printer.h"
#include "parser.h"
#include <math.h>

//#define DEBUG_PARSER
//#define DEBUG_EXPRS

PgfAbstractParser::PgfAbstractParser(ref<PgfConcr> concr)
{
    this->concr = concr;

    this->current_state = NULL;
    this->last_fid = concr->last_fid;
}

void PgfAbstractParser::get_info(CCat *ccat, ref<PgfConcrRule> *prule, size_t **pvalues)
{
    if (ccat->epsilon == 0) {
        Production *prod = ccat->prods[0];
        *prule = prod->rule;
        *pvalues = &prod->vars[0];
    } else {
        ref<PgfItem> pitem = ccat->epsilon->items[0];
        *prule = pitem->rule;
        *pvalues = &pitem->vars[0];
    }
}

PgfAbstractParser::CCat *PgfAbstractParser::get_epsilon_ccat(PgfText *name, PgfMetaId fid)
{
    if (fid == 0)
        return NULL;

    CCat *&ccat = epsilons[fid];
    if (ccat == NULL) {
        ref<PgfCCat> arg = epsilontable_get(concr->epsilontable,
                                            name, fid);
        ccat = new CCat;
        ccat->fid = arg->fid;
        ccat->epsilon = arg;
        ccat->cont = NULL;
        ccat->state = NULL;
        ccat->lin_idx = arg->lin_idx;
        ccat->value = arg->value;
        ccat->covered = true;
        ccat->viterbi_prob = arg->viterbi_prob;
    }
    return ccat;
}

PgfAbstractParser::CCat::~CCat()
{
    for (Production *prod : prods) {
        delete prod;
    }
    for (ExprState *estate : pending) {
        delete estate;
    }
}

PgfAbstractParser::Cont::~Cont()
{
    for (auto it1 : suspended) {
        for (auto it2 : it1.second) {
            for (Item *item : it2.second) {
                delete item;
            }
        }
    }
}

PgfAbstractParser::~PgfAbstractParser()
{
    State *state = current_state;
    while (state != NULL) {
        for (auto it1 : state->completed) {
     /*       for (auto it2 : it1) {
                for (auto it3 : it2) {
                    delete it3;
                }
            }*/
        }
        for (auto it : state->conts1) {
            delete it.second;
        }
        for (auto it : state->conts2) {
            delete it.second;
        }

        State *next = state->next;
        delete state;
        state = next;
    }
}

void PgfAbstractParser::process(Item *item, State *state)
{
#ifdef DEBUG_PARSER
    print_item(item,state);
#endif

    if (item->dot < item->syms.size()) {
        symbol(item,state,item->syms[item->dot]);
    } else if (item->pre_alt > 0) {
        item->dot     = item->pre_dot+1;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = item->rule->syms.as_vector();
        process(item,state);
    } else {
        complete(item,state);
    }
}

PGF_INTERNAL_DECL
int text_symbol_cmp(PgfTextSpot *spot, const uint8_t *end,
                    ref<PgfSymbolKS> sym, bool case_sensitive);

void PgfAbstractParser::symbol(Item *item, State *state, PgfSymbol sym)
{
    switch (ref<PgfSymbol>::get_tag(sym)) {
    case PgfSymbolCat::tag: {
        auto symcat = ref<PgfSymbolCat>::untagged(sym);

        CCat *ccat = item->args[symcat->d];
        if (ccat == NULL) {
            ref<PgfConcrLincat> lincat = 0;
            switch (ref<object>::get_tag(item->rule->container)) {
            case PgfConcrLin::tag: {
                auto lin = ref<PgfConcrLin>::untagged(item->rule->container);
                lincat =
                    namespace_lookup(concr->lincats,
                                     &lin->absfun->type->hypos[symcat->d].type->name);
                break;
            }
            case PgfConcrLincat::tag: {
                lincat = ref<PgfConcrLincat>::untagged(item->rule->container);
                break;
            }
            }

            if (lincat != 0) {
                size_t n_suspended1 = state->conts1.size();
                Cont *&cont = state->conts1[lincat];
                if (cont == NULL) {
                    cont = new Cont;
                    cont->ccat = NULL;
                    cont->lincat = lincat;
                    cont->state = state;
                }

                interval_t value_i   = interval(item->rule, &item->vars[0], item->rule->args[symcat->d]);
                interval_t lin_idx_i = interval(item->rule, &item->vars[0], ref<PgfLParam>::from_ptr(&symcat->r));

                suspend(cont,item,n_suspended1 == 0,symcat,value_i,lin_idx_i);
            }
        } else {
            interval_t value_i   = interval(item->rule, &item->vars[0], item->rule->args[symcat->d]);
            interval_t lin_idx_i = interval(item->rule, &item->vars[0], ref<PgfLParam>::from_ptr(&symcat->r));

            // the following prevents infinite loops with epsilons
            bool found = false;
            CCat *prev_ccat = ccat;
            while (prev_ccat != NULL && prev_ccat->cont != NULL && prev_ccat->cont->state == state) {
                if (prev_ccat->value == value_i && prev_ccat->lin_idx == lin_idx_i) {
                    found = true;
                    break;
                }
                prev_ccat = prev_ccat->cont->ccat;
            }
            if (found) {
                item->dot++;
                state->push_item(item);
                break;
            }

            Cont *&cont = state->conts2[ccat];
            if (cont == NULL) {
                cont = new Cont;
                cont->ccat = ccat;
                if (ccat->epsilon != 0)
                    cont->lincat = ccat->epsilon->lincat;
                else
                    cont->lincat = ccat->cont->lincat;
                cont->state = state;
            }

            bool subsumed = false;
            for (auto it1 : cont->suspended.overlaps(value_i)) {
                if (it1.first.first <= value_i.first && it1.first.second >= value_i.second) {
                    for (auto it2 : it1.second.overlaps(lin_idx_i)) {
                        if (it2.first.first <= lin_idx_i.first && it2.first.second >= lin_idx_i.second) {
                            subsumed = true;
                            goto found;
                        }
                    }
                }
            }
found:
            suspend(cont,item,!subsumed,symcat,value_i,lin_idx_i);
        }
        break;
    }
    case PgfSymbolKS::tag: {
        auto symks = ref<PgfSymbolKS>::untagged(sym);
        symbol_token(item, state, symks);
        break;
    }
    case PgfSymbolKP::tag: {
        auto symkp = ref<PgfSymbolKP>::untagged(sym);

        Item *new_item = new(item) Item;
        new_item->pre_alt = 1;
        new_item->pre_dot = item->dot;
        new_item->dot     = 0;
        new_item->syms    = symkp->default_form;
        new_item->rule    = item->rule;
        new_item->inside_prob  = item->inside_prob;
        new_item->outside_prob = item->outside_prob;
        process(new_item, state);

        for (size_t i = 0; i < symkp->alts.size(); i++) {
            Item *new_item = new(item) Item;
            new_item->pre_alt = i+2;
            new_item->pre_dot = item->dot;
            new_item->dot     = 0;
            new_item->syms    = symkp->alts[i].form;
            new_item->rule    = item->rule;
            new_item->inside_prob  = item->inside_prob;
            new_item->outside_prob = item->outside_prob;
            process(new_item, state);
        }

        delete item;
        break;
    }
    case PgfSymbolBIND::tag:
    case PgfSymbolSOFTBIND::tag:
    case PgfSymbolSOFTSPACE::tag: {
        symbol_bind(item, state, sym);
        break;
    }
    case PgfSymbolNE::tag:
        delete item;
        break;
    case PgfSymbolCAPIT::tag:
    case PgfSymbolALLCAPIT::tag:
        item->dot++;
        process(item, state);
        break;
    }
}

void PgfAbstractParser::complete(Item *item, State *state)
{
    switch (ref<object>::get_tag(item->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(item->rule->container);

        interval_t res = interval(item->rule, &item->vars[0], item->rule->res);
        interval_t lin_idx = interval(item->rule, &item->vars[0], item->rule->lin_idx);
        CCat *&ccat = state->completed[item->cont][res][lin_idx];
        if (ccat == NULL) {
            ccat = new CCat;
            ccat->fid = (++last_fid);
            ccat->epsilon = 0;
            ccat->cont  = item->cont;
            ccat->state = state;
            ccat->lin_idx = lin_idx;
            ccat->value = res;
            ccat->covered = false;
            ccat->viterbi_prob = item->inside_prob;

#ifdef DEBUG_PARSER
            {
                PgfPrinter printer(NULL,0,NULL);
                if (item->rule->ranges.size() > 0) {
                    printer.lvar_ranges(item->rule->ranges, &item->vars[0]);
                    printer.puts(" ");
                }
                printer.nprintf(64,"complete [%zd-%zd; ",item->cont->state->end.pos,state->start.pos);
                if (ccat->cont->ccat == NULL) {
                    printer.efun(&ccat->cont->lincat->name);
                    printer.puts("(");
                    printer.lparam(item->rule->res);
                    printer.puts(")");
                } else {
                     printer.emeta(ccat->cont->ccat->fid);
                }
                printer.puts("; ");
                printer.lparam(item->rule->lin_idx);
                printer.puts("; ");
                printer.emeta(ccat->fid);
                printer.puts("]");
                PgfText *text = printer.get_text();
                fprintf(stderr, "%s\n", text->text);
                free(text);
            }
#endif
        }

        auto prod = new(item) Production;
        prod->rule = item->rule;
        for (size_t i = 0; i < prod->args.size(); i++) {
            if (prod->args[i] != NULL && prod->args[i] != ccat)
                prod->args[i]->covered = true;
        }
        ccat->prods.push_back(prod);

#ifdef DEBUG_PARSER
        print_prod(ccat, prod);
#endif
        final_item(state, ccat, item, res, lin_idx);

        if (ccat->prods.size() == 1) {
            bu_predict(state, ccat);

            for (auto it1 : ccat->cont->suspended.overlaps(ccat->value)) {
                for (auto it2 : it1.second.overlaps(ccat->lin_idx)) {
                    size_t n_items = it2.second.size();
                    for (size_t i = 0; i < n_items; i++) {
                        Item *new_item = new (it2.second[i]) Item;
                        combine(state,new_item,ccat);
                    };
                }
            }
        } else {
            State *next = state;
            while (next != NULL) {
                Cont *cont = next->conts2[ccat];
                if (cont != NULL) {
                    for (auto it1 : cont->suspended) {
                        for (auto it2 : it1.second) {
                            Item *item = it2.second[0];
                            auto symcat = ref<PgfSymbolCat>::untagged(item->syms[item->dot]);
                            td_predict(next,cont,prod,item,symcat);
                        }
                    }
                }
                next = next->next;
            }
        }
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat = ref<PgfConcrLincat>::untagged(item->rule->container);
        interval_t zero = {0,0};
        final_item(state, NULL, item, zero, zero);
        break;
    }
    }

    delete item;
}

#define ZERO_VALUES(rule) \
    ((size_t*) memset(alloca(rule->ranges.size()*sizeof(size_t)), 0, rule->ranges.size()*sizeof(size_t)))
#define CLONE_VALUES(rule,values) \
    ((size_t*) memcpy(alloca(rule->ranges.size()*sizeof(size_t)), values, rule->ranges.size()*sizeof(size_t)))

bool PgfAbstractParser::instantiate(ref<PgfConcrRule> rule1, size_t *values1, ref<PgfLParam> lparam1,
                                    ref<PgfConcrRule> rule2, size_t *values2, ref<PgfLParam> lparam2)
{
    size_t i01 = lparam1->i0;
    for (size_t i = 0; i < lparam1->n_terms; i++) {
        if (values1[lparam1->terms[i].var] > 0) {
            i01 += lparam1->terms[i].factor * (values1[lparam1->terms[i].var]-1);
        }
    }

    size_t i02 = lparam2->i0;
    for (size_t i = 0; i < lparam2->n_terms; i++) {
        if (values2[lparam2->terms[i].var] > 0) {
            i02 += lparam2->terms[i].factor * (values2[lparam2->terms[i].var]-1);
        }
    }

    if (i01 > i02) {
        i01 -= i02;
        i02  = 0;
    } else {
        i02 -= i01;
        i01  = 0;
    }

    size_t i1 = 0, i2 = 0;
    while (i1 < lparam1->n_terms || i2 < lparam2->n_terms) {
        size_t scale1  = 0;
        term t1 = {0,0};
        if (i1 < lparam1->n_terms) {
            t1 = lparam1->terms[i1];
            if (values1[t1.var] > 0) {
                i1++;
                continue;
            }
            scale1 = t1.factor * rule1->ranges[t1.var];
        }

        size_t scale2  = 0;
        term t2 = {0,0};
        if (i2 < lparam2->n_terms) {
            t2 = lparam2->terms[i2];
            if (values2[t2.var] > 0) {
                i2++;
                continue;
            }
            scale2 = t2.factor * rule2->ranges[t2.var];
        }

        if (scale1 > scale2) {
            size_t min = (i02 / t1.factor);
            size_t max = min;
            while (i2 < lparam2->n_terms) {
                t2 = lparam2->terms[i2];
                size_t f = t2.factor / t1.factor;
                if (f == 0)
                    break;

                if (values2[t2.var] == 0) {
                    max += f * (rule2->ranges[t2.var]-1);
                }
                i2++;
            }
            i02 %= t1.factor;

            if (min >= rule1->ranges[t1.var])
                return false;

            if (min == max) {
                if (values1[t1.var] == 0)
                    values1[t1.var] = min+1;
                else if (values1[t1.var] != min+1)
                    return false;
            }

            i1++;
        } else {
            size_t min = (i01 / t2.factor);
            size_t max = min;
            while (i1 < lparam1->n_terms) {
                t1 = lparam1->terms[i1];
                size_t f = t1.factor / t2.factor;
                if (f == 0)
                    break;

                if (values1[t1.var] == 0) {
                    max += f * (rule1->ranges[t1.var]-1);
                }
                i1++;
            }
            i01 %= t2.factor;

            if (min >= rule2->ranges[t2.var])
                return false;

            if (min == max) {
                if (values2[t2.var] == 0) {
                    values2[t2.var] = min+1;
                } else if (values2[t2.var] != min+1)
                    return false;
            }

            i2++;
        }
    }

    return (i01 == i02);
}

interval_t PgfAbstractParser::interval(ref<PgfConcrRule> rule, size_t *values, ref<PgfLParam> lparam)
{
    interval_t interval;
    interval.first  = lparam->i0;
    interval.second = interval.first;
    for (size_t i = 0; i < lparam->n_terms; i++) {
        size_t var = lparam->terms[i].var;
        if (values[var] == 0) {
            interval.second += lparam->terms[i].factor * (rule->ranges[var]-1);
        } else {
            size_t value = lparam->terms[i].factor * (values[var]-1);
            interval.first  += value;
            interval.second += value;
        }
    }
    return interval;
}

void PgfAbstractParser::combine(State *state, Item *item, CCat *ccat)
{
    PgfSymbol sym = item->rule->syms[item->dot];
    auto sym_cat = ref<PgfSymbolCat>::untagged(sym);

    ref<PgfConcrRule> rule;
    size_t *values;
    get_info(ccat, &rule,&values);
    values = CLONE_VALUES(rule, values);

    if (!instantiate(item->rule, &item->vars[0], item->rule->args[sym_cat->d],
                     rule,       values,         rule->res)) {
        delete item;
        return;
    }
    if (!instantiate(item->rule, &item->vars[0], ref<PgfLParam>::from_ptr(&sym_cat->r),
                     rule,       values,         rule->lin_idx)) {
        delete item;
        return;
    }

    item->dot++;
    if (item->args[sym_cat->d] != NULL) {
        item->inside_prob -= item->args[sym_cat->d]->viterbi_prob;
    }
    item->args[sym_cat->d] = ccat;
    item->inside_prob += ccat->viterbi_prob;
    state->push_item(item);
}

void PgfAbstractParser::td_epsilon(State *state, Cont *cont, ref<PgfItem> pitem, Item *xitem, ref<PgfSymbolCat> symcat)
{
    switch (ref<object>::get_tag(pitem->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(pitem->rule->container);

        for (ref<PgfConcrRule> rule : lin->rules) {
            size_t *values1 = ZERO_VALUES(rule);
            size_t *values2 = CLONE_VALUES(xitem->rule, &xitem->vars[0]);
            if (!instantiate(rule,        values1, rule->res,
                             xitem->rule, values2, xitem->rule->args[symcat->d])) {
                continue;
            }
            if (!instantiate(rule,        values1, rule->lin_idx,
                             xitem->rule, values2, ref<PgfLParam>::from_ptr(&symcat->r))) {
                continue;
            }

            size_t *values3 = CLONE_VALUES(pitem->rule, &pitem->vars[0]);
            for (size_t i = 0; i < pitem->args.size(); i++) {
                if (!instantiate(rule,        values1, rule->args[i],
                                 pitem->rule, values3, pitem->rule->args[i])) {
                    goto next;
                }
            }

            {
                interval_t value_i   = interval(rule, values1, rule->res);
                interval_t lin_idx_i = interval(rule, values1, rule->lin_idx);
                Item *&pred = cont->predicted[value_i][lin_idx_i];
                if (pred != NULL && pred != xitem)
                    return;
                pred = xitem;

                Item *item = new (rule) Item;
                item->cont    = cont;
                item->dot     = 0;
                item->pre_alt = 0;
                item->pre_dot = 0;
                item->syms    = rule->syms.as_vector();
                item->rule    = rule;
                item->inside_prob = lin->absfun->prob;
                item->outside_prob = xitem->outside_prob+xitem->inside_prob-xitem->args[symcat->d]->viterbi_prob;

                for (size_t i = 0; i < pitem->args.size(); i++) {
                    if (pitem->args[i] != 0) {
                        item->args[i] = get_epsilon_ccat(&lin->absfun->type->hypos[i].type->name,pitem->args[i]);
                        item->inside_prob += item->args[i]->viterbi_prob;
                    }
                }

                state->push_item(item);
            }
next:;
        }
    }
    default:;
        // should not happend
    }
}

void PgfAbstractParser::td_predict(State *state, Cont *cont, Production *prod, Item *xitem, ref<PgfSymbolCat> symcat)
{
    switch (ref<object>::get_tag(prod->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(prod->rule->container);

        for (ref<PgfConcrRule> rule : lin->rules) {
            size_t *values1 = ZERO_VALUES(rule);
            size_t *values2 = CLONE_VALUES(xitem->rule, &xitem->vars[0]);
            if (!instantiate(rule,        values1, rule->res,
                             xitem->rule, values2, xitem->rule->args[symcat->d])) {
                continue;
            }

            if (!instantiate(rule,        values1, rule->lin_idx,
                             xitem->rule, values2, ref<PgfLParam>::from_ptr(&symcat->r))) {
                continue;
            }

            size_t *values3 = CLONE_VALUES(prod->rule, &prod->vars[0]);
            for (size_t i = 0; i < rule->args.size(); i++) {
                if (!instantiate(rule,       values1, rule->args[i],
                                 prod->rule, values3, prod->rule->args[i])) {
                    goto next;
                }
            }

            {
                interval_t value_i   = interval(rule, values1, rule->res);
                interval_t lin_idx_i = interval(rule, values1, rule->lin_idx);
                Item *&pred = cont->predicted[value_i][lin_idx_i];
                if (pred != NULL && pred != xitem)
                    return;
                pred = xitem;

                Item *item = new (rule) Item;
                item->cont    = cont;
                item->dot     = 0;
                item->pre_alt = 0;
                item->pre_dot = 0;
                item->syms    = rule->syms.as_vector();
                item->rule    = rule;
                item->inside_prob = lin->absfun->prob;
                item->outside_prob = xitem->outside_prob+xitem->inside_prob-xitem->args[symcat->d]->viterbi_prob;

                for (size_t i = 0; i < rule->args.size(); i++) {
                    item->args[i] = prod->args[i];
                    if (item->args[i] != NULL) {
                        item->inside_prob += item->args[i]->viterbi_prob;
                    }
                }

                state->push_item(item);
            }

next:;
        }
    }
    default:;
        // should not happend
    }
}

#ifdef DEBUG_PARSER
static
void print_symbols(PgfPrinter &printer, PgfConcrRule *rule, vector<PgfSymbol> syms, size_t pre_alt, size_t pre_dot, size_t dot)
{
    for (size_t i = 0; i < syms.size(); i++) {
        if (pre_alt == 0 && dot == i) {
            printer.puts(" . ");
            printer.symbol(syms[i]);
        } else if (pre_alt > 0 && pre_dot == i) {
            auto sym_kp = ref<PgfSymbolKP>::untagged(rule->syms[pre_dot]);

            printer.puts("pre {");

            if (pre_alt == 1)
                print_symbols(printer, rule, syms, 0, 0, dot);
            else
                printer.symbols(sym_kp->default_form);

            for (size_t i = 0; i < sym_kp->alts.size(); i++) {
                printer.puts("; ");
                if (pre_alt-2 == i)
                    print_symbols(printer, rule, syms, 0, 0, dot);
                else
                    printer.symbols(sym_kp->alts[i].form);
                printer.puts(" /");
                for (size_t j = 0; j < sym_kp->alts[i].prefixes.size(); j++) {
                    printer.puts(" ");
                    printer.lstr(sym_kp->alts[i].prefixes[j]);
                }
            }

            printer.puts("}");
        } else {
            printer.symbol(syms[i]);
        }
    }
    if (pre_alt == 0 && dot >= syms.size())
        printer.puts(" . ");
}

void PgfAbstractParser::print_item(Item *item, State *state)
{
    PgfPrinter printer(NULL,0,NULL);

    printer.nprintf(32, "[%zd-%zd; ", (item->cont && item->cont->state) ? item->cont->state->end.pos : 0, state->start.pos);

    if (item->vars.size() > 0) {
        printer.lvar_ranges(item->rule->ranges, &item->vars[0]);
        printer.puts(" ");
    }

    if (item->cont) {
        if (item->cont->ccat == NULL) {
            printer.efun(&item->cont->lincat->name);
        } else {
            printer.emeta(item->cont->ccat->fid);
        }
        printer.puts("(");
        printer.lparam(item->rule->res);
        printer.puts(")");
    }
    printer.puts(" -> ");

    switch (ref<object>::get_tag(item->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(item->rule->container);
        printer.efun(&lin->name);

        printer.puts("[");
        for (size_t i = 0; i < item->args.size(); i++) {
            if (i > 0)
                printer.puts(",");

            CCat *ccat = item->args[i];
            if (ccat == NULL) {
                printer.efun(&lin->absfun->type->hypos[i].type->name);
            } else {
                printer.emeta(ccat->fid);
            }
            printer.puts("(");
            printer.lparam(item->rule->args[i]);
            printer.puts(")");
        }
        printer.puts("]; ");
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat = ref<PgfConcrLincat>::untagged(item->rule->container);
        printer.puts("linref ");
        printer.efun(&lincat->name);

        printer.puts("[");
        CCat *ccat = item->args[0];
        if (ccat == NULL) {
            printer.efun(&lincat->name);
            printer.puts("(");
            printer.lparam(item->rule->args[0]);
            printer.puts(")");
        } else {
            printer.emeta(ccat->fid);
        }
        printer.puts("]; ");
        break;
    }
    }

    printer.lparam(item->rule->lin_idx);
    printer.puts(" : ");
    print_symbols(printer, item->rule, item->syms, item->pre_alt, item->pre_dot, item->dot);
    printer.nprintf(40,"; %f+%f=%f]", item->inside_prob, item->outside_prob, item->inside_prob+item->outside_prob);

    PgfText *text = printer.get_text();
    fprintf(stderr, "%s\n", text->text);
    free(text);
}

void PgfAbstractParser::print_prod(CCat *ccat, Production *prod)
{
    PgfPrinter printer(NULL,0,NULL);

    if (prod->vars.size() > 0) {
        printer.lvar_ranges(prod->rule->ranges, &prod->vars[0]);
        printer.puts(" ");
    }

    printer.emeta(ccat->fid);
    printer.puts("(");
    printer.lparam(prod->rule->res);
    printer.puts(")");

    printer.puts(" -> ");

    switch (ref<object>::get_tag(prod->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(prod->rule->container);
        printer.efun(&lin->name);

        printer.puts("[");
        for (size_t i = 0; i < prod->args.size(); i++) {
            if (i > 0)
                printer.puts(",");

            CCat *ccat = prod->args[i];
            if (ccat == NULL) {
                printer.efun(&lin->absfun->type->hypos[i].type->name);
            } else {
                printer.emeta(ccat->fid);
            }
            printer.puts("(");
            printer.lparam(prod->rule->args[i]);
            printer.puts(")");
        }
        printer.puts("]");
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat = ref<PgfConcrLincat>::untagged(prod->rule->container);
        printer.puts("linref ");
        printer.efun(&lincat->name);

        printer.puts("[");
        CCat *ccat = prod->args[0];
        if (ccat == NULL) {
            printer.efun(&lincat->name);
            printer.puts("(");
            printer.lparam(prod->rule->args[0]);
            printer.puts(")");
        } else {
            printer.emeta(ccat->fid);
        }
        printer.puts("]");
        break;
    }
    }

    PgfText *text = printer.get_text();
    fprintf(stderr, "%s\n", text->text);
    free(text);
}
#endif

PgfParser::PgfParser(ref<PgfConcr> concr, PgfText *sentence, bool case_sensitive, PgfMarshaller *m, PgfUnmarshaller *u)
   : PgfAbstractParser(concr)
{
    this->m = m;
    this->u = u;
    this->sentence = textdup(sentence);
    this->end = (uint8_t *) (this->sentence->text+this->sentence->size);
    this->case_sensitive = case_sensitive;
}

PgfParser::~PgfParser()
{
    free(sentence);

    State *state = current_state;
    while (state != NULL) {
        for (auto it1 : state->completed) {
            for (auto it2 : it1.second) {
                for (auto it3 : it2.second) {
                    if (it3.second->epsilon != 0)
                        continue;

                    for (ExprState *estate : it3.second->pending) {
                        if (estate->expr != 0)
                            u->free_ref(estate->expr);
                    }
                    for (ExprProb &ep : it3.second->exprs) {
                        u->free_ref(ep.expr);
                    }
                }
            }
        }

        state = state->next;
    }

    for (auto it1 : epsilons) {
        for (ExprState *estate : it1.second->pending) {
            if (estate->expr != 0)
                u->free_ref(estate->expr);
        }
        for (ExprProb &ep : it1.second->exprs) {
            u->free_ref(ep.expr);
        }
    }
}

void PgfParser::bu_predict(PgfPhrasetable<PgfSymbolKS> phrasetable,
                           State *state,
                           ptrdiff_t min, ptrdiff_t max)
{
    if (phrasetable == 0)
        return;

    PgfTextSpot current = state->end;
    int cmp = text_symbol_cmp(&current,end,phrasetable->value.key,case_sensitive);
    if (cmp < 0) {
        bu_predict(phrasetable->left,state,min,max);
    } else if (cmp > 0) {
        ptrdiff_t len = current.ptr - state->end.ptr;

        if (min <= len-1)
            bu_predict(phrasetable->left,state,min,len-1);

        if (len <= max)
            bu_predict(phrasetable->right,state,len,max);
    } else {
        ptrdiff_t len = current.ptr - state->end.ptr;

        if (min <= len)
            bu_predict(phrasetable->left,state,min,len);

        if (len > 0) {
            State *next_state = new_state(current);
            for (size_t i = 0; i < phrasetable->value.n_items; i++) {
                std::map<ref<PgfConcrLincat>, bool> visited;
                //if (!td_reachable(state, phrasetable->items[i], visited))
                //    continue;
                Item *item = bu_item(state, phrasetable->value.items[i]);
                item->dot++;
                next_state->push_item(item);
            }
        }

        if (len <= max)
            bu_predict(phrasetable->right,state,len,max);
     }
}

void PgfParser::bu_predict(PgfPhrasetable<PgfSymbolBIND> phrasetable,
                           State *state)
{
    size_t n_items = 0;
    vector<ref<PgfItem>> items =
        phrasetable_lookup(concr->phrasetable4,
                           ref<PgfSymbolBIND>(0),
                           &n_items);

    State *next_state = state->next;
    if (next_state == NULL || state->end.pos != next_state->start.pos) {
        next_state = new State;
        next_state->start = state->end;
        next_state->end   = state->end;
        next_state->next  = state->next;
        next_state->needs_bind = false;
        state->next = next_state;
    }

    for (size_t i = 0; i < n_items; i++) {
        //std::map<ref<PgfConcrLincat>, bool> visited;
        //if (!td_reachable(state, phrasetable->items[i], visited))
        //    continue;
        Item *item = bu_item(state, items[i]);
        item->dot++;
        next_state->push_item(item);
    }
}

void PgfParser::bu_predict(State *state, CCat *ccat)
{
    size_t n_items = 0;
    vector<ref<PgfItem>> items = 0;
    if (ccat->cont->ccat == NULL) {
        items = phrasetable_lookup(concr->phrasetable2,
                                   ccat->cont->lincat,
                                   &n_items);
    } else if (ccat->cont->ccat->epsilon != 0) {
        items = phrasetable_lookup(concr->phrasetable3,
                                   ccat->cont->ccat->epsilon,
                                   &n_items);
    }

    for (size_t i = 0; i < n_items; i++) {
        //std::map<ref<PgfConcrLincat>, bool> visited;
        //if (!td_reachable(ccat->cont->state, items[i], visited))
        //    continue;
        auto new_item = bu_item(ccat->cont->state, items[i]);
        combine(state,new_item,ccat);
    }
}

bool PgfParser::td_reachable(State *state, ref<PgfItem> pitem,
                             std::map<ref<PgfConcrLincat>, bool> &visited)
{
    switch (ref<object>::get_tag(pitem->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(pitem->rule->container);

        if (visited[lin->lincat])
            return false;
        visited[lin->lincat] = true;

        auto it = state->conts1.find(lin->lincat);
        if (it != state->conts1.end()) {
            return true;
        }

        size_t n_items = 0;
        vector<ref<PgfItem>> items =
            phrasetable_lookup(concr->phrasetable2,
                               lin->lincat,
                               &n_items);
        for (size_t i = 0; i < n_items; i++) {
            if (td_reachable(state, items[i], visited))
                return true;
        }
        break;
    }
    }
    return false;
}

PgfAbstractParser::Item *PgfParser::bu_item(State *state, ref<PgfItem> pitem)
{
    Item *item = NULL;

    switch (ref<object>::get_tag(pitem->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(pitem->rule->container);

        Cont *cont;
        if (pitem->res == 0) {
            Cont *&tmp = state->conts1[lin->lincat];
            if (tmp == NULL) {
                tmp = new Cont;
                tmp->ccat = NULL;
                tmp->lincat = lin->lincat;
                tmp->state = state;
            }
            cont = tmp;
        } else {
            CCat *ccat = get_epsilon_ccat(&lin->lincat->name,pitem->res);
            Cont *&tmp = state->conts2[ccat];
            if (tmp == NULL) {
                tmp = new Cont;
                tmp->ccat = ccat;
                tmp->lincat = lin->lincat;
                tmp->state = state;
            }
            cont = tmp;
        }

        item = new(pitem->rule) Item;
        item->cont    = cont;
        item->pre_alt = pitem->pre_alt;
        item->pre_dot = pitem->pre_dot;
        item->dot     = pitem->dot;
        item->syms    = pitem->rule->syms.as_vector();
        item->rule    = pitem->rule;
        item->inside_prob = lin->absfun->prob;
        item->outside_prob = 0;

        for (size_t i = 0; i < pitem->args.size(); i++) {
            item->args[i] = 0;
            if (pitem->args[i] != 0) {
                item->args[i] = get_epsilon_ccat(&lin->absfun->type->hypos[i].type->name,pitem->args[i]);
                item->inside_prob += item->args[i]->viterbi_prob;
            }
        }
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat = ref<PgfConcrLincat>::untagged(pitem->rule->container);

        Cont *&cont = state->conts1[0];
        if (cont == NULL) {
            cont = new Cont;
            cont->ccat = NULL;
            cont->lincat = 0;
            cont->state = state;
        }

        item = new(pitem->rule) Item;
        item->cont    = cont;
        item->pre_alt = pitem->pre_alt;
        item->pre_dot = pitem->pre_dot;
        item->dot     = pitem->dot;
        item->syms    = pitem->rule->syms.as_vector();
        item->rule    = pitem->rule;
        item->inside_prob = 0;
        item->outside_prob = 0;
        item->args[0] = 0;
        break;
    }
    }

    if (item->pre_alt > 0) {
        auto symkp = ref<PgfSymbolKP>::untagged(item->syms[item->pre_dot]);

        if (item->pre_alt == 1)
            item->syms = symkp->default_form;
        else
            item->syms = symkp->alts[item->pre_alt-2].form;
    }

    memcpy(&item->vars[0], &pitem->vars[0], sizeof(size_t) * item->vars.size());
    return item;
}

void PgfParser::make_chunks(State *state, std::vector<CCat*> &chunks, prob_t prob)
{
    if (state->completed.size() == 0) {
        ExprState *estate = new(chunks.size()) ExprState;
        estate->expr   = u->emeta(0);
        estate->prob   = prob;
        estate->hash   = '?';
        estate->res    = NULL;
        estate->index  = 0;
        estate->n_args = chunks.size();
        for (size_t i = 0; i < estate->n_args; i++) {
            estate->args[i] = chunks[estate->n_args-i-1];
            estate->prob += estate->args[i]->viterbi_prob;
        }
        queue.push_back(estate);
        std::push_heap(queue.begin(), queue.end(), estate_comp);
    }

    for (auto it1 : state->completed) {
        for (auto it2 : it1.second) {
            for (auto it3 : it2.second) {
                CCat *ccat = it3.second;
                if (!ccat->covered && ccat->cont->state != state) {
                    chunks.push_back(ccat);
                    make_chunks(ccat->cont->state, chunks, prob+ccat->cont->lincat->abscat->prob);
                    chunks.pop_back();
                }
            }
        }
    }
}

void PgfParser::prepare(ref<PgfConcrLincat> start)
{
#ifdef DEBUG_PARSER
    fprintf(stderr, "------------------------------------------\n");
#endif

    PgfTextSpot start_spot = {0, (uint8_t *) sentence->text};
    State *state = new_state(start_spot);

    for (size_t i = start->n_lindefs; i < start->rules.size(); i++) {
        ref<PgfConcrRule> rule = start->rules[i];
        Item *item = new(rule) Item;
        item->cont    = NULL;
        item->dot     = 0;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = rule->syms.as_vector();
        item->rule    = rule;
        item->inside_prob  = 0;
        item->outside_prob = 0;
        state->push_item(item);
    }
}

PgfExpr PgfParser::fetch(PgfDB *db, prob_t *prob)
{
    DB_scope scope(db, READER_SCOPE);

    bool first_fetch = (concr->last_fid == last_fid);

    for (;;) {
        State *state = current_state;
        prob_t min_prob  = INFINITY;
        State *min_state = NULL;
        if (queue.size() > 0) {
            min_prob = queue.front()->prob;
        }

        while (state != NULL) {
            if (state->queue.size() > 0) {
                Item *item = state->queue.front();
                prob_t prob = item->outside_prob + item->inside_prob;
                if (min_prob > prob) {
                    min_prob  = prob;
                    min_state = state;
                }
            }
            state = state->next;
        }

        if (min_state == NULL)
            break;

        State *prev = current_state;
        current_state = NULL;
        while (current_state != min_state) {
            State *next = prev->next;
            prev->next = current_state;
            current_state = prev;
            prev = next;
        }

        Item *item = current_state->pop_item();
        process(item,current_state);

        while (current_state != NULL) {
            State *next = current_state->next;
            current_state->next = prev;
            prev = current_state;
            current_state = next;
        }
        current_state = prev;
    }

    if (first_fetch && queue.size() == 0) {
        std::vector<CCat*> chunks;
        make_chunks(current_state, chunks, 0);
    }

    while (queue.size() > 0) {
        ExprState *estate = queue.front();
        std::pop_heap(queue.begin(), queue.end(), estate_comp);
        queue.pop_back();

#ifdef DEBUG_EXPRS
        print_expr_state(m, estate);
#endif

        PgfExpr expr = process_expr(estate, prob);
        if (expr != 0)
            return expr;
    }
    return 0;
}

PgfExpr PgfParser::process_expr(ExprState *estate, prob_t *prob)
{
    if (estate->index < estate->n_args) {
        CCat *ccat = estate->args[estate->index];

        if (ccat == NULL) {
            ExprState *app_state = new(estate->n_args) ExprState;
            app_state->expr   = estate->expr ? u->eapp(estate->expr, u->emeta(0)) : u->emeta(0);
            app_state->prob   = estate->prob;
            app_state->hash   = estate->hash * 101 + '?';
            app_state->res    = estate->res;
            app_state->index  = estate->index+1;
            app_state->n_args = estate->n_args;
            for (size_t i = 0; i < app_state->n_args; i++) {
                app_state->args[i] = estate->args[i];
            }
            queue.push_back(app_state);
            std::push_heap(queue.begin(), queue.end(), estate_comp);
        } else {
            ccat->pending.push_back(estate);

            if (ccat->pending.size() == 1) {
                if (ccat->epsilon != 0) {
                    for (size_t i = 0; i < ccat->epsilon->n_items; i++) {
                        ref<PgfItem> pitem = ccat->epsilon->items[i];

                        auto lin = ref<PgfConcrLin>::untagged(pitem->rule->container);

                        ExprState *new_estate = new(pitem->args.size()) ExprState;
                        new_estate->expr   = u->efun(&lin->name);
                        new_estate->prob   = estate->prob-ccat->viterbi_prob+lin->absfun->prob;
                        new_estate->hash   = 0;
                        new_estate->res    = ccat;
                        new_estate->index  = 0;
                        new_estate->n_args = pitem->args.size();
                        for (size_t i = 0; i < lin->name.size; i++) {
                            new_estate->hash = new_estate->hash * 101 + lin->name.text[i];
                        }
                        for (size_t i = 0; i < new_estate->n_args; i++) {
                            new_estate->args[i] = NULL;
                            if (pitem->args[i] != 0) {
                                new_estate->args[i] = get_epsilon_ccat(&lin->absfun->type->hypos[i].type->name,pitem->args[i]);
                                new_estate->prob += new_estate->args[i]->viterbi_prob;
                            }
                        }
                        queue.push_back(new_estate);
                        std::push_heap(queue.begin(), queue.end(), estate_comp);
                    }
                } else {
                    for (Production *prod : ccat->prods) {
                        auto lin = ref<PgfConcrLin>::untagged(prod->rule->container);

                        ExprState *new_estate = new(prod->args.size()) ExprState;
                        new_estate->expr   = u->efun(&lin->name);
                        new_estate->prob   = estate->prob-ccat->viterbi_prob+lin->absfun->prob;
                        new_estate->hash   = 0;
                        new_estate->res    = ccat;
                        new_estate->index  = 0;
                        new_estate->n_args = prod->args.size();
                        for (size_t i = 0; i < lin->name.size; i++) {
                            new_estate->hash = new_estate->hash * 101 + lin->name.text[i];
                        }
                        for (size_t i = 0; i < new_estate->n_args; i++) {
                            new_estate->args[i] = prod->args[i];
                            if (prod->args[i] != NULL) {
                                new_estate->prob += prod->args[i]->viterbi_prob;
                            }
                        }
                        queue.push_back(new_estate);
                        std::push_heap(queue.begin(), queue.end(), estate_comp);
                    }
                }
            } else {
                for (ExprProb ep : ccat->exprs) {
                    ExprState *app_state = new(estate->n_args) ExprState;
                    app_state->expr  = estate->expr ? u->eapp(estate->expr, ep.expr) : ep.expr;
                    app_state->prob  = estate->prob-ccat->viterbi_prob+ep.prob;
                    app_state->hash  = estate->hash * 31 + ep.hash;
                    app_state->res   = estate->res;
                    app_state->index = estate->index+1;
                    app_state->n_args= estate->n_args;
                    for (size_t i = 0; i < app_state->n_args; i++) {
                        app_state->args[i] = estate->args[i];
                    }
                    queue.push_back(app_state);
                    std::push_heap(queue.begin(), queue.end(), estate_comp);
                }
            }
        }
    } else {
        if (estate->res == NULL) {
            *prob = estate->prob;
            return estate->expr;
        }

        prob_t prob = estate->prob - (estate->res->pending[0]->prob-estate->res->viterbi_prob);
        for (size_t i = estate->res->exprs.size(); i > 0; i--) {
            ExprProb &ep = estate->res->exprs[i-1];
            if (ep.prob != prob)
                break;
            if (ep.hash == estate->hash)
                return 0;
        }

        estate->res->exprs.emplace_back(estate->expr, prob, estate->hash);
        for (ExprState *parent : estate->res->pending) {
            ExprState *app_state = new(parent->n_args) ExprState;
            app_state->expr  = parent->expr ? u->eapp(parent->expr, estate->expr) : estate->expr;
            app_state->prob  = parent->prob-estate->res->viterbi_prob+prob;
            app_state->hash  = parent->hash * 31 + estate->hash;
            app_state->res   = parent->res;
            app_state->index = parent->index+1;
            app_state->n_args= parent->n_args;
            for (size_t i = 0; i < app_state->n_args; i++) {
                app_state->args[i] = parent->args[i];
            }
            queue.push_back(app_state);
            std::push_heap(queue.begin(), queue.end(), estate_comp);
        }
    }
    return 0;
}

PgfAbstractParser::State *PgfParser::new_state(const PgfTextSpot &start)
{
    State **prev = &current_state;
    State *state = current_state;
    while (state != NULL && state->start.ptr <= start.ptr) {
        if (state->start.ptr == start.ptr)
            return state;
        prev  = &state->next;
        state = state->next;
    }

    state = new State;
    state->start = start;
    state->end   = start;
    state->next  = *prev;
    *prev = state;

    while (state->end.ptr < end) {
        const uint8_t *ptr = state->end.ptr;
        uint32_t ucs = pgf_utf8_decode(&ptr);
        if (!pgf_utf8_is_space(ucs))
            break;
        state->end.pos++;
        state->end.ptr = ptr;
    }

    state->needs_bind = (state->start.pos > 0 && state->start.pos == state->end.pos);

    return state;
}

void PgfParser::symbol_token(Item *item, State *state, ref<PgfSymbolKS> symks)
{
    PgfTextSpot next = state->end;
    if (text_symbol_cmp(&next,end,symks,case_sensitive) != 0)
        return;

    State *next_state = new_state(next);

    item->dot++;
    process(item, next_state);
}

void PgfParser::symbol_bind(Item *item, State *state, PgfSymbol sym)
{
    if (state->needs_bind) {
        State *next_state = state->next;
        if (next_state == NULL || state->end.pos != next_state->start.pos) {
            next_state = new State;
            next_state->start = state->end;
            next_state->end   = state->end;
            next_state->next  = state->next;
            next_state->needs_bind = false;
            state->next = next_state;
        }
        item->dot++;
        next_state->push_item(item);
    } else {
        if (ref<PgfSymbol>::get_tag(sym) == PgfSymbolBIND::tag) {
            delete item;
        } else {
            item->dot++;
            process(item, state);
        }
    }
}

void PgfParser::suspend(Cont *cont,Item *item,bool do_predict,ref<PgfSymbolCat> symcat,interval_t value_i,interval_t lin_idx_i)
{
    auto &suspended = cont->suspended[value_i][lin_idx_i];
    suspended.push_back(item);

    size_t n_suspended = suspended.size();
    if (cont->ccat == NULL) {
        if (n_suspended == 1) {
            std::function<void(ref<PgfCCat>)> f =
                [this,item,cont](ref<PgfCCat> arg) {

                    ref<PgfItem> pitem = arg->items[0];

                    PgfSymbol sym = item->rule->syms[item->dot];
                    auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
                    size_t *values1 = CLONE_VALUES(item->rule,  &item->vars[0]);
                    size_t *values2 = CLONE_VALUES(pitem->rule, &pitem->vars[0]);
                    if (!instantiate(item->rule,  values1, item->rule->args[sym_cat->d],
                                     pitem->rule, values2, pitem->rule->res)) {
                        return;
                    }
                    if (!instantiate(item->rule,  values1, ref<PgfLParam>::from_ptr(&sym_cat->r),
                                     pitem->rule, values2, pitem->rule->lin_idx)) {
                        return;
                    }

                    CCat *&arg_ccat = epsilons[arg->fid];
                    if (arg_ccat == NULL) {
                        arg_ccat = new CCat;
                        arg_ccat->fid = arg->fid;
                        arg_ccat->epsilon = arg;
                        arg_ccat->cont = NULL;
                        arg_ccat->state = NULL;
                        arg_ccat->lin_idx = arg->lin_idx;
                        arg_ccat->value = arg->value;
                        arg_ccat->covered = true;
                        arg_ccat->viterbi_prob = arg->viterbi_prob;
                    }

                    cont->state->completed[cont][arg_ccat->value][arg_ccat->lin_idx] = arg_ccat;
                };
            epsilontable_iter(concr->epsilontable,cont->lincat,f);
        }

        if (do_predict) {
            if (cont->state->needs_bind) {
                bu_predict(concr->phrasetable4, cont->state);
            } else {
                bu_predict(concr->phrasetable1, cont->state, 1, sentence->size);
            }
        }
    } else {
        if (do_predict && n_suspended == 1) {
            if (cont->ccat->epsilon != 0) {
                for (size_t i = 0; i < cont->ccat->epsilon->n_items; i++) {
                    ref<PgfItem> pitem = cont->ccat->epsilon->items[i];
                    td_epsilon(cont->state,cont,pitem,item,symcat);
                }
            } else {
                for (Production *prod : cont->ccat->prods) {
                    td_predict(cont->state,cont,prod,item,symcat);
                }
            }
        }
    }

    State *state = cont->state;
    while (state != NULL) {
        auto it1 = state->completed.find(cont);
        if (it1 != state->completed.end()) {
            for (auto it2 : it1->second.overlaps(value_i)) {
                for (auto it3 : it2.second.overlaps(lin_idx_i)) {
                    Item *new_item = new (item) Item;
                    combine(state, new_item, it3.second);
                }
            }
        }
        state = state->next;
    }
}

void PgfParser::final_item(State *state, CCat *ccat, Item *item, interval_t value, interval_t lin_idx)
{
    if (item->cont == NULL && state->end.ptr == end) {
        ExprState *estate = new(item->args.size()) ExprState;
        estate->expr   = 0;
        estate->prob   = 0;
        estate->hash   = 0;
        estate->res    = ccat;
        estate->index  = 0;
        estate->n_args = item->args.size();
        for (size_t i = 0; i < estate->n_args; i++) {
            estate->args[i] = item->args[i];
            estate->prob += estate->args[i]->viterbi_prob;
        }
        queue.push_back(estate);
        std::push_heap(queue.begin(), queue.end(), estate_comp);
    } else if (ccat != NULL && ccat->pending.size() > 0) {
        auto lin = ref<PgfConcrLin>::untagged(item->rule->container);
        ExprState *estate = new(item->args.size()) ExprState;
        estate->expr   = u->efun(&lin->name);
        estate->prob   = ccat->pending[0]->prob-ccat->viterbi_prob+lin->absfun->prob;
        estate->hash   = 0;
        estate->res    = ccat;
        estate->index  = 0;
        estate->n_args = item->args.size();
        for (size_t i = 0; i < lin->name.size; i++) {
            estate->hash = estate->hash * 101 + lin->name.text[i];
        }
        for (size_t i = 0; i < estate->n_args; i++) {
            estate->args[i] = item->args[i];
            estate->prob += estate->args[i]->viterbi_prob;
        }
        queue.push_back(estate);
        std::push_heap(queue.begin(), queue.end(), estate_comp);
    }
}

#ifdef DEBUG_EXPRS
void PgfParser::print_expr_state_left(PgfPrinter *printer, PgfMarshaller *m, ExprState *estate)
{
    if (estate->res && estate->res->pending.size() > 0) {
        ExprState *parent = estate->res->pending[0];
        print_expr_state_left(printer, m, parent);
        printer->puts(" (");
    }

    if (estate->expr)
        m->match_expr(printer, estate->expr);
    else
        printer->puts("::");
}

void PgfParser::print_expr_state_right(PgfPrinter *printer, ExprState *estate)
{
    for (size_t i = estate->index+1; i < estate->n_args; i++) {
        printer->puts(" ");
        if (estate->args[i] != NULL)
            printer->emeta(estate->args[i]->fid);
        else
            printer->puts("?");
    }

    if (estate->res && estate->res->pending.size() > 0) {
        printer->puts(")");
        ExprState *parent = estate->res->pending[0];
        print_expr_state_right(printer, parent);
    }
}

void PgfParser::print_expr_state(PgfMarshaller *m, ExprState *estate)
{
    PgfPrinter printer(NULL,0,m);
    printer.nprintf(64,"[%f] ",estate->prob);
    print_expr_state_left(&printer, m, estate);
    printer.puts(" . ");

    if (estate->index < estate->n_args) {
        if (estate->args[estate->index] != NULL)
            printer.emeta(estate->args[estate->index]->fid);
        else
            printer.puts("?");
    }

    print_expr_state_right(&printer, estate);

    PgfText *text = printer.get_text();
    fprintf(stderr, "%s\n", text->text);
    free(text);    
}
#endif

PgfParseTableMaker::PgfParseTableMaker(ref<PgfConcr> concr)
    : PgfAbstractParser(concr)
{
    current_state = new State;
    current_state->start.pos = 0;
    current_state->start.ptr = NULL;
    current_state->end       = current_state->start;
    current_state->next      = NULL;
}

ref<PgfItem> PgfParseTableMaker::clone_item(Item *item)
{
    size_t ex_size =
        sizeof(PgfMetaId) * item->args.size() +
        sizeof(size_t)    * item->vars.size();
    auto pitem = PgfDB::malloc<PgfItem>(ex_size);
    pitem->res     = (item->cont->ccat == NULL) ? 0 : item->cont->ccat->fid;
    pitem->pre_alt = item->pre_alt;
    pitem->pre_dot = item->pre_dot;
    pitem->dot     = item->dot;
    pitem->rule    = item->rule;
    memcpy(&pitem->vars[0],&item->vars[0],sizeof(size_t) * item->vars.size());

    for (size_t i = 0; i < item->args.size(); i++) {
        pitem->args[i] = (item->args[i] == NULL) ? 0 : item->args[i]->fid;
    }

    return pitem;
}

PgfAbstractParser::State *PgfParseTableMaker::new_state(const PgfTextSpot &start)
{
    return current_state;
}

void PgfParseTableMaker::symbol_token(Item *item, State *state, ref<PgfSymbolKS> symks)
{
    auto pitem = clone_item(item);
    auto phrasetable1 = phrasetable_insert(concr->phrasetable1,symks,pitem);
    concr->phrasetable1 = phrasetable1;
    delete item;
}

void PgfParseTableMaker::symbol_bind(Item *item, State *state, PgfSymbol sym)
{
    auto pitem = clone_item(item);
    auto phrasetable4 = phrasetable_insert(concr->phrasetable4,ref<PgfSymbolBIND>(0),pitem);
    concr->phrasetable4 = phrasetable4;

    if (ref<PgfSymbol>::get_tag(sym) == PgfSymbolBIND::tag) {
        delete item;
    } else {
        item->dot++;
        process(item,state);
    }
}

void PgfParseTableMaker::suspend(Cont *cont,Item *item,bool do_predict,ref<PgfSymbolCat> symcat,interval_t value_i,interval_t lin_idx_i)
{
    auto &suspended = cont->suspended[value_i][lin_idx_i];
    suspended.push_back(item);

    size_t n_suspended = suspended.size();
    if (cont->ccat == NULL) {
        auto pitem = clone_item(item);
        auto phrasetable2 = phrasetable_insert(concr->phrasetable2,cont->lincat,pitem);
        concr->phrasetable2 = phrasetable2;
    } else {
        if (do_predict && n_suspended == 1) {
            if (cont->ccat->epsilon != 0) {
                for (size_t i = 0; i < cont->ccat->epsilon->n_items; i++) {
                    ref<PgfItem> pitem = cont->ccat->epsilon->items[i];
                    td_epsilon(cont->state,cont,pitem,item,symcat);
                }
            } else {
                for (Production *prod : cont->ccat->prods) {
                    td_predict(cont->state,cont,prod,item,symcat);
                }
            }
        }

        auto pitem = clone_item(item);
        auto phrasetable3 = phrasetable_insert(concr->phrasetable3,cont->ccat->epsilon,pitem);
        concr->phrasetable3 = phrasetable3;
    }

    auto it1 = cont->state->completed.find(cont);
    if (it1 != cont->state->completed.end()) {
        for (auto it2 : it1->second.overlaps(value_i)) {
            for (auto it3 : it2.second.overlaps(lin_idx_i)) {
                CCat *arg = it3.second;
                Item *new_item = new (item) Item;
                combine(cont->state, new_item, arg);
            }
        }
    }
}

void PgfParseTableMaker::final_item(State *state, CCat *ccat, Item *item, interval_t value, interval_t lin_idx)
{
    auto pitem = clone_item(item);

    if (ccat->epsilon == 0) {
        PgfEpsilontable epsilontable = concr->epsilontable;
        epsilontable =
            epsilontable_insert(epsilontable,
                                ccat->cont->lincat,
                                ccat->value, ccat->lin_idx,
                                ccat->fid, ccat->viterbi_prob,
                                pitem,
                                &ccat->epsilon);
        concr->epsilontable = epsilontable;
    } else {
        epsilontable_add(ccat->epsilon, pitem);
    }
}

void PgfParseTableMaker::bu_predict(State *state, CCat *ccat)
{
}

void PgfParseTableMaker::insert_rule(ref<PgfConcrRule> rule)
{
    switch (ref<object>::get_tag(rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(rule->container);

        Cont *&cont = current_state->conts1[lin->lincat];
        if (cont == NULL) {
            cont = new Cont;
            cont->ccat = NULL;
            cont->lincat = lin->lincat;
            cont->state = current_state;
        }

        Item *item = new(rule) Item;
        item->cont    = cont;
        item->dot     = 0;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = rule->syms.as_vector();
        item->rule    = rule;
        item->inside_prob = lin->absfun->prob;
        item->outside_prob = 0;
        current_state->push_item(item);
    }
    }
}

void PgfParseTableMaker::prepare()
{
    while (current_state->has_items()) {
        Item *item = current_state->pop_item();
        process(item,current_state);
    }
}
