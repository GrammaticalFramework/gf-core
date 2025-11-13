#include "data.h"
#include "printer.h"
#include "parser.h"

// #define DEBUG_PARSER
// #define DEBUG_EXPRS

PgfAbstractParser::PgfAbstractParser(ref<PgfConcr> concr)
{
    this->concr = concr;

    this->first_state = NULL;
    this->current_state = NULL;
    this->last_fid = 0;
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
    for (Item *item : suspended) {
        delete item;
    }
}

PgfAbstractParser::~PgfAbstractParser()
{
    State *state = first_state;
    while (state != NULL) {
        for (auto it1 : state->completed) {
            for (auto it2 : it1.second) {
                for (auto it3 : it2.second) {
                    delete it3.second;
                }
            }
        }
        for (auto it : state->conts1) {
            delete it.second;
        }
        for (auto it1 : state->conts2) {
            for (auto it2 : it1.second) {
                delete it2.second;
            }
        }

        State *next = state->next;
        delete state;
        state = next;
    }
}

void PgfAbstractParser::process(Item *item, const PgfTextSpot &spot, bool bind)
{
#ifdef DEBUG_PARSER
    print_item(item,spot);
#endif

    if (item->dot < item->syms.size()) {
        symbol(item,spot,bind,item->syms[item->dot]);
    } else if (item->pre_alt > 0) {
        item->dot     = item->pre_dot+1;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = item->rule->syms.as_vector();
        process(item,spot,bind);
    } else {
        complete(item,spot,bind);
    }
}

PGF_INTERNAL_DECL
int text_symbol_cmp(PgfTextSpot *spot, const uint8_t *end,
                    PgfSymbol sym, bool case_sensitive);

void PgfAbstractParser::symbol(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym)
{
    switch (ref<PgfSymbol>::get_tag(sym)) {
    case PgfSymbolCat::tag: {
        auto symcat = ref<PgfSymbolCat>::untagged(sym);

        State *state = new_state(spot);

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
                suspend(state,lincat,item);
            }
        } else {
            size_t max_value = 1;
            for (size_t i = 0; i < symcat->r.n_terms; i++) {
                size_t var = symcat->r.terms[i].var;
                for (size_t j = 0; j < item->vars.size(); j++) {
                    if (item->rule->vars[j].var == var && item->vars[j] == 0) {
                        max_value *= item->rule->vars[j].range;
                        break;
                    }
                }
            }

            for (size_t value = 0; value < max_value; value++) {
                Item *new_item = new (item) Item;

                size_t value_ = value;
                size_t lin_idx = symcat->r.i0;
                for (size_t i = 0; i < symcat->r.n_terms; i++) {
                    size_t var = symcat->r.terms[i].var;
                    for (size_t j = 0; j < new_item->vars.size(); j++) {
                        if (new_item->rule->vars[j].var == var) {
                            if (new_item->vars[j] == 0) {
                                size_t range = new_item->rule->vars[j].range;
                                new_item->vars[j] = (value_ % range) + 1;
                                value_ = value_ / range;
                            }
                            lin_idx += symcat->r.terms[i].factor * (new_item->vars[j]-1);
                            break;
                        }
                    }
                }

                Cont *&cont = state->conts2[ccat][lin_idx];
                if (cont == NULL) {
                    cont = new Cont;
                    cont->ccat = ccat;
                    cont->lincat = ccat->cont->lincat;
                    cont->state = state;
                }

                cont->suspended.push_back(item);

                if (cont->suspended.size() == 1) {
                    for (Production *prod : cont->ccat->prods) {
                        td_predict(state,cont,prod,lin_idx);
                    }
                } else {
                    State *next = state;
                    while (next != NULL) {
                        
                        auto it1 = next->completed.find(cont);
                        if (it1 != next->completed.end()) {
                            auto it2 = it1->second.find(ccat->value);
                            if (it2 != it1->second.end()) {
                                auto it3 = it2->second.find(lin_idx);
                                if (it3 != it2->second.end()) {
                                    CCat *arg = it3->second;
                                    Item *new_item = new (item) Item;
                                    combine(next, new_item, arg);
                                }
                            }
                        }
                        next = next->next;
                    }
                }
            }
        }
        break;
    }
    case PgfSymbolKS::tag: {
        symbol_token(item, spot, bind, sym);
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
        process(new_item, spot, bind);

        for (size_t i = 0; i < symkp->alts.size(); i++) {
            Item *new_item = new(item) Item;
            new_item->pre_alt = i+2;
            new_item->pre_dot = item->dot;
            new_item->dot     = 0;
            new_item->syms    = symkp->alts[i].form;
            new_item->rule    = item->rule;
            process(new_item, spot, bind);
        }

        // delete item;
        break;
    }
    case PgfSymbolBIND::tag: {
        symbol_bind(item, spot, sym);
        break;
    }
    case PgfSymbolSOFTBIND::tag:
    case PgfSymbolSOFTSPACE::tag: {
        item->dot++;
        process(item, spot, true);
        process(item, spot, false);
        break;
    }
    case PgfSymbolCAPIT::tag:
    case PgfSymbolALLCAPIT::tag:
        item->dot++;
        process(item, spot, bind);
        break;
    }
}

void PgfAbstractParser::complete(Item *item, const PgfTextSpot &spot, bool bind)
{
    State *state = new_state(spot);

    switch (ref<object>::get_tag(item->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(item->rule->container);

        size_t max_value = 1;

        size_t n_inst_vars = 0;
        size_t *inst_vars  = (size_t*)
            alloca(sizeof(size_t)*item->vars.size());

        // Compute which variables to assign to get determinate
        // values of res and lin_idx
        for (size_t i = 0; i < item->vars.size(); i++) {
            if (item->vars[i] != 0)
                continue;

            size_t var = item->rule->vars[i].var;
            for (size_t j = 0; j < item->rule->res->n_terms; j++) {
                if (item->rule->res->terms[j].var == var) {
                    goto found;
                }
            }
            for (size_t j = 0; j < item->rule->lin_idx->n_terms; j++) {
                if (item->rule->lin_idx->terms[j].var == var) {
                    goto found;
                }
            }

            continue;

        found:
            inst_vars[n_inst_vars++] = i;
            max_value *= item->rule->vars[i].range;
        }

        // Go through all possible assignments and create a production
        for (size_t value = 0; value < max_value; value++) {
            size_t value_ = value;
            for (size_t i = 0; i < n_inst_vars; i++) {
                size_t var   = inst_vars[i];
                size_t range = item->rule->vars[var].range;
                item->vars[var] = (value_ % range) + 1;
                value_ = value_ / range;
            }

            size_t res = item->rule->res->i0;
            for (size_t i = 0; i < item->rule->res->n_terms; i++) {
                term t = item->rule->res->terms[i];
                for (size_t j = 0; j < item->vars.size(); j++) {
                    if (t.var == item->rule->vars[j].var) {
                        res += t.factor * (item->vars[j]-1);
                        break;
                    }
                }
            }
            size_t lin_idx = item->rule->lin_idx->i0;
            for (size_t i = 0; i < item->rule->lin_idx->n_terms; i++) {
                term t = item->rule->lin_idx->terms[i];
                for (size_t j = 0; j < item->vars.size(); j++) {
                    if (t.var == item->rule->vars[j].var) {
                        lin_idx += t.factor * (item->vars[j]-1);
                        break;
                    }
                }
            }

            CCat *&ccat = state->completed[item->cont][res][lin_idx];
            if (ccat == NULL) {
                ccat = new CCat;
                ccat->fid = (++last_fid);
                ccat->cont  = item->cont;
                ccat->state = state;
                ccat->lin_idx = lin_idx;
                ccat->value = res;
                ccat->covered = false;

#ifdef DEBUG_PARSER
                {
                    PgfPrinter printer(NULL,0,NULL);
                    printer.nprintf(64,"[%zd-%zd; ",item->cont->state->end.pos,state->start.pos);
                    if (ccat->cont->ccat == NULL) {
                        printer.efun(&ccat->cont->lincat->name);
                        printer.nprintf(64,"(%zd)",ccat->value);
                    } else {
                        printer.emeta(ccat->cont->ccat->fid);
                    }
                    printer.nprintf(64,"; %zd; ",ccat->lin_idx);
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
            final_item(state, item, res, lin_idx);

            if (ccat->prods.size() == 1) {
                if (ccat->cont->ccat == NULL)
                    bu_predict(concr->phrasetable, state, ccat);
                size_t n_items = item->cont->suspended.size();
                for (size_t i = 0; i < n_items; i++) {
                    Item *new_item = new (item->cont->suspended[i]) Item;
                    combine(state,new_item,ccat);
                };
            } else {
                State *next = state;
                while (next != NULL) {
                    for (auto it : next->conts2[ccat]) {
                        size_t lin_idx = it.first;
                        Cont  *cont    = it.second;
                        if (cont != NULL) {
                            size_t n_items = cont->suspended.size();
                            for (size_t i = 0; i < n_items; i++) {
                                td_predict(next,cont,prod,lin_idx);
                            }
                        }
                    }
                    next = next->next;
                }
            }
        }
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat = ref<PgfConcrLincat>::untagged(item->rule->container);
        final_item(state, item, 0, 0);
        break;
    }
    }
}

bool PgfAbstractParser::Item::instantiate(ref<PgfLParam> lparam,size_t value)
{
    if (value < lparam->i0)
        return false;
    value -= lparam->i0;

    for (size_t j = 0; j < lparam->n_terms; j++) {
        term t = lparam->terms[j];
        for (size_t k = 0; k < vars.size(); k++) {
            if (rule->vars[k].var == t.var) {
                if (vars[k] > 0) {
                    if (value < vars[k]-1)
                        return false;
                    value -= vars[k]-1;
                }
                break;
            }
        }
    }

    for (size_t j = 0; j < lparam->n_terms; j++) {
        term t = lparam->terms[j];
        for (size_t k = 0; k < vars.size(); k++) {
            if (rule->vars[k].var == t.var) {
                if (vars[k] == 0) {
                    size_t v_val = value / t.factor;
                    if (v_val >= rule->vars[k].range)
                        return false;
                    vars[k] = v_val + 1;
                    value %= t.factor;
                }
                break;
            }
        }
    }

    return (value == 0);
}

bool PgfAbstractParser::Item::instantiate(ref<PgfLParam> lparam,ref<PgfLParam> value,Item *other)
{
    size_t i = 0;
    size_t i0_lparam = lparam->i0;

    size_t j = 0;
    size_t i0_value  = value->i0;

    while (i < lparam->n_terms && j < value->n_terms) {
        size_t max_lparam = 0, k_lparam = 0;
        while (i < lparam->n_terms) {
            for (k_lparam = 0; k_lparam < this->rule->vars.size(); k_lparam++) {
                if (this->rule->vars[k_lparam].var == lparam->terms[i].var) {
                    break;
                }
            }
            if (this->vars[k_lparam] > 0) {
                i0_lparam += lparam->terms[i].factor * (this->vars[k_lparam]-1);
                i++;
            } else {
                max_lparam = lparam->terms[i].factor * this->rule->vars[k_lparam].range;
                break;
            }
        }

        size_t max_value = 0, k_value = 0;
        while (j < value->n_terms) {
            for (k_value = 0; k_value < other->rule->vars.size(); k_value++) {
                if (other->rule->vars[k_value].var == value->terms[j].var) {
                    break;
                }
            }
            if (other->vars[k_value] > 0) {
                i0_lparam += value->terms[j].factor * (other->vars[k_value]-1);
                j++;
            } else {
                max_value = value->terms[j].factor * other->rule->vars[k_value].range;
                break;
            }
        }

        if (max_lparam > max_value) {
            this->vars[k_lparam] = i0_value / this->rule->vars[k_lparam].range;
            i0_value = i0_value % this->rule->vars[k_lparam].range;
            i++;
        } else {
            //other->vars[k_value] = i0_lparam / other->rule->vars[k_value].range;
            i0_lparam = i0_lparam % other->rule->vars[k_value].range;
            j++;
        }
    }

    return (i0_lparam == i0_value);
}

void PgfAbstractParser::bu_predict(PgfPhrasetable phrasetable,
                                   State *state, CCat *ccat)
{
    if (phrasetable == 0) {
        return;
	}

    int cmp;
    uint8_t tag = ref<PgfSymbol>::get_tag(phrasetable->sym);
    if (PgfSymbolACat::tag != tag) {
		cmp = ((int) PgfSymbolACat::tag) - ((int) tag);
	} else {
        auto symcf = ref<PgfSymbolACat>::untagged(phrasetable->sym);
        cmp = textcmp(&ccat->cont->lincat->name, &symcf->name);
    }
    if (cmp < 0) {
        bu_predict(phrasetable->left,state,ccat);
    } else if (cmp > 0) {
        bu_predict(phrasetable->right,state,ccat);
    } else {        
        for (size_t i = 0; i < phrasetable->n_items; i++) {
            auto new_item = bu_item(ccat->cont->state, phrasetable->items[i]);
            combine(state,new_item,ccat);
        }
    }
}

PgfAbstractParser::Item *PgfAbstractParser::bu_item(State *state, ref<PgfItem> pitem)
{
    Item *item = NULL;

    switch (ref<object>::get_tag(pitem->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(pitem->rule->container);

        Cont *&cont = state->conts1[lin->lincat];
        if (cont == NULL) {
            cont = new Cont;
            cont->ccat = NULL;
            cont->lincat = lin->lincat;
            cont->state = state;
        }

        item = new(pitem->rule) Item;
        item->cont    = cont;
        item->pre_alt = pitem->pre_alt;
        item->pre_dot = pitem->pre_dot;
        item->dot     = pitem->dot;
        item->syms    = pitem->rule->syms.as_vector();
        item->rule    = pitem->rule;
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

    for (size_t i = 0; i < pitem->args.size(); i++) {
        ref<PgfSymbolCCat> arg = pitem->args[i];

        item->args[i] = 0;

        if (arg != 0) {
            Cont *&arg_cont = state->conts1[arg->lincat];
            if (arg_cont == NULL) {
                arg_cont = new Cont;
                arg_cont->ccat = NULL;
                arg_cont->lincat = arg->lincat;
                arg_cont->state = state;
            }
            item->args[i] =
                td_epsilon(state, arg_cont, arg);
        }
    }

    return item;
}

void PgfAbstractParser::combine(State *state, Item *item, CCat *ccat)
{
    PgfSymbol sym = item->rule->syms[item->dot];
    auto sym_cat = ref<PgfSymbolCat>::untagged(sym);

    if (!item->instantiate(item->rule->args[sym_cat->d],ccat->value)) {
        // delete item;
        return;
    }
    if (!item->instantiate(ref<PgfLParam>::from_ptr(&sym_cat->r),ccat->lin_idx)) {
        // delete item;
        return;
    }
    item->dot++;
    item->args[sym_cat->d] = ccat;

    process(item, state->start, false);
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

void PgfAbstractParser::print_item(Item *item, const PgfTextSpot &spot)
{
    PgfPrinter printer(NULL,0,NULL);

    printer.nprintf(32, "[%zd-%zd; ", item->cont ? item->cont->state->end.pos : 0, spot.pos);

    if (item->vars.size() > 0) {
        printer.lvar_ranges(item->rule->vars, &item->vars[0]);
        printer.puts(" ");
    }

    if (item->cont) {
        if (item->cont->ccat == NULL) {
            printer.efun(&item->cont->lincat->name);
            printer.puts("(");
            printer.lparam(item->rule->res);
            printer.puts(")");
        } else {
            printer.emeta(item->cont->ccat->fid);
        }
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
                printer.puts("(");
                printer.lparam(item->rule->args[i]);
                printer.puts(")");
            } else {
                printer.emeta(ccat->fid);
            }
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
    printer.puts("]");

    PgfText *text = printer.get_text();
    fprintf(stderr, "%s\n", text->text);
    free(text);
}

void PgfAbstractParser::print_prod(CCat *ccat, Production *prod)
{
    PgfPrinter printer(NULL,0,NULL);

    if (prod->vars.size() > 0) {
        printer.lvar_ranges(prod->rule->vars, &prod->vars[0]);
        printer.puts(" ");
    }

    printer.emeta(ccat->fid);
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
                printer.puts("(");
                printer.lparam(prod->rule->args[i]);
                printer.puts(")");
            } else {
                printer.emeta(ccat->fid);
            }
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
    this->sentence = sentence;
    this->end = (uint8_t *) (sentence->text+sentence->size);
    this->case_sensitive = case_sensitive;
}

PgfParser::~PgfParser()
{
    State *state = first_state;
    while (state != NULL) {
        for (auto it1 : state->completed) {
            for (auto it2 : it1.second) {
                for (auto it3 : it2.second) {
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
}

void PgfParser::bu_predict(PgfPhrasetable phrasetable,
                           State *state,
                           ptrdiff_t min, ptrdiff_t max)
{
    if (phrasetable == 0)
        return;

    PgfTextSpot current = state->end;
    int cmp;
    if (state->needs_bind) {
        uint8_t tag = ref<PgfSymbol>::get_tag(phrasetable->sym);
        cmp = ((int) PgfSymbolBIND::tag) - ((int) tag);
    } else {
        cmp = text_symbol_cmp(&current,end,phrasetable->sym,case_sensitive);
    }
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
            for (size_t i = 0; i < phrasetable->n_items; i++) {
                Item *item = bu_item(state, phrasetable->items[i]);
                item->dot++;
                if (item != NULL)
                    process(item, current, false);
            }
        }

        if (len <= max)
            bu_predict(phrasetable->right,state,len,max);
     }
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
    PgfTextSpot start_spot = {0, (uint8_t *) sentence->text};
    State *state = new_state(start_spot);
    state->needs_bind = false;
    current_state = state;

    for (size_t i = start->n_lindefs; i < start->rules.size(); i++) {
        ref<PgfConcrRule> rule = start->rules[i];
        Item *item = new(rule) Item;
        item->cont    = NULL;
        item->dot     = 0;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = rule->syms.as_vector();
        item->rule    = rule;
        process(item, start_spot, false);
    }

    while (current_state != NULL) {
        bu_predict(concr->phrasetable, current_state, 1, sentence->size);
        state = current_state;
        current_state = current_state->next;
    }

    if (queue.size() == 0) {
        std::vector<CCat*> chunks;
        make_chunks(state, chunks, 0);
    }
}

PgfExpr PgfParser::fetch(PgfDB *db, prob_t *prob)
{
    DB_scope scope(db, READER_SCOPE);

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
                for (Production *prod : ccat->prods) {
                    auto lin = ref<PgfConcrLin>::untagged(prod->rule->container);

                    ExprState *new_estate = new(prod->args.size()) ExprState;
                    new_estate->expr   = u->efun(&lin->name);
                    new_estate->prob   = estate->prob+lin->absfun->prob;
                    new_estate->hash   = 0;
                    new_estate->res    = ccat;
                    new_estate->index  = 0;
                    new_estate->n_args = prod->args.size();
                    for (size_t i = 0; i < lin->name.size; i++) {
                        new_estate->hash = new_estate->hash * 101 + lin->name.text[i];
                    }
                    for (size_t i = 0; i < new_estate->n_args; i++) {
                        new_estate->args[i] = prod->args[i];
                    }
                    queue.push_back(new_estate);
                    std::push_heap(queue.begin(), queue.end(), estate_comp);
                }
            } else {
                for (ExprProb ep : ccat->exprs) {
                    ExprState *app_state = new(estate->n_args) ExprState;
                    app_state->expr  = estate->expr ? u->eapp(estate->expr, ep.expr) : ep.expr;
                    app_state->prob  = estate->prob+ep.prob;
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

        prob_t prob = estate->prob - estate->res->pending[0]->prob;
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
            app_state->prob  = parent->prob+estate->prob;
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
    State **prev = &first_state;
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

    state->needs_bind = (state->start.pos == state->end.pos);

    return state;
}

void PgfParser::symbol_token(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym)
{
    PgfTextSpot next = spot;

    const uint8_t *start = next.ptr;
    for (;;) {
        const uint8_t *ptr = next.ptr;
        uint32_t ucs = pgf_utf8_decode(&ptr);
        if (!pgf_utf8_is_space(ucs))
            break;
        next.ptr = ptr;
        next.pos++;
    }

    if (bind != (spot.ptr == next.ptr))
        return;

    if (text_symbol_cmp(&next,end,sym,case_sensitive) != 0)
        return;

    item->dot++;
    process(item, next, false);
}

void PgfParser::symbol_bind(Item *item, const PgfTextSpot &spot, PgfSymbol sym)
{
    item->dot++;
    process(item, spot, true);
}

PgfAbstractParser::CCat *PgfAbstractParser::td_epsilon(State *state, Cont *cont, ref<PgfSymbolCCat> arg)
{
    CCat *&ccat = state->completed[cont][arg->value][arg->lin_idx];
    if (ccat == NULL) {
        ccat = new CCat;
        ccat->fid = (++last_fid);
        ccat->cont  = cont;
        ccat->state = state;
        ccat->lin_idx = arg->lin_idx;
        ccat->value = arg->value;
        ccat->covered = true;

#ifdef DEBUG_PARSER
        {
            PgfPrinter printer(NULL,0,NULL);
            printer.nprintf(64,"[%zd-%zd; ",cont->state->end.pos,state->start.pos);
            printer.efun(&ccat->cont->lincat->name);
            printer.nprintf(64,"(%zd); %zd; ",ccat->value,ccat->lin_idx);
            printer.emeta(ccat->fid);
            printer.puts("]");
            PgfText *text = printer.get_text();
            fprintf(stderr, "%s\n", text->text);
            free(text);
        }
#endif

        size_t n_items = 0;
        vector<ref<PgfItem>> items =
            phrasetable_lookup(concr->phrasetable, arg.tagged(), &n_items);

        for (size_t i = 0; i < n_items; i++) {
            ref<PgfItem> pitem = items[i];

            Production *prod = new (pitem) Production;
            prod->rule = pitem->rule;
            memcpy(&prod->vars[0], &pitem->vars[0], sizeof(size_t) * prod->vars.size());

            for (size_t j = 0; j < pitem->args.size(); j++) {
                ref<PgfSymbolCCat> arg = pitem->args[j];

                prod->args[j] = 0;

                if (arg != 0) {
                    Cont *&arg_cont = state->conts1[arg->lincat];
                    if (arg_cont == NULL) {
                        arg_cont = new Cont;
                        arg_cont->ccat = NULL;
                        arg_cont->lincat = arg->lincat;
                        arg_cont->state = state;
                    }
                    prod->args[j] =
                        td_epsilon(state, arg_cont, arg);
                }
            }

#ifdef DEBUG_PARSER
            print_prod(ccat, prod);
#endif
            ccat->prods.push_back(prod);
        }
    }

    return ccat;
}

PgfAbstractParser::CCat *PgfAbstractParser::td_epsilon(State *state, Cont *cont, ref<PgfSymbolCCat> arg,
                                                       size_t n_items, vector<ref<PgfItem>> items)
{
    CCat *&ccat = state->completed[cont][arg->value][arg->lin_idx];
    if (ccat == NULL) {
        ccat = new CCat;
        ccat->fid = (++last_fid);
        ccat->cont  = cont;
        ccat->state = state;
        ccat->lin_idx = arg->lin_idx;
        ccat->value = arg->value;
        ccat->covered = true;

#ifdef DEBUG_PARSER
        {
            PgfPrinter printer(NULL,0,NULL);
            printer.nprintf(64,"[%zd-%zd; ",cont->state->end.pos,state->start.pos);
            printer.efun(&ccat->cont->lincat->name);
            printer.nprintf(64,"(%zd); %zd; ",ccat->value,ccat->lin_idx);
            printer.emeta(ccat->fid);
            printer.puts("]");
            PgfText *text = printer.get_text();
            fprintf(stderr, "%s\n", text->text);
            free(text);
        }
#endif

        for (size_t i = 0; i < n_items; i++) {
            ref<PgfItem> pitem = items[i];

            Production *prod = new (pitem) Production;
            prod->rule = pitem->rule;
            memcpy(&prod->vars[0], &pitem->vars[0], sizeof(size_t) * prod->vars.size());

            for (size_t j = 0; j < pitem->args.size(); j++) {
                ref<PgfSymbolCCat> arg = pitem->args[j];

                prod->args[j] = 0;

                if (arg != 0) {
                    Cont *&arg_cont = state->conts1[arg->lincat];
                    if (arg_cont == NULL) {
                        arg_cont = new Cont;
                        arg_cont->ccat = NULL;
                        arg_cont->lincat = arg->lincat;
                        arg_cont->state = state;
                    }
                    prod->args[j] =
                        td_epsilon(state, arg_cont, arg);
                }
            }

#ifdef DEBUG_PARSER
            print_prod(ccat, prod);
#endif
            ccat->prods.push_back(prod);
        }
    }

    return ccat;
}

void PgfAbstractParser::td_predict(State *state, Cont *cont, Production *prod, size_t lin_idx)
{
    switch (ref<object>::get_tag(prod->rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(prod->rule->container);

        for (ref<PgfConcrRule> rule : lin->rules) {
            Item *item = new (rule) Item;
            item->cont    = cont;
            item->dot     = 0;
            item->pre_alt = 0;
            item->pre_dot = 0;
            item->syms    = rule->syms.as_vector();
            item->rule    = rule;

            if (!item->instantiate(item->rule->res, cont->ccat->value)) {
                // delete item;
                continue;
            }

            if (!item->instantiate(item->rule->lin_idx, lin_idx)) {
                // delete item;
                continue;
            }

            for (size_t i = 0; i < item->args.size(); i++) {
                if (prod->args[i] != NULL) {
                    if (!item->instantiate(item->rule->args[i], prod->args[i]->value)) {
                        // delete item;
                        goto next;
                    }
                } else {
                    /*if (!item->instantiate(item->rule->args[i], prod->args[i]->value)) {
                        delete item;
                        continue;
                    }*/
                }
                item->args[i] = prod->args[i];
            }

            process(item, state->start, false);
        next:;
        }
    }
    default:;
        // should not happend
    }
}

void PgfParser::suspend(State *state,ref<PgfConcrLincat> lincat,Item *item)
{
    Cont *&cont = state->conts1[lincat];
    if (cont == NULL) {
        cont = new Cont;
        cont->ccat = NULL;
        cont->lincat = lincat;
        cont->state = state;
    }

    cont->suspended.push_back(item);

    if (cont->suspended.size() == 1) {
        std::function<void(ref<PgfSymbolCCat>,size_t,vector<ref<PgfItem>>)> f =
            [this,state,item,cont](ref<PgfSymbolCCat> symcf, size_t n_items, vector<ref<PgfItem>> items) {

                Item *new_item = new (item) Item;
                PgfSymbol sym = new_item->rule->syms[new_item->dot];
                auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
                if (!new_item->instantiate(new_item->rule->args[sym_cat->d],symcf->value))
                    return;
                if (!new_item->instantiate(ref<PgfLParam>::from_ptr(&sym_cat->r),symcf->lin_idx))
                    return;

                new_item->dot++;
                new_item->args[sym_cat->d] =
                    td_epsilon(state,cont,symcf,n_items,items);

                process(new_item, state->start, false);
            };
        phrasetable_iter(concr->phrasetable,lincat,f);
    }
}

void PgfParser::final_item(State *state, Item *item, size_t value, size_t lin_idx)
{
    if (item->cont == NULL && state->end.ptr == end) {
        ExprState *estate = new(item->args.size()) ExprState;
        estate->expr   = 0;
        estate->prob   = 0;
        estate->hash   = 0;
        estate->res    = NULL;
        estate->index  = 0;
        estate->n_args = item->args.size();
        for (size_t i = 0; i < estate->n_args; i++) {
            estate->args[i] = item->args[i];
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

void PgfParser::print_expr_state_right(PgfPrinter *printer, PgfMarshaller *m, ExprState *estate)
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
        print_expr_state_right(printer, m, parent);
    }
}

void PgfParser::print_expr_state(PgfMarshaller *m, ExprState *estate)
{
    PgfPrinter printer(NULL,0,m);
    printer.nprintf(64,"[%f] ",estate->prob);
    print_expr_state_left(&printer, m, estate);
    printer.puts(" .");
    print_expr_state_right(&printer, m, estate);

    PgfText *text = printer.get_text();
    fprintf(stderr, "%s\n", text->text);
    free(text);    
}
#endif

PgfParseTableMaker::PgfParseTableMaker(ref<PgfConcr> concr)
    : PgfAbstractParser(concr)
{
    first_state = new State;
    first_state->start.pos = 0;
    first_state->start.ptr = NULL;
    first_state->end       = first_state->start;
    first_state->next      = NULL;
    current_state = first_state;
}

ref<PgfItem> PgfParseTableMaker::clone_item(Item *item)
{
    size_t ex_size =
        sizeof(ref<PgfSymbolCCat>) * item->args.size() +
        sizeof(size_t)             * item->vars.size();
    auto pitem = PgfDB::malloc<PgfItem>(ex_size);
    pitem->pre_alt = item->pre_alt;
    pitem->pre_dot = item->pre_dot;
    pitem->dot     = item->dot;
    pitem->rule    = item->rule;
    memcpy(&pitem->vars[0],&item->vars[0],sizeof(size_t) * item->vars.size());
    
    for (size_t i = 0; i < item->args.size(); i++) {
        ref<PgfSymbolCCat> symcf = 0;
        if (item->args[i] != NULL) {
            symcf = PgfDB::malloc<PgfSymbolCCat>();
            symcf->lincat  = item->args[i]->cont->lincat;
            symcf->value   = item->args[i]->value;
            symcf->lin_idx = item->args[i]->lin_idx;
        }
        pitem->args[i] = symcf;
    }

    return pitem;
}

PgfAbstractParser::State *PgfParseTableMaker::new_state(const PgfTextSpot &start)
{
    return this->first_state;
}

void PgfParseTableMaker::symbol_token(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym)
{
    auto pitem = clone_item(item);
    auto phrasetable = phrasetable_insert(concr->phrasetable,sym,pitem);
    concr->phrasetable = phrasetable;
}

void PgfParseTableMaker::symbol_bind(Item *item, const PgfTextSpot &spot, PgfSymbol sym)
{
    auto pitem = clone_item(item);
    auto phrasetable = phrasetable_insert(concr->phrasetable,sym,pitem);
    concr->phrasetable = phrasetable;
}

void PgfParseTableMaker::suspend(State *state,ref<PgfConcrLincat> lincat,Item *item)
{
    Cont *&cont = state->conts1[lincat];
    if (cont == NULL) {
        cont = new Cont;
        cont->ccat = NULL;
        cont->lincat = lincat;
        cont->state = state;
    }

    cont->suspended.push_back(item);

    for (auto it1 : state->completed[cont]) {
        for (auto it2 : it1.second) {
            CCat *ccat = it2.second;
            if (ccat != NULL) {
                Item *new_item = new (item) Item;
                combine(state,new_item,ccat);
            }
        }
    }

    auto pitem = clone_item(item);
    auto acat  = ref<PgfSymbolACat>::from_ptr((PgfSymbolACat*) &lincat->name);
    auto phrasetable = phrasetable_insert(concr->phrasetable,acat.tagged(),pitem);
    concr->phrasetable = phrasetable;
}

void PgfParseTableMaker::final_item(State *state, Item *item, size_t value, size_t lin_idx)
{
    auto pitem = clone_item(item);
    
    PgfPhrasetable phrasetable = concr->phrasetable;
    phrasetable = phrasetable_insert(phrasetable,
                                     item->cont->lincat, value, lin_idx,
                                     pitem);
    concr->phrasetable = phrasetable;
}

void PgfParseTableMaker::bu_predict(PgfPhrasetable phrasetable, State *state, CCat *ccat)
{
}

void PgfParseTableMaker::insert_rule(ref<PgfConcrRule> rule)
{    
    switch (ref<object>::get_tag(rule->container)) {
    case PgfConcrLin::tag: {
        auto lin = ref<PgfConcrLin>::untagged(rule->container);

        Cont *&cont = first_state->conts1[lin->lincat];
        if (cont == NULL) {
            cont = new Cont;
            cont->ccat = NULL;
            cont->lincat = lin->lincat;
            cont->state = first_state;
        }

        Item *item = new(rule) Item;
        item->cont    = cont;
        item->dot     = 0;
        item->pre_alt = 0;
        item->pre_dot = 0;
        item->syms    = rule->syms.as_vector();
        item->rule    = rule;
        return process(item, first_state->end, false);
    }
    }
}
