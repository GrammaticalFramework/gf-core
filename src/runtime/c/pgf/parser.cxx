#include "data.h"
#include "printer.h"
#include "parser.h"
#include <algorithm>

//#define DEBUG_STATE_CREATION
//#define DEBUG_AUTOMATON
//#define DEBUG_PARSER
//#define DEBUG_GENERATOR

struct PgfLRTableMaker::CCat {
    CCat *parent;
    size_t lin_idx;
    ref<PgfConcrLincat> lincat;

    size_t id;
    bool productive;                 // true if it has non epsilon rules
    std::vector<Item*> items;        // productive items
	std::vector<Item*> suspended;    // items that can progress on epsilon
    std::vector<Production*> prods;  // epsilon productions

    CCat(size_t id, CCat *parent, size_t lin_idx) {
        this->parent     = parent;
        this->lin_idx    = lin_idx;
        this->lincat     = (parent != NULL) ? parent->lincat : 0;
        this->id         = id;
        this->productive = false;
    }

    ~CCat();
};

struct PgfLRTableMaker::Production {
    ref<PgfConcrLin> lin;
    size_t index;
    CCat *args[];

    void *operator new(size_t size, Item *item);

    Production() {
        // If there is no constructor, GCC will zero the object,
        // while it has already been initialized in the new operator.
    }
};

struct PgfLRTableMaker::Item {
    CCat* ccat;
    object lin_obj;
    ref<PgfSequence> seq;
    size_t seq_idx;
    size_t sym_idx;
    size_t n_args;
    CCat *args[];

    void *operator new(size_t size, CCat* ccat, ref<PgfConcrLin> lin, size_t seq_idx);
    void *operator new(size_t size, ref<PgfConcrLincat> lincat, size_t index);
    void *operator new(size_t size, CCat* ccat, Production *prod, size_t lin_idx);
    void *operator new(size_t size, Item *item, CCat *ccat);

    Item() {
        // If there is no constructor, GCC will zero the object,
        // while it has already been initialized in the new operator.
    }
};

struct PgfLRTableMaker::CompareItem : std::less<Item*> {
    bool operator() (const Item *item1, const Item *item2) const {
        if (item1->lin_obj < item2->lin_obj)
            return true;
        else if (item1->lin_obj > item2->lin_obj)
            return false;

        if (item1->seq_idx < item2->seq_idx)
            return true;
        else if (item1->seq_idx > item2->seq_idx)
            return false;

        if (item1->sym_idx < item2->sym_idx)
            return true;
        else if (item1->sym_idx > item2->sym_idx)
            return false;

        for (size_t i = 0; i < item1->n_args; i++) {
            if (item1->args[i] < item2->args[i])
                return true;
            else if (item1->args[i] > item2->args[i])
                return false;
        }
    
        return false;
    }
};

const PgfLRTableMaker::CompareItem PgfLRTableMaker::compare_item;

PgfLRTableMaker::CCat::~CCat() {
    for (Item *item : items) {
        delete item;
    }
    for (Item *item : suspended) {
        delete item;
    }
    for (Production *prod : prods) {
        delete prod;
    }
}

void *PgfLRTableMaker::Production::operator new(size_t size, Item *item) {
    Production *prod = (Production *) malloc(size+sizeof(CCat)*item->n_args);
    prod->lin = ref<PgfConcrLin>::untagged(item->lin_obj);
    size_t n_fields = prod->lin->seqs->len / prod->lin->res->len;
    prod->index = item->seq_idx / n_fields;

    for (size_t i = 0; i < item->n_args; i++) {
        prod->args[i] = item->args[i];
    }

    return prod;
}

void *PgfLRTableMaker::Item::operator new(size_t size, CCat* ccat, ref<PgfConcrLin> lin, size_t seq_idx) {
    size_t n_args = lin->absfun->type->hypos->len;
    size_t n_fields = lin->seqs->len / lin->res->len;
    ref<PgfPResult> res = *vector_elem(lin->res, seq_idx / n_fields);

    Item *item = (Item *) malloc(size+sizeof(CCat*)*n_args);
    item->ccat = ccat;
    item->lin_obj = lin.tagged();
    item->seq = *vector_elem(lin->seqs,seq_idx);
    item->seq_idx = seq_idx;
    item->sym_idx = 0;
    item->n_args = n_args;
    for (size_t i = 0; i < n_args; i++) {
        item->args[i] = NULL;
    }
    return item;
}

void *PgfLRTableMaker::Item::operator new(size_t size, ref<PgfConcrLincat> lincat, size_t index) {
    ref<PgfPResult> res = *vector_elem(lincat->res, lincat->n_lindefs+index);
    size_t seq_idx =
        lincat->n_lindefs*lincat->fields->len + index;

    size_t n_args = 1;
    Item *item = (Item *) malloc(size+sizeof(CCat*)*n_args);
    item->ccat = NULL;
    item->lin_obj = lincat.tagged();
    item->seq = *vector_elem(lincat->seqs,seq_idx);
    item->seq_idx = seq_idx;
    item->sym_idx = 0;
    item->n_args = n_args;
    for (size_t i = 0; i < n_args; i++) {
        item->args[i] = NULL;
    }
    return item;
}

void *PgfLRTableMaker::Item::operator new(size_t size, CCat* ccat, Production *prod, size_t lin_idx) {
    size_t n_args = prod->lin->absfun->type->hypos->len;
    size_t n_fields = prod->lin->seqs->len / prod->lin->res->len;
    ref<PgfPResult> res = *vector_elem(prod->lin->res, prod->index);

    Item *item = (Item *) malloc(size+sizeof(CCat*)*n_args);
    item->ccat = ccat;
    item->lin_obj = prod->lin.tagged();
    item->seq_idx = prod->index*n_fields+lin_idx;
    item->seq = *vector_elem(prod->lin->seqs,item->seq_idx);
    item->sym_idx = 0;
    item->n_args = n_args;
    for (size_t i = 0; i < n_args; i++) {
        item->args[i] = NULL;
    }
    return item;
}

void *PgfLRTableMaker::Item::operator new(size_t size, Item *item, CCat *ccat) {
    Item *new_item = (Item *) malloc(size+sizeof(CCat*)*item->n_args);
    new_item->ccat = item->ccat;
    new_item->lin_obj = item->lin_obj;
    new_item->seq = item->seq;
    new_item->seq_idx = item->seq_idx;
    new_item->sym_idx = item->sym_idx+1;
    new_item->n_args = item->n_args;
    for (size_t i = 0; i < item->n_args; i++) {
        new_item->args[i] = item->args[i];
    }

    ref<PgfSymbolCat> scat =
        ref<PgfSymbolCat>::untagged(item->seq->syms.data[item->sym_idx]);
    new_item->args[scat->d] = ccat;

    return new_item;
}

struct PgfLRTableMaker::State {
    size_t id;
    std::vector<Item*> items;
    std::vector<Item*> completed;
    std::map<Key1,State*,CompareKey1> ccats1;
    std::map<Key2,State*,CompareKey2> ccats2;

    State() {
        this->id = 0;
    }

    ~State() {
        for (Item *item : items) {
            delete item;
        }
    }

    void push_item(Item *item) {
        items.push_back(item);
        push_heap(items.begin(), items.end(), compare_item);
    }
};

PgfLRTableMaker::PgfLRTableMaker(ref<PgfAbstr> abstr, ref<PgfConcr> concr)
{
    this->abstr = abstr;
    this->concr = concr;
    this->ccat_id = 0;

    PgfText *startcat = (PgfText *)
        alloca(sizeof(PgfText)+9);
    startcat->size = 8;
    strcpy(startcat->text, "startcat");

    ref<PgfFlag> flag =
        namespace_lookup(abstr->aflags, startcat);

    ref<PgfConcrLincat> lincat = 0;
    if (flag != 0) {
        switch (ref<PgfLiteral>::get_tag(flag->value)) {
        case PgfLiteralStr::tag: {
            auto lstr = ref<PgfLiteralStr>::untagged(flag->value);

            State *state = new State();

            lincat =
                namespace_lookup(concr->lincats, &lstr->val);

            MD5Context ctxt;

            for (size_t i = 0; i < lincat->res->len-lincat->n_lindefs; i++) {
                Item *item = new(lincat, i) Item;

                ctxt.update(item->lin_obj);
                ctxt.update(item->seq_idx);
                ctxt.update(item->sym_idx);

                state->push_item(item);
            }

            MD5Digest digest;
            ctxt.finalize(&digest);

            states[digest] = state;
            todo.push(state);
        }
        }
    }
}

PgfLRTableMaker::~PgfLRTableMaker()
{
    /*
    for (auto p : states) {
        delete p.second;
    }

    for (auto p : ccats1) {
        delete p.second;
    }

    for (auto p : ccats2) {
        delete p.second;
    }*/
}

#if defined(DEBUG_STATE_CREATION) || defined(DEBUG_AUTOMATON)
void PgfLRTableMaker::print_production(CCat *ccat, Production *prod)
{
    PgfPrinter printer(NULL, 0, NULL);

    ref<PgfPResult> res = *vector_elem(prod->lin->res, prod->index);
    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    ref<PgfDTyp> type = prod->lin->absfun->type;
    printer.nprintf(37, "?%zu -> ", ccat->id);
    printer.puts(&prod->lin->name);
    printer.puts("[");
    PgfDBMarshaller m;
    size_t args_start = type->hypos->len * prod->index;
    for (size_t i = 0; i < type->hypos->len; i++) {
        if (i > 0)
            printer.puts(",");

        if (prod->args[i] == NULL) {
            ref<PgfPArg> arg = vector_elem(prod->lin->args, args_start + i);
            m.match_type(&printer, vector_elem(type->hypos, i)->type.as_object());
            printer.puts("(");
            printer.lparam(arg->param);
            printer.puts(")");
        } else {
            printer.nprintf(32, "?%zu", prod->args[i]->id);
        }
    }
    printer.puts("]\n");

    PgfText *text = printer.get_text();
    fputs(text->text, stderr);
    free(text);
}

void PgfLRTableMaker::print_item(Item *item)
{
    PgfPrinter printer(NULL, 0, NULL);

    switch (ref<PgfConcrLin>::get_tag(item->lin_obj)) {
    case PgfConcrLin::tag: {
        auto lin =
            ref<PgfConcrLin>::untagged(item->lin_obj);

        size_t index = item->seq_idx / lin->lincat->fields->len;
        ref<PgfPResult> res = *vector_elem(lin->res, index);
        if (res->vars != 0) {
            printer.lvar_ranges(res->vars);
            printer.puts(" . ");
        }

        if (item->ccat->parent == NULL) {
            printer.puts(&item->ccat->lincat->name);
            printer.puts("(");
            printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
            printer.puts(") -> ");
        } else {
            printer.nprintf(32,"?%zu -> ",item->ccat->parent->id);
        }

        printer.puts(&lin->name);
        printer.puts("[");
        PgfDBMarshaller m;
        ref<PgfDTyp> type = lin->absfun->type;
        size_t args_start = type->hypos->len * index;
        for (size_t i = 0; i < type->hypos->len; i++) {
            if (i > 0)
                printer.puts(",");

            if (item->args[i] == NULL) {
                ref<PgfPArg> arg = vector_elem(lin->args, args_start + i);
                m.match_type(&printer, vector_elem(type->hypos, i)->type.as_object());
                printer.puts("(");
                printer.lparam(arg->param);
                printer.puts(")");
            } else {
                printer.nprintf(32, "?%zu", item->args[i]->id);
            }
        }
        printer.nprintf(32, "]; %zu : ", item->seq_idx);
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat =
            ref<PgfConcrLincat>::untagged(item->lin_obj);

        size_t index = item->seq_idx - lincat->n_lindefs*lincat->fields->len;
        ref<PgfPResult> res = *vector_elem(lincat->res, lincat->n_lindefs+index);
        if (res->vars != 0) {
            printer.lvar_ranges(res->vars);
            printer.puts(" . ");
        }

        printer.puts("linref ");
        printer.puts(&lincat->name);
        printer.puts("[");
        if (item->args[0] == NULL) {
            printer.puts(&lincat->name);
            printer.puts("(");
            printer.lparam(vector_elem(lincat->args, lincat->n_lindefs+index)->param);
            printer.puts(")");
        } else {
            printer.nprintf(32, "?%zu", item->args[0]->id);
        }
        printer.puts("]; 0 : ");
        break;
    }
    }

    if (item->sym_idx == 0)
        printer.puts(". ");

    for (size_t i = 0; i < item->seq->syms.len; i++) {
        PgfSymbol sym = item->seq->syms.data[i];
        printer.symbol(sym);

        if (i+1 == item->sym_idx)
            printer.puts(" . ");
    }
    printer.puts("\n");

    PgfText *text = printer.get_text();
    fputs(text->text, stderr);
    free(text);
}
#endif

void PgfLRTableMaker::process(State *state, Fold fold, Item *item)
{
#if defined(DEBUG_STATE_CREATION)
    print_item(item);
#endif

    if (item->sym_idx < item->seq->syms.len) {
        PgfSymbol sym = item->seq->syms.data[item->sym_idx];
        symbol(state, fold, item, sym);
    } else {
        complete(state, fold, item);
    }
}

void PgfLRTableMaker::symbol(State *state, Fold fold, Item *item, PgfSymbol sym)
{
    switch (ref<PgfSymbol>::get_tag(sym)) {
    case PgfSymbolCat::tag: {
        auto symcat = ref<PgfSymbolCat>::untagged(sym);

        switch (ref<PgfConcrLin>::get_tag(item->lin_obj)) {
        case PgfConcrLin::tag: {
            auto lin =
                ref<PgfConcrLin>::untagged(item->lin_obj);
            ref<PgfPResult> res =
                *vector_elem(lin->res, item->seq_idx / lin->lincat->fields->len);
            CCat *arg = item->args[symcat->d];
            if (arg != NULL) {
                predict(state, fold, item, arg, res->vars, &symcat->r);
            } else {
                ref<PgfHypo> hypo = vector_elem(lin->absfun->type->hypos, symcat->d);
                predict(state, fold, item, ref<PgfText>::from_ptr(&hypo->type->name), res->vars, &symcat->r);
            }
            break;
        }
        case PgfConcrLincat::tag: {
            auto lincat =
                ref<PgfConcrLincat>::untagged(item->lin_obj);
            ref<PgfPResult> res =
                *vector_elem(lincat->res, lincat->n_lindefs + item->seq_idx - lincat->n_lindefs*lincat->fields->len);
            CCat *arg = item->args[symcat->d];
            if (arg != NULL) {
                predict(state, fold, item, arg, res->vars, &symcat->r);
            } else {
                predict(state, fold, item, ref<PgfText>::from_ptr(&lincat->name), res->vars, &symcat->r);
            }
            break;
        }
        }
        break;
    }
    case PgfSymbolKS::tag: {
        auto symks = ref<PgfSymbolKS>::untagged(sym);
        if (fold == PROBE) {
            item->ccat->productive = true;
        }
    }
    }
}

struct PGF_INTERNAL_DECL PgfVariableValue {
    size_t range;
    size_t value;
};

template<class T>
void PgfLRTableMaker::predict(State *state, Fold fold, Item *item, T cat,
                              ref<Vector<PgfVariableRange>> vars, PgfLParam *r)
{
    PgfVariableValue *values = (PgfVariableValue *)
        alloca(sizeof(PgfVariableValue)*r->n_terms);
    for (size_t i = 0; i < r->n_terms; i++)
    {
        size_t var = r->terms[i].var;
        for (size_t j = 0; j < vars->len; j++)
        {
            ref<PgfVariableRange> range = vector_elem(vars, j);
            if (range->var == var) {
                values[i].range = range->range;
                values[i].value = 0;
                break;
            }
        }
    }

    size_t index = r->i0;
    for (;;) {
        predict(state, fold, item, cat, index);

        size_t i = r->n_terms;
        while (i > 0) {
            i--;
            values[i].value++;
            if (values[i].value < values[i].range) {
                index += r->terms[i].factor;
                i++;
                break;
            }

            index -= (values[i].value-1) * r->terms[i].factor;
            values[i].value = 0;
        }

        if (i == 0) {
            break;
        }
    }
}

void PgfLRTableMaker::predict(State *state, Fold fold, Item *item, ref<PgfText> cat, size_t lin_idx)
{
    CCat *&ccat = ccats1[Key0(cat,lin_idx)];
    CCat *tmp = ccat;
    if (ccat == NULL) {
        ccat = new CCat(++ccat_id, NULL, lin_idx);
    }
    if (fold == PROBE) {
        ccat->suspended.push_back(item);
    }
    if (tmp == NULL) {
        std::function<bool(ref<PgfAbsFun>)> f =
            [this,ccat](ref<PgfAbsFun> fun) {
                predict(fun, ccat);
                return true;
            };
        probspace_iter(abstr->funs_by_cat, cat, f, false);
    }

    if (fold == PROBE) {
        if (item->ccat != NULL && ccat->productive) {
            item->ccat->productive = true;
            item->ccat->items.push_back(item);
        }
    } else {
        if (ccat->productive) {
            State *&new_state = state->ccats1[Key1(ccat->lincat,lin_idx)];
            if (new_state == NULL) {
                new_state = new State;
            }
            new_state->push_item(new(item,NULL) Item);

            if (new_state->items.size() == 1) {
                for (size_t i = 0; i < ccat->items.size(); i++) {
                    process(state, REPEAT, ccat->items[i]);
                }
            }
        }

        if (fold == INIT && ccat->prods.size() > 0) {
            Item *new_item = new (item, ccat) Item;
            process(state, fold, new_item);
        }
    }
}

void PgfLRTableMaker::predict(State *state, Fold fold, Item *item, CCat *ccat, size_t lin_idx)
{
    CCat *&new_ccat = ccats2[Key2(ccat,lin_idx)];
    CCat *tmp = new_ccat;
    if (new_ccat == NULL) {
        new_ccat = new CCat(++ccat_id, ccat, lin_idx);
    }
    if (fold == PROBE) {
        new_ccat->suspended.push_back(item);
    }
    if (tmp == NULL) {
        size_t n_prods = ccat->prods.size();
        for (size_t i = 0; i < n_prods; i++) {
            Production *prod = ccat->prods[i];
            Item *item = new(new_ccat, prod, lin_idx) Item;
            process(NULL, PROBE, item);
        }
    }

    if (fold == PROBE) {
        if (item->ccat != NULL && new_ccat->productive) {
            item->ccat->productive = true;
            item->ccat->items.push_back(item);
        }
    } else {
        if (new_ccat->productive) {
            State *&new_state = state->ccats2[Key2(new_ccat,lin_idx)];
            if (new_state == NULL) {
                new_state = new State;
            }
            new_state->push_item(new(item,NULL) Item);

            if (new_state->items.size() == 1) {
                for (size_t i = 0; i < new_ccat->items.size(); i++) {
                    process(state, REPEAT, new_ccat->items[i]);
                }
            }
        }
        if (fold == INIT && new_ccat->prods.size() > 0) {
            Item *new_item = new (item, new_ccat) Item;
            process(state, fold, new_item);
        }
    }
}

void PgfLRTableMaker::predict(ref<PgfAbsFun> absfun, CCat *ccat)
{
    ref<PgfConcrLin> lin =
        namespace_lookup(concr->lins, &absfun->name);

    if (lin != 0) {
        ccat->lincat = lin->lincat;

        size_t n_fields = lin->seqs->len / lin->res->len;
        for (size_t i = 0; i < lin->res->len; i++) {
            size_t seq_idx = n_fields * i + ccat->lin_idx;
            Item *item = new(ccat, lin, seq_idx) Item;
            process(NULL, PROBE, item);
        }
    }
}

void PgfLRTableMaker::complete(State *state, Fold fold, Item *item)
{
    if (fold == PROBE) {
        Production *prod = new(item) Production;
        item->ccat->prods.push_back(prod);

#if defined(DEBUG_STATE_CREATION) || defined(DEBUG_AUTOMATON)
        print_production(item->ccat, prod);
#endif

        if (item->ccat->prods.size() == 1) {
            // If this is the first epsilon production,
            // resume the suspended items.

            // We don't use an iterator here since the vector suspended,
            // may get updated in the recursion.
            size_t n_susp = item->ccat->suspended.size();
            for (size_t i = 0; i < n_susp; i++) {
                Item *susp = item->ccat->suspended[i];
                if (susp != NULL) {
                    Item *new_item = new (susp, item->ccat) Item;
                    process(state, PROBE, new_item);
                }
            }
        }
    } else {
        state->completed.push_back(item);

#if defined(DEBUG_AUTOMATON)
        fprintf(stderr, "reduce ");
        print_item(item);
#endif
    }
}

ref<PgfLRTable> PgfLRTableMaker::make()
{
    size_t state_id = 0;
    while (!todo.empty()) {
        State *state = todo.front(); todo.pop();

#if defined(DEBUG_AUTOMATON) || defined(DEBUG_STATE_CREATION)
        fprintf(stderr, "--------------- state %ld ---------------\n", state->id);
#endif

        while (!state->items.empty()) {
            Item *item = state->items.back(); state->items.pop_back();

#if defined(DEBUG_AUTOMATON) && !defined(DEBUG_STATE_CREATION)
            // The order in which we process the items should not matter,
            // For debugging however it is useful to see them in the same order.
            pop_heap(state->items.begin(),state->items.end(),compare_item);
            print_item(item);
#endif

            process(state, INIT, item);
            // delete item;
        }

        for (auto &i : state->ccats1) {
            MD5Context ctxt;
            auto begin = i.second->items.begin();
            auto end = i.second->items.end();
            while (begin != end) {
                Item *item = *(--end);
                ctxt.update(item->lin_obj);
                ctxt.update(item->seq_idx);
                ctxt.update(item->sym_idx);
                for (size_t i = 0; i < item->n_args; i++) {
                    ctxt.update(item->args[i]);
                }

                pop_heap(begin,end,compare_item);
            }

            MD5Digest digest;
            ctxt.finalize(&digest);

            State *&next_state = states[digest];
            if (next_state == NULL) {
                next_state = i.second;
                next_state->id = ++state_id;
                todo.push(next_state);
            } else {
                delete i.second;
                i.second = next_state;
            }
            
#if defined(DEBUG_AUTOMATON)
            fprintf(stderr, "%s.%zu: state %ld\n",
                            i.first.first->name.text, i.first.second, i.second->id);
#endif
        }
    }

    ref<PgfLRTable> lrtable = vector_new<PgfLRState>(states.size());
    for (auto v : states) {
        State *state = v.second;
        ref<PgfLRState> lrstate = vector_elem(lrtable, state->id);

        size_t index = 0;
        lrstate->shifts = vector_new<PgfLRShift>(state->ccats1.size());
        for (auto i : state->ccats1) {
            ref<PgfLRShift> shift = vector_elem(lrstate->shifts,index++);
            shift->lincat = i.first.first;
            shift->r = i.first.second;
            shift->next_state = i.second->id;
        }

        lrstate->reductions = vector_new<PgfLRReduce>(state->completed.size());
        for (size_t i = 0; i < state->completed.size(); i++) {
            Item *item = state->completed[i];
            ref<PgfLRReduce> reduction = vector_elem(lrstate->reductions,i);
            reduction->lin_obj = item->lin_obj;
            reduction->seq_idx = item->seq_idx;
            reduction->args = vector_new<bool>(item->n_args);
            for (size_t j = 0; j < item->n_args; j++) {
                *vector_elem(reduction->args, j) = (item->args[j] == NULL);
            }
        }
    }
    return lrtable;
}

struct PgfParser::Choice {
    int fid;
    std::vector<Production*> prods;
    std::vector<ExprState*> states;
    std::vector<ExprInstance> exprs;

    Choice(int fid) {
        this->fid = fid;
    }
};

struct PgfParser::Production {
    ref<PgfConcrLin> lin;
    size_t n_args;
    Choice *args[];

    void *operator new(size_t size, ref<PgfConcrLin> lin) {
        size_t n_args = lin->args->len / lin->res->len;
        Production *prod = (Production *)
            malloc(size+sizeof(Choice*)*n_args);
        prod->lin    = lin;
        prod->n_args = n_args;
        for (size_t i = 0; i < n_args; i++) {
            prod->args[i] = NULL;
        }
        return prod;
    }

    Production() {
        // If there is no constructor, GCC will zero the object,
        // while it has already been initialized in the new operator.
    }

    void operator delete(void *p) {
        free(p);
    }
};

struct PgfParser::StackNode {
    Stage *stage;
    size_t state_id;
    Choice *choice;
    std::vector<StackNode*> parents;

    StackNode(Stage *stage, size_t state_id) {
        this->stage    = stage;
        this->state_id = state_id;
        this->choice   = NULL;
    }
};

struct PgfParser::Stage {
    Stage *next;
    PgfTextSpot start;
    PgfTextSpot end;
    std::vector<StackNode*> nodes;

    Stage(PgfTextSpot spot) {
        next = NULL;
        start = spot;
        end   = spot;
    }
};

struct PgfParser::ExprState {
    prob_t prob;

    Choice *choice;
    Production *prod;
    size_t n_args;
    PgfExpr expr;
};

struct PgfParser::ExprInstance {
    PgfExpr expr;
    prob_t prob;

    ExprInstance(PgfExpr expr, prob_t prob) {
        this->expr = expr;
        this->prob = prob;
    }
};

#if defined(DEBUG_STATE_CREATION) || defined(DEBUG_AUTOMATON) || defined(DEBUG_PARSER)
void PgfParser::print_prod(Choice *choice, Production *prod)
{
    PgfPrinter printer(NULL, 0, m);

    printer.nprintf(32, "?%d -> ", choice->fid);

    ref<PgfDTyp> type = prod->lin->absfun->type;
    printer.puts(&prod->lin->name);
    printer.nprintf(32,"[");
    PgfDBMarshaller m;
    for (size_t i = 0; i < prod->n_args; i++) {
        Choice *choice = prod->args[i];
        if (i > 0)
            printer.puts(",");
        if (choice == NULL) {
            m.match_type(&printer, vector_elem(type->hypos, i)->type.as_object());
        } else {
            printer.nprintf(32, "?%d", choice->fid);
        }
    }
    printer.puts("]\n");

    PgfText *text = printer.get_text();
    fputs(text->text, stderr);
    free(text);
}

void PgfParser::print_transition(StackNode *source, StackNode *target, Stage *stage)
{
    fprintf(stderr, "state %ld --- ?%d ---> state %ld (position %zu-%zu, nodes %zu)\n",
                source->state_id, target->choice->fid, target->state_id,
                stage->start.pos, stage->end.pos,
                stage->nodes.size());
}
#endif

PgfParser::PgfParser(ref<PgfConcr> concr, ref<PgfConcrLincat> start, PgfText *sentence, PgfMarshaller *m, PgfUnmarshaller *u)
{
    this->concr = concr;
    this->sentence = sentence;
    this->m = m;
    this->u = u;
    this->last_fid = 0;
    this->top_choice = NULL;
    this->top_choice_index = 0;

    PgfTextSpot spot;
    spot.pos = 0;
    spot.ptr = (uint8_t*) sentence->text;

    this->before = NULL;
    this->after  = NULL;
    this->ahead  = new Stage(spot);

    StackNode *node = new StackNode(ahead, 0);
    this->ahead->nodes.push_back(node);
}

void PgfParser::shift(StackNode *parent, ref<PgfConcrLincat> lincat, size_t r, Production *prod,
                      Stage *before, Stage *after)
{
    ref<Vector<PgfLRShift>> shifts = vector_elem(concr->lrtable,parent->state_id)->shifts;
    for (size_t i = 0; i < shifts->len; i++) {
        ref<PgfLRShift> shift = vector_elem(shifts,i);
        if (lincat == shift->lincat && r == shift->r) {
            StackNode *node = NULL;
            for (StackNode *n : after->nodes) {
                if (n->stage == before && n->state_id == shift->next_state) {
                    node = n;
                    break;
                }
            }
            if (node == NULL) {
                node = new StackNode(before, shift->next_state);
                node->choice = new Choice(++last_fid);
                after->nodes.push_back(node);
            }

            if (std::find(node->choice->prods.begin(), node->choice->prods.end(), prod) == node->choice->prods.end()) {
                node->choice->prods.push_back(prod);
#ifdef DEBUG_PARSER
                print_prod(node->choice, prod);
#endif
            }

            if (std::find(node->parents.begin(), node->parents.end(), parent) == node->parents.end()) {
                node->parents.push_back(parent);
#ifdef DEBUG_PARSER
                print_transition(parent,node,after);
#endif
            }

            break;
        }
    }
}

PgfParser::Choice *PgfParser::intersect_choice(Choice *choice1, Choice *choice2, intersection_map &im)
{
    if (choice1 == NULL)
        return choice2;
    if (choice2 == NULL)
        return choice1;
    if (choice1 == choice2)
        return choice1;

    std::pair<Choice*,Choice*> key(choice1,choice2);
    auto it = im.find(key);
    if (it != im.end()) {
        return it->second;
    }

    Choice *choice = new Choice(++last_fid);
    im[key] = choice;
    for (Production *prod1 : choice1->prods) {
        for (Production *prod2 : choice2->prods) {
            if (prod1->lin == prod2->lin) {
                Production *prod = new(prod1->lin) Production();
                choice->prods.push_back(prod);

                for (size_t i = 0; i < prod->n_args; i++) {
                    Choice *arg = intersect_choice(prod1->args[i],prod2->args[i],im);
                    if (arg == NULL) {
                        //delete choice;
                        return NULL;
                    }
                    prod->args[i] = arg;
                }                
            }
        }
    }

    return choice;
}

void PgfParser::reduce(StackNode *parent, ref<PgfConcrLin> lin, ref<PgfLRReduce> red,
                       size_t n, std::vector<Choice*> &args,
                       Stage *before, Stage *after)
{
    if (n == 0) {
        ref<PgfConcrLincat> lincat = lin->lincat;
        size_t r = red->seq_idx % lincat->fields->len;

        Production *prod = new(lin) Production();

        ref<PgfSequence> seq = *vector_elem(lin->seqs, red->seq_idx);
        for (size_t i = 0; i < seq->syms.len; i++) {
            PgfSymbol sym = seq->syms.data[i];
            switch (ref<PgfSymbol>::get_tag(sym)) {
            case PgfSymbolCat::tag: {
                auto symcat =
                    ref<PgfSymbolCat>::untagged(sym);
                Choice *choice = args[seq->syms.len-i-1];
                if (choice != NULL) {
                    intersection_map im;
                    choice = intersect_choice(choice, prod->args[symcat->d], im);
                    if (choice == NULL) {
                        //delete prod;
                        return;
                    }
                }
                prod->args[symcat->d] = choice;
                break;
            }
            }
        }

        shift(parent, lincat, r, prod, before, after);
        return;
    }

    if (*vector_elem(red->args, n-1)) {
        args.push_back(parent->choice);
        for (auto node : parent->parents) {
            reduce(node, lin, red, n-1, args, parent->stage, after);
        }
        args.pop_back();    
    } else {
        args.push_back(NULL);
        reduce(parent, lin, red, n-1, args, before, after);
        args.pop_back();
    }
}

void PgfParser::complete(StackNode *parent, ref<PgfConcrLincat> lincat, size_t seq_index,
                         size_t n, std::vector<Choice*> &args)
{
    if (n == 0) {
        top_choice = args[0];
        return;
    }

    args.push_back(parent->choice);
    for (auto node : parent->parents) {
        complete(node, lincat, seq_index, n-1, args);
    }
    args.pop_back();
}

void PgfParser::reduce_all(StackNode *node)
{
    ref<Vector<PgfLRReduce>> reductions = vector_elem(concr->lrtable,node->state_id)->reductions;
    for (size_t j = 0; j < reductions->len; j++) {
        ref<PgfLRReduce> red = vector_elem(reductions,j);
        switch (ref<PgfConcrLin>::get_tag(red->lin_obj)) {
        case PgfConcrLin::tag: {
            auto lin =
                ref<PgfConcrLin>::untagged(red->lin_obj);
            ref<PgfSequence> seq = *vector_elem(lin->seqs,red->seq_idx);
            std::vector<Choice*> args;
            reduce(node, lin, red, seq->syms.len, args, before, before);
            break;
        }
        case PgfConcrLincat::tag: {
            auto lincat =
                ref<PgfConcrLincat>::untagged(red->lin_obj);
            ref<PgfSequence> seq = *vector_elem(lincat->seqs,red->seq_idx);
            std::vector<Choice*> args;
            if (before->end.pos == sentence->size) {
                complete(node, lincat, red->seq_idx, seq->syms.len, args);
            }
        }
        }
    }
}

void PgfParser::space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)
{
#ifdef DEBUG_PARSER
    fprintf(stderr, "------------------ position %zu-%zu ------------------\n",
                    start->pos, end->pos);
#endif

    while (ahead != NULL && ahead->start.pos <= start->pos) {
        Stage *tmp = ahead->next;
        ahead->next = before;
        before = ahead;
        ahead = tmp;
    }

    before->end = *end;

    size_t i = 0;
    while (i < before->nodes.size()) {
        StackNode *node = before->nodes[i++];
        reduce_all(node);
    }
}

void PgfParser::start_matches(PgfTextSpot *end, PgfExn* err)
{
    Stage **last = &ahead; after = *last;
    while (after != NULL && after->start.pos < end->pos) {
        last = &after->next; after = *last;
    }

    if (after == NULL) {
        *last = new Stage(*end);
        after = *last;
    }
}

void PgfParser::match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)
{
    size_t r = seq_index % lin->lincat->fields->len;

    Production *prod = new(lin) Production();

    for (StackNode *parent : before->nodes) {
        shift(parent, lin->lincat, r, prod, before, after);
    }
}

void PgfParser::end_matches(PgfTextSpot *end, PgfExn* err)
{
}

bool PgfParser::CompareExprState::operator() (const ExprState *state1, const ExprState *state2) const {
    return state1->prob > state2->prob;
}

void PgfParser::prepare()
{
    if (top_choice != NULL)
        predict_expr_states(top_choice, 0);
}

void PgfParser::predict_expr_states(Choice *choice, prob_t outside_prob)
{
    for (Production *prod : choice->prods) {
        ExprState *state = new ExprState;
        state->choice = choice;
        state->prod = prod;
        state->n_args = 0;
        state->expr = u->efun(&prod->lin->name);
        state->prob = outside_prob+prod->lin->absfun->prob;
        queue.push(state);
    }
}

#ifdef DEBUG_GENERATOR
void PgfParser::print_expr_state_before(PgfPrinter *printer, ExprState *state)
{
    if (state->choice->states.size() > 0) {
        ExprState *parent = state->choice->states[0];
        print_expr_state_before(printer, parent);
        printer->puts(" [");
    }
    m->match_expr(printer, state->expr);
}

void PgfParser::print_expr_state_after(PgfPrinter *printer, ExprState *state)
{
    for (size_t i = state->n_args+1; i < state->prod->n_args; i++) {
        if (state->prod->args[i] == NULL)
            printer->puts(" ?");
        else
            printer->nprintf(32, " ?%d", state->prod->args[i]->fid);
    }

    if (state->choice->states.size() > 0) {
        printer->puts("]");
        ExprState *parent = state->choice->states[0];
        print_expr_state_after(printer, parent);
    }
}

void PgfParser::print_expr_state(ExprState *state)
{
    PgfPrinter printer(NULL, 0, m);

    printer.nprintf(16, "[%f] ", state->prob);
    print_expr_state_before(&printer, state);
    if (state->n_args < state->prod->n_args) {
        Choice *choice = state->prod->args[state->n_args];
        if (choice == NULL)
            printer.puts(" ?");
        else
            printer.nprintf(32, " ?%d", state->prod->args[state->n_args]->fid);
    }
    print_expr_state_after(&printer, state);
    printer.puts("\n");

    PgfText *text = printer.get_text();
    fputs(text->text, stderr);
    free(text);
}
#endif

bool PgfParser::process_expr_state(ExprState *state)
{
    if (state->n_args >= state->prod->n_args) {
        complete_expr_state(state);
        return true;
    }

    Choice *choice = state->prod->args[state->n_args];
    if (choice == NULL) {
        PgfExpr meta = u->emeta(0);
        PgfExpr app = u->eapp(state->expr, meta);
        u->free_ref(state->expr);
        u->free_ref(meta);
        state->expr = app;
        state->n_args++;
        queue.push(state);
    } else {
        choice->states.push_back(state);

        if (choice->states.size() == 1) {
            predict_expr_states(choice, state->prob);
        } else {
            for (ExprInstance p : choice->exprs) {
                combine_expr_state(state,p);
            }
        }
    }

    return false;
}

void PgfParser::complete_expr_state(ExprState *state)
{
    Choice *choice = state->choice;

    prob_t outside_prob;
    if (choice == top_choice)
        outside_prob = 0;
    else
        outside_prob = choice->states[0]->prob;

    prob_t inside_prob = state->prob-outside_prob;
    choice->exprs.emplace_back(state->expr,inside_prob);
    for (ExprState *state : choice->states) {
        combine_expr_state(state,choice->exprs.back());
    }
}

void PgfParser::combine_expr_state(ExprState *state, ExprInstance &inst)
{
    PgfExpr app = u->eapp(state->expr, inst.expr);

    ExprState *app_state = new ExprState();
    app_state->prob   = state->prob + inst.prob;
    app_state->choice = state->choice;
    app_state->prod   = state->prod;
    app_state->n_args = state->n_args+1;
    app_state->expr   = app;
    queue.push(app_state);
}

void PgfParser::release_expr_state(ExprState *state)
{
    
}

PgfExpr PgfParser::fetch(PgfDB *db, prob_t *prob)
{
    DB_scope scope(db, READER_SCOPE);

    if (top_choice == NULL)
        return 0;

    for (;;) {
        if (top_choice_index < top_choice->exprs.size()) {
            auto inst = top_choice->exprs[top_choice_index++];
            *prob = inst.prob;
            return inst.expr;
        }

        if (queue.empty())
            return 0;

        ExprState *state = queue.top(); queue.pop();
#ifdef DEBUG_GENERATOR
        print_expr_state(state);
#endif

        if (process_expr_state(state)) {
            release_expr_state(state);
        }
    }

    return 0;
}
