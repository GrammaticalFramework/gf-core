#include "data.h"
#include "printer.h"
#include "parser.h"
#include <algorithm>

//#define DEBUG_STATE_CREATION
//#define DEBUG_AUTOMATON
#define DEBUG_PARSER
#define DEBUG_GENERATOR

struct PgfLRTableMaker::Item {
    object lin_obj;
    size_t seq_index;
    ref<PgfSequence> seq;
    size_t dot;
};

struct PgfLRTableMaker::State {
    size_t id;
    Predictions *preds;
    std::vector<Item*> items;
    std::vector<PgfLRShift> shifts;
    std::vector<PgfLRReduce> reductions;

    State(Predictions *preds) {
        this->id = 0;
        this->preds = preds;
    }

    ~State() {
        for (Item *item : items) {
            delete item;
        }
    }
};

struct PgfLRTableMaker::Predictions {
    ref<PgfConcrLincat> lincat;
    size_t r;
    bool is_epsilon;
    std::vector<Item*> items;

    ~Predictions() {
        for (Item *item : items) {
            delete item;
        }
    }
};

struct PgfLRTableMaker::CompareItem : std::less<Item*> {
    bool operator() (const Item *item1, const Item *item2) const {
        if (item1->lin_obj < item2->lin_obj)
            return true;
        else if (item1->lin_obj > item2->lin_obj)
            return false;

        if (item1->seq_index < item2->seq_index)
            return true;
        else if (item1->seq_index > item2->seq_index)
            return false;

        return (item1->dot < item2->dot);
    }
};

const PgfLRTableMaker::CompareItem PgfLRTableMaker::compare_item;

PgfLRTableMaker::PgfLRTableMaker(ref<PgfAbstr> abstr, ref<PgfConcr> concr)
{
    this->abstr = abstr;
    this->concr = concr;

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

            State *state = new State(NULL);

            lincat =
                namespace_lookup(concr->lincats, &lstr->val);

            MD5Context ctxt;

            for (size_t i = lincat->n_lindefs; i < lincat->res->len; i++) {
                ref<PgfPResult> res = *vector_elem(lincat->res, i);
                size_t seq_index = 
                    lincat->n_lindefs*lincat->fields->len + i-lincat->n_lindefs;
                ref<PgfSequence> seq = *vector_elem(lincat->seqs, seq_index);
                Item *item = new Item;
                item->lin_obj = lincat.tagged();
                item->seq_index = seq_index;
                item->seq = seq;
                item->dot = 0;

                ctxt.update(item->lin_obj);
                ctxt.update(item->seq_index);
                ctxt.update(item->dot);

                state->items.push_back(item);
            }

            MD5Digest digest;
            ctxt.finalize(&digest);

            states[digest] = state;
            todo.push_back(state);
        }
        }
    }

    this->reductions = NULL;
}

PgfLRTableMaker::~PgfLRTableMaker()
{
    for (auto p : states) {
        delete p.second;
    }

    for (auto p : predictions) {
        delete p.second;
    }
}

#if defined(DEBUG_STATE_CREATION) || defined(DEBUG_AUTOMATON)
void PgfLRTableMaker::print_item(Item *item)
{
    PgfPrinter printer(NULL, 0, NULL);

    switch (ref<PgfConcrLin>::get_tag(item->lin_obj)) {
    case PgfConcrLin::tag: {
        auto lin =
            ref<PgfConcrLin>::untagged(item->lin_obj);
        ref<PgfDTyp> type = lin->absfun->type;
        printer.puts(&type->name);
        printer.puts(" -> ");
        printer.puts(&lin->name);
        printer.puts("[");
        PgfDBMarshaller m;
        for (size_t i = 0; i < type->hypos->len; i++) {
            if (i > 0)
                printer.puts(",");
            m.match_type(&printer, vector_elem(type->hypos, i)->type.as_object());
        }
        printer.nprintf(32, "]; %ld : ", item->seq_index / lin->lincat->fields->len);
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat =
            ref<PgfConcrLincat>::untagged(item->lin_obj);
        printer.puts("linref ");
        printer.puts(&lincat->name);
        printer.puts("[");
        printer.puts(&lincat->name);
        printer.nprintf(32, "]; %ld : ", item->seq_index - lincat->fields->len*lincat->n_lindefs);
        break;
    }
    }

    if (item->dot == 0)
        printer.puts(". ");

    for (size_t i = 0; i < item->seq->syms.len; i++) {
        PgfSymbol sym = item->seq->syms.data[i];
        printer.symbol(sym);

        if (i+1 == item->dot)
            printer.puts(" . ");
    }
    printer.puts("\n");

    PgfText *text = printer.get_text();
    fputs(text->text, stderr);
    free(text);
}
#endif

void PgfLRTableMaker::process(Item *item)
{
#ifdef DEBUG_STATE_CREATION
    print_item(item);
#endif

    if (item->dot < item->seq->syms.len) {
        PgfSymbol sym = item->seq->syms.data[item->dot];
        symbol(item, sym);
    } else {
        complete(item);
    }
}

void PgfLRTableMaker::symbol(Item *item, PgfSymbol sym)
{
    switch (ref<PgfSymbol>::get_tag(sym)) {
    case PgfSymbolCat::tag: {
        auto symcat = ref<PgfSymbolCat>::untagged(sym);

        switch (ref<PgfConcrLin>::get_tag(item->lin_obj)) {
        case PgfConcrLin::tag: {
            auto lin =
                ref<PgfConcrLin>::untagged(item->lin_obj);
            ref<PgfPResult> res =
                *vector_elem(lin->res, item->seq_index / lin->lincat->fields->len);
            ref<PgfHypo> hypo = vector_elem(lin->absfun->type->hypos, symcat->d);
            predict(item, ref<PgfText>::from_ptr(&hypo->type->name), res->vars, &symcat->r);
            break;
        }
        case PgfConcrLincat::tag: {
            auto lincat =
                ref<PgfConcrLincat>::untagged(item->lin_obj);
            ref<PgfPResult> res =
                *vector_elem(lincat->res,
                                item->seq_index - lincat->n_lindefs*lincat->fields->len);
            predict(item, ref<PgfText>::from_ptr(&lincat->name), res->vars, &symcat->r);
            break;
        }
        }
        break;
    }
    }
}

struct PGF_INTERNAL_DECL PgfVariableValue {
    size_t range;
    size_t value;
};

void PgfLRTableMaker::predict(Item *item, ref<PgfText> cat,
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
        predict(item, cat, index);

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

void PgfLRTableMaker::predict(Item *item, ref<PgfText> cat, size_t r)
{
    Predictions *&preds = predictions[Key(cat,r)];
    Predictions *tmp_preds = preds;
    if (preds == NULL) {
        preds = new Predictions;
        preds->lincat = 0;
        preds->r = r;
        preds->is_epsilon = false;
    }

    State *&next_state = continuations[preds];
    State *tmp = next_state;
    if (next_state == NULL) {
        next_state = new State(preds);
    }
    Item *next_item = new Item;
    next_item->lin_obj = item->lin_obj;
    next_item->seq_index = item->seq_index;
    next_item->seq = item->seq;
    next_item->dot = item->dot+1;
    next_state->items.push_back(next_item);
    push_heap(next_state->items.begin(), next_state->items.end(), compare_item);

    if (tmp == NULL) {
        if (tmp_preds == NULL) {
            std::function<bool(ref<PgfAbsFun>)> f =
                [this,preds](ref<PgfAbsFun> fun) {
                    predict(fun, preds);
                    return true;
                };
            probspace_iter(abstr->funs_by_cat, cat, f, false);
        } else {
            for (Item *new_item : preds->items) {
                process(new_item);
            }
        }
    }
}

void PgfLRTableMaker::predict(ref<PgfAbsFun> absfun, Predictions *preds)
{
    ref<PgfConcrLin> lin =
        namespace_lookup(concr->lins, &absfun->name);

    if (lin != 0) {
        preds->lincat = lin->lincat;

        size_t n_args = absfun->type->hypos->len;
        size_t n_fields = lin->lincat->fields->len;
        for (size_t i = 0; i < lin->res->len; i++) {
            size_t seq_index = n_fields * i + preds->r;
            ref<PgfSequence> seq = *vector_elem(lin->seqs,seq_index);

            if (seq->syms.len == 0) {
                preds->is_epsilon = true;
            } else {
                Item *item = new Item;
                item->lin_obj = lin.tagged();
                item->seq_index = seq_index;
                item->seq = seq;
                item->dot = 0;
                preds->items.push_back(item);
                process(item);
            }
        }
    }
}

void PgfLRTableMaker::complete(Item *item)
{
    PgfLRReduce red;
    red.lin_obj = item->lin_obj;
    red.seq_index = item->seq_index;
    reductions->push_back(red);
#if defined(DEBUG_AUTOMATON)
    switch (ref<PgfConcrLin>::get_tag(red.lin_obj)) {
    case PgfConcrLin::tag: {
        auto lin =
            ref<PgfConcrLin>::untagged(red.lin_obj);
        fprintf(stderr, "reduce %s/%zu\n", lin->name.text, red.seq_index);
        break;
    }
    case PgfConcrLincat::tag: {
        auto lincat =
            ref<PgfConcrLincat>::untagged(red.lin_obj);
        fprintf(stderr, "reduce linref %s/%zu\n", lincat->name.text, red.seq_index);
        break;
    }
    }
#endif
}

ref<PgfLRTable> PgfLRTableMaker::make()
{
    size_t state_id = 0;
    while (!todo.empty()) {
        State *state = todo.back(); todo.pop_back();

#if defined(DEBUG_AUTOMATON)
        fprintf(stderr, "--------------- state %ld ---------------\n", state->id);
#endif

        reductions = &state->reductions;
        while (!state->items.empty()) {
            Item *item = state->items.back(); state->items.pop_back();

#if defined(DEBUG_AUTOMATON) && !defined(DEBUG_STATE_CREATION)
            // The order in which we process the items should not matter,
            // For debugging however it is useful to see them in the same order.
            pop_heap(state->items.begin(),state->items.end(),compare_item);
            print_item(item);
#endif

           process(item);
           delete item;
        }
        state->items.shrink_to_fit();

        for (auto i : continuations) {
            MD5Context ctxt;
            auto begin = i.second->items.begin();
            auto end   = i.second->items.end();
            while (begin != end) {
                Item *item = *(--end);
                ctxt.update(item->lin_obj);
                ctxt.update(item->seq_index);
                ctxt.update(item->dot);

                pop_heap(begin,end,compare_item);
            }

            MD5Digest digest;
            ctxt.finalize(&digest);

            State *&next_state = states[digest];
            if (next_state == NULL) {
                next_state = i.second;
                next_state->id = ++state_id;
                todo.push_back(next_state);
            } else {
                delete i.second;
                i.second = next_state;
            }

            PgfLRShift shift;
            shift.lincat = next_state->preds->lincat;
            shift.r = next_state->preds->r;
            shift.next_state = next_state->id;
            shift.is_epsilon = next_state->preds->is_epsilon;
            state->shifts.push_back(shift);
#if defined(DEBUG_AUTOMATON)
            fprintf(stderr, "%s.%zu: state %ld%s\n",
                            shift.lincat->name.text, shift.r, shift.next_state,
                            shift.is_epsilon ? " (epsilon)" : ""
                            );
#endif
        }
        continuations.clear();
    }

    ref<PgfLRTable> lrtable = vector_new<PgfLRState>(states.size());
    for (auto v : states) {
        State *state = v.second;
        ref<PgfLRState> lrstate = vector_elem(lrtable, state->id);

        lrstate->shifts = vector_new<PgfLRShift>(state->shifts.size());
        for (size_t i = 0; i < state->shifts.size(); i++) {
            *vector_elem(lrstate->shifts,i) = state->shifts[i];
        }

        lrstate->reductions = vector_new<PgfLRReduce>(state->reductions.size());
        for (size_t i = 0; i < state->reductions.size(); i++) {
            *vector_elem(lrstate->reductions,i) = state->reductions[i];
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

void PgfParser::reduce(StackNode *parent, ref<PgfConcrLin> lin, size_t seq_index,
                       size_t n, std::vector<Choice*> &args,
                       Stage *before, Stage *after)
{
    if (n == 0) {
        ref<PgfConcrLincat> lincat = lin->lincat;
        size_t r = seq_index % lincat->fields->len;

        Production *prod = new(lin) Production();

        ref<PgfSequence> seq = *vector_elem(lin->seqs, seq_index);
        for (size_t i = 0; i < seq->syms.len; i++) {
            PgfSymbol sym = seq->syms.data[i];
            switch (ref<PgfSymbol>::get_tag(sym)) {
            case PgfSymbolCat::tag: {
                auto symcat =
                    ref<PgfSymbolCat>::untagged(sym);
                Choice *choice = args[seq->syms.len-i-1];
                intersection_map im;
                choice = intersect_choice(choice, prod->args[symcat->d], im);
                if (choice == NULL) {
                    //delete prod;
                    return;
                }
                prod->args[symcat->d] = choice;
                break;
            }
            }
        }

        shift(parent, lincat, r, prod, before, after);
        return;
    }

    args.push_back(parent->choice);
    for (auto node : parent->parents) {
        reduce(node, lin, seq_index, n-1, args, parent->stage, after);
    }
    args.pop_back();
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
    ref<Vector<PgfLRShift>> shifts = vector_elem(concr->lrtable,node->state_id)->shifts;
    for (size_t j = 0; j < shifts->len; j++) {
        ref<PgfLRShift> shift = vector_elem(shifts,j);
        if (shift->is_epsilon) {
            StackNode *new_node = NULL;
            for (StackNode *n : before->nodes) {
                if (n->stage == before && n->state_id == shift->next_state) {
                    new_node = n;
                    break;
                }
            }

            if (new_node == NULL) {
                new_node = new StackNode(before, shift->next_state);
                new_node->choice = new Choice(++last_fid);
                before->nodes.push_back(new_node);

                std::function<void(ref<PgfConcrLin>, size_t seq_index)> f =
                    [this,new_node](ref<PgfConcrLin> lin, size_t seq_index) {
                        Production *prod = new(lin) Production();
                        new_node->choice->prods.push_back(prod);
#ifdef DEBUG_PARSER
                        print_prod(new_node->choice, prod);
#endif
                    };
                phrasetable_lookup_epsilons(concr->phrasetable,
                                            shift->lincat, shift->r, f);
            }

            if (std::find(new_node->parents.begin(), new_node->parents.end(), node) == new_node->parents.end()) {
                new_node->parents.push_back(node);
#ifdef DEBUG_PARSER
                print_transition(node,new_node,before);
#endif
            }
        }
    }

    ref<Vector<PgfLRReduce>> reductions = vector_elem(concr->lrtable,node->state_id)->reductions;
    for (size_t j = 0; j < reductions->len; j++) {
        ref<PgfLRReduce> red = vector_elem(reductions,j);
        switch (ref<PgfConcrLin>::get_tag(red->lin_obj)) {
        case PgfConcrLin::tag: {
            auto lin =
                ref<PgfConcrLin>::untagged(red->lin_obj);
            ref<PgfSequence> seq = *vector_elem(lin->seqs,red->seq_index);
            std::vector<Choice*> args;
            reduce(node, lin, red->seq_index, seq->syms.len, args, before, before);
            break;
        }
        case PgfConcrLincat::tag: {
            auto lincat =
                ref<PgfConcrLincat>::untagged(red->lin_obj);
            ref<PgfSequence> seq = *vector_elem(lincat->seqs,red->seq_index);
            std::vector<Choice*> args;
            if (before->end.pos == sentence->size) {
                complete(node, lincat, red->seq_index, seq->syms.len, args);
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
