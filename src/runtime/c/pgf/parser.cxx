#include "data.h"
#include "printer.h"
#include "parser.h"
#include "math.h"
#include <type_traits>
#include <map>
#include <vector>
#include <queue>

#define PARSER_DEBUG

class PGF_INTERNAL_DECL PgfParser::CFGCat {
public:
    ref<PgfLincatField> field;
    size_t value;

    // copy assignment
    bool operator<(const CFGCat& other) const
    {
        if (field < other.field)
            return true;
        else if (field == other.field)
            return (value < other.value);
        else
            return false;
    }
};

struct PGF_INTERNAL_DECL PgfParser::Choice
{
    ParseItemConts* conts;
    size_t id;
    prob_t viterbi_prob;
    bool is_chunk;
    std::vector<Production*> prods;
    std::vector<Item*> items;
    std::vector<std::pair<PgfExpr,prob_t>> exprs;

    Choice(ParseItemConts* conts, size_t id, prob_t prob)
    {
        this->conts = conts;
        this->id = id;
        this->viterbi_prob = prob;
        this->is_chunk = true;
    }

    void trace(State *state);
};

class PGF_INTERNAL_DECL PgfParser::Production
{
public:
    void trace(Choice *res);

    ref<PgfConcrLin> lin;
    size_t seq_index;
    Choice *args[];
};

struct PGF_INTERNAL_DECL PgfParser::ParseItemConts {
    State *state;
    ref<PgfLincatField> field;
    size_t value;
    std::vector<ParseItem*> items;
};

class PGF_INTERNAL_DECL PgfParser::State
{
public:
    ParseItemConts *get_conts(ref<PgfLincatField> field, size_t value)
    {
        ParseItemConts *conts;
        CFGCat cfg_cat = {field, value};
        auto itr1 = contss.find(cfg_cat);
        if (itr1 == contss.end()) {
            conts = new ParseItemConts();
            conts->state = this;
            conts->field = field;
            conts->value = value;
            contss.insert(std::pair<CFGCat,ParseItemConts*>(cfg_cat, conts));
        } else {
            conts = itr1->second;
        }
        return conts;
    }

public:
    PgfTextSpot start, end;
    State *prev, *next;

    prob_t viterbi_prob;

    class ResultComparator : std::less<Item*> {
    public:
        bool operator()(Item* &lhs, Item* &rhs) const
        {
            return lhs->get_prob() > rhs->get_prob();
        }
    };

    std::map<CFGCat,ParseItemConts*> contss;
    std::map<ParseItemConts*,Choice*> choices;
    std::priority_queue<Item*,std::vector<Item*>,ResultComparator> queue;
};

class PGF_INTERNAL_DECL PgfParser::ParseItem : public Item
{
public:
    void* operator new(size_t size, PgfLinSeqIndex *r)
    {
        size_t n_args = r->lin->absfun->type->hypos->len;
        size_t ex_size = sizeof(Choice*)*n_args;
        ParseItem *item = (ParseItem *) malloc(size+ex_size);
        memset(item->args, 0, ex_size);
        return item;
    }

    void* operator new(size_t size, ParseItem *item)
    {
        size_t n_args = item->lin->absfun->type->hypos->len;
        size_t ex_size = sizeof(Choice*)*n_args;
        ParseItem *new_item = (ParseItem *) malloc(size+ex_size);
        memcpy(new_item, item, size+ex_size);
        return new_item;
    }

    ParseItem(ParseItemConts *conts, size_t values, ref<PgfConcrLin> lin, size_t seq_index)
    {
        this->outside_prob = lin->lincat->abscat->prob;
        this->inside_prob  = lin->absfun->prob;
        this->conts        = conts;
        this->lin          = lin;
        this->seq_index    = seq_index;
        this->dot          = lin->seqs->data[seq_index]->syms.len;
        this->values       = values;
    }

    ParseItem(ParseItemConts *conts, PgfLincatBackref *backref,
              size_t d, Choice *choice)
    {
        this->outside_prob = backref->lin->lincat->abscat->prob;
        this->inside_prob  = backref->lin->absfun->prob + choice->viterbi_prob;
        this->conts        = conts;
        this->lin          = backref->lin;
        this->seq_index    = backref->seq_index;
        this->dot          = backref->dot+1;
        this->args[d]      = choice;
    }

    ParseItem(ParseItemConts *conts, PgfLincatEpsilon *epsilon, prob_t outside_prob)
    {
        this->outside_prob = outside_prob;
        this->inside_prob  = epsilon->lin->absfun->prob;
        this->conts        = conts;
        this->lin          = epsilon->lin;
        this->seq_index    = epsilon->seq_index;
        this->dot          = 0;
    }

    ParseItem(size_t d, Choice *choice)
    {
        this->inside_prob += choice->viterbi_prob;
        this->dot         += 1;
        this->args[d]     = choice;
    }

    static void bu_predict(ref<PgfLincatField> field, State *state, Choice *choice)
    {
        if (field->backrefs == 0)
            return;

        for (size_t i = 0; i < field->backrefs->len; i++) {
            ref<PgfLincatBackref> backref = vector_elem(field->backrefs, i);

            ref<PgfSequence> seq =
                *vector_elem(backref->lin->seqs, backref->seq_index);
            PgfSymbol sym = seq->syms.data[backref->dot];
            ref<PgfSymbolCat> symcat = ref<PgfSymbolCat>::untagged(sym);

            size_t index = backref->seq_index % backref->lin->lincat->fields->len;
            ref<PgfLincatField> up_field = vector_elem(backref->lin->lincat->fields, index);
            ParseItemConts *conts = choice->conts->state->get_conts(up_field, 0);

            state->queue.push(new(&*backref) ParseItem(conts, backref,
                                                       symcat->d, choice));
        }
    }

    static void eps_predict(ref<PgfLincatField> field, State *state, ParseItemConts *conts, prob_t outside_prob)
    {
        if (field->epsilons == 0)
            return;

        for (size_t i = 0; i < field->epsilons->len; i++) {
            ref<PgfLincatEpsilon> epsilon = vector_elem(field->epsilons, i);
            state->queue.push(new(&*epsilon) ParseItem(conts, epsilon, outside_prob));
        }
    }

    void combine(State *state, Choice *choice)
    {
        ref<PgfSequence> seq = lin->seqs->data[seq_index];
        PgfSymbol sym = *vector_elem(&seq->syms,dot);
        auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
        state->queue.push(new(this) ParseItem(sym_cat->d, choice));
    }

    void complete(PgfParser *parser, ref<PgfSequence> seq)
    {
        // the last child as a non-chunk
        size_t dot = seq->syms.len;
        while (dot > 0) {
            dot--;
            PgfSymbol sym = *vector_elem(&seq->syms,dot);
            if (ref<PgfSymbol>::get_tag(sym) == PgfSymbolCat::tag) {
                auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
                Choice *last = args[sym_cat->d];
                if (last != NULL) {
                    if (last->conts == conts)
                        continue;
#ifdef PARSER_DEBUG
                    if (last->is_chunk) {
                        fprintf(stderr, "not-chunk(?%ld)\n", last->id);
                    }
#endif
                    last->is_chunk = false;
                }
            }
            break;
        }

        // Create a new choice
        Choice *choice;
        auto itr2 = parser->after->choices.find(conts);
        if (itr2 == parser->after->choices.end()) {
            choice = new Choice(conts, ++parser->last_choice_id, inside_prob);
            choice->trace(parser->after);
            parser->after->choices.insert(std::pair<ParseItemConts*,Choice*>(conts, choice));
        } else {
            choice = itr2->second;
        }

        // Create a new production
        size_t n_args = lin->absfun->type->hypos->len;

        Production *prod = (Production*)
            malloc(sizeof(Production)+sizeof(Choice*)*n_args);
        prod->lin       = lin;
        prod->seq_index = seq_index;
        memcpy(prod->args, this+1, sizeof(Choice*)*n_args);

        prod->trace(choice);
        choice->prods.push_back(prod);

        // If this the first time when we complete this category
        if (itr2 == parser->after->choices.end()) {
            // Combine with top-down predicted rules
            for (ParseItem *item : conts->items) {
                item->combine(parser->after,choice);
            }
            if (conts->state != parser->after) {
                // Bottom up prediction if this is not an epsilon rule
                bu_predict(conts->field,parser->after,choice);
            }
        }
    }

    void symbol(PgfParser *parser, PgfSymbol sym) {
        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolCat::tag: {
            auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
            Choice *arg = args[sym_cat->d];
            if (arg == NULL) {
                ref<PgfDTyp> ty =
                    vector_elem(lin->absfun->type->hypos, sym_cat->d)->type;
                ref<PgfConcrLincat> lincat =
                    namespace_lookup(parser->concr->lincats, &ty->name);
                if (lincat != 0) {
                    ref<PgfLincatField> field = vector_elem(lincat->fields, sym_cat->r.i0);
                    ParseItemConts *conts = parser->after->get_conts(field, 0);
                    conts->items.push_back(this);

                    if (conts->items.size() == 1) {
                        eps_predict(field, parser->after, conts, inside_prob+outside_prob);
                    }
                }
            }
        }
        default:;
            // Nothing
        }
    }

    virtual State *proceed(PgfParser *parser, PgfUnmarshaller *u)
    {
        ref<PgfSequence> seq = lin->seqs->data[seq_index];

        if (dot >= seq->syms.len) {
            complete(parser, seq);
        } else {
            PgfSymbol sym = *vector_elem(&seq->syms,dot);
            symbol(parser, sym);
        }

        return NULL;
    }

    virtual bool combine(PgfParser *parser, ParseItemConts *conts, PgfExpr expr, prob_t prob, PgfUnmarshaller *u)
    {
        return false;
    }

    virtual void print1(PgfPrinter *printer, State *state, PgfMarshaller *m)
    {
#ifdef PARSER_DEBUG
        printer->nprintf(32,"%ld-%ld; ", conts->state->end.pos, state->start.pos);

        size_t index = seq_index / lin->lincat->fields->len;
        ref<PgfPResult> res = *vector_elem(lin->res, index);
        ref<PgfDTyp> ty = lin->absfun->type;

        if (res->vars != 0) {
            printer->puts("{");
            size_t values = this->values;
            for (size_t i = 0; i < res->vars->len; i++) {
                if (i > 0)
                    printer->puts(", ");

                printer->lvar(res->vars->data[i].var);
                size_t val = values / res->vars->data[i].range;
                printer->nprintf(32,"=%ld",val);

                values = values % res->vars->data[i].range;
            }
            printer->puts("} . ");
        }

        printer->efun(&ty->name);
        printer->puts("(");
        printer->lparam(ref<PgfLParam>::from_ptr(&res->param));
        printer->puts(") -> ");

        printer->efun(&lin->name);
        printer->puts("[");
        size_t n_args = lin->args->len / lin->res->len;
        for (size_t i = 0; i < n_args; i++) {
            if (i > 0)
                printer->puts(",");

            if (args[i] == NULL)
                printer->parg(vector_elem(ty->hypos, i)->type,
                              vector_elem(lin->args, index*n_args + i));
            else
                printer->nprintf(10, "?%ld", args[i]->id);
        }

        printer->nprintf(10, "]; %ld : ", seq_index % lin->lincat->fields->len);
        ref<PgfSequence> seq = *vector_elem(lin->seqs, seq_index);
        for (size_t i = 0; i < seq->syms.len; i++) {
            if (i > 0)
                printer->puts(" ");
            if (dot == i)
                printer->puts(". ");
            printer->symbol(*vector_elem(&seq->syms, i));
        }

        if (dot == seq->syms.len)
            printer->puts(" . ");
#endif
    }

    virtual void print2(PgfPrinter *printer, State *state, int x, PgfMarshaller *m)
    {
    }

    virtual PgfExpr get_expr(PgfUnmarshaller *u)
    {
        return 0;
    }

private:
    ParseItemConts *conts;
    ref<PgfConcrLin> lin;
    size_t seq_index;
    size_t dot;
    size_t values;
    Choice *args[];
};

class PgfParser::ExprItem : public Item
{
public:
    ExprItem(Choice *parent, Production *prod, prob_t outside_prob, PgfUnmarshaller *u)
    {
        this->parent       = parent;
        this->outside_prob = outside_prob;
        this->inside_prob  = prod->lin->absfun->prob;
        this->prod         = prod;
        this->arg_index    = 0;
        this->expr         = u->efun(&prod->lin->name);

        size_t n_args = prod->lin->absfun->type->hypos->len;
        for (size_t i = 0; i < n_args; i++) {
            if (prod->args[i] != NULL)
                this->inside_prob += prod->args[i]->viterbi_prob;
        }
    }

    ExprItem(ExprItem *prev, PgfExpr arg, prob_t prob, PgfUnmarshaller *u)
    {
        this->parent       = prev->parent;
        this->outside_prob = prev->outside_prob;
        this->inside_prob  = prev->inside_prob;
        this->prod         = prev->prod;
        this->arg_index    = prev->arg_index + 1;
        this->expr         = u->eapp(prev->expr,arg);

        this->inside_prob -= prod->args[prev->arg_index]->viterbi_prob;
        this->inside_prob += prob;
    }

    virtual State *proceed(PgfParser *parser, PgfUnmarshaller *u)
    {
        size_t n_args = prod->lin->absfun->type->hypos->len;
        while (arg_index < n_args) {
            Choice *choice = prod->args[arg_index];

            if (choice != NULL) {
                choice->items.push_back(this);

                if (choice->items.size() == 1) {
                    prob_t outside_prob = get_prob()-choice->viterbi_prob;
                    for (auto prod : choice->prods) {
                        parser->before->queue.push(new ExprItem(choice,prod,outside_prob,u));
                    }
                } else {
                    for (auto ep : choice->exprs) {
                        combine(parser,choice->conts,ep.first,ep.second,u);
                    }
                }
                return parser->before;
            }

            PgfExpr arg = u->emeta(0);
            expr = u->eapp(expr,arg);
            u->free_ref(arg);
            arg_index++;
        }

        State *prev = parser->before;
        parent->exprs.push_back(std::pair<PgfExpr,prob_t>(expr,inside_prob));
        for (auto item : parent->items) {
            if (item->combine(parser,parent->conts,expr,inside_prob,u)) {
                prev = parent->conts->state;
            }
        }

        return prev;
    }

    virtual bool combine(PgfParser *parser, ParseItemConts *conts, PgfExpr expr, prob_t prob, PgfUnmarshaller *u)
    {
        parser->before->queue.push(new ExprItem(this,expr,prob,u));
        return false;
    }

    virtual void print1(PgfPrinter *printer, State *state, PgfMarshaller *m)
    {
#ifdef PARSER_DEBUG
        parent->items[0]->print1(printer,state,m);

        printer->puts(" ");

        size_t n_args = prod->lin->absfun->type->hypos->len;
        if (n_args > 0)
            printer->puts("(");
        m->match_expr(printer,expr);
#endif
    }

    virtual void print2(PgfPrinter *printer, State *state, int x, PgfMarshaller *m)
    {
#ifdef PARSER_DEBUG
        size_t n_args = prod->lin->absfun->type->hypos->len;
        for (size_t i = arg_index+x; i < n_args; i++) {
            if (prod->args[i])
                printer->nprintf(10," ?%ld",prod->args[i]->id);
            else
                printer->puts(" ?");
        }
        if (n_args > 0)
            printer->puts(")");

        parent->items[0]->print2(printer,state,1,m);
#endif
    }

    virtual PgfExpr get_expr(PgfUnmarshaller *u)
    {
        return expr;
    }

private:
    Choice *parent;
    Production *prod;
    size_t arg_index;
    PgfExpr expr;
};

class PgfParser::MetaItem : public Item
{
public:
    MetaItem(State *state,
               PgfExpr arg,
               prob_t inside_prob,
               MetaItem *next)
    {
        this->outside_prob = state->viterbi_prob;
        this->inside_prob  = inside_prob;
        this->state        = state;
        this->arg          = arg;
        this->next         = next;
    }

    virtual State *proceed(PgfParser *parser, PgfUnmarshaller *u)
    {
        if (state->prev == NULL)
            return NULL;

        if (state->choices.size() == 0) {
            State *prev = state;
            while (prev->prev != NULL && prev->choices.size() == 0) {
                prev = prev->prev;
            }

            size_t size = state->start.ptr-prev->end.ptr;
            PgfText *token = (PgfText *) alloca(sizeof(PgfText)+size+1);
            token->size = size;
            memcpy(token->text,prev->end.ptr,size);
            token->text[size] = 0;

            PgfExpr expr = u->elit(u->lstr(token));
            prev->queue.push(new MetaItem(prev, expr,
                                          inside_prob,
                                          this));
            return prev;
        } else {
            for (auto it : state->choices) {
                ParseItemConts *conts = it.first;
                Choice *choice = it.second;

                if (!choice->is_chunk)
                    continue;

                choice->items.push_back(this);

                if (choice->items.size() == 1) {
                    prob_t prob = conts->state->viterbi_prob+inside_prob;
                    for (Production *prod : choice->prods) {
                        parser->before->queue.push(new ExprItem(choice,
                                                                prod, prob+prod->lin->lincat->abscat->prob, u));
                    }
                } else {
                    for (auto ep : choice->exprs) {
                        combine(parser,conts,ep.first,ep.second,u);
                    }
                }
            }
            return parser->before;
        }
    }

    virtual bool combine(PgfParser *parser, ParseItemConts *conts, PgfExpr expr, prob_t prob, PgfUnmarshaller *u)
    {
        conts->state->queue.push(new MetaItem(conts->state,
                                              expr,
                                              this->inside_prob+conts->field->lincat->abscat->prob+prob,
                                              this));
        return true;
    }

    virtual void print1(PgfPrinter *printer, State *state, PgfMarshaller *m)
    {
#ifdef PARSER_DEBUG
        printer->puts("?");
#endif
    }

    virtual void print2(PgfPrinter *printer, State *state, int x, PgfMarshaller *m)
    {
#ifdef PARSER_DEBUG
        MetaItem *res = this;
        while (res->arg != 0) {
            printer->puts(" (");
            m->match_expr(printer, res->arg);
            printer->puts(")");
            res = res->next;
        }
#endif
    }

    virtual PgfExpr get_expr(PgfUnmarshaller *u)
    {
        MetaItem *res = this;
        PgfExpr expr = u->emeta(0);
        while (res->arg != 0) {
            PgfExpr expr1 = u->eapp(expr, res->arg);
            u->free_ref(expr);
            expr = expr1;
            res  = res->next;
        }
        return expr;
    }

private:
    State *state;
    PgfExpr arg;
    MetaItem *next;
};

void PgfParser::Item::trace(State *state, PgfMarshaller *m)
{
#ifdef PARSER_DEBUG
    PgfPrinter printer(NULL,0,m);
    printer.puts("[");
    print1(&printer, state, m);
    print2(&printer, state, 0, m);
    printer.nprintf(40,"; %f+%f=%f]\n",inside_prob,outside_prob,inside_prob+outside_prob);
    printer.dump();
#endif
}

void PgfParser::Choice::trace(State *state)
{
#ifdef PARSER_DEBUG
    size_t seq_index = conts->field-conts->field->lincat->fields->data;

    PgfPrinter printer(NULL,0,NULL);
    printer.nprintf(40,"[%ld-%ld; ", conts->state->end.pos, state->start.pos);
    printer.efun(&conts->field->lincat->name);
    printer.nprintf(30,"(%ld); %ld", conts->value, seq_index);
    printer.nprintf(40,"; ?%ld; %f]\n", id, viterbi_prob);
    printer.dump();
#endif
}

void PgfParser::Production::trace(PgfParser::Choice *res) {
#ifdef PARSER_DEBUG
    PgfPrinter printer(NULL,0,NULL);
    printer.nprintf(10, "?%ld = ", res->id);
    printer.puts(&lin->name);

    printer.puts("[");
    auto hypos = lin->absfun->type->hypos;
    for (size_t i = 0; i < hypos->len; i++) {
        if (i > 0)
            printer.puts(",");

        if (args[i] == NULL)
            printer.efun(&hypos->data[i].type->name);
        else
            printer.nprintf(10, "?%ld", args[i]->id);
    }
    printer.puts("]\n");
    printer.dump();
#endif
}

PgfParser::PgfParser(ref<PgfConcr> concr, ref<PgfConcrLincat> start, PgfText *sentence,
                     PgfMarshaller *m, PgfUnmarshaller *u)
{
    this->concr = concr;
    this->start = start;
    this->sentence = textdup(sentence);
    this->last_choice_id = 0;
    this->before = NULL;
    this->after  = NULL;
    this->m = m;
    this->u = u;
}

void PgfParser::space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)
{
    State *prev = NULL;
    State *next = before;
    while (next != NULL && next->start.pos < start->pos) {
        prev = next;
        next = next->next;
    }

    if (next == NULL || next->start.pos != start->pos) {
        before = new State();
        before->start = *start;
        before->end   = *end;
        before->prev  = prev;
        before->next  = next;
        before->viterbi_prob = prev ? prev->viterbi_prob : 0;

        if (prev != NULL) prev->next = before;
        if (next != NULL) next->prev = before;
    } else {
        before = next;
        before->end = *end;
    }
}

void PgfParser::start_matches(PgfTextSpot *end, PgfExn* err)
{
    State *prev = NULL;
    State *next = before;
    while (next != NULL && next->start.pos < end->pos) {
        prev = next;
        next = next->next;
    }

    if (next == NULL || next->start.pos != end->pos) {
        after = new State();
        after->start = *end;
        after->end   = *end;
        after->prev  = prev;
        after->next  = next;
        after->viterbi_prob = INFINITY;

        if (prev != NULL) prev->next = after;
        if (next != NULL) next->prev = after;
    } else {
        after = next;
    }
}

void PgfParser::match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)
{
    ref<PgfLincatField> field  = vector_elem(lin->lincat->fields, seq_index % lin->lincat->fields->len);
    ref<PgfPResult> result = *vector_elem(lin->res, seq_index / lin->lincat->fields->len);

    ParseItemConts *conts = before->get_conts(field, result->param.i0);
    PgfLinSeqIndex r = {lin, seq_index};
    after->queue.push(new(&r) ParseItem(conts, result->param.i0, lin, seq_index));
}

void PgfParser::end_matches(PgfTextSpot *end, PgfExn* err)
{
    while (!after->queue.empty()) {
        Item *item = after->queue.top();
        after->queue.pop();

        item->trace(after,m);
        item->proceed(this,NULL);
    }

    for (auto i : after->choices) {
        ParseItemConts *conts  = i.first;
        Choice *choice = i.second;

        if (choice->is_chunk) {
            prob_t viterbi_prob = conts->state->viterbi_prob+
                                  conts->field->lincat->abscat->prob+
                                  choice->viterbi_prob;
            if (after->viterbi_prob > viterbi_prob)
                after->viterbi_prob = viterbi_prob;
        }
    }

    if (isinf(after->viterbi_prob))
        after->viterbi_prob = before->viterbi_prob;
}

void PgfParser::prepare()
{
    after->queue.push(new MetaItem(after,0,0,NULL));
    before = after;
}

PgfExpr PgfParser::fetch(PgfDB *db, prob_t *prob)
{    
    DB_scope scope(db, READER_SCOPE);

    while (before != NULL && before->queue.empty()) {
        before = before->next;
    }

    while (!before->queue.empty()) {
        Item *item = before->queue.top();
        before->queue.pop();

        item->trace(after,m);

        if (before->prev == NULL) {
            *prob = item->get_prob();
            return item->get_expr(u);
        }

        before = item->proceed(this,u);
    }

    return 0;
}

PgfParser::~PgfParser()
{
    free(sentence);
    printf("~PgfParser()\n");
}