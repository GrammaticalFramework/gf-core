#include "data.h"
#include "printer.h"
#include "parser.h"
#include <type_traits>
#include <map>
#include <vector>
#include <queue>

// #define PARSER_DEBUG

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

struct PGF_INTERNAL_DECL PgfParser::Choice {
    size_t id;
    std::vector<Production*> prods;
    
    Choice(size_t id) {
        this->id = id;
    }
};


class PGF_INTERNAL_DECL PgfParser::Production
{
public:
    static
    void predict(Choice *choice, ref<PgfConcrLin> lin, size_t seq_index)
    {
        size_t n_args = lin->absfun->type->hypos->len;

        Production *prod = (Production*)
            malloc(sizeof(Production)+sizeof(Choice*)*n_args);
        prod->lin       = lin;
        prod->seq_index = seq_index;
        memset(prod->args, 0, sizeof(Choice*)*n_args);

        prod->log(choice);
        choice->prods.push_back(prod);
    }

    void log(Choice *res) {
#ifdef PARSER_DEBUG
        PgfPrinter printer(NULL,0,NULL);
        printer.nprintf(10, "?%ld = ", res->id);
        printer.puts(&lin->name);
        
        auto hypos = lin->absfun->type->hypos;
        for (size_t i = 0; i < hypos->len; i++) {
            if (args[i] == NULL)
                printer.efun(&hypos->data[i].type->name);
            else
                printer.nprintf(10, " ?%ld", args[i]->id);
        }
        printer.puts("\n");
        printer.dump();
#endif
    }

    ref<PgfConcrLin> lin;
    size_t seq_index;
    Choice *args[];
};

struct PGF_INTERNAL_DECL PgfParser::ItemConts {
    State *state;
    std::vector<Item> items;
};

class PGF_INTERNAL_DECL PgfParser::Item
{
public:
    static
    void combine(State *state, PgfLincatBackref *backref, Choice *choice)
    {
        ref<PgfSequence> seq = 
            *vector_elem(backref->lin->seqs, backref->seq_index);

        size_t index = backref->seq_index % backref->lin->lincat->fields->len;
        ref<PgfLincatField> field = vector_elem(backref->lin->lincat->fields, index);

//        state->get_conts(field, 0);
        if (backref->dot+1 < seq->syms.len) {
            size_t n_args = backref->lin->absfun->type->hypos->len;

            Item *item = (Item*)
                malloc(sizeof(Item)+sizeof(Choice*)*n_args);
            item->lin       = backref->lin;
            item->seq_index = backref->seq_index;
            item->dot       = backref->dot+1;

            memset(item->args, 0, sizeof(Choice*)*n_args);
            ref<PgfSequence> seq = 
                *vector_elem(item->lin->seqs, backref->seq_index);
            PgfSymbol sym = seq->syms.data[backref->dot];
            ref<PgfSymbolCat> symcat = ref<PgfSymbolCat>::untagged(sym);
            item->args[symcat->d] = choice;
            
            item->log();
        } else {
            Production::predict(choice, backref->lin, backref->seq_index);
        }
    }

    Production *complete()
    {
        size_t n_args = lin->absfun->type->hypos->len;

        Production *prod = (Production*)
            malloc(sizeof(Production)+sizeof(Choice*)*n_args);
        prod->lin       = lin;
        prod->seq_index = seq_index;
        memcpy(prod->args, args, sizeof(Choice*)*n_args);

        return prod;
    }

    void log() {
#ifdef PARSER_DEBUG
        PgfPrinter printer(NULL,0,NULL);

        size_t index = seq_index / lin->lincat->fields->len;
        ref<PgfPResult> res = *vector_elem(lin->res, index);
        ref<PgfDTyp> ty = lin->absfun->type;

        if (res->vars != 0) {
            printer.lvar_ranges(res->vars);
            printer.puts(" . ");
        }

        printer.efun(&ty->name);
        printer.puts("(");
        printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
        printer.puts(") -> ");

        printer.efun(&lin->name);
        printer.puts("[");
        size_t n_args = lin->args->len / lin->res->len;
        for (size_t i = 0; i < n_args; i++) {
            if (i > 0)
                printer.puts(",");

            if (args[i] == NULL)
                printer.parg(vector_elem(ty->hypos, i)->type,
                             vector_elem(lin->args, index*n_args + i));
            else
                printer.nprintf(10, "?%ld", args[i]->id);
        }

        printer.nprintf(10, "]; %ld : ", seq_index % lin->lincat->fields->len);
        ref<PgfSequence> seq = *vector_elem(lin->seqs, seq_index);
        for (size_t i = 0; i < seq->syms.len; i++) {
            if (i > 0)
                printer.puts(" ");
            if (i == dot)
                printer.puts(". ");
            printer.symbol(*vector_elem(&seq->syms, i));
        }
        printer.puts("\n");

        printer.dump();
#endif
    }


private:
    ItemConts *conts;
    ref<PgfConcrLin> lin;
    size_t seq_index;
    size_t dot;
    Choice *args[];
};

class PGF_INTERNAL_DECL PgfParser::State
{
public:
    ItemConts *get_conts(ref<PgfLincatField> field, size_t value)
    {
        ItemConts *conts;
        CFGCat cfg_cat = {field, value};
        auto itr1 = contss.find(cfg_cat);
        if (itr1 == contss.end()) {
            conts = new ItemConts();
            conts->state = this;
            contss.insert(std::pair<CFGCat,ItemConts*>(cfg_cat, conts));
        } else {
            conts = itr1->second;
        }
        return conts;
    }

public:
    size_t start, end;
    State *prev, *next;

    std::map<CFGCat,ItemConts*> contss;
    std::map<ItemConts*,Choice*> choices;
    std::priority_queue<PgfParser::Result*,std::vector<PgfParser::Result*>,PgfParser::ResultComparator> queue;
};


class PgfParser::ResultExpr : public Result
{
public:    
    ResultExpr(Production *prod)
    {
        this->inside_prob  = prod->lin->absfun->prob;
        this->outside_prob = prod->lin->lincat->abscat->prob;
        this->prod         = prod;
        this->arg_index    = 0;
    }

    virtual prob_t prob()
    {
        return inside_prob+outside_prob;
    }

    virtual PgfExpr expr(PgfUnmarshaller *u)
    {
        return u->efun(&prod->lin->name);
    }

    virtual void proceed(PgfParser *parser, PgfUnmarshaller *u)
    {
    }

private:
    prob_t inside_prob;
    prob_t outside_prob;

    Production *prod;
    size_t arg_index;
};

class PgfParser::ResultMeta : public Result
{
public:
    ResultMeta(State *state,
               PgfExpr arg, prob_t prob,
               ResultMeta *next)
    {
        this->inside_prob = prob + (next ? next->inside_prob : 0);
        this->state       = state;
        this->arg         = arg;
        this->next        = next;
    }

    virtual prob_t prob()
    {
        return inside_prob;
    }

    virtual PgfExpr expr(PgfUnmarshaller *u)
    {
        ResultMeta *res = this;        
        PgfExpr expr = u->emeta(0);
        while (res->arg != 0) {
            PgfExpr expr1 = u->eapp(expr, res->arg);
            u->free_ref(expr);
            expr = expr1;
            res  = res->next;
        }
        return expr;
    }

    virtual void proceed(PgfParser *parser, PgfUnmarshaller *u)
    {
        if (state->choices.size() == 0) {
            State *prev = state;
            while (prev->prev != NULL && prev->choices.size() == 0) {
                prev = prev->prev;
            }

            size_t size = state->start-prev->end;
            PgfText *token = (PgfText *) alloca(sizeof(PgfText)+size+1);
            token->size = size;
            memcpy(token->text,parser->sentence->text+prev->end,size);
            token->text[size] = 0;
            
            PgfExpr expr = u->elit(u->lstr(token));
            prev->queue.push(new ResultMeta(prev,
                                            expr, 0,
                                            this));
        } else {
            for (auto it : state->choices) {
                ItemConts *conts = it.first;
                Choice *choice = it.second;

                for (Production *prod : choice->prods) {
                    PgfExpr expr = u->efun(&prod->lin->name);
                    prob_t prob = prod->lin->absfun->prob +
                                  prod->lin->lincat->abscat->prob;
                    conts->state->queue.push(new ResultMeta(conts->state,
                                                            expr, prob,
                                                            this));
                }
            }
        }
    }

private:
    prob_t inside_prob;
    State *state;
    PgfExpr arg;
    ResultMeta *next;
};

PgfParser::PgfParser(ref<PgfConcrLincat> start, PgfText *sentence)
{
    this->start = start;
    this->sentence = textdup(sentence);
    this->last_choice_id = 0;
    this->before = NULL;
    this->after  = NULL;
    this->fetch_state = NULL;
}

void PgfParser::space(size_t start, size_t end, PgfExn* err)
{
    State *prev = NULL;
    State *next = before;
    while (next != NULL && next->start < start) {
        prev = next;
        next = next->next;
    }

    if (next == NULL || next->start != start) {
        before = new State();
        before->start = start;
        before->end   = end;
        before->prev  = prev;
        before->next  = next;

        if (prev != NULL) prev->next = before;
        if (next != NULL) next->prev = before;
    } else {
        before = next;
        before->end = end;
    }

    if (end == sentence->size) {
        fetch_state = after;
        fetch_state->queue.push(new ResultMeta(after,0,0,NULL));
    }
}

void PgfParser::start_matches(size_t end, PgfExn* err)
{
    State *prev = NULL;
    State *next = before;
    while (next != NULL && next->start < end) {
        prev = next;
        next = next->next;
    }

    if (next == NULL || next->start != end) {
        after = new State();
        after->start = end;
        after->end   = end;
        after->prev  = prev;
        after->next  = next;

        if (prev != NULL) prev->next = after;
        if (next != NULL) next->prev = after;
    } else {
        after = next;
    }
}

void PgfParser::match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)
{
    size_t index = seq_index % lin->lincat->fields->len;
    ref<PgfLincatField> field = vector_elem(lin->lincat->fields, index);

    ItemConts *conts = before->get_conts(field, 0);

    Choice *choice;
    auto itr2 = after->choices.find(conts);
    if (itr2 == after->choices.end()) {
        choice = new Choice(++last_choice_id);
        after->choices.insert(std::pair<ItemConts*,Choice*>(conts, choice));
    } else {
        choice = itr2->second;
    }

    Production::predict(choice,lin,seq_index);
/*
    if (itr2 == after->choices.end()) {
        for (size_t i = 0; i < field->backrefs->len; i++) {
            PgfLincatBackref *backref = vector_elem(field->backrefs, i);
            Item::combine(before, backref, choice);
        }
    }*/
}

void PgfParser::end_matches(size_t end, PgfExn* err)
{
    if (end == sentence->size) {
        fetch_state = after;
        fetch_state->queue.push(new ResultMeta(after,0,0,NULL));
    }
}

PgfExpr PgfParser::fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob)
{    
    DB_scope scope(db, READER_SCOPE);

    while (fetch_state != NULL && fetch_state->queue.empty()) {
        fetch_state = fetch_state->next;
    }

    if (fetch_state == NULL) {
        return 0;
    }

    while (fetch_state->prev != NULL) {
        if (!fetch_state->queue.empty()) {
            Result *res = fetch_state->queue.top();
            fetch_state->queue.pop();
            res->proceed(this,u);
        }

        fetch_state = fetch_state->prev;
    }

    if (fetch_state->queue.empty()) {
        return 0;
    }

    Result *res = fetch_state->queue.top();
    fetch_state->queue.pop();
    *prob = res->prob();
    
    return res->expr(u);
}

PgfParser::~PgfParser()
{
    free(sentence);
    printf("~PgfParser()\n");
}
