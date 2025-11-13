#ifndef LR_TABLE_H
#define LR_TABLE_H

class PgfPrinter;

class PGF_INTERNAL_DECL PgfAbstractParser
{
    typedef size_t hash_t;

protected:
    ref<PgfConcr> concr;

    struct CCat;
    struct Cont;
    struct Item;
    struct State;
    struct ExprState;

    struct Production {
        ref<PgfConcrRule> rule;

        struct {
            size_t &operator[](int i) {
                Production *prod = containerof(Production,vars,this);
                return ((size_t*) (((CCat**) (prod+1))+prod->args.size()))[i];
            }
            size_t size() {
                Production *prod = containerof(Production,vars,this);
                return prod->rule->vars.size();
            }
        } vars;

        struct {
            CCat *&operator[](int i) {
                Production *prod = containerof(Production,args,this);
                return ((CCat**) (prod+1))[i];
            }
            size_t size() {
                Production *prod = containerof(Production,args,this);
                return (prod->rule->args != 0) ? prod->rule->args.size() : 0;
            }
        } args;

        void *operator new(size_t sz, Item *item)
        {
            size_t sz2 = item->args.size()*sizeof(CCat*)
                       + item->vars.size()*sizeof(size_t);
            Production *prod = (Production *) malloc(sz+sz2);
            memcpy(prod+1, item+1, sz2);
            return prod;
        }

        void *operator new(size_t sz, ref<PgfItem> pitem)
        {
            size_t sz2 = pitem->args.size()*sizeof(CCat*)
                       + pitem->vars.size()*sizeof(size_t);
            Production *prod = (Production *) malloc(sz+sz2);
            memset(prod+1,0,sz2);
            return prod;
        }

        void operator delete(void *p)
        {
            free(p);
        }

        Production() {
        }
    };

    struct ExprProb {
        PgfExpr expr;
        prob_t prob;
        hash_t hash;
        
        ExprProb(PgfExpr expr, prob_t prob, hash_t hash) {
            this->expr = expr;
            this->prob = prob;
            this->hash = hash;
        }
    };

    struct CCat {
        PgfMetaId fid;
        Cont *cont;
        State *state;
        size_t value;
        size_t lin_idx;
        bool covered;
        std::vector<Production*> prods;
        std::vector<ExprState*> pending;
        std::vector<ExprProb> exprs;

        ~CCat();
    };

    struct State {
        PgfTextSpot start, end;
        bool needs_bind;
        std::map<ref<PgfConcrLincat>,Cont*> conts1;
        std::map<CCat*,std::map<size_t,Cont*>> conts2;
        std::map<Cont*,std::map<size_t,std::map<size_t,CCat*>>> completed;
        State *next;
    };

    struct Cont {
        CCat *ccat;
        ref<PgfConcrLincat> lincat;
        State *state;
        std::vector<Item*> suspended;

        ~Cont();
    };

    struct Item {
        Cont *cont;
        uint16_t pre_alt;
        uint16_t pre_dot;
        uint16_t dot;
        vector<PgfSymbol> syms;
        ref<PgfConcrRule> rule;

        struct {
            size_t &operator[](int i) {
                Item *item = containerof(Item,vars,this);
                return ((size_t*) (((CCat**) (item+1))+item->args.size()))[i];
            }
            size_t size() {
                Item *item = containerof(Item,vars,this);
                return item->rule->vars.size();
            }
        } vars;

        struct {
            CCat *&operator[](int i) {
                Item *item = containerof(Item,args,this);
                return ((CCat**) (item+1))[i];
            }
            size_t size() {
                Item *item = containerof(Item,args,this);
                return (item->rule->args != 0) ? item->rule->args.size() : 0;
            }
        } args;

        void *operator new(size_t sz, ref<PgfConcrRule> rule)
        {
            size_t sz2 = rule->args.size()*sizeof(CCat*)
                       + rule->vars.size()*sizeof(size_t);
            Item *new_item = (Item *) malloc(sz+sz2);
            memset(new_item+1, 0, sz2);
            return new_item;
        }

        void *operator new(size_t sz, Item *item)
        {
            size_t sz2 = item->args.size()*sizeof(CCat*)
                       + item->vars.size()*sizeof(size_t);
            Item *new_item = (Item *) malloc(sz+sz2);
            memcpy(new_item, item, sz+sz2);
            return new_item;
        }

        void operator delete(void *p)
        {
            free(p);
        }
        
        Item() {
        }

        bool instantiate(ref<PgfLParam> lparam,size_t value);
        bool instantiate(ref<PgfLParam> lparam,ref<PgfLParam> value,Item *other);
    };

    struct ExprState {
        PgfExpr expr;
        prob_t prob;
        hash_t hash;

        CCat *res;

        size_t index;
        size_t n_args;
        CCat *args[];

        void *operator new(size_t sz, size_t n_args)
        {
            ExprState *estate = (ExprState *)
                malloc(sz+n_args*sizeof(CCat*));
            return estate;
        }
        
        void operator delete(void *p)
        {
            free(p);
        }

        ExprState() {
        }
    };

    State *first_state, *current_state;
    PgfMetaId last_fid;

    void process(Item *item, const PgfTextSpot &spot, bool bind);
    void symbol(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym);
    void complete(Item *item, const PgfTextSpot &spot, bool bind);

    virtual State *new_state(const PgfTextSpot &start)=0;
    virtual void symbol_token(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym)=0;
    virtual void symbol_bind(Item *item, const PgfTextSpot &spot, PgfSymbol sym)=0;
    virtual void suspend(State *state,ref<PgfConcrLincat> lincat, Item *item)=0;
    virtual void final_item(State *state,Item *item,size_t value,size_t lin_idx)=0;

    virtual void bu_predict(PgfPhrasetable phrasetable, State *state, CCat *ccat);
    Item *bu_item(State *state, ref<PgfItem> pitem);
    CCat *td_epsilon(State *state, Cont *cont, ref<PgfSymbolCCat> arg);
    CCat *td_epsilon(State *state, Cont *cont, ref<PgfSymbolCCat> arg,
                     size_t n_items, vector<ref<PgfItem>> items);
    void td_predict(State *state, Cont *cont, Production *prod, size_t lin_idx);
    void combine(State *state, Item *item, CCat *ccat);

    static
    void print_item(Item *item, const PgfTextSpot &spot);

    static
    void print_prod(CCat *ccat, Production *prod);

public:
    PgfAbstractParser(ref<PgfConcr> concr);
    virtual ~PgfAbstractParser();
};

class PGF_INTERNAL_DECL PgfParser : private PgfAbstractParser, public PgfExprEnum
{
    PgfMarshaller *m;
    PgfUnmarshaller *u;
    PgfText *sentence;
    uint8_t *end;
    bool case_sensitive;

    virtual State *new_state(const PgfTextSpot &start);
    virtual void symbol_token(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym);
    virtual void symbol_bind(Item *item, const PgfTextSpot &spot, PgfSymbol sym);
    virtual void suspend(State *state,ref<PgfConcrLincat> lincat, Item *item);
    virtual void final_item(State *state,Item *item,size_t value,size_t lin_idx);

    void bu_predict(PgfPhrasetable phrasetable, State *state, ptrdiff_t min, ptrdiff_t max);
    void make_chunks(State *state, std::vector<CCat*> &chunks, prob_t prob);
    PgfExpr process_expr(ExprState *estate, prob_t *prob);

    static
    void print_expr_state_left(PgfPrinter *printer, PgfMarshaller *m, ExprState *estate);
    static
    void print_expr_state_right(PgfPrinter *printer, PgfMarshaller *m, ExprState *estate);
    static
    void print_expr_state(PgfMarshaller *m, ExprState *estate);

    struct ExprStateComparator : std::less<ExprState*> {
        bool operator()(ExprState *estate1, ExprState *estate2) {
            return estate1->prob > estate2->prob;
        }
    } estate_comp;

    std::vector<ExprState*> queue;

public:
    PgfParser(ref<PgfConcr> concr, PgfText *sentence, bool case_sensitive, PgfMarshaller *m, PgfUnmarshaller *u);
    virtual ~PgfParser();

    void prepare(ref<PgfConcrLincat> start);

    PgfExpr fetch(PgfDB *db, prob_t *prob);
};

class PGF_INTERNAL_DECL PgfParseTableMaker : private PgfAbstractParser
{
private:
    virtual State *new_state(const PgfTextSpot &start);
    virtual void symbol_token(Item *item, const PgfTextSpot &spot, bool bind, PgfSymbol sym);
    virtual void symbol_bind(Item *item, const PgfTextSpot &spot, PgfSymbol sym);
    virtual void suspend(State *state,ref<PgfConcrLincat> lincat,Item *item);
    virtual void final_item(State *state,Item *item,size_t value,size_t lin_idx);
    virtual void bu_predict(PgfPhrasetable phrasetable, State *state, CCat *ccat);

    static
    ref<PgfItem> clone_item(Item *item);

public:
    PgfParseTableMaker(ref<PgfConcr> concr);
    void insert_rule(ref<PgfConcrRule> rule);
};

#endif
