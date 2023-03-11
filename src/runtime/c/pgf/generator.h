#ifndef GENERATOR_H
#define GENERATOR_H

class PGF_INTERNAL_DECL PgfRandomGenerator : public PgfUnmarshaller
{
    const static int VAR_PROB = 0.1;

    ref<PgfPGF> pgf;
    size_t depth;
    uint64_t *seed;
    prob_t prob;
    PgfMarshaller *m;
    PgfInternalMarshaller i_m;
    PgfUnmarshaller *u;

    struct Scope {
        Scope *next;
        PgfType type;
        PgfMarshaller *m;
        PgfBindType bind_type;
        PgfText var;
    };

    Scope *scope;
    size_t scope_len;

    prob_t rand() {
        *seed = *seed * 1103515245 + 12345;
        return (prob_t)((*seed/65536) % 32768)/32768;
    }

public:
    PgfRandomGenerator(ref<PgfPGF> pgf, size_t depth, uint64_t *seed,
                       PgfMarshaller *m, PgfUnmarshaller *u);
    prob_t getProb() { return prob; }
    ~PgfRandomGenerator();

    PgfExpr descend(PgfExpr expr, size_t n_hypos, PgfTypeHypo *hypos);

    virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body);
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg);
    virtual PgfExpr elit(PgfLiteral lit);
    virtual PgfExpr emeta(PgfMetaId meta);
    virtual PgfExpr efun(PgfText *name);
    virtual PgfExpr evar(int index);
    virtual PgfExpr etyped(PgfExpr expr, PgfType typ);
    virtual PgfExpr eimplarg(PgfExpr expr);
    virtual PgfLiteral lint(size_t size, uintmax_t *v);
    virtual PgfLiteral lflt(double v);
    virtual PgfLiteral lstr(PgfText *v);
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs);
    virtual void free_ref(object x);
};

class PGF_INTERNAL_DECL PgfExhaustiveGenerator : public PgfUnmarshaller, public PgfExprEnum
{
    struct Result;

    ref<PgfPGF> pgf;
    size_t depth;
    PgfMarshaller *m;
    Result *top_res;
    size_t top_res_index;

    struct State {
        Result *res;
        prob_t prob;
        virtual bool process(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u) = 0;
        virtual void free_refs(PgfUnmarshaller *u);
        static void release(State *state, PgfUnmarshaller *u);
    };

    struct State0 : State {
        PgfProbspace space;
        virtual bool process(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u);
    };

    struct State1 : State {
        ref<PgfDTyp> type;
        size_t n_args;
        PgfExpr expr;

        virtual bool process(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u);
        virtual void free_refs(PgfUnmarshaller *u);
        void combine(PgfExhaustiveGenerator *gen, 
                     PgfExpr expr, prob_t prob,
                     PgfUnmarshaller *u);
        void complete(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u);
    };

    struct Result {
        size_t ref_count;
        std::vector<State1*> states;
        std::vector<std::pair<PgfExpr,prob_t>> exprs;

        Result();

        prob_t outside_prob(PgfExhaustiveGenerator *gen) {
            if (this == gen->top_res)
                return 0;
            return states[0]->prob;
        }
    };

    class CompareState : public std::less<State*> {
    public:
        bool operator() (const State* s1, const State* s2) const {
            return s1->prob > s2->prob;
        }
    };

    class CompareText : public std::less<ref<PgfText>> {
    public:
        bool operator() (const ref<PgfText> t1, const ref<PgfText> t2) const {
            return textcmp(t1, t2) < 0;
        }
    };

    std::map<ref<PgfText>, Result*, CompareText> results;
    std::priority_queue<State*, std::vector<State*>, CompareState> queue;

    void push_left_states(PgfProbspace space, PgfText *cat, Result *res);

public:
    PgfExhaustiveGenerator(ref<PgfPGF> pgf, size_t depth,
                           PgfMarshaller *m, PgfUnmarshaller *u);
    virtual ~PgfExhaustiveGenerator();

    virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body);
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg);
    virtual PgfExpr elit(PgfLiteral lit);
    virtual PgfExpr emeta(PgfMetaId meta);
    virtual PgfExpr efun(PgfText *name);
    virtual PgfExpr evar(int index);
    virtual PgfExpr etyped(PgfExpr expr, PgfType typ);
    virtual PgfExpr eimplarg(PgfExpr expr);
    virtual PgfLiteral lint(size_t size, uintmax_t *v);
    virtual PgfLiteral lflt(double v);
    virtual PgfLiteral lstr(PgfText *v);
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs);
    virtual void free_ref(object x);

    virtual PgfExpr fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob);
    virtual void free_refs(PgfUnmarshaller *u);
};

#endif
