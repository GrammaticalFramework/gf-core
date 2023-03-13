#ifndef GENERATOR_H
#define GENERATOR_H

struct PGF_INTERNAL_DECL Scope {
    constexpr static prob_t VAR_PROB = 0.1;

    Scope *next;
    PgfType type;
    PgfMarshaller *m;
    PgfBindType bind_type;
    PgfText var;
};

class PGF_INTERNAL_DECL PgfRandomGenerator : public PgfUnmarshaller
{
    ref<PgfPGF> pgf;
    size_t depth;
    uint64_t *seed;
    prob_t prob;
    PgfMarshaller *m;
    PgfInternalMarshaller i_m;
    PgfUnmarshaller *u;

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
    PgfInternalMarshaller i_m;
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
                     Scope *scope, PgfExpr expr, prob_t prob,
                     PgfUnmarshaller *u);
        void complete(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u);
    };

    struct Goal {
        ref<PgfText> cat;
        Scope *scope;
        size_t scope_len;

        Goal(ref<PgfText> cat) {
            this->cat       = cat;
            this->scope     = NULL;
            this->scope_len = 0;
        }

        Goal(ref<PgfText> cat, Goal &g) {
            this->cat       = cat;
            this->scope     = g.scope;
            this->scope_len = g.scope_len;
        }

        Goal(Goal &g) {
            this->cat       = g.cat;
            this->scope     = g.scope;
            this->scope_len = g.scope_len;
        }
    };

    struct Result : Goal {
        size_t ref_count;
        std::vector<State1*> states;
        std::vector<std::pair<PgfExpr,prob_t>> exprs;

        Result(ref<PgfText> cat) : Goal(cat) {
            this->ref_count = 0;
        }

        Result(ref<PgfText> cat, Goal &g) : Goal(cat,g) {
            this->ref_count = 0;
        }

        Result(Goal &g) : Goal(g) {
            this->ref_count = 0;
        }
    };

    class CompareState : public std::less<State*> {
    public:
        bool operator() (const State* s1, const State* s2) const {
            return s1->prob > s2->prob;
        }
    };

    struct CompareGoal : public std::less<Goal> {
        bool operator() (const Goal &g1, const Goal &g2) const {
            int cmp = textcmp(g1.cat, g2.cat);
            if (cmp < 0)
                return true;
            else if (cmp > 0)
                return false;
            else
                return (g1.scope < g2.scope);
        }
    };

    struct Result2Goal {
        Goal &operator()(Result *res) {
            return *res;
        }
    };

    std::_Rb_tree<Goal&, Result*, Result2Goal, CompareGoal> results;
    std::priority_queue<State*, std::vector<State*>, CompareState> queue;
    std::vector<Scope*> scopes;

    void push_left_states(PgfProbspace space, PgfText *cat, Result *res, prob_t outside_prob);

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
