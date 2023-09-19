#ifndef LR_TABLE_H
#define LR_TABLE_H

#include "md5.h"

class PGF_INTERNAL_DECL PgfLRTableMaker
{
    struct CCat;
    struct Production;
    struct Item;
    struct State;

    struct CompareItem;
    static const CompareItem compare_item;

    typedef std::pair<ref<PgfText>,size_t> Key0;

    struct PGF_INTERNAL_DECL CompareKey0 : std::less<Key0> {
        bool operator() (const Key0& k1, const Key0& k2) const {
            int cmp = textcmp(k1.first,k2.first);
            if (cmp < 0)
                return true;
            else if (cmp > 0)
                return false;

            return (k1.second < k2.second);
        }
    };

    typedef std::pair<ref<PgfConcrLincat>,size_t> Key1;

    struct PGF_INTERNAL_DECL CompareKey1 : std::less<Key1> {
        bool operator() (const Key1& k1, const Key1& k2) const {
            if (k1.first < k2.first)
                return true;
            else if (k1.first > k2.first)
                return false;

            return (k1.second < k2.second);
        }
    };

    typedef std::pair<CCat*,size_t> Key2;

    struct PGF_INTERNAL_DECL CompareKey2 : std::less<Key2> {
        bool operator() (const Key2& k1, const Key2& k2) const {
            if (k1.first < k2.first)
                return true;
            else if (k1.first > k2.first)
                return false;

            return (k1.second < k2.second);
        }
    };

    ref<PgfAbstr> abstr;
    ref<PgfConcr> concr;

    size_t ccat_id;

    std::queue<State*> todo;
    std::map<MD5Digest,State*> states;
    std::map<Key0,CCat*,CompareKey0> ccats1;
    std::map<Key2,CCat*,CompareKey2> ccats2;

    // The Threefold Way of building an automaton
    typedef enum { INIT, PROBE, REPEAT } Fold;

    void process(State *state, Fold fold, Item *item);
    void symbol(State *state, Fold fold, Item *item, PgfSymbol sym);

    template<class T>
    void predict(State *state, Fold fold, Item *item, T cat, bool exact,
                 ref<Vector<PgfVariableRange>> vars, PgfLParam *r);
    void predict(State *state, Fold fold, Item *item, ref<PgfText> cat, bool exact, size_t lin_idx);
    void predict(State *state, Fold fold, Item *item, CCat *ccat, bool exact, size_t lin_idx);
    void predict(ref<PgfAbsFun> absfun, CCat *ccat);
    void complete(State *state, Fold fold, Item *item);

    void print_production(CCat *ccat, Production *prod);
    void print_item(Item *item);

public:
    PgfLRTableMaker(ref<PgfAbstr> abstr, ref<PgfConcr> concr);
    ref<PgfLRTable> make();
    ~PgfLRTableMaker();
};

class PgfPrinter;

class PGF_INTERNAL_DECL PgfParser : public PgfPhraseScanner, public PgfExprEnum
{
    ref<PgfConcr> concr;
    PgfText *sentence;
    PgfMarshaller *m;
    PgfUnmarshaller *u;

    struct Choice;
    struct Production;
    struct StackNode;
    struct Stage;
    struct ExprState;
    struct ExprInstance;
    struct CompareExprState : std::less<ExprState*> {
        bool operator() (const ExprState *state1, const ExprState *state2) const;
    };

    Stage *before, *after, *ahead;
    std::priority_queue<ExprState*, std::vector<ExprState*>, CompareExprState> queue;
    int last_fid;

    Choice *top_choice;
    size_t top_choice_index;

    void shift(StackNode *parent, ref<PgfConcrLincat> lincat, size_t r, Production *prod,
               Stage *before, Stage *after);
    void reduce(StackNode *parent, ref<PgfConcrLin> lin, ref<PgfLRReduce> red,
                size_t n, std::vector<Choice*> &args,
                Stage *before, Stage *after);
    void complete(StackNode *parent, ref<PgfConcrLincat> lincat, size_t seq_index,
                  size_t n, std::vector<Choice*> &args);
    void reduce_all(StackNode *state);
    void print_prod(Choice *choice, Production *prod);
    void print_transition(StackNode *source, StackNode *target, Stage *stage);

    typedef std::map<std::pair<Choice*,Choice*>,Choice*> intersection_map;

    Choice *intersect_choice(Choice *choice1, Choice *choice2, intersection_map &im);

    void print_expr_state_before(PgfPrinter *printer, ExprState *state);
    void print_expr_state_after(PgfPrinter *printer, ExprState *state);
    void print_expr_state(ExprState *state);

    void predict_expr_states(Choice *choice, prob_t outside_prob);
    bool process_expr_state(ExprState *state);
    void complete_expr_state(ExprState *state);
    void combine_expr_state(ExprState *state, ExprInstance &inst);
    void release_expr_state(ExprState *state);

public:
    PgfParser(ref<PgfConcr> concr, ref<PgfConcrLincat> start, PgfText *sentence, PgfMarshaller *m, PgfUnmarshaller *u);

	virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err);
    virtual void start_matches(PgfTextSpot *end, PgfExn* err);
    virtual void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err);
	virtual void end_matches(PgfTextSpot *end, PgfExn* err);

    void prepare();

    PgfExpr fetch(PgfDB *db, prob_t *prob);
};
#endif
