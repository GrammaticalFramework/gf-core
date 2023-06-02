#ifndef LR_TABLE_H
#define LR_TABLE_H

#include "md5.h"

class PGF_INTERNAL_DECL PgfLRTableMaker
{
    struct State;
    struct Item;
    struct Predictions;

    struct CompareItem;
    static const CompareItem compare_item;

    typedef std::pair<ref<PgfText>,size_t> Key;

    struct PGF_INTERNAL_DECL CompareKey : std::less<Key> {
        bool operator() (const Key& k1, const Key& k2) const {
            int cmp = textcmp(k1.first,k2.first);
            if (cmp < 0)
                return true;
            else if (cmp > 0)
                return false;

            return (k1.second < k2.second);
        }
    };

    ref<PgfAbstr> abstr;
    ref<PgfConcr> concr;

    std::vector<State*> todo;
    std::map<MD5Digest,State*> states;
    std::map<Key,Predictions*,CompareKey> predictions;
    std::map<Predictions*,State*> continuations;
    std::vector<PgfLRReduce> *reductions;

    void process(Item *item);
    void symbol(Item *item, PgfSymbol sym);
    void predict(Item *item, ref<PgfText> cat,
                 ref<Vector<PgfVariableRange>> vars, PgfLParam *r);
    void predict(Item *item, ref<PgfText> cat, size_t r);
    void predict(ref<PgfAbsFun> absfun, Predictions *preds);
    void complete(Item *item);

    static void print_item(Item *item);

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
    void reduce(StackNode *parent, ref<PgfConcrLin> lin, size_t seq_index,
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
