#ifndef PARSER_H
#define PARSER_H

class PGF_INTERNAL_DECL PgfParser : public PgfPhraseScanner, public PgfExprEnum {
public:
    PgfParser(ref<PgfConcr> concr, ref<PgfConcrLincat> start, PgfText *sentence, PgfMarshaller *m);

	void space(size_t start, size_t end, PgfExn* err);
    void start_matches(size_t end, PgfExn* err);
    void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err);
	void end_matches(size_t end, PgfExn* err);

    void prepare();
    PgfExpr fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob);

    virtual ~PgfParser();

private:
    class CFGCat;
    class State;
    class Choice;
    class Production;

    class ParseItemConts;

    class Item {
    public:
        prob_t get_prob() { return inside_prob + outside_prob; };

        virtual bool    proceed(PgfParser *parser, PgfUnmarshaller *u) = 0;
        virtual void    combine(PgfParser *parser, ParseItemConts *conts, PgfExpr expr, prob_t inside_prob, PgfUnmarshaller *u) = 0;
        virtual void    print1(PgfPrinter *printer, State *state, PgfMarshaller *m) = 0;
        virtual void    print2(PgfPrinter *printer, State *state, int x, PgfMarshaller *m) = 0;
        virtual PgfExpr get_expr(PgfUnmarshaller *u) = 0;

        void trace(State *state, PgfMarshaller *m);

    protected:
        prob_t inside_prob;
        prob_t outside_prob;
    };

    class ParseItem;
    class ExprItem;
    class MetaItem;

    ref<PgfConcr> concr;
    ref<PgfConcrLincat> start;
    PgfText *sentence;

    size_t last_choice_id;

    State *before, *after, *fetch_state;

    PgfMarshaller *m;
};

#endif
