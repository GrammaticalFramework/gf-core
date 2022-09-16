#ifndef PARSER_H
#define PARSER_H

class PGF_INTERNAL_DECL PgfParser : public PgfPhraseScanner, public PgfExprEnum {
public:
    PgfParser(ref<PgfConcrLincat> start, PgfText *sentence);

	void space(size_t start, size_t end, PgfExn* err);
    void start_matches(size_t end, PgfExn* err);
    void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err);
	void end_matches(size_t end, PgfExn* err);

    PgfExpr fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob);

    virtual ~PgfParser();

private:
    class CFGCat;
    class State;
    class Item;
    class ItemConts;
    class Choice;
    class Production;

    class Result {
    public:
        virtual prob_t  prob() = 0;
        virtual PgfExpr expr(PgfUnmarshaller *u) = 0;
        virtual void    proceed(PgfParser *parser, PgfUnmarshaller *u) = 0;
    };

    class ResultExpr;
    class ResultMeta;

    class ResultComparator : std::less<Result*> {
    public:
        bool operator()(Result* &lhs, Result* &rhs) const 
        {
            return lhs->prob() > rhs->prob();
        }
    };

    ref<PgfConcrLincat> start;
    PgfText *sentence;

    size_t last_choice_id;
    
    State *before, *after, *fetch_state;
};

#endif
