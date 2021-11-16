#ifndef EXPR_H_
#define EXPR_H_

struct PgfHypo;
struct PgfDTyp;

struct PGF_INTERNAL_DECL PgfLiteralStr {
    static const uint8_t tag = 0;

	PgfText val;
} ;

struct PGF_INTERNAL_DECL PgfLiteralInt {
    static const uint8_t tag = 1;

    size_t size;
	uintmax_t val[];
} ;

struct PGF_INTERNAL_DECL PgfLiteralFlt {
    static const uint8_t tag = 2;

	double val;
};

struct PGF_INTERNAL_DECL PgfHypo {
	PgfBindType bind_type;
	ref<PgfText> cid;
	ref<PgfDTyp> type;
};

struct PGF_INTERNAL_DECL PgfDTyp {
	ref<Vector<PgfHypo>> hypos;
    ref<Vector<PgfExpr>> exprs;
	PgfText name;
};

struct PGF_INTERNAL_DECL PgfExprAbs {
    static const uint8_t tag = 0;
    
	PgfBindType bind_type;
	PgfExpr body;
	PgfText name;
};

struct PGF_INTERNAL_DECL PgfExprApp {
    static const uint8_t tag = 1;

	PgfExpr fun;
	PgfExpr arg;
};

struct PGF_INTERNAL_DECL PgfExprLit {
    static const uint8_t tag = 2;

	PgfLiteral lit;
};

struct PGF_INTERNAL_DECL PgfExprMeta {
    static const uint8_t tag = 3;

	PgfMetaId id;
};

struct PGF_INTERNAL_DECL PgfExprFun {
    static const uint8_t tag = 4;

	PgfText name;
};

struct PGF_INTERNAL_DECL PgfExprVar {
    static const uint8_t tag = 5;
    
	int var;
};

struct PGF_INTERNAL_DECL PgfExprTyped {
    static const uint8_t tag = 6;

	PgfExpr expr;
	ref<PgfDTyp> type;
};

struct PGF_INTERNAL_DECL PgfExprImplArg {
    static const uint8_t tag = 7;

	PgfExpr expr;
};

// PgfPatt

typedef object PgfPatt;

struct PGF_INTERNAL_DECL PgfPattApp {
    static const uint8_t tag = 0;

	ref<PgfText> ctor;
    Vector<PgfPatt> args;
};

struct PGF_INTERNAL_DECL PgfPattVar {
    static const uint8_t tag = 1;

	PgfText name;
};

struct PGF_INTERNAL_DECL PgfPattAs {
    static const uint8_t tag = 2;

	PgfPatt patt;
	PgfText name;
};

struct PGF_INTERNAL_DECL PgfPattWild {
    static const uint8_t tag = 3;
};

struct PGF_INTERNAL_DECL PgfPattLit {
    static const uint8_t tag = 4;

	PgfLiteral lit;
};

struct PGF_INTERNAL_DECL PgfPattImplArg {
    static const uint8_t tag = 5;

	PgfPatt patt;
};

struct PGF_INTERNAL_DECL PgfPattTilde {
    static const uint8_t tag = 6;

	PgfExpr expr;
};

typedef float prob_t;

typedef struct {
	prob_t prob;
	PgfExpr expr;
} PgfExprProb;

struct PGF_INTERNAL_DECL PgfDBMarshaller : public PgfMarshaller {
    virtual object match_lit(PgfUnmarshaller *u, PgfLiteral l);
    virtual object match_expr(PgfUnmarshaller *u, PgfExpr e);
    virtual object match_type(PgfUnmarshaller *u, PgfType ty);
};

struct PGF_INTERNAL_DECL PgfDBUnmarshaller : public PgfUnmarshaller {
    PgfMarshaller *m;

    PgfDBUnmarshaller(PgfMarshaller *marshaller) { m = marshaller; }

    virtual PgfExpr eabs(PgfBindType bind_type, PgfText *name, PgfExpr body);
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg);
    virtual PgfExpr elit(PgfLiteral lit);
    virtual PgfExpr emeta(PgfMetaId meta_id);
    virtual PgfExpr efun(PgfText *name);
    virtual PgfExpr evar(int index);
    virtual PgfExpr etyped(PgfExpr expr, PgfType ty);
    virtual PgfExpr eimplarg(PgfExpr expr);
    virtual PgfLiteral lint(size_t size, uintmax_t *val);
    virtual PgfLiteral lflt(double val);
    virtual PgfLiteral lstr(PgfText *val);
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs);
    virtual void free_ref(object x);
};

struct PGF_INTERNAL_DECL PgfInternalMarshaller : public PgfMarshaller {
    virtual object match_lit(PgfUnmarshaller *u, PgfLiteral l);
    virtual object match_expr(PgfUnmarshaller *u, PgfExpr e);
    virtual object match_type(PgfUnmarshaller *u, PgfType ty);
};

typedef struct PgfBind {
    PgfBindType bind_type;
    struct PgfBind *next;
    PgfText var;
} PgfBind;

PGF_INTERNAL_DECL bool
pgf_is_ident_first(uint32_t ucs);

PGF_INTERNAL_DECL bool
pgf_is_ident_rest(uint32_t ucs);

class PGF_INTERNAL_DECL PgfExprParser {
    enum PGF_TOKEN_TAG {
        PGF_TOKEN_LPAR,
        PGF_TOKEN_RPAR,
        PGF_TOKEN_LCURLY,
        PGF_TOKEN_RCURLY,
        PGF_TOKEN_QUESTION,
        PGF_TOKEN_LAMBDA,
        PGF_TOKEN_RARROW,
        PGF_TOKEN_LTRIANGLE,
        PGF_TOKEN_RTRIANGLE,
        PGF_TOKEN_COMMA,
        PGF_TOKEN_COLON,
        PGF_TOKEN_SEMI,
        PGF_TOKEN_WILD,
        PGF_TOKEN_IDENT,
        PGF_TOKEN_INT,
        PGF_TOKEN_FLT,
        PGF_TOKEN_STR,
        PGF_TOKEN_UNKNOWN,
        PGF_TOKEN_EOF,
    };

    PgfUnmarshaller *u;
	PGF_TOKEN_TAG token_tag;
	PgfText *token_value;
    PgfText *inp;
    const char *token_pos, *pos;
    uint32_t ch;

    bool getc();
    void putc(uint32_t ch);

public:
    PgfExprParser(PgfText* input, PgfUnmarshaller *unmarshaller);
    ~PgfExprParser();

    bool str_char();
    void token();
    bool lookahead(int ch);

    PgfBind *parse_bind(PgfBind *next);
    PgfBind *parse_binds(PgfBind *next);

    PgfExpr parse_arg();
    PgfExpr parse_term();
    PgfExpr parse_expr();

    bool parse_hypos(size_t *n_hypos, PgfTypeHypo **hypos);
    PgfType parse_type();

    bool eof();

    const char *get_token_pos() { return token_pos; }
};

class PGF_INTERNAL_DECL PgfExprProbEstimator : public PgfUnmarshaller {
    PgfPGF *pgf;
    PgfMarshaller *m;
    prob_t prob;

public:
    PgfExprProbEstimator(PgfPGF *pgf, PgfMarshaller *marshaller) {
        this->pgf = pgf;
        this->m = marshaller;
        this->prob = 0;
    }

    virtual PgfExpr eabs(PgfBindType bind_type, PgfText *name, PgfExpr body);
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg);
    virtual PgfExpr elit(PgfLiteral lit);
    virtual PgfExpr emeta(PgfMetaId meta_id);
    virtual PgfExpr efun(PgfText *name);
    virtual PgfExpr evar(int index);
    virtual PgfExpr etyped(PgfExpr expr, PgfType ty);
    virtual PgfExpr eimplarg(PgfExpr expr);
    virtual PgfLiteral lint(size_t size, uintmax_t *val);
    virtual PgfLiteral lflt(double val);
    virtual PgfLiteral lstr(PgfText *val);
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs);
    virtual void free_ref(object x);

    prob_t get_prob() { return prob; };
};

/* The following functions release the memory in the database,
 * allocated for values of the corresponding types.
 */

PGF_INTERNAL_DECL
void pgf_literal_free(PgfLiteral literal);

PGF_INTERNAL_DECL
void pgf_expr_free(PgfExpr expr);

PGF_INTERNAL_DECL
void pgf_context_free(ref<Vector<PgfHypo>> hypos);

PGF_INTERNAL_DECL
void pgf_type_free(ref<PgfDTyp> dtyp);

PGF_INTERNAL_DECL
void pgf_patt_free(PgfPatt patt);

#endif /* EXPR_H_ */
