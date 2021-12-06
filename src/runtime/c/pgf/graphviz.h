#ifndef GRAPHVIZ_H
#define GRAPHVIZ_H

class PGF_INTERNAL_DECL PgfLinearizationGraphvizOutput : public PgfLinearizationOutputIface {
    struct ParseNode;

    struct ParseLevel {
        size_t n_nodes;
        ParseNode **nodes;
    };

    struct ParseNode {
        int id;
        ParseNode* parent;
        PgfText *fun;
        PgfText *label;

        ParseNode(ParseLevel *level, int id, ParseNode *parent, PgfText *fun, PgfText *label);
    };

	ParseNode* parent;
	size_t level;

    size_t n_internals;
    ParseLevel **internals;

    ParseLevel leaves;

    PgfText *meta;

    void generate_graphviz_level(PgfPrinter *printer, PgfGraphvizOptions* opts, ParseLevel *level);

public:
    PgfLinearizationGraphvizOutput();
    ~PgfLinearizationGraphvizOutput();

    PgfText *generate_graphviz(PgfGraphvizOptions* opts);

    virtual void symbol_token(PgfText *tok);
	virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void symbol_ne();
	virtual void symbol_bind();
	virtual void symbol_meta(PgfMetaId id);
};

class PGF_INTERNAL_DECL PgfAbstractGraphvizOutput : public PgfUnmarshaller {
    PgfAbstr *abstr;
    PgfMarshaller *m;
    PgfPrinter printer;
    PgfGraphvizOptions* opts;

    int id;
    size_t n_vars;

public:
    PgfAbstractGraphvizOutput(PgfAbstr *abstr, PgfGraphvizOptions* opts, PgfMarshaller *m);

    PgfText *generate_graphviz(PgfExpr expr);

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

#endif
