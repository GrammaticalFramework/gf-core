#ifndef LINEARIZER_H
#define LINEARIZER_H

class PGF_INTERNAL_DECL PgfLinearizationOutput : public PgfLinearizationOutputIface {
    PgfPrinter printer;
    bool bind;
    bool capit;
    bool allcapit;

public:
    PgfLinearizationOutput();

    PgfText *get_text() { return printer.get_text(); };
    
	virtual void symbol_token(PgfText *tok);
	virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void symbol_ne();
	virtual void symbol_bind();
	virtual void symbol_capit();
	virtual void symbol_allcapit();
	virtual void symbol_meta(PgfMetaId id);
};

class PGF_INTERNAL_DECL PgfLinearizer : public PgfUnmarshaller {
    ref<PgfConcr> concr;
    PgfMarshaller *m;

    struct TreeNode {
        TreeNode *next;
        TreeNode *next_arg;
        TreeNode *args;

        ref<PgfConcrLin> lin;
        size_t lin_index;

        size_t value;
        size_t var_count;
        size_t *var_values;

        TreeNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin);
        size_t eval_param(PgfLParam *param);
    };

    TreeNode *root;
    TreeNode *first;
    TreeNode *args;

    void linearize(PgfLinearizationOutputIface *out, TreeNode *node, ref<Vector<PgfSymbol>> syms);
    void linearize(PgfLinearizationOutputIface *out, TreeNode *node, size_t lindex);

public:
    PgfLinearizer(ref<PgfConcr> concr, PgfMarshaller *m);

    bool resolve();
    void linearize(PgfLinearizationOutputIface *out) {
        linearize(out, root, 0);
    }

    ~PgfLinearizer();

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

#endif
