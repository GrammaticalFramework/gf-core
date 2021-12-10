#ifndef LINEARIZER_H
#define LINEARIZER_H

class PGF_INTERNAL_DECL PgfLinearizationOutput : public PgfLinearizationOutputIface {
    PgfPrinter printer;
    bool bind;
    bool nonexist;

public:
    PgfLinearizationOutput();

    PgfText *get_text();

	virtual void symbol_token(PgfText *tok);
	virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
	virtual void symbol_ne();
	virtual void symbol_bind();
};

class PGF_INTERNAL_DECL PgfLinearizer : public PgfUnmarshaller {
    // List of free variables in order reverse to the order of binding
    PgfPrinter printer;

    ref<PgfConcr> concr;
    PgfMarshaller *m;

    struct TreeNode {
        TreeNode *next;
        TreeNode *next_arg;
        TreeNode *args;

        int fid;

        size_t value;
        size_t var_count;
        size_t *var_values;

        size_t n_hoas_vars;
        PgfText **hoas_vars;

        TreeNode(PgfLinearizer *linearizer);
        virtual bool resolve(PgfLinearizer *linearizer) { return true; };
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat)=0;
        virtual void linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, PgfLParam *r);
        virtual void linearize_var(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r);
        virtual void linearize_syms(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, ref<Vector<PgfSymbol>> syms);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)=0;
        size_t eval_param(PgfLParam *param);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer)=0;
        virtual ~TreeNode() { free(var_values); free(hoas_vars); };
    };

    struct TreeLinNode : public TreeNode {
        ref<PgfConcrLin> lin;
        size_t lin_index;

        TreeLinNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
    };

    struct TreeLindefNode : public TreeNode {
        ref<PgfConcrLincat> lincat;
        size_t lin_index;
        PgfText *literal;

        TreeLindefNode(PgfLinearizer *linearizer, PgfText *lit);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, PgfLParam *r);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        ~TreeLindefNode() { free(literal); };
    };

    struct TreeLinrefNode : public TreeNode {
        size_t lin_index;

        TreeLinrefNode(PgfLinearizer *linearizer, TreeNode *root);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat) {};
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
    };

    struct TreeLitNode : public TreeNode {
        ref<PgfConcrLincat> lincat;
        PgfText *literal;

        TreeLitNode(PgfLinearizer *linearizer, ref<PgfConcrLincat> lincat, PgfText *lit);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        ~TreeLitNode() { free(literal); };
    };

    TreeNode *prev;
    TreeNode *next;
    TreeNode *args;

    enum CapitState { CAPIT_NONE, CAPIT_FIRST, CAPIT_ALL };

    CapitState capit;

    struct BracketStack {
        BracketStack *next;
        bool begin;
        int fid;
        PgfText *cat;
        PgfText *field;
        PgfText *fun;

        void flush(PgfLinearizationOutputIface *out);
    };

    struct PreStack {
        PreStack *next;
        TreeNode *node;
        ref<PgfSymbolKP> sym_kp;
        bool bind;
        CapitState capit;
        BracketStack *bracket_stack;
    };

    PreStack *pre_stack;
    void flush_pre_stack(PgfLinearizationOutputIface *out, PgfText *token);

    PgfText *wild;

public:
    PgfLinearizer(PgfPrintContext *ctxt, ref<PgfConcr> concr, PgfMarshaller *m);

    bool resolve();
    void reverse_and_label(bool add_linref);
    void linearize(PgfLinearizationOutputIface *out, size_t lindex) {
        prev->linearize(out, this, lindex);
        flush_pre_stack(out, NULL);
    }
    ref<PgfConcrLincat> get_lincat() {
        return prev->get_lincat(this);
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
