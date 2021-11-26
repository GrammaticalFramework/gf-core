#ifndef LINEARIZER_H
#define LINEARIZER_H

class PGF_INTERNAL_DECL PgfLinearizer : public PgfUnmarshaller {
    ref<PgfConcr> concr;
    PgfMarshaller *m;

    struct TreeNode {
        TreeNode *next;
        TreeNode *next_arg;
        TreeNode *args;

        ref<PgfConcrLin> lin;

        TreeNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin) {
            this->next     = linearizer->root;
            this->next_arg = NULL;
            this->args     = linearizer->args;
            this->lin      = lin;

            if (linearizer->first == NULL)
                linearizer->first = this;

            linearizer->root = this;
        }
    };

    TreeNode *root;
    TreeNode *first;
    TreeNode *args;

public:
    PgfLinearizer(ref<PgfConcr> concr, PgfMarshaller *m) {
        this->concr = concr;
        this->m = m;
        this->root  = NULL;
        this->first = NULL;
        this->args  = NULL;
    };

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
