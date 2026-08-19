#ifndef COMPUTE_H
#define COMPUTE_H

class PGF_INTERNAL_DECL PgfEvalExpr : public PgfUnmarshaller
{
    ref<PgfPGF> pgf;
    PgfMarshaller *m;
    PgfUnmarshaller *u;
    PgfExn *err;

    struct Value {
        Value *next;                // chain for garabage collection
    };

    struct VThunk : Value {
        PgfExpr e;
    };

    struct VApp : Value {
        ref<PgfConcrLin> lin;
        Value *args[];
    };

    struct VMeta : Value {
        PgfMetaId id;
        Value *args[];
    };

    struct VClosure : Value {
        PgfExpr e;
    };

    struct ExprNode {
        PgfExpr e;
        PgfExpr value;
        ExprNode *next;
    };

    ExprNode *stack;
    ExprNode *env;

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
                         PgfText *name,
                         size_t n_exprs, PgfExpr *exprs);
    virtual void free_ref(object x);

    PgfExpr force(ExprNode *node);
    PgfExpr apply(PgfExpr e);

public:
    PgfEvalExpr(ref<PgfPGF> pgf,
                PgfMarshaller *m, PgfUnmarshaller *u,
                ExprNode *env,
                PgfExn *err);
};

#endif // COMPUTE_H
