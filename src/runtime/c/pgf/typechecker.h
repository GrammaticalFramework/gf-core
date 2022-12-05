#ifndef TYPECHECKER_H
#define TYPECHECKER_H

class PGF_INTERNAL_DECL PgfTypechecker : public PgfUnmarshaller {
    ref<PgfPGF> gr;
    ref<PgfDTyp> type;
    size_t n_args;
    PgfMarshaller *m;
    PgfUnmarshaller *u;

public:
    PgfTypechecker(ref<PgfPGF> gr, PgfMarshaller *m, PgfUnmarshaller *u) {
        this->gr = gr;
        this->m  = m;
        this->u  = u;
    };

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
