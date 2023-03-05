#ifndef GENERATOR_H
#define GENERATOR_H

class PGF_INTERNAL_DECL PgfRandomGenerator : public PgfUnmarshaller
{
    const static int VAR_PROB = 0.1;

    ref<PgfPGF> pgf;
    size_t depth;
    uint64_t *seed;
    prob_t prob;
    PgfMarshaller *m;
    PgfInternalMarshaller i_m;
    PgfUnmarshaller *u;

    struct Scope {
        Scope *next;
        PgfType type;
        PgfMarshaller *m;
        PgfBindType bind_type;
        PgfText var;
    };

    Scope *scope;
    size_t scope_len;

    prob_t rand() {
        *seed = *seed * 1103515245 + 12345;
        return (prob_t)((*seed/65536) % 32768)/32768;
    }

public:
    PgfRandomGenerator(ref<PgfPGF> pgf, size_t depth, uint64_t *seed,
                       PgfMarshaller *m, PgfUnmarshaller *u);
    prob_t getProb() { return prob; }
    ~PgfRandomGenerator();

    PgfExpr descend(PgfExpr expr, size_t n_hypos, PgfTypeHypo *hypos);

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
