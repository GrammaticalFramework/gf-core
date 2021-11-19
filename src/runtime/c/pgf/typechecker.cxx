#include "data.h"
#include "typechecker.h"

PgfExpr PgfTypechecker::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    return u->eabs(btype,name,body);
}

PgfExpr PgfTypechecker::eapp(PgfExpr fun, PgfExpr arg)
{
    return u->eapp(fun, arg);
}

PgfExpr PgfTypechecker::elit(PgfLiteral lit)
{
    return u->elit(lit);
}

PgfExpr PgfTypechecker::emeta(PgfMetaId meta)
{
    return u->emeta(meta);
}

PgfExpr PgfTypechecker::efun(PgfText *name)
{
    return u->efun(name);
}

PgfExpr PgfTypechecker::evar(int index)
{
    return u->evar(index);
}

PgfExpr PgfTypechecker::etyped(PgfExpr expr, PgfType ty)
{
    return u->etyped(expr,ty);
}

PgfExpr PgfTypechecker::eimplarg(PgfExpr expr)
{
    return u->eimplarg(expr);
}

PgfLiteral PgfTypechecker::lint(size_t size, uintmax_t *v)
{
    return u->lint(size,v);
}

PgfLiteral PgfTypechecker::lflt(double v)
{
    return u->lflt(v);
}

PgfLiteral PgfTypechecker::lstr(PgfText *v)
{
    return u->lstr(v);
}

PgfType PgfTypechecker::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                             PgfText *cat,
                             size_t n_exprs, PgfExpr *exprs)
{
    return u->dtyp(n_hypos, hypos, cat, n_exprs, exprs);
}

void PgfTypechecker::free_ref(object x)
{
    u->free_ref(x);
}
