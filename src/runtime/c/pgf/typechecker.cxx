#include "data.h"
#include "typechecker.h"

PgfExpr PgfTypechecker::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    return u->eabs(btype,name,body);
}

PgfExpr PgfTypechecker::eapp(PgfExpr fun, PgfExpr arg)
{
    fun = m->match_expr(this, fun);

    size_t       fun_n_args = n_args;
    ref<PgfDTyp> fun_type   = type;

    arg = m->match_expr(this, arg);
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
    ref<PgfAbsFun> absfun =
        namespace_lookup(gr->abstract.funs, name);
    if (absfun == 0)
        throw pgf_error("Unknown function");

    type   = absfun->type;
    n_args = 0;
    return u->efun(name);
}

PgfExpr PgfTypechecker::evar(int index)
{
    return u->evar(index);
}

PgfExpr PgfTypechecker::etyped(PgfExpr expr, PgfType ty)
{
    expr = m->match_expr(this, expr);
    return u->etyped(expr,ty);
}

PgfExpr PgfTypechecker::eimplarg(PgfExpr expr)
{
    expr = m->match_expr(this, expr);
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
