#include "data.h"
#include "compute.h"

PgfExpr PgfEvalExpr::eabs(PgfBindType bind_type, PgfText *name, PgfExpr body)
{
    if (stack != NULL) {
        ExprNode *tmp;
        tmp = stack->next;
        stack->next = env;
        env = stack;
        stack = tmp;
        return m->match_expr(this, body);
    } else {
        return 0;
    }
}

PgfExpr PgfEvalExpr::eapp(PgfExpr fun, PgfExpr arg)
{
    ExprNode node;
    node.e = arg;
    node.value = 0;
    node.next = stack;
    stack = &node;
    PgfExpr e = m->match_expr(this, fun);
    if (node.value != 0) {
        //u->free_ref(node.value);
    }
    return e;
}

PgfExpr PgfEvalExpr::elit(PgfLiteral lit)
{
    lit = m->match_lit(this, lit);
    PgfExpr e = u->elit(lit);
    u->free_ref(lit);
    return e;
}

PgfExpr PgfEvalExpr::emeta(PgfMetaId meta_id)
{
    return apply(u->emeta(meta_id));
}

PgfExpr PgfEvalExpr::efun(PgfText *name)
{
    return apply(u->efun(name));
}

PgfExpr PgfEvalExpr::evar(int index)
{
    ExprNode *node = env;
    while (index > 0) {
        if (node == NULL) {
            err->type = PGF_EXN_PGF_ERROR;
            err->msg  = strdup("Unbounded variable");
            return 0;
        }
        node = node->next;
    }

    if (node == NULL) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup("Unbounded variable");
        return 0;
    }
    return apply(force(node));
}

PgfExpr PgfEvalExpr::etyped(PgfExpr expr, PgfType ty)
{
    return m->match_expr(this, expr);
}

PgfExpr PgfEvalExpr::eimplarg(PgfExpr expr)
{
    return m->match_expr(this, expr);
}

PgfLiteral PgfEvalExpr::lint(size_t size, uintmax_t *val)
{
    return u->lint(size, val);
}

PgfLiteral PgfEvalExpr::lflt(double val)
{
    return u->lflt(val);
}

PgfLiteral PgfEvalExpr::lstr(PgfText *val)
{
    return u->lstr(val);
}

PgfType PgfEvalExpr::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                          PgfText *name,
                          size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

void PgfEvalExpr::free_ref(object x)
{
    return u->free_ref(x);
}

PgfExpr PgfEvalExpr::force(ExprNode *node)
{
    if (node->value == 0) {
        PgfEvalExpr eval(pgf,m,u,env,err);
        node->value = m->match_expr(&eval, node->e);
    }
    return node->value;
}

PgfExpr PgfEvalExpr::apply(PgfExpr e)
{
    while (stack != NULL) {
        PgfExpr arg = force(stack);
        if (arg == 0) {
            u->free_ref(e);
            return 0;
        }

        PgfExpr app = u->eapp(e,arg);
        u->free_ref(e);
        e = app;
        stack = stack->next;
    }
    return e;
}

PgfEvalExpr::PgfEvalExpr(ref<PgfPGF> pgf,
                         PgfMarshaller *m, PgfUnmarshaller *u,
                         ExprNode *env,
                         PgfExn *err)
{
    this->m = m;
    this->u = u;
    this->stack = NULL;
    this->env   = env;
}
