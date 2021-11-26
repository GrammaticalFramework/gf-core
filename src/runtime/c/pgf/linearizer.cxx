#include "data.h"
#include "linearizer.h"

PgfLinearizer::~PgfLinearizer()
{
    while (first != NULL) {
        TreeNode *next = first->next;
        delete first;
        first = next;
    }
}

PgfExpr PgfLinearizer::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    return 0;
}

PgfExpr PgfLinearizer::eapp(PgfExpr fun, PgfExpr arg)
{
    TreeNode *args = this->args;
    this->args = NULL;
    TreeNode *node = (TreeNode*) m->match_expr(this, arg);
    node->next_arg = args;
    this->args = node;

    m->match_expr(this, fun);
    return 0;
}

PgfExpr PgfLinearizer::elit(PgfLiteral lit)
{
    return m->match_lit(this, lit);
}

PgfExpr PgfLinearizer::emeta(PgfMetaId meta)
{
    return 0;
}

PgfExpr PgfLinearizer::efun(PgfText *name)
{
    ref<PgfConcrLin> lin = namespace_lookup(concr->lins, name);
    TreeNode *node = new TreeNode(this, lin);
    return (PgfExpr) node;
}

PgfExpr PgfLinearizer::evar(int index)
{
    return 0;
}

PgfExpr PgfLinearizer::etyped(PgfExpr expr, PgfType ty)
{
    return m->match_expr(this, expr);
}

PgfExpr PgfLinearizer::eimplarg(PgfExpr expr)
{
    return m->match_expr(this, expr);
}

PgfLiteral PgfLinearizer::lint(size_t size, uintmax_t *v)
{
    return 0;
}

PgfLiteral PgfLinearizer::lflt(double v)
{
    return 0;
}

PgfLiteral PgfLinearizer::lstr(PgfText *v)
{
    return 0;
}

PgfType PgfLinearizer::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                             PgfText *cat,
                             size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

void PgfLinearizer::free_ref(object x)
{
}
