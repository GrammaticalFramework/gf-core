#include <math.h>
#include "data.h"
#include "generator.h"

PgfRandomGenerator::PgfRandomGenerator(ref<PgfPGF> pgf,
                                       size_t depth, uint64_t *seed,
                                       PgfMarshaller *m, PgfUnmarshaller *u)
{
    this->pgf  = pgf;
    this->depth = depth;
    this->seed = seed;
    this->m = m;
    this->u = u;
    this->prob  = 0;
    this->scope = NULL;
    this->scope_len = 0;
}

PgfRandomGenerator::~PgfRandomGenerator()
{
}

PgfExpr PgfRandomGenerator::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    body = m->match_expr(this, body);
    return u->eabs(btype, name, body);
}

PgfExpr PgfRandomGenerator::eapp(PgfExpr fun, PgfExpr arg)
{
    fun = m->match_expr(this, fun);
    arg = m->match_expr(this, arg);
    return u->eapp(fun, arg);
}

PgfExpr PgfRandomGenerator::elit(PgfLiteral lit)
{
    lit = m->match_lit(this, lit);
    return u->elit(lit);
}

PgfExpr PgfRandomGenerator::emeta(PgfMetaId meta)
{
    return 0;
}

PgfExpr PgfRandomGenerator::efun(PgfText *name)
{
    return u->efun(name);
}

PgfExpr PgfRandomGenerator::evar(int index)
{
    return u->evar(index);
}

PgfExpr PgfRandomGenerator::etyped(PgfExpr expr, PgfType typ)
{
    expr = m->match_expr(this, expr);
    typ  = m->match_type(u, typ);
    return u->etyped(expr, typ);
}

PgfExpr PgfRandomGenerator::eimplarg(PgfExpr expr)
{
    expr = m->match_expr(this, expr);
    return u->eimplarg(expr);
}

PgfLiteral PgfRandomGenerator::lint(size_t size, uintmax_t *v)
{
    return u->lint(size,v);
}

PgfLiteral PgfRandomGenerator::lflt(double v)
{
    return u->lflt(v);
}

PgfLiteral PgfRandomGenerator::lstr(PgfText *v)
{
    return u->lstr(v);
}

class PGF_INTERNAL_DECL PgfVarGenerator : public PgfDBUnmarshaller
{
    size_t index;
    PgfText *cat;
    PgfRandomGenerator *r_gen;

public:
    PgfVarGenerator(PgfRandomGenerator *rgen,
                      int index,
                      PgfText *cat, size_t n_exprs, PgfExpr *exprs);

    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs);
};

PgfVarGenerator::PgfVarGenerator(PgfRandomGenerator *r_gen,
                                 int index,
                                 PgfText *cat, size_t n_exprs, PgfExpr *exprs)
  : PgfDBUnmarshaller(NULL)
{
    this->r_gen = r_gen;
    this->index = index;
    this->cat = cat;
}

PgfType PgfVarGenerator::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                              PgfText *cat,
                              size_t n_exprs, PgfExpr *exprs)
{
    if (textcmp(cat, this->cat) == 0) {
        PgfExpr expr = r_gen->evar(index);
        return r_gen->descend(expr, n_hypos, hypos);
    }
    return 0;
}

PgfType PgfRandomGenerator::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                 PgfText *cat,
                                 size_t n_exprs, PgfExpr *exprs)
{
    Scope *entry_scope = scope;
    for (size_t i = 0; i < n_hypos; i++) {
        size_t buf_size = 16;
        Scope *new_scope = (Scope *) malloc(sizeof(Scope)+buf_size);
        new_scope->type = hypos[i].type;
        new_scope->m    = m;
        new_scope->bind_type = hypos[i].bind_type;
        new_scope->next = scope;

        size_t out;
again:  {
            new_scope->var.size =
                snprintf(new_scope->var.text, buf_size, "v%zu", scope_len+1);
            if (new_scope->var.size >= buf_size) {
                buf_size = new_scope->var.size+1;
                new_scope = (Scope*)
                    realloc(new_scope,sizeof(Scope)+buf_size);
                goto again;
            }
        }

        scope = new_scope;
        scope_len++;
    }

    PgfExpr expr = 0;
    PgfExpr var_expr = 0;

    int index = 0;
    Scope *sc = scope;
    auto tmp = m;
    while (sc != NULL) {
        m = sc->m;
        PgfVarGenerator v_gen(this, index, cat, n_exprs, exprs);
        expr = m->match_type(&v_gen, sc->type);
        if (expr != 0) {
            if (rand() < VAR_PROB) {
                prob += -log(VAR_PROB);
                break;
            } else {
                prob += -log(1-VAR_PROB);
                if (var_expr != 0)
                    u->free_ref(var_expr);
                var_expr = expr;
                expr = 0;
            }
        }
        sc = sc->next;
        index++;
    }
    m = tmp;

    if (expr == 0) {
        if (strcmp(cat->text, "Int") == 0) {
            uintmax_t value = 999;
            PgfExpr lint = u->lint(1,&value);
            expr = u->elit(lint);
            u->free_ref(lint);
        } else if (strcmp(cat->text, "Float") == 0) {
            PgfExpr lflt = u->lflt(3.14);
            expr = u->elit(lflt);
            u->free_ref(lflt);
        } else if (strcmp(cat->text, "String") == 0) {
            PgfText *value = (PgfText *) alloca(sizeof(PgfText)+4);
            value->size = 3;
            strcpy(value->text, "Foo");

            PgfExpr lstr = u->lstr(value);
            expr = u->elit(lstr);
            u->free_ref(lstr);
        } else {
            prob_t rand_value = rand();

            ref<PgfAbsFun> fun = probspace_random(pgf->abstract.funs_by_cat, cat, rand_value);
            if (fun == 0) {
                if (var_expr != 0) {
                    prob += -log(VAR_PROB/(1-VAR_PROB));
                    expr = var_expr;
                }
            } else {
                if (depth > 0 || fun->type->hypos->len > 0) {
                    prob += fun->prob;
                    expr = u->efun(&fun->name);

                    ref<Vector<PgfHypo>> hypos = fun->type->hypos;
                    PgfTypeHypo *t_hypos = (PgfTypeHypo *)
                        alloca(hypos->len * sizeof(PgfTypeHypo));
                    for (size_t i = 0; i < hypos->len; i++) {
                        t_hypos[i].bind_type = hypos->data[i].bind_type;
                        t_hypos[i].cid = &(*hypos->data[i].cid);
                        t_hypos[i].type = hypos->data[i].type.as_object();
                    }
                    auto tmp = this->m;
                    this->m = &i_m;
                    expr = descend(expr, hypos->len, t_hypos);
                    this->m = tmp;
                }
            }
        }
    }

    if (expr != 0) {
        while (scope != entry_scope) {
            PgfExpr abs_expr = u->eabs(scope->bind_type, &scope->var, expr);
            u->free_ref(expr);
            expr = abs_expr;
            Scope *next = scope->next;
            free(scope);
            scope = next;
        }
    }

    return expr;
}

void PgfRandomGenerator::free_ref(object x)
{
    u->free_ref(x);
}

PgfExpr PgfRandomGenerator::descend(PgfExpr expr,
                                    size_t n_hypos, PgfTypeHypo *hypos)
{
    depth--;
    for (size_t i = 0; i < n_hypos; i++) {
        PgfExpr arg = m->match_type(this, hypos[i].type);
        if (arg == 0) {
            u->free_ref(expr);
            return 0;
        }

        if (hypos[i].bind_type == PGF_BIND_TYPE_IMPLICIT) {
            PgfExpr impl_app = u->eimplarg(arg);
            u->free_ref(arg);
            arg = impl_app;
        }
        PgfExpr app = u->eapp(expr, arg);
        u->free_ref(arg);
        u->free_ref(expr);
        expr = app;
    }
    depth++;

    return expr;
}
