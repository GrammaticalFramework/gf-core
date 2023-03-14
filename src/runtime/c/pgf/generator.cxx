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
            if (rand() < Scope::VAR_PROB) {
                prob += -log(Scope::VAR_PROB);
                break;
            } else {
                prob += -log(1-Scope::VAR_PROB);
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
            PgfLiteral lint = u->lint(1,&value);
            expr = u->elit(lint);
            u->free_ref(lint);
        } else if (strcmp(cat->text, "Float") == 0) {
            PgfLiteral lflt = u->lflt(3.14);
            expr = u->elit(lflt);
            u->free_ref(lflt);
        } else if (strcmp(cat->text, "String") == 0) {
            PgfText *value = (PgfText *) alloca(sizeof(PgfText)+4);
            value->size = 3;
            strcpy(value->text, "Foo");

            PgfLiteral lstr = u->lstr(value);
            expr = u->elit(lstr);
            u->free_ref(lstr);
        } else {
            prob_t rand_value = rand();

            ref<PgfAbsFun> fun = probspace_random(pgf->abstract.funs_by_cat, cat, rand_value);
            if (fun == 0) {
                if (var_expr != 0) {
                    prob += -log(Scope::VAR_PROB/(1-Scope::VAR_PROB));
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






PgfExhaustiveGenerator::PgfExhaustiveGenerator(ref<PgfPGF> pgf,
                                               size_t depth,
                                               PgfMarshaller *m, PgfUnmarshaller *u)
{
    this->pgf = pgf;
    this->depth = depth;
    this->m = m;
    this->top_res = NULL;
    this->top_res_index = 0;
    
    PgfText *text_Int = string2text("Int");
    ref<PgfAbsCat> cat_Int =
        namespace_lookup(pgf->abstract.cats, text_Int);
    free(text_Int);
    if (cat_Int != 0) {
        uintmax_t value = 999;
        PgfLiteral lint = u->lint(1,&value);
        PgfExpr expr = u->elit(lint);
        u->free_ref(lint);
        Goal g(ref<PgfText>::from_ptr(&cat_Int->name),NULL);
        Result *res = new Result();
        res->exprs.push_back(std::pair<PgfExpr,prob_t>(expr,0));
        results[g] = res;
    }

    PgfText *text_Float = string2text("Float");
    ref<PgfAbsCat> cat_Float =
        namespace_lookup(pgf->abstract.cats, text_Float);
    free(text_Float);
    if (cat_Float != 0) {
        PgfLiteral lflt = u->lflt(3.14);
        PgfExpr expr = u->elit(lflt);
        u->free_ref(lflt);
        Goal g(ref<PgfText>::from_ptr(&cat_Float->name),NULL);
        Result *res = new Result();
        res->exprs.push_back(std::pair<PgfExpr,prob_t>(expr,0));
        results[g] = res;
    }

    PgfText *text_String = string2text("String");
    ref<PgfAbsCat> cat_String =
        namespace_lookup(pgf->abstract.cats, text_String);
    free(text_String);
    if (cat_String != 0) {
        PgfText *value = (PgfText *) alloca(sizeof(PgfText)+4);
        value->size = 3;
        strcpy(value->text, "Foo");

        PgfLiteral lstr = u->lstr(value);
        PgfExpr expr = u->elit(lstr);
        u->free_ref(lstr);
        Goal g(ref<PgfText>::from_ptr(&cat_String->name),NULL);
        Result *res = new Result();
        res->exprs.push_back(std::pair<PgfExpr,prob_t>(expr,0));
        results[g] = res;
    }
}

PgfExpr PgfExhaustiveGenerator::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::eapp(PgfExpr fun, PgfExpr arg)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::elit(PgfLiteral lit)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::emeta(PgfMetaId meta)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::efun(PgfText *name)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::evar(int index)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::etyped(PgfExpr expr, PgfType typ)
{
    return 0;
}

PgfExpr PgfExhaustiveGenerator::eimplarg(PgfExpr expr)
{
    return 0;
}

PgfLiteral PgfExhaustiveGenerator::lint(size_t size, uintmax_t *v)
{
    return 0;
}

PgfLiteral PgfExhaustiveGenerator::lflt(double v)
{
    return 0;
}

PgfLiteral PgfExhaustiveGenerator::lstr(PgfText *v)
{
    return 0;
}

void PgfExhaustiveGenerator::push_left_states(PgfProbspace space, PgfText *cat, Result *res, prob_t outside_prob)
{
    while (space != 0) {
        int cmp = textcmp(cat,&(*space->value.cat));
        if (cmp < 0) {
            space = space->left;
        } else if (cmp > 0) {
            space = space->right;
        } else {
            if (space->value.is_result()) {
                res->ref_count++;

                State0 *state = new State0();
                state->res   = res;
                state->prob  = outside_prob +
                               space->value.fun->prob;
                state->space = space;
                queue.push(state);
            } else {
                push_left_states(space->right, cat, res, outside_prob);
            }
            space = space->left;
        }
    }
}

PgfType PgfExhaustiveGenerator::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                     PgfText *cat,
                                     size_t n_exprs, PgfExpr *exprs)
{
    ref<PgfAbsCat> abscat =
        namespace_lookup(pgf->abstract.cats, cat);
    if (abscat == 0)
        return 0;

    Goal g(ref<PgfText>::from_ptr(&abscat->name), NULL);
    Result *&res = results[g];
    if (res == NULL) {
        res = new Result();
    }
    top_res = res;

    push_left_states(pgf->abstract.funs_by_cat, cat, top_res, 0);
    return 0;
}

void PgfExhaustiveGenerator::free_ref(object x)
{
}

void PgfExhaustiveGenerator::State::free_refs(PgfUnmarshaller *u)
{
}

void PgfExhaustiveGenerator::State::release(State *state, PgfUnmarshaller *u)
{
    state->res->ref_count--;
    if (state->res->ref_count == 0) {
        while (!state->res->states.empty()) {
            State1 *parent = state->res->states.back();
            u->free_ref(parent->expr);
            State::release(parent,u);
            state->res->states.pop_back();
        }
        state->res->states.shrink_to_fit();
    }

    delete state;
}

bool PgfExhaustiveGenerator::State0::process(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u)
{
    ref<PgfAbsFun> fun = space->value.fun;
    prob_t outside_prob = this->prob-fun->prob;

    gen->push_left_states(space->right, &(*space->value.cat), res, outside_prob);

    PgfExpr expr = u->efun(&fun->name);

    res->ref_count++;

    State1 *state = new State1();
    state->res    = res;
    state->prob   = prob;
    state->type   = fun->type;
    state->n_args = 0;
    state->expr   = expr;

    if (state->process(gen, u)) {
        State::release(state,u);
    }

    return true;
}

bool PgfExhaustiveGenerator::State1::process(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u)
{
    if (n_args >= type->hypos->len) {
        complete(gen, u);
        return true;
    }

    Scope *scope = res->scope;
    size_t scope_len = res->scope_len;
    ref<PgfDTyp> arg_type = vector_elem(type->hypos, n_args)->type;
    for (size_t i = 0; i < arg_type->hypos->len; i++) {
        ref<PgfHypo> hypo = vector_elem(arg_type->hypos, i);

        size_t buf_size = 16;
        Scope *new_scope = (Scope *) malloc(sizeof(Scope)+buf_size);
        new_scope->next = scope;
        new_scope->type = hypo->type.as_object();
        new_scope->m    = &gen->i_m;
        new_scope->bind_type = hypo->bind_type;

        size_t out;
again:  {
            new_scope->var.size =
                snprintf(new_scope->var.text, buf_size, "v%zu", res->scope_len+1);
            if (new_scope->var.size >= buf_size) {
                buf_size = new_scope->var.size+1;
                new_scope = (Scope*)
                    realloc(new_scope,sizeof(Scope)+buf_size);
                goto again;
            }
        }

        gen->scopes.push_back(new_scope);
        scope = new_scope;
        scope_len++;
    }

    Goal g(ref<PgfText>::from_ptr(&arg_type->name), scope);
    Result *&arg_res = gen->results[g];
    Result *tmp      = arg_res;
    if (arg_res == NULL) {
        arg_res = new Result(scope, scope_len);
    }
    arg_res->states.push_back(this);

    if (tmp == NULL) {
        // predict local variables
        size_t index = 0;
        Scope *s = scope;
        prob_t outside_prob = this->prob;
        while (s != NULL) {
            ref<PgfDTyp> type = s->type;
            if (textcmp(&type->name, g.first) == 0) {
                State1 *var_state = new State1();
                var_state->res    = arg_res;
                var_state->prob   = outside_prob - log(Scope::VAR_PROB);
                var_state->type   = type;
                var_state->n_args = 0;
                var_state->expr   = u->evar(index);
                gen->queue.push(var_state);

                outside_prob += -log(1-Scope::VAR_PROB);
            }

            index++;
            s = s->next;
        }

        // predict global functions
        gen->push_left_states(gen->pgf->abstract.funs_by_cat, g.first, arg_res, outside_prob);
    } else {
        for (std::pair<PgfExpr,prob_t> p : arg_res->exprs) {
            this->combine(gen,arg_res->scope,p.first,p.second,u);
        }
    }

    return false;
}

void PgfExhaustiveGenerator::State1::combine(PgfExhaustiveGenerator *gen, 
                                             Scope *scope, PgfExpr expr, prob_t prob,
                                             PgfUnmarshaller *u)
{
    Scope *s = scope;
    while (s != res->scope) {
        PgfExpr abs = u->eabs(s->bind_type, &s->var, expr);
        if (s != scope) {
            // if expr is a lambda created in the previous iteration
            u->free_ref(expr);
        }
        expr = abs;
        s = s->next;
    }

    PgfBindType bind_type = vector_elem(type->hypos, n_args)->bind_type;

    if (bind_type == PGF_BIND_TYPE_IMPLICIT) {
        PgfExpr implarg = u->eimplarg(expr);
        if (scope != res->scope) {
            // if expr is a lambda created in the previous loop
            u->free_ref(expr);
        }
        expr = implarg;
    }

    PgfExpr app = u->eapp(this->expr, expr);

    if (bind_type == PGF_BIND_TYPE_IMPLICIT || scope != res->scope) {
        // if expr is either a lambda or an implicit argument
        u->free_ref(expr);
    }

    res->ref_count++;

    State1 *app_state = new State1();
    app_state->res    = res;
    app_state->prob   = this->prob + prob;
    app_state->type   = type;
    app_state->n_args = n_args+1;
    app_state->expr   = app;
    gen->queue.push(app_state);
}

void PgfExhaustiveGenerator::State1::complete(PgfExhaustiveGenerator *gen, PgfUnmarshaller *u)
{
    prob_t outside_prob;
    if (res == gen->top_res)
        outside_prob = 0;
    else
        outside_prob = res->states[0]->prob;

    prob_t inside_prob = prob-outside_prob;
    res->exprs.push_back(std::pair<PgfExpr,prob_t>(expr,inside_prob));
    for (State1 *state : res->states) {
        state->combine(gen,res->scope,expr,inside_prob,u);
    }
}

void PgfExhaustiveGenerator::State1::free_refs(PgfUnmarshaller *u)
{
    u->free_ref(expr);
}

PgfExpr PgfExhaustiveGenerator::fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob)
{
    DB_scope scope(db, READER_SCOPE);

    if (top_res == NULL)
        return 0;

    for (;;) {
        if (top_res_index < top_res->exprs.size()) {
            auto pair = top_res->exprs[top_res_index++];
            *prob = pair.second;
            return pair.first;
        }

        if (queue.empty())
            return 0;

        State *state = queue.top(); queue.pop();

        if (state->process(this, u)) {
            State::release(state, u);
        }
    }
}

void PgfExhaustiveGenerator::free_refs(PgfUnmarshaller *u)
{
    while (!queue.empty()) {
        State  *state = queue.top(); queue.pop();
        state->free_refs(u);
        State::release(state,u);
    }

    for (auto i : results) {
        for (auto j : i.second->exprs) {
            free_ref(j.first);
        }
    }
}

PgfExhaustiveGenerator::~PgfExhaustiveGenerator()
{
    while (!scopes.empty()) {
        Scope *scope = scopes.back(); scopes.pop_back();
        delete scope;
    }
}
