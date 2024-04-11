#include <stdarg.h>
#include "data.h"
#include "typechecker.h"

class PgfTypechecker::Unmarshaller1 : public PgfUnmarshaller {
    virtual PgfExpr eabs(PgfBindType bind_type, PgfText *name, PgfExpr body) { return 0; }
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg) { return 0; }
    virtual PgfExpr elit(PgfLiteral lit) { return 0; }
    virtual PgfExpr emeta(PgfMetaId meta_id) { return 0; }
    virtual PgfExpr efun(PgfText *name) { return 0; }
    virtual PgfExpr evar(int index) { return 0; }
    virtual PgfExpr etyped(PgfExpr expr, PgfType ty) { return 0; }
    virtual PgfExpr eimplarg(PgfExpr expr) { return 0; }
    virtual PgfLiteral lint(size_t size, uintmax_t *val) { return 0; }
    virtual PgfLiteral lflt(double val) { return 0; }
    virtual PgfLiteral lstr(PgfText *val) { return 0; }

    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *name,
                         size_t n_exprs, PgfExpr *exprs)
    {
        Type *ty = new(name) Cat;
        while (n_hypos > 0) {
            PgfTypeHypo *hypo = &hypos[--n_hypos];
            ty = new(hypo->bind_type, hypo->cid, (Type*) hypo->type, ty) Pi;
        }
        return (PgfType) ty;
    }

    virtual void free_ref(object x) { }
};

class PgfTypechecker::Unmarshaller2 : public Unmarshaller1 {
    PgfMarshaller *m;

public:
    Unmarshaller2(PgfMarshaller *m) {
        this->m = m;
    }

    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *name,
                         size_t n_exprs, PgfExpr *exprs)
    {
        Type *ty = new(name) Cat;
        while (n_hypos > 0) {
            PgfTypeHypo *hypo = &hypos[--n_hypos];
            Type *arg_ty = (Type *) m->match_type(this, hypo->type);
            ty = new(hypo->bind_type, hypo->cid, arg_ty, ty) Pi;
        }
        return (PgfType) ty;
    }

    virtual void free_ref(object x) { }
};

PgfType PgfTypechecker::marshall_type(Type *ty, PgfUnmarshaller *u)
{
    size_t n_hypos = 0;
    PgfTypeHypo *hypos = 0;
    for (;;) {
        Pi *pi = ty->is_pi();
        if (pi) {
            hypos = (PgfTypeHypo *) realloc(hypos, n_hypos*sizeof(PgfTypeHypo));
            PgfTypeHypo *hypo = &hypos[n_hypos++];
            hypo->bind_type = pi->bind_type;
            hypo->cid = &pi->var;
            hypo->type = marshall_type(pi->arg, u);
            ty = pi->res;
        }
        Cat *cat = ty->is_cat();
        if (cat) {
            return u->dtyp(n_hypos,hypos,&cat->name,0,NULL);
        }
    }
}

PgfTypechecker::PgfTypechecker(ref<PgfPGF> gr, PgfMarshaller *m, PgfUnmarshaller *u, PgfExn* err) {
    this->gr   = gr;
    this->m    = m;
    this->u    = u;
    this->err  = err;
};

PgfTypechecker::~PgfTypechecker() {
    while (temps.size() > 0) {
        Type *ty = temps.back(); temps.pop_back();
        delete(ty);
    }
}

PgfTypechecker::Context::Context(PgfTypechecker *tc, Scope *scope, Type *exp_type, PgfBindType bind_type) {
    this->tc         = tc;
    this->scope      = scope;
    this->bind_type  = bind_type;
    this->exp_type   = exp_type;
    this->inf_type   = NULL;
}

PgfExpr PgfTypechecker::Context::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    if (!checkImplArgument())
        return 0;

    if (exp_type == NULL) {
        return tc->type_error("Cannot infer the type of a lambda abstraction");
    }

    Pi *pi = exp_type->is_pi();
    if (!pi) {
        return tc->type_error("A lambda abstraction must have a function type");
    }

    Scope new_scope;
    new_scope.tail=scope;
    new_scope.var=name;
    new_scope.ty=pi->arg;
    Context body_ctxt(tc,&new_scope,pi->res);
    body = tc->m->match_expr(&body_ctxt, body);
    if (body == 0)
        return 0;

    return tc->u->eabs(btype,name,body);
}

PgfExpr PgfTypechecker::Context::eapp(PgfExpr fun, PgfExpr arg)
{
    if (!checkImplArgument())
        return 0;

    Context fun_ctxt(tc, scope);
    fun = tc->m->match_expr(&fun_ctxt, fun);
    if (fun == 0)
        return 0;

repeat:
    Pi *pi = fun_ctxt.inf_type->is_pi();
    if (!pi) {
        tc->type_error("Too many arguments");
        return 0;
    }

    Context arg_ctxt(tc,scope,pi->arg,pi->bind_type);
    PgfExpr new_arg = tc->m->match_expr(&arg_ctxt, arg);
    if (new_arg == 0) {
        if (tc->err->type == PGF_EXN_TYPE_ERROR && tc->err->code == 1) {
            tc->err->type = PGF_EXN_NONE;
            tc->err->code = 0;
            tc->err->msg  = NULL;
            PgfExpr meta = tc->u->emeta(0);
            PgfExpr impl = tc->u->eimplarg(meta);
            PgfExpr new_fun = tc->u->eapp(fun, impl);
            tc->u->free_ref(fun);
            tc->u->free_ref(impl);
            tc->u->free_ref(meta);
            fun = new_fun;
            fun_ctxt.inf_type = pi->res;
            goto repeat;
        }
        return 0;
    }

    PgfExpr e = tc->u->eapp(fun, new_arg);
    free_ref(fun);
    free_ref(new_arg);

    inf_type = pi->res;

    if (!unifyTypes(&e)) {
        free_ref(e);
        return 0;
    }

    return e;
}

PgfExpr PgfTypechecker::Context::elit(PgfLiteral lit)
{
    if (!checkImplArgument())
        return 0;

    lit = tc->m->match_lit(this, lit);
    if (!lit)
        return 0;

    PgfExpr e = tc->u->elit(lit);  free_ref(lit);

    if (!unifyTypes(&e)) {
        tc->u->free_ref(e);
        return 0;
    }

    return e;
}

PgfExpr PgfTypechecker::Context::emeta(PgfMetaId meta)
{
    if (!checkImplArgument())
        return 0;

    if (exp_type == NULL)
        return tc->type_error("Cannot infer the type of a meta variable");

    PgfExpr e = tc->u->emeta(meta);

    inf_type = exp_type;

    if (!unifyTypes(&e)) {
        tc->u->free_ref(e);
        return 0;
    }

    return e;
}

PgfExpr PgfTypechecker::Context::efun(PgfText *name)
{
    if (!checkImplArgument())
        return 0;

    ref<PgfAbsFun> absfun =
        namespace_lookup(tc->gr->abstract.funs, name);
    if (absfun == 0)
        return tc->type_error("Function %s is not defined", name->text);

    Unmarshaller1 tu;
    inf_type = (Type*) tc->db_m.match_type(&tu, absfun->type.as_object());
    tc->temps.push_back(inf_type);

    PgfExpr e = tc->u->efun(name);

    if (!unifyTypes(&e)) {
        tc->u->free_ref(e);
        return 0;
    }

    return e;
}

PgfExpr PgfTypechecker::Context::evar(int index)
{
    if (!checkImplArgument())
        return 0;

    Scope *s = scope;
    while (s != NULL && index > 0) {
        s = s->tail;
        index--;
    }

    if (s == NULL) {
        return tc->type_error("Cannot type check an open expression (de Bruijn index %d)", index);
    }

    inf_type = s->ty;

    PgfExpr e = tc->u->evar(index);

    if (!unifyTypes(&e)) {
        tc->u->free_ref(e);
        return 0;
    }

    return e;
}

PgfExpr PgfTypechecker::Context::etyped(PgfExpr expr, PgfType type)
{
    if (!checkImplArgument())
        return 0;

    Context type_ctxt(tc, scope);
    type = tc->m->match_type(&type_ctxt, type);
    if (type == 0)
        return 0;

    Unmarshaller2 tu(tc->m);
    Type *ty = (Type*) tc->m->match_type(&tu,type);

    Context expr_ctxt(tc, scope, ty, PGF_BIND_TYPE_EXPLICIT);
    expr = tc->m->match_expr(&expr_ctxt, expr);
    if (expr == 0) {
        free_ref(type);
        return 0;
    }
    inf_type = expr_ctxt.inf_type;

    PgfExpr e = tc->u->etyped(expr,type);

    free_ref(expr);
    free_ref(type);

    return e;
}

PgfExpr PgfTypechecker::Context::eimplarg(PgfExpr expr)
{
    if (bind_type != PGF_BIND_TYPE_IMPLICIT) {
        tc->type_error("Unexpected implicit argument");
        return 0;
    }

    Context expr_ctxt(tc,scope,exp_type,PGF_BIND_TYPE_EXPLICIT);
    expr = tc->m->match_expr(&expr_ctxt, expr);
    if (expr == 0) {
        return 0;
    }

    inf_type = expr_ctxt.inf_type;

    PgfExpr e = tc->u->eimplarg(expr);

    free_ref(expr);

    return e;
}

PgfLiteral PgfTypechecker::Context::lint(size_t size, uintmax_t *v)
{
    inf_type = new("Int") Cat;
    tc->temps.push_back(inf_type);
    return tc->u->lint(size,v);
}

PgfLiteral PgfTypechecker::Context::lflt(double v)
{
    inf_type = new("Float") Cat;
    tc->temps.push_back(inf_type);
    return tc->u->lflt(v);
}

PgfLiteral PgfTypechecker::Context::lstr(PgfText *v)
{
    inf_type = new("String") Cat;
    tc->temps.push_back(inf_type);
    return tc->u->lstr(v);
}

PgfType PgfTypechecker::Context::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                      PgfText *cat,
                                      size_t n_exprs, PgfExpr *exprs)
{
    ref<PgfAbsCat> abscat =
        namespace_lookup(tc->gr->abstract.cats, cat);
    if (abscat == 0)
        return tc->type_error("Category %s is not defined", cat->text);

    PgfType ty = 0;
    PgfTypeHypo *new_hypos = (PgfTypeHypo *)
        alloca(sizeof(PgfTypeHypo)*n_hypos);
    size_t n_new_exprs = abscat->context->len;
    PgfExpr *new_exprs = (PgfExpr *)
        alloca(sizeof(PgfExpr)*n_new_exprs);

    for (size_t i = 0; i < n_hypos; i++) {
        new_hypos[i].bind_type = hypos[i].bind_type;
        new_hypos[i].cid       = hypos[i].cid;
        new_hypos[i].type      = tc->m->match_type(this, hypos[i].type);
        if (new_hypos[i].type == 0) {
            n_hypos = i;
            n_new_exprs = 0;
            goto exit;
        }
    }

    size_t i, j;
    for (i = 0, j = 0; i < n_new_exprs && j < n_exprs; i++) {
        Unmarshaller1 tu;
        PgfHypo *hypo = vector_elem(abscat->context,i);
        Type *ty = (Type *) tc->db_m.match_type(&tu,hypo->type.as_object());
        tc->temps.push_back(ty);
        Context expr_ctxt(tc,scope,ty,hypo->bind_type);
        new_exprs[i] = tc->m->match_expr(&expr_ctxt, exprs[j]);
        if (new_exprs[i] == 0) {
            if (tc->err->type == PGF_EXN_TYPE_ERROR && tc->err->code == 1) {
                tc->err->type = PGF_EXN_NONE;
                tc->err->code = 0;
                tc->err->msg  = NULL;
                PgfExpr e1 = tc->u->emeta(0);
                new_exprs[i] = tc->u->eimplarg(e1);  free_ref(e1);
            } else {
                n_new_exprs = i;
                goto exit;
            }
        } else {
            j++;
        }
    }

    if (i < n_new_exprs) {
        tc->type_error("Too few arguments to category %s - %ld expected but %ld given",
                       cat->text,
                       n_new_exprs,
                       n_exprs);
        n_new_exprs = n_exprs;
        goto exit;
    }
    if (j < n_exprs) {
        tc->type_error("Too many arguments to category %s - %ld expected but %ld given",
                       cat->text,
                       n_new_exprs,
                       n_exprs);
        goto exit;
    }

    ty = tc->u->dtyp(n_hypos, new_hypos, cat, n_new_exprs, new_exprs);

exit:
    for (size_t i = 0; i < n_hypos; i++) {
        free_ref(new_hypos[i].type);
    }
    for (size_t i = 0; i < n_new_exprs; i++) {
        free_ref(new_exprs[i]);
    }
    return ty;
}

void PgfTypechecker::Context::free_ref(object x)
{
    tc->u->free_ref(x);
}

bool PgfTypechecker::Context::checkImplArgument()
{
    if (bind_type == PGF_BIND_TYPE_IMPLICIT) {
        tc->err->type = PGF_EXN_TYPE_ERROR;
        tc->err->code = 1;
        tc->err->msg  = "Unexpected explicit argument";
        return false;
    }
    return true;
}

bool PgfTypechecker::unifyTypes(Type *ty1, Type *ty2)
{
    Cat *cat1 = ty1->is_cat();
    Cat *cat2 = ty2->is_cat();
    if (cat1 && cat2) {
        if (textcmp(&cat1->name, &cat2->name) != 0) {
            return type_error("Types %s and %s doesn't match", cat1->name.text, cat2->name.text);
        }
        return true;
    }

    Pi *pi1 = ty1->is_pi();
    Pi *pi2 = ty2->is_pi();
    if (pi1 && pi2) {
        return unifyTypes(pi1->arg, pi2->arg) && unifyTypes(pi1->res, pi2->res);
    }

    return type_error("Types doesn't match");
}

bool PgfTypechecker::Context::unifyTypes(PgfExpr *e)
{
    if (exp_type == NULL)
        return true;
    return tc->unifyTypes(inf_type,exp_type);
}

bool PgfTypechecker::type_error(const char *fmt, ...)
{
    va_list arg_ptr;

    va_start(arg_ptr, fmt);
    size_t n_bytes = vsnprintf(NULL, 0, fmt, arg_ptr);
    va_end(arg_ptr);

    char *buffer = (char*) err->msg;

    va_start(arg_ptr, fmt);
    buffer = (char*) realloc(buffer, n_bytes+1);
    if (buffer) {
        vsnprintf(buffer, n_bytes+1, fmt, arg_ptr);
        err->type = PGF_EXN_TYPE_ERROR;
        err->code = 0;
        err->msg  = buffer;
    }
    va_end(arg_ptr);

    return false;
}

PgfType PgfTypechecker::infer_expr(PgfExpr *pe)
{
    Context ctxt(this,NULL);
    *pe = m->match_expr(&ctxt, *pe);
    if (*pe == 0)
        return 0;
    return marshall_type(ctxt.inf_type, u);
}

PgfExpr PgfTypechecker::check_expr(PgfExpr expr, PgfType type)
{
    Unmarshaller2 tu(m);
    Type *ty = (Type*) m->match_type(&tu, type);
    Context ctxt(this,NULL,ty,PGF_BIND_TYPE_EXPLICIT);
    expr = m->match_expr(&ctxt, expr);
    return expr;
}

PgfType PgfTypechecker::check_type(PgfType type)
{
    Context ctxt(this,NULL);
    return m->match_type(&ctxt, type);
}
