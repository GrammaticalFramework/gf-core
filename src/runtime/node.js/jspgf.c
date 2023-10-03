#include <stdlib.h>
#include <node_api.h> 
#include <pgf/pgf.h>

typedef struct {
    napi_env env;
    napi_ref exprAbs;
    napi_ref exprApp;
    napi_ref exprLit;
    napi_ref exprFun;
    napi_ref exprVar;
    napi_ref exprTyped;
    napi_ref exprImplArg;
    PgfUnmarshaller unmarshaller;
} JSPGFContext;

#define context(p,field) ((JSPGFContext*)(((char*) p)-offsetof(JSPGFContext,field)))

static
napi_value node_create_expr_abs(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 3;
    napi_value argv[3];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;
    
    status = napi_set_named_property(env, this, "btype", argv[0]);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "name", argv[1]);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "body", argv[2]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_app(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 2;
    napi_value argv[2];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;
    
    status = napi_set_named_property(env, this, "fun", argv[0]);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "arg", argv[1]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_lit(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 1;
    napi_value argv[1];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;
    
    status = napi_set_named_property(env, this, "value", argv[0]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_fun(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 1;
    napi_value argv[1];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;
    
    status = napi_set_named_property(env, this, "name", argv[0]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_var(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 1;
    napi_value argv[1];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "index", argv[0]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_typed(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 2;
    napi_value argv[2];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "expr", argv[0]);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "type", argv[1]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static
napi_value node_create_expr_implarg(napi_env env, napi_callback_info cbinfo) {
    napi_status status;

    napi_value this;
    size_t argc = 1;
    napi_value argv[1];
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              &this,
                              NULL);
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, this, "expr", argv[0]);
    if (status != napi_ok) return NULL;

    return NULL;
}

static PgfExpr
eabs(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsbtype;
    status = napi_get_boolean(context->env, (btype == PGF_BIND_TYPE_EXPLICIT), &jsbtype);
    if (status != napi_ok) return 0;

    napi_value jsname;
    status = napi_create_string_utf8(context->env, name->text, name->size, &jsname);
    if (status != napi_ok) return 0;

    napi_value exprAbs;
    status = napi_get_reference_value(context->env,
                                      context->exprAbs,
                                      &exprAbs);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { jsbtype, jsname, (napi_value) body };
    status = napi_new_instance(context->env,
                               exprAbs,
                               3, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
eapp(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value exprApp;
    status = napi_get_reference_value(context->env,
                                      context->exprApp,
                                      &exprApp);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { (napi_value) fun, (napi_value) arg };
    status = napi_new_instance(context->env,
                               exprApp,
                               2, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
elit(PgfUnmarshaller *this, PgfLiteral lit)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value exprLit;
    status = napi_get_reference_value(context->env,
                                      context->exprLit,
                                      &exprLit);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { (napi_value) lit };
    status = napi_new_instance(context->env,
                               exprLit,
                               1, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
emeta(PgfUnmarshaller *this, PgfMetaId meta)
{
    return 0;
}

static PgfExpr
efun(PgfUnmarshaller *this, PgfText *name)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsname;
    status = napi_create_string_utf8(context->env, name->text, name->size, &jsname);
    if (status != napi_ok) return 0;

    napi_value exprFun;
    status = napi_get_reference_value(context->env,
                                      context->exprFun,
                                      &exprFun);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { jsname };
    status = napi_new_instance(context->env,
                               exprFun,
                               1, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
evar(PgfUnmarshaller *this, int index)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsindex;
    status = napi_create_int64(context->env, index, &jsindex);
    if (status != napi_ok) return 0;

    napi_value exprVar;
    status = napi_get_reference_value(context->env,
                                      context->exprVar,
                                      &exprVar);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { jsindex };
    status = napi_new_instance(context->env,
                               exprVar,
                               1, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
etyped(PgfUnmarshaller *this, PgfExpr expr, PgfType typ)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value exprTyped;
    status = napi_get_reference_value(context->env,
                                      context->exprTyped,
                                      &exprTyped);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { (napi_value) expr, (napi_value) typ };
    status = napi_new_instance(context->env,
                               exprTyped,
                               2, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfExpr
eimplarg(PgfUnmarshaller *this, PgfExpr expr)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value exprImplArg;
    status = napi_get_reference_value(context->env,
                                      context->exprImplArg,
                                      &exprImplArg);
    if (status != napi_ok) return 0;

    napi_value result;
    napi_value argv[] = { (napi_value) expr };
    status = napi_new_instance(context->env,
                               exprImplArg,
                               1, argv,
                               &result);
    if (status != napi_ok) return 0;

    return (PgfExpr) result;
}

static PgfLiteral
lint(PgfUnmarshaller *this, size_t size, uintmax_t *v)
{
    if (size != 1)
        return 0;

    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsvalue;
    status = napi_create_int64(context->env, v[0], &jsvalue);
    if (status != napi_ok) return 0;

    return (PgfLiteral) jsvalue;
}

static PgfLiteral
lflt(PgfUnmarshaller *this, double v)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsvalue;
    status = napi_create_double(context->env, v, &jsvalue);
    if (status != napi_ok) return 0;

    return (PgfLiteral) jsvalue;
}

static PgfLiteral
lstr(PgfUnmarshaller *this, PgfText *v)
{
    napi_status status;
    JSPGFContext *context = context(this,unmarshaller);

    napi_value jsvalue;
    status = napi_create_string_utf8(context->env, v->text, v->size, &jsvalue);
    if (status != napi_ok) return 0;

    return (PgfLiteral) jsvalue;
}

static PgfType
dtyp(PgfUnmarshaller *this, size_t n_hypos, PgfTypeHypo *hypos, PgfText *cat, size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

static void
free_ref(PgfUnmarshaller *this, object x)
{
}

static PgfUnmarshallerVtbl unmarshallerVtbl =
{
    eabs,
    eapp,
    elit,
    emeta,
    efun,
    evar,
    etyped,
    eimplarg,
    lint,
    lflt,
    lstr,
    dtyp,
    free_ref
};

static
napi_value node_readExpr(napi_env env, napi_callback_info cbinfo)
{
    napi_status status;

    size_t argc = 1;
    napi_value argv[1];
    JSPGFContext *context;
    status = napi_get_cb_info(env,
                              cbinfo,
                              &argc,
                              argv,
                              NULL,
                              (void**)&context);
    if (status != napi_ok) return NULL;
    context->env = env;

    size_t size;
    status = napi_get_value_string_utf8(env,
                                        argv[0],
                                        NULL,
                                        0,
                                        &size);
    if (status != napi_ok) {
        if (status == napi_string_expected) {
            napi_throw_type_error(env, NULL,
                                  "A string argument is expected");
        }
        return NULL;
    }

    PgfText *s = alloca(sizeof(PgfText)+size+1);
    status = napi_get_value_string_utf8(env,
                                        argv[0],
                                        s->text,
                                        size+1,
                                        &s->size);
    if (status != napi_ok) return NULL;

    return (napi_value) pgf_read_expr(s, &context->unmarshaller);
}

static napi_status
node_create_class(napi_env env, napi_value exports, const char *name, napi_callback callback, napi_ref *pref)
{
    napi_status status;

    napi_value value;
    status = napi_define_class(env,
                               name,
                               NAPI_AUTO_LENGTH,
                               callback,
                               NULL,
                               0,
                               NULL,
                               &value);
    if (status != napi_ok) return status;

    status = napi_set_named_property(env, exports, name, value);
    if (status != napi_ok) return status;

    return napi_create_reference(env,
                                 value,
                                 1,
                                 pref);
}

NAPI_MODULE_INIT() {
    napi_status status;

    JSPGFContext *context = malloc(sizeof(JSPGFContext));
    context->unmarshaller.vtbl = &unmarshallerVtbl;

    status = node_create_class(env, exports,
                               "ExprAbs", node_create_expr_abs,
                               &context->exprAbs);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprApp", node_create_expr_app,
                               &context->exprApp);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprLit", node_create_expr_lit,
                               &context->exprLit);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprFun", node_create_expr_fun,
                               &context->exprFun);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprVar", node_create_expr_var,
                               &context->exprVar);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprTyped", node_create_expr_typed,
                               &context->exprTyped);
    if (status != napi_ok) return NULL;

    status = node_create_class(env, exports,
                               "ExprImplArg", node_create_expr_implarg,
                               &context->exprImplArg);
    if (status != napi_ok) return NULL;

    napi_value readExpr;
    status = napi_create_function(env,
                                  "readExpr",
                                  NAPI_AUTO_LENGTH,
                                  node_readExpr,
                                  context,
                                  &readExpr); 
    if (status != napi_ok) return NULL;

    status = napi_set_named_property(env, exports, "readExpr", readExpr);
    if (status != napi_ok) return NULL;

    return exports;
}
