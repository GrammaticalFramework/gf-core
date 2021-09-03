// #define PY_SSIZE_T_CLEAN
// #include <Python.h>

#include <pgf/pgf.h>
#include "./expr.h"

/* The PgfUnmarshaller structure tells the runtime how to create
 * abstract syntax expressions and types in the heap of the host language.
 * In Python the expressions are normal objects.
 * From the point of view of the runtime, each node is a value of type object.
 * For Python that would be a PyObject pointer.
 */

PgfExpr eabs(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body)
{
    return 0;
}

PgfExpr eapp(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg)
{
    return 0;
}

PgfExpr elit(PgfUnmarshaller *this, PgfLiteral lit)
{
    return 0;
}

PgfExpr emeta(PgfUnmarshaller *this, PgfMetaId meta)
{
    return 0;
}

PgfExpr efun(PgfUnmarshaller *this, PgfText *name)
{
    return 0;
}

PgfExpr evar(PgfUnmarshaller *this, int index)
{
    return 0;
}

PgfExpr etyped(PgfUnmarshaller *this, PgfExpr expr, PgfType typ)
{
    return 0;
}

PgfExpr eimplarg(PgfUnmarshaller *this, PgfExpr expr)
{
    return 0;
}

PgfLiteral lint(PgfUnmarshaller *this, size_t size, uintmax_t *v)
{
    return 0;
}

PgfLiteral lflt(PgfUnmarshaller *this, double v)
{
    return 0;
}

PgfLiteral lstr(PgfUnmarshaller *this, PgfText *v)
{
    return 0;
}

PgfType dtyp(PgfUnmarshaller *this, int n_hypos, PgfTypeHypo *hypos, PgfText *cat, int n_exprs, PgfExpr *exprs)
{
    PgfText* catname = (PgfText*) malloc(sizeof(PgfText)+cat->size+1);
    memcpy(catname->text, cat->text, cat->size+1);
    catname->size = cat->size;

    TypeObject* pytype = (TypeObject*) pgf_TypeType.tp_alloc(&pgf_TypeType, 0);
    pytype->cat = catname;
    return (PgfType) pytype;
}

void free_ref(PgfUnmarshaller *this, object x)
{
    return;
}

static PgfUnmarshallerVtbl unmarshallervtbl =
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

/* static */
PgfUnmarshaller unmarshaller = { &unmarshallervtbl };
