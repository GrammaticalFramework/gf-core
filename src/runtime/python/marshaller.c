#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

#include "./compat.h"
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
    PyObject *i = PyLong_FromUnsignedLong(*v);
    return (PgfLiteral) i;
}

PgfLiteral lflt(PgfUnmarshaller *this, double v)
{
    PyObject *d = PyFloat_FromDouble(v);
    return (PgfLiteral) d;
}

PgfLiteral lstr(PgfUnmarshaller *this, PgfText *v)
{
    PyObject *s = PyUnicode_FromStringAndSize(v->text, v->size);
    return (PgfLiteral) s;
}

PgfType dtyp(PgfUnmarshaller *this, int n_hypos, PgfTypeHypo *hypos, PgfText *cat, int n_exprs, PgfExpr *exprs)
{
    TypeObject *pytype = (TypeObject *)pgf_TypeType.tp_alloc(&pgf_TypeType, 0);

    pytype->hypos = PyList_New(0);
    for (int i = 0; i < n_hypos; i++) {
        PgfTypeHypo *hypo = hypos + i;
        PyObject *tup = PyTuple_New(3);
        PyTuple_SetItem(tup, 0, PyLong_FromLong(hypo->bind_type == PGF_BIND_TYPE_EXPLICIT ? 0 : 1)); // TODO
        PyTuple_SetItem(tup, 1, PyString_FromStringAndSize(hypo->cid->text, hypo->cid->size));
        PyTuple_SetItem(tup, 2, (PyObject *)hypo->type);
        Py_INCREF(hypo->type);
        PyList_Append(pytype->hypos, tup);
    }

    pytype->cat = PyString_FromStringAndSize(cat->text, cat->size);

    pytype->exprs = PyList_New(0);
    return (PgfType) pytype;
}

void free_ref(PgfUnmarshaller *this, object x)
{
    Py_XDECREF(x);
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
