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
    PyErr_SetString(PyExc_NotImplementedError, "eabs not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr eapp(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg)
{
    PyErr_SetString(PyExc_NotImplementedError, "eapp not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr elit(PgfUnmarshaller *this, PgfLiteral lit)
{
    ExprLitObject *pyexpr = (ExprLitObject *)pgf_ExprLitType.tp_alloc(&pgf_ExprLitType, 0);
    PyObject *pyobj = (PyObject *)lit;
    pyexpr->value = pyobj;
    Py_INCREF(pyobj);
    return (PgfExpr) pyexpr;
}

PgfExpr emeta(PgfUnmarshaller *this, PgfMetaId meta)
{
    PyErr_SetString(PyExc_NotImplementedError, "emeta not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr efun(PgfUnmarshaller *this, PgfText *name)
{
    PyErr_SetString(PyExc_NotImplementedError, "efun not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr evar(PgfUnmarshaller *this, int index)
{
    PyErr_SetString(PyExc_NotImplementedError, "evar not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr etyped(PgfUnmarshaller *this, PgfExpr expr, PgfType typ)
{
    PyErr_SetString(PyExc_NotImplementedError, "etyped not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfExpr eimplarg(PgfUnmarshaller *this, PgfExpr expr)
{
    PyErr_SetString(PyExc_NotImplementedError, "eimplarg not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

PgfLiteral lint(PgfUnmarshaller *this, size_t size, uintmax_t *v)
{
    intmax_t *v0 = (intmax_t *)v;
    if (size == 0) {
        return (PgfLiteral) 0;
    } else if (size > 1) {
        // TODO: string concatenation works but probably not optimal
        PyObject *sb = PyUnicode_FromFormat("%ld", *v0);
        for (size_t n = 1; n < size; n++) {
            uintmax_t *vn = v + n;
            PyObject *t = PyUnicode_FromFormat("%lu", *vn);
            sb = PyUnicode_Concat(sb, t);
        }
        PyObject *i = PyLong_FromUnicodeObject(sb, 10);
        return (PgfLiteral) i;
    } else {
        PyObject *i = PyLong_FromLong(*v0);
        return (PgfLiteral) i;
    }
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
        PyTuple_SetItem(tup, 1, PyUnicode_FromStringAndSize(hypo->cid->text, hypo->cid->size));
        PyTuple_SetItem(tup, 2, (PyObject *)hypo->type);
        Py_INCREF(hypo->type);
        PyList_Append(pytype->hypos, tup);
    }

    pytype->cat = PyUnicode_FromStringAndSize(cat->text, cat->size);

    pytype->exprs = PyList_New(0);
    for (int i = 0; i < n_exprs; i++) {
        // TODO
        // PgfExpr *expr = exprs + i;
    }

    return (PgfType) pytype;
}

void free_ref(PgfUnmarshaller *this, object x)
{
    Py_XDECREF(x);
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

/* static */
PgfUnmarshaller unmarshaller = { &unmarshallerVtbl };

// ----------------------------------------------------------------------------

static PgfText *
PyUnicode_AsPgfText(PyObject *pystr)
{
    if (!PyUnicode_Check(pystr)) {
        PyErr_SetString(PyExc_TypeError, "input to PyUnicode_AsPgfText is not a string");
        return NULL;
    }
    if (PyUnicode_READY(pystr) != 0) {
        return NULL;
    }

    Py_ssize_t size;
    const char * enc = PyUnicode_AsUTF8AndSize(pystr, &size);
    PgfText *ptext = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(ptext->text, enc, size+1);
    ptext->size = size;
    // Py_INCREF(ptext);
    return ptext;
}

// ----------------------------------------------------------------------------

object match_lit(PgfMarshaller *this, PgfUnmarshaller *u, PgfLiteral lit)
{
    PyObject *pyobj = (PyObject *)lit;

    if (PyLong_Check(pyobj)) {
        // TODO
        uintmax_t i = PyLong_AsUnsignedLong(pyobj);
        size_t size = 1;
        return u->vtbl->lint(u, size, &i);
    } else if (PyFloat_Check(pyobj)) {
        double d = PyFloat_AsDouble(pyobj);
        return u->vtbl->lflt(u, d);
    } else if (PyUnicode_Check(pyobj)) {
        PgfText *t = PyUnicode_AsPgfText(pyobj);
        return u->vtbl->lstr(u, t);
    } else {
        PyErr_SetString(PyExc_TypeError, "unable to match on literal");
        return 0;
    }
}

object match_expr(PgfMarshaller *this, PgfUnmarshaller *u, PgfExpr expr)
{
    PyErr_SetString(PyExc_NotImplementedError, "match_expr not implemented");
    Py_RETURN_NOTIMPLEMENTED;
}

object match_type(PgfMarshaller *this, PgfUnmarshaller *u, PgfType ty)
{
    TypeObject *type = (TypeObject *)ty;

    int n_hypos = 0; //PyList_Size(type->hypos);
    PgfTypeHypo *hypos = NULL; // TODO

    PgfText *cat = PyUnicode_AsPgfText(type->cat);
    if (cat == NULL) {
        return 0;
    }

    int n_exprs = PyList_Size(type->exprs);
    PgfExpr *exprs;
    if (n_exprs > 0) {
        exprs = (PgfExpr *)PyList_GetItem(type->exprs, 0);
        // TODO lay out others in memory in some way?
    } else {
        exprs = NULL;
    }

    return u->vtbl->dtyp(u, n_hypos, hypos, cat, n_exprs, exprs);
}

static PgfMarshallerVtbl marshallerVtbl =
{
    match_lit,
    match_expr,
    match_type
};

/* static */
PgfMarshaller marshaller = { &marshallerVtbl };
