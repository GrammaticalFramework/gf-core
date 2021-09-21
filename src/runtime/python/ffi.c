#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <math.h>
#include <stdbool.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"

// ----------------------------------------------------------------------------
// errors

PyObject *PGFError;

PgfExnType handleError(PgfExn err)
{
    if (err.type == PGF_EXN_SYSTEM_ERROR) {
        errno = err.code;
        PyErr_SetFromErrnoWithFilename(PyExc_IOError, err.msg);
    } else if (err.type == PGF_EXN_PGF_ERROR) {
        PyErr_SetString(PGFError, err.msg);
        free((char*) err.msg);
    } else if (err.type == PGF_EXN_OTHER_ERROR) {
        PyErr_SetString(PGFError, "an unknown error occured");
    }
    return err.type;
}

// ----------------------------------------------------------------------------
// string conversions

PgfText *
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
    const char *enc = PyUnicode_AsUTF8AndSize(pystr, &size);
    PgfText *ptext = malloc(sizeof(PgfText)+size+1);
    memcpy(ptext->text, enc, size+1);
    ptext->size = size;
    return ptext;
}

PyObject *
PyUnicode_FromPgfText(PgfText *text)
{
    return PyUnicode_FromStringAndSize(text->text, text->size);
}

// ----------------------------------------------------------------------------
// unmarshaller

static PgfExpr
eabs(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body)
{
    ExprAbsObject *pyexpr = (ExprAbsObject *)pgf_ExprAbsType.tp_alloc(&pgf_ExprAbsType, 0);
    pyexpr->bindType = PyLong_FromLong(btype);
    pyexpr->var = PyUnicode_FromPgfText(name);
    pyexpr->expr = (ExprObject *)body;
    // Py_INCREF(body);
    return (PgfExpr) pyexpr;
}

static PgfExpr
eapp(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg)
{
    ExprAppObject *pyexpr = (ExprAppObject *)pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
    pyexpr->e1 = (ExprObject *)fun;
    pyexpr->e2 = (ExprObject *)arg;
    // Py_INCREF(fun);
    // Py_INCREF(arg);
    return (PgfExpr) pyexpr;
}

static PgfExpr
elit(PgfUnmarshaller *this, PgfLiteral lit)
{
    ExprLitObject *pyexpr = (ExprLitObject *)pgf_ExprLitType.tp_alloc(&pgf_ExprLitType, 0);
    PyObject *pyobj = (PyObject *)lit;
    pyexpr->value = pyobj;
    Py_INCREF(pyobj);
    return (PgfExpr) pyexpr;
}

static PgfExpr
emeta(PgfUnmarshaller *this, PgfMetaId meta)
{
    ExprMetaObject *pyexpr = (ExprMetaObject *)pgf_ExprMetaType.tp_alloc(&pgf_ExprMetaType, 0);
    pyexpr->id = PyLong_FromLong(meta);
    return (PgfExpr) pyexpr;
}

static PgfExpr
efun(PgfUnmarshaller *this, PgfText *name)
{
    ExprFunObject *pyexpr = (ExprFunObject *)pgf_ExprFunType.tp_alloc(&pgf_ExprFunType, 0);
    PyObject *pyobj = PyUnicode_FromPgfText(name);
    pyexpr->name = pyobj;
    Py_INCREF(pyobj);
    return (PgfExpr) pyexpr;
}

static PgfExpr
evar(PgfUnmarshaller *this, int index)
{
    ExprVarObject *pyexpr = (ExprVarObject *)pgf_ExprVarType.tp_alloc(&pgf_ExprVarType, 0);
    pyexpr->index = PyLong_FromLong(index);
    return (PgfExpr) pyexpr;
}

static PgfExpr
etyped(PgfUnmarshaller *this, PgfExpr expr, PgfType typ)
{
    ExprTypedObject *pyexpr = (ExprTypedObject *)pgf_ExprTypedType.tp_alloc(&pgf_ExprTypedType, 0);
    pyexpr->expr = (ExprObject *)expr;
    pyexpr->type = (TypeObject *)typ;
    // Py_INCREF(expr);
    // Py_INCREF(typ);
    return (PgfExpr) pyexpr;
}

static PgfExpr
eimplarg(PgfUnmarshaller *this, PgfExpr expr)
{
    ExprImplArgObject *pyexpr = (ExprImplArgObject *)pgf_ExprImplArgType.tp_alloc(&pgf_ExprImplArgType, 0);
    pyexpr->expr = (ExprObject *)expr;
    // Py_INCREF(expr);
    return (PgfExpr) pyexpr;
}

static PgfLiteral
lint(PgfUnmarshaller *this, size_t size, uintmax_t *v)
{
    intmax_t *v0 = (intmax_t *)v;
    PyObject *i = PyLong_FromLong(*v0);

    if (size == 0) {
        return (PgfLiteral) 0;
    } else {
        PyObject *intShifter = PyLong_FromUnsignedLong(pow(10, floor(log10(ULONG_MAX))));
        for (size_t n = 1; n < size; n++) {
            i = PyNumber_Multiply(i, intShifter);
            if (*v0 >= 0) {
                i = PyNumber_Add(i, PyLong_FromUnsignedLong(v[n]));
            } else {
                i = PyNumber_Subtract(i, PyLong_FromUnsignedLong(v[n]));
            }
        }
        if (PyErr_Occurred()) {
            return 0;
        }
        return (PgfLiteral) i;
    }
}

static PgfLiteral
lflt(PgfUnmarshaller *this, double v)
{
    PyObject *d = PyFloat_FromDouble(v);
    return (PgfLiteral) d;
}

static PgfLiteral
lstr(PgfUnmarshaller *this, PgfText *v)
{
    PyObject *s = PyUnicode_FromStringAndSize(v->text, v->size);
    return (PgfLiteral) s;
}

static PgfType
dtyp(PgfUnmarshaller *this, int n_hypos, PgfTypeHypo *hypos, PgfText *cat, int n_exprs, PgfExpr *exprs)
{
    TypeObject *pytype = (TypeObject *)pgf_TypeType.tp_alloc(&pgf_TypeType, 0);

    pytype->hypos = PyList_New(n_hypos);
    for (int i = 0; i < n_hypos; i++) {
        PyObject *tup = PyTuple_New(3);
        PyTuple_SetItem(tup, 0, PyLong_FromLong(hypos[i].bind_type));
        PyTuple_SetItem(tup, 1, PyUnicode_FromStringAndSize(hypos[i].cid->text, hypos[i].cid->size));
        PyTuple_SetItem(tup, 2, (PyObject *)hypos[i].type);
        Py_INCREF(hypos[i].type);
        PyList_SetItem(pytype->hypos, i, tup);
    }

    pytype->cat = PyUnicode_FromStringAndSize(cat->text, cat->size);

    pytype->exprs = PyList_New(n_exprs);
    for (int i = 0; i < n_exprs; i++) {
        PyList_SetItem(pytype->exprs, i, (PyObject *)exprs[i]);
    }

    return (PgfType) pytype;
}

static void
free_ref(PgfUnmarshaller *this, object x)
{
    // Py_XDECREF(x);
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

PgfUnmarshaller unmarshaller = { &unmarshallerVtbl };

// ----------------------------------------------------------------------------
// marshaller

static object
match_lit(PgfMarshaller *this, PgfUnmarshaller *u, PgfLiteral lit)
{
    PyObject *pyobj = (PyObject *)lit;

    if (PyLong_Check(pyobj)) {
        PyObject *intShifter = PyLong_FromUnsignedLong(pow(10, floor(log10(ULONG_MAX))));

        // determine size
        size_t size = 1;
        PyObject *x = PyNumber_Absolute(PyNumber_Long(pyobj)); // make a copy, ignore sign
        while (PyObject_RichCompareBool(x, intShifter, Py_GE) == 1) {
            size++;
            x = PyNumber_FloorDivide(x, intShifter);
        }

        // chop up into chunks, always positive
        bool isPos = PyObject_RichCompareBool(pyobj, PyLong_FromLong(0), Py_GE) == 1;
        x = PyNumber_Absolute(PyNumber_Long(pyobj)); // make a copy, ignore sign
        uintmax_t *i = malloc(sizeof(uintmax_t)*size);
        for (int n = size-1; n > 0; n--) {
            PyObject *rem = PyNumber_Remainder(x, intShifter);
            i[n] = PyLong_AsUnsignedLong(rem);
            x = PyNumber_FloorDivide(x, intShifter);
        }

        // first chunk, re-applying polarity
        if (isPos)
            i[0] = PyLong_AsLong(x);
        else
            i[0] = PyLong_AsLong(PyNumber_Negative(x));

        if (PyErr_Occurred()) {
            return 0;
        }
        return u->vtbl->lint(u, size, i);
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

static object
match_expr(PgfMarshaller *this, PgfUnmarshaller *u, PgfExpr expr)
{
    PyObject *pyobj = (PyObject *)expr;

    if (PyObject_TypeCheck(pyobj, &pgf_ExprAbsType)) {
        ExprAbsObject *eabs = (ExprAbsObject *)expr;
        return u->vtbl->eabs(u, PyLong_AsLong(eabs->bindType), PyUnicode_AsPgfText(eabs->var), (PgfExpr) eabs->expr);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprAppType)) {
        ExprAppObject *eapp = (ExprAppObject *)expr;
        return u->vtbl->eapp(u, (PgfExpr) eapp->e1, (PgfExpr) eapp->e2);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprLitType)) {
        ExprLitObject *elit = (ExprLitObject *)expr;
        return this->vtbl->match_lit(this, u, (PgfLiteral) elit->value);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprMetaType)) {
        ExprMetaObject *emeta = (ExprMetaObject *)expr;
        return u->vtbl->emeta(u, (PgfMetaId) PyLong_AsLong(emeta->id));
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprFunType)) {
        ExprFunObject *efun = (ExprFunObject *)expr;
        return u->vtbl->efun(u, PyUnicode_AsPgfText(efun->name));
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprVarType)) {
        ExprVarObject *evar = (ExprVarObject *)expr;
        return u->vtbl->evar(u, PyLong_AsLong(evar->index));
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprTypedType)) {
        ExprTypedObject *etyped = (ExprTypedObject *)expr;
        return u->vtbl->etyped(u, (PgfExpr) etyped->expr, (PgfType) etyped->type);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprImplArgType)) {
        ExprImplArgObject *eimplarg = (ExprImplArgObject *)expr;
        return u->vtbl->eimplarg(u, (PgfExpr) eimplarg->expr);
    } else {
        PyErr_SetString(PyExc_TypeError, "unable to match on expression");
        return 0;
    }
}

static object
match_type(PgfMarshaller *this, PgfUnmarshaller *u, PgfType ty)
{
    TypeObject *type = (TypeObject *)ty;

    Py_ssize_t n_hypos = PyList_Size(type->hypos);
    PgfTypeHypo hypos[n_hypos];
    // PgfTypeHypo *hypos = alloca(sizeof(PgfTypeHypo)*n_hypos);
    for (Py_ssize_t i = 0; i < n_hypos; i++) {
        PyObject *hytup = (PyObject *)PyList_GetItem(type->hypos, i);
        hypos[i].bind_type = PyLong_AsLong(PyTuple_GetItem(hytup, 0));
        hypos[i].cid = PyUnicode_AsPgfText(PyTuple_GetItem(hytup, 1));
        hypos[i].type = (PgfType) PyTuple_GetItem(hytup, 2);
        Py_INCREF(hypos[i].type);
    }

    PgfText *cat = PyUnicode_AsPgfText(type->cat);
    if (cat == NULL) {
        return 0;
    }

    Py_ssize_t n_exprs = PyList_Size(type->exprs);
    PgfExpr exprs[n_exprs];
    // PgfExpr *exprs = alloca(sizeof(PgfExpr)*n_exprs);
    for (Py_ssize_t i = 0; i < n_exprs; i++) {
        exprs[i] = (PgfExpr)PyList_GetItem(type->exprs, i);
        Py_INCREF(exprs[i]);
    }

    object res = u->vtbl->dtyp(u, n_hypos, hypos, cat, n_exprs, exprs);

    for (Py_ssize_t i = 0; i < n_exprs; i++) {
        Py_DECREF(exprs[i]);
    }
    for (Py_ssize_t i = 0; i < n_hypos; i++) {
        free(hypos[i].cid);
        Py_DECREF(hypos[i].type);
    }
    free(cat);

    return res;
}

static PgfMarshallerVtbl marshallerVtbl =
{
    match_lit,
    match_expr,
    match_type
};

PgfMarshaller marshaller = { &marshallerVtbl };
