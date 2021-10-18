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
        free((char *)err.msg);
    }
    return err.type;
}

// ----------------------------------------------------------------------------
// conversions

PgfText *
CString_AsPgfText(const char *s, size_t size) {
    PgfText *txt = (PgfText *)PyMem_RawMalloc(sizeof(PgfText)+size+1);
    memcpy(txt->text, s, size+1);
    txt->size = size;
    return txt;
}

PgfText *
PyUnicode_AsPgfText(PyObject *pystr)
{
    if (!PyUnicode_Check(pystr)) {
        PyErr_SetString(PyExc_TypeError, "input to PyUnicode_AsPgfText is not a string");
        return NULL;
    }

    Py_ssize_t size;
    const char *enc = PyUnicode_AsUTF8AndSize(pystr, &size);
    PgfText *ptext = PyMem_RawMalloc(sizeof(PgfText)+size+1);
    memcpy(ptext->text, enc, size+1);
    ptext->size = size;
    return ptext;
}

PyObject *
PyUnicode_FromPgfText(PgfText *text)
{
    return PyUnicode_FromStringAndSize(text->text, text->size);
}

PgfTypeHypo *
PySequence_AsHypos(PyObject *pyseq, Py_ssize_t *n_hypos)
{
    if (!PySequence_Check(pyseq)) {
        PyErr_SetString(PyExc_TypeError, "hypotheses must be a sequence");
        return NULL;
    }
    Py_ssize_t n = PySequence_Size(pyseq);
    *n_hypos = n;
    PgfTypeHypo *hypos = PyMem_RawMalloc(sizeof(PgfTypeHypo)*n);

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *tup = PySequence_GetItem(pyseq, i);
        if (!PyTuple_Check(tup) || PyTuple_Size(tup) != 3) {
            PyErr_SetString(PyExc_TypeError, "hypothesis must be a tuple");
            FreeHypos(hypos, i);
            return NULL;
        }

        PyObject *t0 = PyTuple_GET_ITEM(tup, 0);
        if (!PyBool_Check(t0)) {
            PyErr_SetString(PyExc_TypeError, "hypothesis bind type must be an boolean");
            FreeHypos(hypos, i);
            return NULL;
        }
        hypos[i].bind_type = t0 == Py_True ? PGF_BIND_TYPE_EXPLICIT : PGF_BIND_TYPE_IMPLICIT;

        PyObject *t1 = PyTuple_GET_ITEM(tup, 1);
        if (!PyUnicode_Check(t1)) {
            PyErr_SetString(PyExc_TypeError, "hypothesis variable must be a string");
            FreeHypos(hypos, i);
            return NULL;
        }
        hypos[i].cid = PyUnicode_AsPgfText(t1);

        PyObject *t2 = PyTuple_GET_ITEM(tup, 2);
        if (!PyObject_TypeCheck(t2, &pgf_TypeType)) {
            PyErr_SetString(PyExc_TypeError, "hypothesis type must be a Type");
            FreePgfText(hypos[i].cid);
            FreeHypos(hypos, i);
            return NULL;
        }
        hypos[i].type = (PgfType) t2;

        Py_INCREF(hypos[i].type);
    }

    return hypos;
}

PyObject *
PyTuple_FromHypos(PgfTypeHypo *hypos, const size_t n_hypos)
{
    PyObject *pytuple = PyTuple_New(n_hypos);
    if (pytuple == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < n_hypos; i++) {
        PyObject *tup = PyTuple_New(3);
        PyObject *bt = hypos[i].bind_type == 0 ? Py_True : Py_False;
        PyTuple_SET_ITEM(tup, 0, bt);
        PyTuple_SET_ITEM(tup, 1, PyUnicode_FromStringAndSize(hypos[i].cid->text, hypos[i].cid->size));
        PyTuple_SET_ITEM(tup, 2, (PyObject *)hypos[i].type);
        Py_INCREF(bt);
        Py_INCREF(hypos[i].type);
        PyTuple_SET_ITEM(pytuple, i, tup);
    }
    if (PyErr_Occurred()) {
        Py_DECREF(pytuple);
        return NULL;
    }

    return pytuple;
}

PgfPrintContext *
PyList_AsPgfPrintContext(PyObject *pylist)
{
    PgfPrintContext *ctxt = NULL;
    for (Py_ssize_t i = PyList_Size(pylist); i > 0 ; i--) {
        PyObject *item = PyList_GetItem(pylist, i-1);
        if (!PyUnicode_Check(item)) {
            PyErr_SetString(PyExc_TypeError, "variable argument in context must be a string");
            return NULL;
        }

        if (PyUnicode_READY(item) != 0) {
            return NULL;
        }

        Py_ssize_t size;
        const char *enc = PyUnicode_AsUTF8AndSize(item, &size);

        PgfPrintContext *this = (PgfPrintContext *)PyMem_RawMalloc(sizeof(PgfPrintContext) + size + 1);
        this->next = ctxt;
        this->name.size = size;
        memcpy(&this->name.text, enc, size+1);
        ctxt = this;
    }

    return ctxt;
}

// ----------------------------------------------------------------------------
// freers

void
FreePgfText(PgfText *txt)
{
    PyMem_RawFree(txt);
}

void
FreeHypos(PgfTypeHypo *hypos, Py_ssize_t n_hypos)
{
    for (Py_ssize_t i = 0; i < n_hypos; i++) {
        FreePgfText(hypos[i].cid);
        Py_DECREF(hypos[i].type);
    }
    PyMem_RawFree(hypos);
}

void
FreePgfPrintContext(PgfPrintContext *ctxt)
{
    if (ctxt == NULL) return;
    FreePgfPrintContext(ctxt->next);
    PyMem_RawFree(ctxt);
}

// ----------------------------------------------------------------------------
// unmarshaller

static PgfExpr
eabs(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body)
{
    ExprAbsObject *pyexpr = (ExprAbsObject *)pgf_ExprAbsType.tp_alloc(&pgf_ExprAbsType, 0);
    pyexpr->bind_type = btype == 0 ? Py_True : Py_False;
    pyexpr->name = PyUnicode_FromPgfText(name);
    pyexpr->body = (ExprObject *)body;
    Py_INCREF(pyexpr->bind_type);
    Py_INCREF(body);
    return (PgfExpr) pyexpr;
}

static PgfExpr
eapp(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg)
{
    ExprAppObject *pyexpr = (ExprAppObject *)pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
    pyexpr->fun = (ExprObject *)fun;
    pyexpr->arg = (ExprObject *)arg;
    Py_INCREF(fun);
    Py_INCREF(arg);
    return (PgfExpr) pyexpr;
}

static PgfExpr
elit(PgfUnmarshaller *this, PgfLiteral lit)
{
    ExprLitObject *pyexpr = (ExprLitObject *)pgf_ExprLitType.tp_alloc(&pgf_ExprLitType, 0);
    PyObject *pyobj = (PyObject *)lit;
    pyexpr->lit = pyobj;
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
    pyexpr->name = PyUnicode_FromPgfText(name);
    return (PgfExpr) pyexpr;
}

static PgfExpr
evar(PgfUnmarshaller *this, int index)
{
    ExprVarObject *pyexpr = (ExprVarObject *)pgf_ExprVarType.tp_alloc(&pgf_ExprVarType, 0);
    pyexpr->var = PyLong_FromLong(index);
    return (PgfExpr) pyexpr;
}

static PgfExpr
etyped(PgfUnmarshaller *this, PgfExpr expr, PgfType typ)
{
    ExprTypedObject *pyexpr = (ExprTypedObject *)pgf_ExprTypedType.tp_alloc(&pgf_ExprTypedType, 0);
    pyexpr->expr = (ExprObject *)expr;
    pyexpr->type = (TypeObject *)typ;
    Py_INCREF(expr);
    Py_INCREF(typ);
    return (PgfExpr) pyexpr;
}

static PgfExpr
eimplarg(PgfUnmarshaller *this, PgfExpr expr)
{
    ExprImplArgObject *pyexpr = (ExprImplArgObject *)pgf_ExprImplArgType.tp_alloc(&pgf_ExprImplArgType, 0);
    pyexpr->expr = (ExprObject *)expr;
    Py_INCREF(expr);
    return (PgfExpr) pyexpr;
}

static PgfLiteral
lint(PgfUnmarshaller *this, size_t size, uintmax_t *v)
{
    if (size == 0) {
        return (PgfLiteral) PyLong_FromLong(0);
    } else {
        intmax_t *v0 = (intmax_t *)v;
        PyObject *i = PyLong_FromLong(*v0);
        PyObject *intShifter = PyLong_FromUnsignedLong(LINT_BASE);
        for (size_t n = 1; n < size; n++) {
            PyObject *tmp = PyNumber_Multiply(i, intShifter);
            Py_DECREF(i);
            i = tmp;
            PyObject *py_v = PyLong_FromUnsignedLong(v[n]);
            if (*v0 >= 0) {
                tmp = PyNumber_Add(i, py_v);
            } else {
                tmp = PyNumber_Subtract(i, py_v);
            }
            Py_DECREF(py_v);
            Py_DECREF(i);
            i = tmp;
        }
        Py_DECREF(intShifter);
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

    pytype->hypos = PyTuple_FromHypos(hypos, n_hypos);
    pytype->name = PyUnicode_FromStringAndSize(cat->text, cat->size);
    pytype->exprs = PyTuple_New(n_exprs);
    for (int i = 0; i < n_exprs; i++) {
        PyObject *expr = (PyObject *)exprs[i];
        PyTuple_SET_ITEM(pytype->exprs, i, expr);
        Py_INCREF(expr);
    }

    return (PgfType) pytype;
}

static void
free_ref(PgfUnmarshaller *this, object x)
{
    Py_XDECREF((PyObject *) x);
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
        PyObject *intShifter = PyLong_FromUnsignedLong(LINT_BASE);

        // determine size
        size_t size = 1;
        PyObject *x = PyNumber_Absolute(pyobj); // make a copy, ignore sign
        while (PyObject_RichCompareBool(x, intShifter, Py_GE) == 1) {
            size++;
            PyObject *tmp = PyNumber_FloorDivide(x, intShifter);
            Py_DECREF(x);
            x = tmp;
        }
        Py_DECREF(x);

        // chop up into chunks, always positive
        PyObject *zero = PyLong_FromLong(0);
        bool isPos = PyObject_RichCompareBool(pyobj, zero, Py_GE) == 1;
        Py_DECREF(zero);
        x = PyNumber_Absolute(pyobj); // make a copy, ignore sign
        uintmax_t *i = PyMem_RawMalloc(sizeof(uintmax_t)*size); // TODO when does this get freed?
        for (int n = size-1; n > 0; n--) {
            PyObject *rem = PyNumber_Remainder(x, intShifter);
            i[n] = PyLong_AsUnsignedLong(rem);
            Py_DECREF(rem);
            PyObject *tmp = PyNumber_FloorDivide(x, intShifter);
            Py_DECREF(x);
            x = tmp;
        }

        Py_DECREF(intShifter);

        // first chunk, re-applying polarity
        if (isPos)
            i[0] = PyLong_AsLong(x);
        else
            i[0] = -PyLong_AsLong(x);

        Py_DECREF(x);

        if (PyErr_Occurred()) {
            return 0;
        }

        object res = u->vtbl->lint(u, size, i);

        PyMem_RawFree(i);

        return res;
    } else if (PyFloat_Check(pyobj)) {
        double d = PyFloat_AsDouble(pyobj);
        return u->vtbl->lflt(u, d);
    } else if (PyUnicode_Check(pyobj)) {
        PgfText *t = PyUnicode_AsPgfText(pyobj);
        object res = u->vtbl->lstr(u, t);
        FreePgfText(t);
        return res;
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
        long bt = eabs->bind_type == Py_True ? 0 : 1;
        PgfText *name = PyUnicode_AsPgfText(eabs->name);
        object res = u->vtbl->eabs(u, bt, name, (PgfExpr) eabs->body);
        FreePgfText(name);
        return res;
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprAppType)) {
        ExprAppObject *eapp = (ExprAppObject *)expr;
        return u->vtbl->eapp(u, (PgfExpr) eapp->fun, (PgfExpr) eapp->arg);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprLitType)) {
        ExprLitObject *elit = (ExprLitObject *)expr;
        return this->vtbl->match_lit(this, u, (PgfLiteral) elit->lit);
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprMetaType)) {
        ExprMetaObject *emeta = (ExprMetaObject *)expr;
        return u->vtbl->emeta(u, (PgfMetaId) PyLong_AsLong(emeta->id));
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprFunType)) {
        ExprFunObject *efun = (ExprFunObject *)expr;
        PgfText *name = PyUnicode_AsPgfText(efun->name);
        object res = u->vtbl->efun(u, name);
        FreePgfText(name);
        return res;
    } else
    if (PyObject_TypeCheck(pyobj, &pgf_ExprVarType)) {
        ExprVarObject *evar = (ExprVarObject *)expr;
        return u->vtbl->evar(u, PyLong_AsLong(evar->var));
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

    Py_ssize_t n_hypos;
    PgfTypeHypo *hypos = PySequence_AsHypos(type->hypos, &n_hypos);
    if (hypos == NULL) {
        return 0;
    }

    PgfText *cat = PyUnicode_AsPgfText(type->name);
    if (cat == NULL) {
        return 0;
    }

    Py_ssize_t n_exprs = PySequence_Size(type->exprs);
    PgfExpr exprs[n_exprs];
    for (Py_ssize_t i = 0; i < n_exprs; i++) {
        exprs[i] = (PgfExpr)PySequence_GetItem(type->exprs, i);
        Py_INCREF(exprs[i]);
    }

    object res = u->vtbl->dtyp(u, n_hypos, hypos, cat, n_exprs, exprs);

    for (Py_ssize_t i = 0; i < n_exprs; i++) {
        Py_DECREF(exprs[i]);
    }
    FreeHypos(hypos, n_hypos);
    FreePgfText(cat);

    return res;
}

static PgfMarshallerVtbl marshallerVtbl =
{
    match_lit,
    match_expr,
    match_type
};

PgfMarshaller marshaller = { &marshallerVtbl };
