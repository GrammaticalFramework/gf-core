#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdbool.h>

#include "./compat.h"
#include "./expr.h"
#include "./marshaller.h"

static PyObject *
Expr_str(ExprObject *self)
{
    PgfText *s = pgf_print_expr((PgfExpr) &self, NULL, 1, &marshaller);
    return PyString_FromStringAndSize(s->text, s->size);
}

static PyObject *
Expr_richcompare(ExprObject *e1, ExprObject *e2, int op)
{
    bool same = false;

    // TODO

    // same = true;
// done:

    if (op == Py_EQ) {
        if (same) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
    } else if (op == Py_NE) {
        if (same) Py_RETURN_FALSE; else Py_RETURN_TRUE;
    } else {
        PyErr_SetString(PyExc_TypeError, "comparison operation not supported");
        Py_RETURN_NOTIMPLEMENTED;
    }
}

static PyMethodDef Expr_methods[] = {
//     {"unpack", (PyCFunction)Expr_unpack, METH_VARARGS,
//      "Decomposes an expression into its components"
//     },
//     {"visit", (PyCFunction)Expr_visit, METH_VARARGS,
//      "Implementation of the visitor pattern for abstract syntax trees. "
//      "If e is an expression equal to f a1 .. an then "
//      "e.visit(self) calls method self.on_f(a1,..an). "
//      "If the method doesn't exist then the method self.default(e) "
//      "is called."
//     },
//     {"__reduce_ex__", (PyCFunction)Expr_reduce_ex, METH_VARARGS,
//      "This method allows for transparent pickling/unpickling of expressions."
//     },
    {NULL}  /* Sentinel */
};

static PyGetSetDef Expr_getseters[] = {
//     {"fun",
//      NULL, NULL,
//      "this is the function in a function application",
//      NULL},
//     {"arg",
//      NULL, NULL,
//      "this is the argument in a function application",
//      NULL},
//     {"val",
//      NULL, NULL,
//      "this is the value of a literal",
//      NULL},
//     {"id",
//      NULL, NULL,
//      "this is the id of a meta variable",
//      NULL},
//     {"name",
//      NULL, NULL,
//      "this is the name of a function",
//      NULL},
//     {"index",
//      NULL, NULL,
//      "this is the de Bruijn index of a variable",
//      NULL},
    {NULL}  /* Sentinel */
};

/* static */
PyTypeObject pgf_ExprType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Expr",                /*tp_name*/
    sizeof(ExprObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0, //(destructor)Expr_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0, //(hashfunc) Expr_hash,      /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    (reprfunc) Expr_str,      /*tp_str*/
    0, //(getattrofunc) Expr_getattro,/*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "abstract syntax tree",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) Expr_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    Expr_getseters,            /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0, //(initproc)Expr_init,       /*tp_init */
    0,                         /*tp_alloc */
    0, //(newfunc) Expr_new,        /*tp_new */
};

// ----------------------------------------------------------------------------

static PyObject *
ExprLit_richcompare(ExprLitObject *t1, ExprLitObject *t2, int op)
{
    bool same = false;
    if (t1->type != t2->type) goto done;

    if (t1->type == 0) {
        int o1, o2;
        int l1 = PyLong_AsLongAndOverflow(t1->value, &o1);
        int l2 = PyLong_AsLongAndOverflow(t2->value, &o2);
        if (!(l1 == l2 && o1 == o2)) goto done;
    } else if (t1->type == 1) {
        if (PyFloat_AsDouble(t1->value) != PyFloat_AsDouble(t2->value)) goto done;
    } else if (t1->type == 2) {
        if (PyString_Compare(t1->value, t2->value) != 0) goto done;
    } else {
        PyErr_SetString(PyExc_TypeError, "unknown literal type");
        return NULL;
    }

    same = true;
done:

    if (op == Py_EQ) {
        if (same) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
    } else if (op == Py_NE) {
        if (same) Py_RETURN_FALSE; else Py_RETURN_TRUE;
    } else {
        PyErr_SetString(PyExc_TypeError, "comparison operation not supported");
        Py_RETURN_NOTIMPLEMENTED;
    }
}

/* static */
PyTypeObject pgf_ExprLitType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprLit",                /*tp_name*/
    sizeof(ExprLitObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0, //(destructor)Expr_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0, //(hashfunc) Expr_hash,      /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0, //(reprfunc) Expr_str,      /*tp_str*/
    0, //(getattrofunc) Expr_getattro,/*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "literal",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprLit_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0, //(initproc)Expr_init,       /*tp_init */
    0,                         /*tp_alloc */
    0, //(newfunc) Expr_new,        /*tp_new */
};

// ----------------------------------------------------------------------------

static PyObject *
Type_str(TypeObject *self)
{
    PgfText *s = pgf_print_type((PgfType) &self, NULL, 1, &marshaller);
    return PyString_FromStringAndSize(s->text, s->size);
}

static PyObject *
Type_richcompare(TypeObject *t1, TypeObject *t2, int op)
{
    bool same = false;
    if (PyString_Compare(t1->cat, t2->cat) != 0) goto done;

    if (PyList_Size(t1->hypos) != PyList_Size(t2->hypos)) goto done;
    for (Py_ssize_t n = 0; n < PyList_Size(t1->hypos); n++) {
        PyObject *h1 = PyList_GetItem(t1->hypos, n);
        PyObject *h2 = PyList_GetItem(t2->hypos, n);
        if (PyTuple_GetItem(h1, 0) != PyTuple_GetItem(h2, 0)) goto done;
        if (PyString_Compare(PyTuple_GetItem(h1, 1), PyTuple_GetItem(h2, 1)) != 0) goto done;
        TypeObject *ht1 = (TypeObject *)PyTuple_GetItem(h1, 2);
        TypeObject *ht2 = (TypeObject *)PyTuple_GetItem(h2, 2);
        if (Type_richcompare(ht1, ht2, Py_EQ) != Py_True) goto done;
    }

    if (PyList_Size(t1->exprs) != PyList_Size(t2->exprs)) goto done;
    // for (Py_ssize_t n = 0; n < PyList_Size(t1->exprs); n++) {
    //     ExprObject *e1 = PyList_GetItem(t1->exprs, n);
    //     ExprObject *e2 = PyList_GetItem(t2->exprs, n);
    //     if (Expr_richcompare(e1, e2, Py_EQ) != Py_True) goto done; // TODO
    // }

    same = true;
done:

    if (op == Py_EQ) {
        if (same) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
    } else if (op == Py_NE) {
        if (same) Py_RETURN_FALSE; else Py_RETURN_TRUE;
    } else {
        PyErr_SetString(PyExc_TypeError, "comparison operation not supported");
        Py_RETURN_NOTIMPLEMENTED;
    }
}

static PyMethodDef Type_methods[] = {
//     {"unpack", (PyCFunction)Type_unpack, METH_VARARGS,
//      "Decomposes a type into its components"
//     },
//     {"__reduce_ex__", (PyCFunction)Type_reduce_ex, METH_VARARGS,
//      "This method allows for transparent pickling/unpickling of types."
//     },
    {NULL}  /* Sentinel */
};

static PyGetSetDef Type_getseters[] = {
//     {"hypos",
//      (getter)Type_getHypos, NULL,
//      "this is the list of hypotheses in the type signature",
//      NULL},
//     {"cat",
//      (getter)Type_getCat, NULL,
//      "this is the name of the category",
//      NULL},
//     {"exprs",
//      (getter)Type_getExprs, NULL,
//      "this is the list of indices for the category",
//      NULL},
    {NULL}  /* Sentinel */
};

/* static */
PyTypeObject pgf_TypeType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Type",                /*tp_name*/
    sizeof(TypeObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0, //(destructor) Type_dealloc, /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    (reprfunc) Type_str,       /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "abstract syntax type",    /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) Type_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    Type_methods,              /*tp_methods */
    0,                         /*tp_members */
    Type_getseters,            /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0, //(initproc) Type_init,      /*tp_init */
    0,                         /*tp_alloc */
    0, //(newfunc) Type_new,        /*tp_new */
};
