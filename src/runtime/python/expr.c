#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdbool.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./marshaller.h"

// ----------------------------------------------------------------------------

static TypeObject *
Type_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    TypeObject* self = (TypeObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
Type_init(TypeObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* hypos;
    PyObject* cat;
    PyObject* exprs;
    if (!PyArg_ParseTuple(args, "O!UO!", &PyList_Type, &hypos, &cat, &PyList_Type, &exprs)) {
        return -1;
    }

    for (Py_ssize_t i = 0; i < PyList_Size(hypos); i++) {
        if (!PyObject_TypeCheck(PyList_GetItem(hypos, i), &PyTuple_Type)) {
            PyErr_SetString(PyExc_TypeError, "invalid hypo in Type_init");
            return -1;
        }
        // TODO check tuple elements
        // Py_INCREF(&hypos[i]);
    }
    for (Py_ssize_t i = 0; i < PyList_Size(exprs); i++) {
        if (!PyObject_TypeCheck(PyList_GetItem(exprs, i), &pgf_ExprType)) {
            PyErr_SetString(PyExc_TypeError, "invalid expression in Type_init");
            return -1;
        }
        // Py_INCREF(&exprs[i]);
    }
    self->hypos = hypos;
    self->cat = cat;
    self->exprs = exprs;
    Py_INCREF(hypos);
    // Py_INCREF(cat);
    Py_INCREF(exprs);

    return 0;
}

static PyObject *
Type_str(TypeObject *self)
{
    PgfText *s = pgf_print_type((PgfType) self, NULL, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
}

static PyObject *
Type_richcompare(TypeObject *t1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_TypeType)) goto done;
    TypeObject *t2 = (TypeObject *)p2;

    if (PyUnicode_Compare(t1->cat, t2->cat) != 0) goto done;

    if (PyList_Size(t1->hypos) != PyList_Size(t2->hypos)) goto done;
    for (Py_ssize_t n = 0; n < PyList_Size(t1->hypos); n++) {
        PyObject *h1 = PyList_GetItem(t1->hypos, n);
        PyObject *h2 = PyList_GetItem(t2->hypos, n);
        if (!PyObject_RichCompareBool(h1, h2, Py_EQ))
            goto done;
    }

    if (PyList_Size(t1->exprs) != PyList_Size(t2->exprs)) goto done;
    for (Py_ssize_t n = 0; n < PyList_Size(t1->exprs); n++) {
        PyObject *e1 = PyList_GetItem(t1->exprs, n);
        PyObject *e2 = PyList_GetItem(t2->exprs, n);
        if (!PyObject_RichCompareBool(e1, e2, Py_EQ))
            goto done;
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
    (initproc) Type_init,      /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) Type_new,        /*tp_new */
};

// ----------------------------------------------------------------------------

static PyObject *
Expr_str(ExprObject *self)
{
    PgfText *s = pgf_print_expr((PgfExpr) self, NULL, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
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
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    0,                         /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
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

static ExprAbsObject *
ExprAbs_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprAbsObject* self = (ExprAbsObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprAbs_init(ExprAbsObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* bindType = NULL;
    PyObject* var = NULL;
    ExprObject* expr = NULL;
    if (!PyArg_ParseTuple(args, "O!UO!", &PyLong_Type, &bindType, &var, &pgf_ExprType, &expr)) {
        return -1;
    }
    self->bindType = bindType;
    self->var = var;
    self->expr = expr;
    Py_INCREF(bindType);
    Py_INCREF(var);
    Py_INCREF(expr);
    return 0;
}

static PyObject *
ExprAbs_richcompare(ExprAbsObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprAbsType)) goto done;
    ExprAbsObject *e2 = (ExprAbsObject *)p2;
    if (!PyObject_RichCompareBool(e1->bindType, e2->bindType, Py_EQ)) goto done;
    if (PyUnicode_Compare(e1->var, e2->var) != 0) goto done;
    if (!PyObject_RichCompareBool((PyObject*)e1->expr, (PyObject*)e2->expr, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprAbsType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprAbs",                /*tp_name*/
    sizeof(ExprAbsObject),        /*tp_basicsize*/
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
    "lambda abstraction",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprAbs_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprAbs_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprAbs_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprAppObject *
ExprApp_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprAppObject* self = (ExprAppObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprApp_init(ExprAppObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* e1 = NULL;
    PyObject* e2 = NULL;
    if (!PyArg_ParseTuple(args, "O!O!", &pgf_ExprType, &e1, &pgf_ExprType, &e2)) {
        return -1;
    }
    self->e1 = (ExprObject *)e1;
    self->e2 = (ExprObject *)e2;
    Py_INCREF(e1);
    Py_INCREF(e2);
    return 0;
}

static PyObject *
ExprApp_richcompare(ExprAppObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprAppType)) goto done;
    ExprAppObject *e2 = (ExprAppObject *)p2;
    if (!PyObject_RichCompareBool((PyObject*)e1->e1, (PyObject*)e2->e1, Py_EQ)) goto done;
    if (!PyObject_RichCompareBool((PyObject*)e1->e2, (PyObject*)e2->e2, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprAppType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprApp",                /*tp_name*/
    sizeof(ExprAppObject),        /*tp_basicsize*/
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
    "application",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprApp_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprApp_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprApp_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprLitObject *
ExprLit_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprLitObject* self = (ExprLitObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprLit_init(ExprLitObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "O", &lit)) {
        return -1;
    }
    if (PyLong_Check(lit) || PyFloat_Check(lit) || PyUnicode_Check(lit)) {
        self->value = lit;
        Py_INCREF(lit);
        return 0;
    } else {
        PyErr_SetString(PyExc_TypeError, "invalid argument in ExprLit_init");
        return -1;
    }
}

static PyObject *
ExprLit_richcompare(ExprLitObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprLitType)) goto done;
    ExprLitObject *e2 = (ExprLitObject *)p2;

    if (PyLong_Check(e1->value)) {
        if (!PyLong_Check(e2->value)) goto done;
        int o1, o2;
        int l1 = PyLong_AsLongAndOverflow(e1->value, &o1);
        int l2 = PyLong_AsLongAndOverflow(e2->value, &o2);
        if (!(l1 == l2 && o1 == o2)) goto done;
    } else if (PyFloat_Check(e1->value)) {
        if (!PyFloat_Check(e2->value)) goto done;
        if (PyFloat_AsDouble(e1->value) != PyFloat_AsDouble(e2->value)) goto done;
    } else if (PyUnicode_Check(e1->value)) {
        if (!PyUnicode_Check(e2->value)) goto done;
        if (PyUnicode_Compare(e1->value, e2->value) != 0) goto done;
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
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprLit_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprLit_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprMetaObject *
ExprMeta_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprMetaObject* self = (ExprMetaObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprMeta_init(ExprMetaObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "|O", &lit)) {
        return -1;
    }
    if (lit == NULL) {
        self->id = PyLong_FromLong(0);
        return 0;
    } else if (PyLong_Check(lit)) {
        self->id = lit;
        Py_INCREF(lit);
        return 0;
    } else {
        PyErr_SetString(PyExc_TypeError, "invalid argument in ExprMeta_init");
        return -1;
    }
}

static PyObject *
ExprMeta_richcompare(ExprMetaObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprMetaType)) goto done;
    ExprMetaObject *e2 = (ExprMetaObject *)p2;
    if (!PyObject_RichCompareBool(e1->id, e2->id, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprMetaType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprMeta",                /*tp_name*/
    sizeof(ExprMetaObject),        /*tp_basicsize*/
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
    "meta variable",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprMeta_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprMeta_init,  /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprMeta_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprFunObject *
ExprFun_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprFunObject* self = (ExprFunObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprFun_init(ExprFunObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "U", &lit)) {
        return -1;
    }
    self->name = lit;
    Py_INCREF(lit);
    return 0;
}

static PyObject *
ExprFun_richcompare(ExprFunObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprFunType)) goto done;
    ExprFunObject *e2 = (ExprFunObject *)p2;
    if (PyUnicode_Compare(e1->name, e2->name) != 0) goto done;

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
PyTypeObject pgf_ExprFunType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprFun",                /*tp_name*/
    sizeof(ExprFunObject),        /*tp_basicsize*/
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
    "function or data constructor",    /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprFun_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprFun_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprFun_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprVarObject *
ExprVar_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprVarObject* self = (ExprVarObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprVar_init(ExprVarObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "|O", &lit)) {
        return -1;
    }
    if (lit == NULL) {
        self->index = PyLong_FromLong(0);
        return 0;
    } else if (PyLong_Check(lit)) {
        self->index = lit;
        Py_INCREF(lit);
        return 0;
    } else {
        PyErr_SetString(PyExc_TypeError, "invalid argument in ExprVar_init");
        return -1;
    }
}

static PyObject *
ExprVar_richcompare(ExprVarObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprVarType)) goto done;
    ExprVarObject *e2 = (ExprVarObject *)p2;
    if (!PyObject_RichCompareBool(e1->index, e2->index, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprVarType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprVar",                /*tp_name*/
    sizeof(ExprVarObject),        /*tp_basicsize*/
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
    "variable",                 /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprVar_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprVar_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprVar_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprTypedObject *
ExprTyped_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprTypedObject* self = (ExprTypedObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprTyped_init(ExprTypedObject *self, PyObject *args, PyObject *kwds)
{
    ExprObject* expr;
    TypeObject* type;
    if (!PyArg_ParseTuple(args, "O!O!", &pgf_ExprType, &expr, &pgf_TypeType, &type)) {
        return -1;
    }
    self->expr = expr;
    self->type = type;
    Py_INCREF(expr);
    Py_INCREF(type);
    return 0;
}

static PyObject *
ExprTyped_richcompare(ExprTypedObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprTypedType)) goto done;
    ExprTypedObject *e2 = (ExprTypedObject *)p2;
    if (!PyObject_RichCompareBool((PyObject*)e1->expr, (PyObject*)e2->expr, Py_EQ)) goto done;
    if (!PyObject_RichCompareBool((PyObject*)e1->type, (PyObject*)e2->type, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprTypedType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprTyped",                /*tp_name*/
    sizeof(ExprTypedObject),        /*tp_basicsize*/
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
    "expression with local type signature", /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprTyped_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprTyped_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprTyped_new,     /*tp_new */
};

// ----------------------------------------------------------------------------

static ExprImplArgObject *
ExprImplArg_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    ExprImplArgObject* self = (ExprImplArgObject *)subtype->tp_alloc(subtype, 0);
    return self;
}

static int
ExprImplArg_init(ExprImplArgObject *self, PyObject *args, PyObject *kwds)
{
    ExprObject* expr;
    if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &expr)) {
        return -1;
    }
    self->expr = expr;
    Py_INCREF(expr);
    return 0;
}

static PyObject *
ExprImplArg_richcompare(ExprImplArgObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprImplArgType)) goto done;
    ExprImplArgObject *e2 = (ExprImplArgObject *)p2;
    if (!PyObject_RichCompareBool((PyObject*)e1->expr, (PyObject*)e2->expr, Py_EQ)) goto done;

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
PyTypeObject pgf_ExprImplArgType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprImplArg",                /*tp_name*/
    sizeof(ExprImplArgObject),        /*tp_basicsize*/
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
    "implicit argument in expression", /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    (richcmpfunc) ExprImplArg_richcompare, /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    0, //Expr_methods,              /*tp_methods */
    0,                         /*tp_members */
    0, //Expr_getseters,            /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprImplArg_init,   /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) ExprImplArg_new,     /*tp_new */
};
