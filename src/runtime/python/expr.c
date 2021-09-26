#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdbool.h>
#include <structmember.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"

// ----------------------------------------------------------------------------
// types

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
    PyObject* name;
    PyObject* exprs;
    if (!PyArg_ParseTuple(args, "O!UO!", &PyList_Type, &hypos, &name, &PyList_Type, &exprs)) {
        return -1;
    }

    for (Py_ssize_t i = 0; i < PyList_Size(hypos); i++) {
        PyObject *tup = PyList_GetItem(hypos, i);
        if (!PyObject_TypeCheck(tup, &PyTuple_Type)) {
            PyErr_SetString(PyExc_TypeError, "invalid hypo in Type_init: not a tuple");
            return -1;
        }
        if (!PyLong_Check(PyTuple_GetItem(tup, 0))) {
            PyErr_SetString(PyExc_TypeError, "invalid hypo in Type_init: bind type not an integer");
            return -1;
        }
        if (!PyUnicode_Check(PyTuple_GetItem(tup, 1))) {
            PyErr_SetString(PyExc_TypeError, "invalid hypo in Type_init: variable not a string");
            return -1;
        }
        if (!PyObject_TypeCheck(PyTuple_GetItem(tup, 2), &pgf_TypeType)) {
            PyErr_SetString(PyExc_TypeError, "invalid hypo in Type_init: type not a type");
            return -1;
        }
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
    self->name = name;
    self->exprs = exprs;
    Py_INCREF(self->hypos);
    Py_INCREF(self->name);
    Py_INCREF(self->exprs);

    return 0;
}

static void
Type_dealloc(TypeObject *self)
{
    Py_XDECREF(self->hypos);
    Py_XDECREF(self->name);
    Py_XDECREF(self->exprs);
    Py_TYPE(self)->tp_free(self);
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

    if (PyUnicode_Compare(t1->name, t2->name) != 0) goto done;

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
    {NULL}  /* Sentinel */
};

static PyMemberDef Type_members[] = {
    {"hypos", T_OBJECT_EX, offsetof(TypeObject, hypos), 0, "list of hypotheses in the type signature"},
    {"cat", T_OBJECT_EX, offsetof(TypeObject, name), 0, "name of the category"},
    {"exprs", T_OBJECT_EX, offsetof(TypeObject, exprs), 0, "list of indices for the category"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_TypeType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Type",                /*tp_name*/
    sizeof(TypeObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) Type_dealloc, /*tp_dealloc*/
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
    0, //(getattrofunc) Type_getattro, /*tp_getattro*/
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
    Type_members,              /*tp_members */
    0, //Type_getseters,            /*tp_getset */
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
// expressions

static PyObject *
Expr_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    Py_ssize_t tuple_size = PyTuple_Size(args);

	if (tuple_size == 0) {
        return pgf_ExprMetaType.tp_alloc(&pgf_ExprMetaType, 0);
	} else if (tuple_size == 1) {
		return pgf_ExprLitType.tp_alloc(&pgf_ExprLitType, 0);
	} else if (tuple_size == 2) {
		return pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
	} else {
		PyErr_Format(PyExc_TypeError, "function takes 0, 1 or 2 arguments (%d given)", (int) tuple_size);
		return NULL;
	}
}

static PyObject *
Expr_str(ExprObject *self)
{
    PgfText *s = pgf_print_expr((PgfExpr) self, NULL, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
}

static PyObject*
Expr_reduce_ex(ExprObject* self, PyObject *args)
{
	int protocol;
	if (!PyArg_ParseTuple(args, "i", &protocol))
		return NULL;

	PyObject* myModule = PyImport_ImportModule("pgf");
	if (myModule == NULL)
		return NULL;
	PyObject* py_readExpr = PyObject_GetAttrString(myModule, "readExpr");
	Py_DECREF(myModule);
	if (py_readExpr == NULL)
		return NULL;

	PyObject* py_str = Expr_str(self);
	if (py_str == NULL) {
		Py_DECREF(py_readExpr);
		return NULL;
	}

	PyObject* py_tuple =
		Py_BuildValue("O(O)", py_readExpr, py_str);

	Py_DECREF(py_str);
	Py_DECREF(py_readExpr);

	return py_tuple;
}

static PyMethodDef Expr_methods[] = {
    {"__reduce_ex__", (PyCFunction)Expr_reduce_ex, METH_VARARGS,
     "This method allows for transparent pickling/unpickling of expressions."
    },
    {NULL}  /* Sentinel */
};

static PyGetSetDef Expr_getseters[] = {
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Expr",                /*tp_name*/
    sizeof(ExprObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0,                         /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0, //(hashfunc) Expr_hash,      /*tp_hash */
    0,                         /*tp_call*/
    (reprfunc) Expr_str,       /*tp_str*/
    0,                         /*tp_getattro*/
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
    0,                         /*tp_init */
    0,                         /*tp_alloc */
    Expr_new,                  /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprAbs_init(ExprAbsObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* bind_type = NULL;
    PyObject* name = NULL;
    ExprObject* body = NULL;
    if (!PyArg_ParseTuple(args, "O!UO!", &PyLong_Type, &bind_type, &name, &pgf_ExprType, &body)) {
        return -1;
    }
    self->bind_type = bind_type;
    self->name = name;
    self->body = body;
    Py_INCREF(self->bind_type);
    Py_INCREF(self->name);
    Py_INCREF(self->body);
    return 0;
}

static void
ExprAbs_dealloc(ExprAbsObject *self)
{
    Py_XDECREF(self->bind_type);
    Py_XDECREF(self->name);
    Py_XDECREF(self->body);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
ExprAbs_richcompare(ExprAbsObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprAbsType)) goto done;
    ExprAbsObject *e2 = (ExprAbsObject *)p2;
    if (!PyObject_RichCompareBool(e1->bind_type, e2->bind_type, Py_EQ)) goto done;
    if (PyUnicode_Compare(e1->name, e2->name) != 0) goto done;
    if (!PyObject_RichCompareBool((PyObject*)e1->body, (PyObject*)e2->body, Py_EQ)) goto done;

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

static PyMemberDef ExprAbs_members[] = {
    {"bind_type", T_OBJECT_EX, offsetof(ExprAbsObject, bind_type), 0, "bind type (explicit or implicit)"},
    {"name", T_OBJECT_EX, offsetof(ExprAbsObject, name), 0, "name of the abstraction"},
    {"body", T_OBJECT_EX, offsetof(ExprAbsObject, body), 0, "body of the abstraction"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprAbsType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprAbs",                /*tp_name*/
    sizeof(ExprAbsObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprAbs_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "lambda abstraction",      /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprAbs_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprAbs_members,           /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprAbs_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprApp_init(ExprAppObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* fun = NULL;
    PyObject* arg = NULL;
    if (!PyArg_ParseTuple(args, "O!O!", &pgf_ExprType, &fun, &pgf_ExprType, &arg)) {
        return -1;
    }
    self->fun = (ExprObject *)fun;
    self->arg = (ExprObject *)arg;
    Py_INCREF(self->fun);
    Py_INCREF(self->arg);
    return 0;
}

static void
ExprApp_dealloc(ExprAppObject *self)
{
    Py_XDECREF(self->fun);
    Py_XDECREF(self->arg);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
ExprApp_richcompare(ExprAppObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprAppType)) goto done;
    ExprAppObject *e2 = (ExprAppObject *)p2;
    if (!PyObject_RichCompareBool((PyObject*)e1->fun, (PyObject*)e2->fun, Py_EQ)) goto done;
    if (!PyObject_RichCompareBool((PyObject*)e1->arg, (PyObject*)e2->arg, Py_EQ)) goto done;

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

static PyMemberDef ExprApp_members[] = {
    {"fun", T_OBJECT_EX, offsetof(ExprAppObject, fun), 0, "the function in a function application"},
    {"arg", T_OBJECT_EX, offsetof(ExprAppObject, arg), 0, "the argument in a function application"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprAppType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprApp",             /*tp_name*/
    sizeof(ExprAppObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprApp_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "application",             /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprApp_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprApp_members,           /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprApp_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprLit_init(ExprLitObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "O", &lit)) {
        return -1;
    }
    if (PyLong_Check(lit) || PyFloat_Check(lit) || PyUnicode_Check(lit)) {
        self->lit = lit;
        Py_INCREF(self->lit);
        return 0;
    } else {
        PyErr_SetString(PyExc_TypeError, "a literal expression can be initialised with an integer, float, or string");
        return -1;
    }
}

static void
ExprLit_dealloc(ExprLitObject *self)
{
    Py_XDECREF(self->lit);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
ExprLit_richcompare(ExprLitObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprLitType)) goto done;
    ExprLitObject *e2 = (ExprLitObject *)p2;

    if (PyLong_Check(e1->lit)) {
        if (!PyLong_Check(e2->lit)) goto done;
        int o1, o2;
        int l1 = PyLong_AsLongAndOverflow(e1->lit, &o1);
        int l2 = PyLong_AsLongAndOverflow(e2->lit, &o2);
        if (!(l1 == l2 && o1 == o2)) goto done;
    } else if (PyFloat_Check(e1->lit)) {
        if (!PyFloat_Check(e2->lit)) goto done;
        if (PyFloat_AsDouble(e1->lit) != PyFloat_AsDouble(e2->lit)) goto done;
    } else if (PyUnicode_Check(e1->lit)) {
        if (!PyUnicode_Check(e2->lit)) goto done;
        if (PyUnicode_Compare(e1->lit, e2->lit) != 0) goto done;
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

static PyMemberDef ExprLit_members[] = {
    {"val", T_OBJECT_EX, offsetof(ExprLitObject, lit), 0, "the value of the literal"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprLitType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprLit",             /*tp_name*/
    sizeof(ExprLitObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprLit_dealloc,  /*tp_dealloc*/
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
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "literal",                 /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprLit_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprLit_members,           /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprLit_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprMeta_init(ExprMetaObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "|O", &lit)) {
        return -1;
    }
    if (lit == NULL) {
        self->id = PyLong_FromLong(0);
        Py_INCREF(self->id);
        return 0;
    } else if (PyLong_Check(lit)) {
        self->id = lit;
        Py_INCREF(self->id);
        return 0;
    } else {
        // TODO check positive
        PyErr_SetString(PyExc_TypeError, "a meta variable must be initialised with an integer");
        return -1;
    }
}

static void
ExprMeta_dealloc(ExprMetaObject *self)
{
    Py_XDECREF(self->id);
    Py_TYPE(self)->tp_free(self);
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

static PyMemberDef ExprMeta_members[] = {
    {"id", T_OBJECT_EX, offsetof(ExprMetaObject, id), 0, "the id of a meta variable"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprMetaType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprMeta",            /*tp_name*/
    sizeof(ExprMetaObject),    /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprMeta_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "meta variable",           /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprMeta_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprMeta_members,          /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprMeta_init,  /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprFun_init(ExprFunObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "U", &lit)) {
        return -1;
    }
    self->name = lit;
    Py_INCREF(self->name);
    return 0;
}

static void
ExprFun_dealloc(ExprFunObject *self)
{
    Py_XDECREF(self->name);
    Py_TYPE(self)->tp_free(self);
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

static PyMemberDef ExprFun_members[] = {
    {"name", T_OBJECT_EX, offsetof(ExprFunObject, name), 0, "the name of the function"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprFunType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprFun",             /*tp_name*/
    sizeof(ExprFunObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprFun_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "function or data constructor",    /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprFun_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprFun_members,           /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprFun_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprVar_init(ExprVarObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* lit = NULL;
    if (!PyArg_ParseTuple(args, "|O", &lit)) {
        return -1;
    }
    if (lit == NULL) {
        self->var = PyLong_FromLong(0);
        Py_INCREF(self->var);
        return 0;
    } else if (PyLong_Check(lit)) {
        self->var = lit;
        Py_INCREF(self->var);
        return 0;
    } else {
        // TODO check positive
        PyErr_SetString(PyExc_TypeError, "variable expression must be initialised with an integer");
        return -1;
    }
}

static void
ExprVar_dealloc(ExprVarObject *self)
{
    Py_XDECREF(self->var);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
ExprVar_richcompare(ExprVarObject *e1, PyObject *p2, int op)
{
    bool same = false;
    if (!PyObject_TypeCheck(p2, &pgf_ExprVarType)) goto done;
    ExprVarObject *e2 = (ExprVarObject *)p2;
    if (!PyObject_RichCompareBool(e1->var, e2->var, Py_EQ)) goto done;

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

static PyMemberDef ExprVar_members[] = {
    {"index", T_OBJECT_EX, offsetof(ExprVarObject, var), 0, "the de Bruijn index of a variable"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprVarType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprVar",             /*tp_name*/
    sizeof(ExprVarObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprVar_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "variable",                /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprVar_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprVar_members,           /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprVar_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

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
    Py_INCREF(self->expr);
    Py_INCREF(self->type);
    return 0;
}

static void
ExprTyped_dealloc(ExprTypedObject *self)
{
    Py_XDECREF(self->expr);
    Py_XDECREF(self->type);
    Py_TYPE(self)->tp_free(self);
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

static PyMemberDef ExprTyped_members[] = {
    {"expr", T_OBJECT_EX, offsetof(ExprTypedObject, expr), 0, "the expression"},
    {"type", T_OBJECT_EX, offsetof(ExprTypedObject, type), 0, "the type"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprTypedType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprTyped",                /*tp_name*/
    sizeof(ExprTypedObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprTyped_dealloc,  /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0, //(ternaryfunc) Expr_call,   /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "expression with local type signature", /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprTyped_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprTyped_members,         /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprTyped_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprImplArg_init(ExprImplArgObject *self, PyObject *args, PyObject *kwds)
{
    ExprObject* expr;
    if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &expr)) {
        return -1;
    }
    self->expr = expr;
    Py_INCREF(self->expr);
    return 0;
}

static void
ExprImplArg_dealloc(ExprImplArgObject *self)
{
    Py_XDECREF(self->expr);
    Py_TYPE(self)->tp_free(self);
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

static PyMemberDef ExprImplArg_members[] = {
    {"expr", T_OBJECT_EX, offsetof(ExprImplArgObject, expr), 0, "the inner expression"},
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_ExprImplArgType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.ExprImplArg",                /*tp_name*/
    sizeof(ExprImplArgObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) ExprImplArg_dealloc,  /*tp_dealloc*/
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
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "implicit argument in expression", /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) ExprImplArg_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    0,                         /*tp_methods */
    ExprImplArg_members,       /*tp_members */
    0,                         /*tp_getset */
    &pgf_ExprType,             /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) ExprImplArg_init,   /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};
