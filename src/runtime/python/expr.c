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
    PyObject* exprs = NULL;
    switch (PyTuple_Size(args)) {
    case 1:
        hypos = NULL;
        name  = PyTuple_GET_ITEM(args, 0);
        exprs = NULL;
        if (!PyUnicode_Check(name)) {
            PyErr_SetString(PyExc_TypeError, "category must be a string");
            return -1;
        }
        break;
    case 2:
        if (PySequence_Check(PyTuple_GET_ITEM(args, 0)) &&
            PyUnicode_Check(PyTuple_GET_ITEM(args, 1))) {
            hypos = PyTuple_GET_ITEM(args, 0);
            name  = PyTuple_GET_ITEM(args, 1);
            exprs = NULL;
        } else if (PyUnicode_Check(PyTuple_GET_ITEM(args, 0)) &&
                   PySequence_Check(PyTuple_GET_ITEM(args, 1))) {
            hypos = NULL;
            name  = PyTuple_GET_ITEM(args, 0);
            exprs = PyTuple_GET_ITEM(args, 1);
        } else {
            PyErr_SetString(PyExc_TypeError, "The arguments must be hypotheses and category or category and expressions");
            return -1;
        }
        break;
    case 3:
        hypos = PyTuple_GET_ITEM(args, 0);
        name  = PyTuple_GET_ITEM(args, 1);
        exprs = PyTuple_GET_ITEM(args, 2);

        if (!PySequence_Check(hypos)) {
            PyErr_SetString(PyExc_TypeError, "hypotheses must be a sequence");
            return -1;
        }
        if (!PyUnicode_Check(name)) {
            PyErr_SetString(PyExc_TypeError, "category must be a string");
            return -1;
        }
        if (!PySequence_Check(exprs)) {
            PyErr_SetString(PyExc_TypeError, "expressions must be a sequence");
            return -1;
        }
        break;
    default:
        PyErr_SetString(PyExc_TypeError, "1, 2 or 3 arguments are expected");
        return -1;
    }

    if (hypos != NULL) {
        Py_ssize_t n_hypos = PySequence_Size(hypos);
        self->hypos = PyTuple_New(n_hypos);
        for (Py_ssize_t i = 0; i < n_hypos; i++) {
            PyObject *item = PySequence_GetItem(hypos, i);
            if (PyTuple_Check(item) && PyTuple_Size(item) == 3) {
                if (!PyBool_Check(PyTuple_GetItem(item, 0))) {
                    PyErr_SetString(PyExc_TypeError, "hypothesis bind type must be a boolean");
                    return -1;
                }
                if (!PyUnicode_Check(PyTuple_GetItem(item, 1))) {
                    PyErr_SetString(PyExc_TypeError, "hypothesis variable must be a string");
                    return -1;
                }
                if (!PyObject_TypeCheck(PyTuple_GetItem(item, 2), &pgf_TypeType)) {
                    PyErr_SetString(PyExc_TypeError, "hypothesis type must be a Type");
                    return -1;
                }
                Py_INCREF(item);
                PyTuple_SET_ITEM(self->hypos, i, item);
            } else if (PyObject_TypeCheck(item, &pgf_TypeType)) {
                PyObject *wildcard = PyUnicode_FromString("_");
                PyObject *tuple = PyTuple_Pack(3, Py_True,
                                                  PyUnicode_FromString("_"),
                                                  item);
                Py_DECREF(wildcard);
                PyTuple_SET_ITEM(self->hypos, i, tuple);
            } else if (PyUnicode_Check(item)) {
                Py_INCREF(item);
                TypeObject *pytype = (TypeObject *)pgf_TypeType.tp_alloc(&pgf_TypeType, 0);
                pytype->hypos = PyTuple_New(0);
                pytype->name = item;
                pytype->exprs = PyTuple_New(0);

                PyObject *wildcard = PyUnicode_FromString("_");
                PyObject *tuple = PyTuple_Pack(3, Py_True,
                                                  wildcard,
                                                  pytype);
                Py_DECREF(wildcard);
                Py_DECREF(pytype);
                PyTuple_SET_ITEM(self->hypos, i, tuple);
            } else {
                PyErr_SetString(PyExc_TypeError, "Each hypothesis must be either a tuple of size 3, a type or a string");
                return -1;
            }
        }
    } else {
        self->hypos = PyTuple_New(0);
    }

    self->name = name;
    Py_INCREF(self->name);

    if (exprs != NULL) {
        Py_ssize_t n_exprs = PySequence_Size(exprs);
        self->exprs = PyTuple_New(n_exprs);
        for (Py_ssize_t i = 0; i < n_exprs; i++) {
            PyObject *expr = PySequence_GetItem(exprs, i);
            if (!PyObject_TypeCheck(expr, &pgf_ExprType)) {
                PyErr_SetString(PyExc_TypeError, "invalid expression in Type initialisation");
                return -1;
            }
            PyTuple_SET_ITEM(self->exprs, i, expr);
        }
    } else {
        self->exprs = PyTuple_New(0);
    }

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

    if (PyTuple_Size(t1->hypos) != PyTuple_Size(t2->hypos)) goto done;
    for (Py_ssize_t n = 0; n < PyTuple_Size(t1->hypos); n++) {
        PyObject *h1 = PyTuple_GetItem(t1->hypos, n);
        PyObject *h2 = PyTuple_GetItem(t2->hypos, n);
        if (!PyObject_RichCompareBool(h1, h2, Py_EQ))
            goto done;
    }

    if (PyTuple_Size(t1->exprs) != PyTuple_Size(t2->exprs)) goto done;
    for (Py_ssize_t n = 0; n < PyTuple_Size(t1->exprs); n++) {
        PyObject *e1 = PyTuple_GetItem(t1->exprs, n);
        PyObject *e2 = PyTuple_GetItem(t2->exprs, n);
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

static PyObject*
Type_unpack(TypeObject* self, PyObject *fargs)
{
	return Py_BuildValue("OOO", self->hypos, self->name, self->exprs);
}

static PyObject*
Type_reduce_ex(TypeObject* self, PyObject *args)
{
	int protocol;
	if (!PyArg_ParseTuple(args, "i", &protocol))
		return NULL;

	PyObject* myModule = PyImport_ImportModule("pgf");
	if (myModule == NULL)
		return NULL;
	PyObject* py_readType = PyObject_GetAttrString(myModule, "readType");
	Py_DECREF(myModule);
	if (py_readType == NULL)
		return NULL;

	PyObject* py_str = Type_str(self);
	if (py_str == NULL) {
		Py_DECREF(py_readType);
		return NULL;
	}

	PyObject* py_tuple =
		Py_BuildValue("O(O)", py_readType, py_str);

	Py_DECREF(py_str);
	Py_DECREF(py_readType);

	return py_tuple;
}

static PyMethodDef Type_methods[] = {
    {"unpack", (PyCFunction)Type_unpack, METH_VARARGS,
     "Decomposes a type into its components"
    },
    {"__reduce_ex__", (PyCFunction)Type_reduce_ex, METH_VARARGS,
     "This method allows for transparent pickling/unpickling of types."
    },
    {NULL}  /* Sentinel */
};

static PyMemberDef Type_members[] = {
    {"hypos", T_OBJECT_EX, offsetof(TypeObject, hypos), READONLY, "hypotheses in the type signature"},
    {"cat", T_OBJECT_EX, offsetof(TypeObject, name), READONLY, "name of the category"},
    {"exprs", T_OBJECT_EX, offsetof(TypeObject, exprs), READONLY, "indices for the category"},
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
// hypos

// static HypoObject *
// Hypo_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
// {
//     HypoObject *self = (HypoObject *)subtype->tp_alloc(subtype, 0);
//     return self;
// }
//
// static int
// Hypo_init(HypoObject *self, PyObject *args, PyObject *kwds)
// {
//     PyObject *bind_type;
//     PyObject *cid;
//     TypeObject *type;
//     if (!PyArg_ParseTuple(args, "O!UO!", &PyBool_Type, &bind_type, &cid, &pgf_TypeType, &type)) {
//         return -1;
//     }
//
//     self->bind_type = bind_type;
//     self->cid = cid;
//     self->type = type;
//     Py_INCREF(self->bind_type);
//     Py_INCREF(self->cid);
//     Py_INCREF(self->type);
//
//     return 0;
// }
//
// static void
// Hypo_dealloc(HypoObject *self)
// {
//     Py_XDECREF(self->bind_type);
//     Py_XDECREF(self->cid);
//     Py_XDECREF(self->type);
//     Py_TYPE(self)->tp_free(self);
// }
//
// static PyObject *
// Hypo_richcompare(HypoObject *t1, PyObject *p2, int op)
// {
//     bool same = false;
//     if (!PyObject_TypeCheck(p2, &pgf_HypoType)) goto done;
//     HypoObject *t2 = (HypoObject *)p2;
//
//     if (!PyObject_RichCompareBool(t1->bind_type, t2->bind_type, Py_EQ)) goto done;
//     if (PyUnicode_Compare(t1->cid, t2->cid) != 0) goto done;
//     if (!PyObject_RichCompareBool((PyObject *)t1->type, (PyObject *)t2->type, Py_EQ)) goto done;
//
//     same = true;
// done:
//
//     if (op == Py_EQ) {
//         if (same) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
//     } else if (op == Py_NE) {
//         if (same) Py_RETURN_FALSE; else Py_RETURN_TRUE;
//     } else {
//         PyErr_SetString(PyExc_TypeError, "comparison operation not supported");
//         Py_RETURN_NOTIMPLEMENTED;
//     }
// }
//
// static PyMemberDef Hypo_members[] = {
//     {"bind_type", T_OBJECT_EX, offsetof(HypoObject, bind_type), READONLY, "bind type (explicit or implicit)"},
//     {"cid", T_OBJECT_EX, offsetof(HypoObject, cid), READONLY, "category name"},
//     {"type", T_OBJECT_EX, offsetof(HypoObject, type), READONLY, "type"},
//     {NULL}  /* Sentinel */
// };
//
// PyTypeObject pgf_HypoType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                         /*ob_size*/
//     "pgf.Hypo",                /*tp_name*/
//     sizeof(HypoObject),        /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     (destructor) Hypo_dealloc, /*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     0,                         /*tp_hash */
//     0,                         /*tp_call*/
//     0,                         /*tp_str*/
//     0,                         /*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "hypothesis in a type",    /*tp_doc*/
//     0,                         /*tp_traverse */
//     0,                         /*tp_clear */
//     (richcmpfunc) Hypo_richcompare, /*tp_richcompare */
//     0,                         /*tp_weaklistoffset */
//     0,                         /*tp_iter */
//     0,                         /*tp_iternext */
//     0,                         /*tp_methods */
//     Hypo_members,              /*tp_members */
//     0,                         /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     (initproc) Hypo_init,      /*tp_init */
//     0,                         /*tp_alloc */
//     (newfunc) Hypo_new,        /*tp_new */
// };

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
        PyObject* arg = PyTuple_GetItem(args, 1);
        if (PyList_Check(arg) && PyList_Size(arg) == 0)
            return pgf_ExprFunType.tp_alloc(&pgf_ExprFunType, 0);
        else
            return pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
    } else {
        PyErr_Format(PyExc_TypeError, "function takes 0, 1 or 2 arguments (%d given)", (int) tuple_size);
        return NULL;
    }
}

static PyObject *
Expr_subclass_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds)
{
    return subtype->tp_alloc(subtype, 0);
}

static PyObject *
Expr_str(ExprObject *self)
{
    PgfText *s = pgf_print_expr((PgfExpr) self, NULL, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
}

static ExprObject*
Expr_call(ExprObject* self, PyObject* args, PyObject* kw)
{
    ExprObject *res = self; Py_INCREF(self);

    size_t n_args = PyTuple_Size(args);
    for (size_t i = 0; i < n_args; i++) {
        PyObject* arg = PyTuple_GetItem(args, i);
        if (arg == NULL) {
            Py_DECREF(res);
            return NULL;
        }
        if (!PyObject_TypeCheck(arg, &pgf_ExprType)) {
            Py_DECREF(res);
            PyErr_SetString(PyExc_TypeError, "the arguments must be expressions");
            return NULL;
        }

        ExprAppObject *pyexpr = (ExprAppObject *)pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
        if (pyexpr == NULL) {
            Py_DECREF(res);
            return NULL;
        }
        pyexpr->fun = res;
        pyexpr->arg = (ExprObject *)arg; Py_INCREF(arg);
        res = (ExprObject *) pyexpr;
    }

    return res;
}

static PyObject*
Expr_unpack(ExprObject *self, PyObject *fargs)
{
    ExprObject *expr = self;
    PyObject *args = PyList_New(0);

    for (;;) {
        if (PyObject_TypeCheck(expr, &pgf_ExprAbsType)) {
            ExprAbsObject *eabs = (ExprAbsObject *) expr;
            PyObject* res =
                Py_BuildValue("OOOO", eabs->bind_type, eabs->name, eabs->body, args);
            Py_DECREF(args);
            return res;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprAppType)) {
            if (PyList_Insert(args, 0, (PyObject*) ((ExprAppObject *) expr)->arg) == -1) {
                Py_DECREF(args);
                return NULL;
            }
            expr = ((ExprAppObject *) expr)->fun;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprLitType)) {
            PyObject* res = ((ExprLitObject *) expr)->lit;
            Py_INCREF(res);
            Py_DECREF(args);
            return res;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprMetaType)) {
            PyObject* res = Py_BuildValue("OO", Py_None, args);
            Py_DECREF(args);
            return res;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprFunType)) {
            PyObject* res = Py_BuildValue("OO", ((ExprFunObject *) expr)->name, args);
            Py_DECREF(args);
            return res;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprVarType)) {
            PyObject* res = Py_BuildValue("OO", ((ExprVarObject *) expr)->var, args);
            Py_DECREF(args);
            return res;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprTypedType)) {
            expr = ((ExprTypedObject *) expr)->expr;
        } else if (PyObject_TypeCheck(expr, &pgf_ExprImplArgType)) {
            expr = ((ExprImplArgObject *) expr)->expr;
        } else {
            PyErr_SetString(PyExc_TypeError, "Unsupported kind of abstract expression");
            return NULL;
        }
    }
}

static PyObject*
Expr_visit(ExprObject* self, PyObject *args)
{
    PyObject* py_visitor = NULL;
    if (!PyArg_ParseTuple(args, "O", &py_visitor))
        return NULL;

    if (PyObject_TypeCheck(self, &pgf_ExprTypedType)) {
        self = ((ExprTypedObject *) self)->expr;
    }

    ExprObject *o = (ExprObject *) self;
    size_t arity = 0;
    while (PyObject_TypeCheck(o, &pgf_ExprAppType)) {
        arity++;
        o = ((ExprAppObject *) o)->fun;
        if (PyObject_TypeCheck(o, &pgf_ExprTypedType)) {
            o = ((ExprTypedObject *) o)->expr;
        }
    }
    if (PyObject_TypeCheck(o, &pgf_ExprFunType)) {
        Py_ssize_t len;
        const char *text = PyUnicode_AsUTF8AndSize(((ExprFunObject *) o)->name, &len);
        if (text == NULL)
            return NULL;

        char* method_name = alloca(len+4);
        strcpy(method_name, "on_");
        memcpy(method_name+3, text, len+1);
        if (PyObject_HasAttrString(py_visitor, method_name)) {
            PyObject* method_args = PyTuple_New(arity);
            if (method_args == NULL) {
                return NULL;
            }

            o = (ExprObject *) self;
            for (size_t i = 0; i < arity; i++) {
                ExprObject *arg = ((ExprAppObject *) o)->arg;
                if (PyObject_TypeCheck(arg, &pgf_ExprImplArgType)) {
                    arg = ((ExprImplArgObject *) arg)->expr;
                }
                if (PyObject_TypeCheck(arg, &pgf_ExprTypedType)) {
                    arg = ((ExprTypedObject *) arg)->expr;
                }

                Py_INCREF(arg);
                if (PyTuple_SetItem(method_args, i, (PyObject *) arg) == -1) {
                    Py_DECREF(method_args);
                    return NULL;
                }

                o = ((ExprAppObject *) o)->fun;
                if (PyObject_TypeCheck(o, &pgf_ExprTypedType)) {
                    o = ((ExprTypedObject *) o)->expr;
                }
            }

            PyObject* method =
                PyObject_GetAttrString(py_visitor, method_name);
            if (method == NULL) {
                Py_DECREF(method_args);
                return NULL;
            }

            PyObject *res = PyObject_CallObject(method, method_args);

            Py_DECREF(method_args);

            return res;
        }
    }

    return PyObject_CallMethod(py_visitor, "default", "O", self);
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
    {"unpack", (PyCFunction)Expr_unpack, METH_VARARGS,
     "Decomposes an expression into its components"
    },
    {"visit", (PyCFunction)Expr_visit, METH_VARARGS,
     "Implementation of the visitor pattern for abstract syntax trees. "
     "If e is an expression equal to f a1 .. an then "
     "e.visit(self) calls method self.on_f(a1,..an). "
     "If the method doesn't exist then the method self.default(e) "
     "is called."
    },
    {"__reduce_ex__", (PyCFunction)Expr_reduce_ex, METH_VARARGS,
     "This method allows for transparent pickling/unpickling of expressions."
    },
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
    0,                         /*tp_hash */
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
    0,                         /*tp_getset */
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
    if (!PyArg_ParseTuple(args, "O!UO!", &PyBool_Type, &bind_type, &name, &pgf_ExprType, &body)) {
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

static Py_hash_t
ExprAbs_hash(ExprAbsObject *eabs)
{
    return PyObject_Hash((PyObject*) eabs->name) * 101 + 
           PyObject_Hash((PyObject*) eabs->body);
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
    {"bind_type", T_OBJECT_EX, offsetof(ExprAbsObject, bind_type), READONLY, "bind type (explicit or implicit)"},
    {"name", T_OBJECT_EX, offsetof(ExprAbsObject, name), READONLY, "name of the abstraction"},
    {"body", T_OBJECT_EX, offsetof(ExprAbsObject, body), READONLY, "body of the abstraction"},
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
    (hashfunc) ExprAbs_hash,   /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    Expr_subclass_new,         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprApp_init(ExprAppObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* fun = NULL;
    PyObject* arg = NULL;
    if (!PyArg_ParseTuple(args, "OO", &fun, &arg)) {
        return -1;
    }

    if (PyObject_TypeCheck(fun, &pgf_ExprType) && PyObject_TypeCheck(arg, &pgf_ExprType)) {
        self->fun = (ExprObject *)fun;
        self->arg = (ExprObject *)arg;
        Py_INCREF(self->fun);
        Py_INCREF(self->arg);
        return 0;
    } else if (PyList_Check(arg)) {
        if (PyUnicode_Check(fun)) {
            ExprFunObject *fun_expr =
                (ExprFunObject *) pgf_ExprFunType.tp_alloc(&pgf_ExprFunType, 0);
            fun_expr->name = fun;
            Py_INCREF(fun);
            fun = (PyObject *) fun_expr;
        } else if (PyObject_TypeCheck(fun, &pgf_ExprType)) {
            Py_INCREF(fun);
        } else {
            PyErr_SetString(PyExc_TypeError, "argument 1 must be Expr or str");
            return -1;
        }

        Py_ssize_t n_args = PyList_Size(arg);
        if (n_args == 0) {
            PyErr_SetString(PyExc_TypeError, "The list of arguments must be non-empty");
            return -1;
        }

        for (Py_ssize_t i = 0; i < n_args-1; i++) {
            PyObject *item = PyList_GetItem(arg, i);
            if (!PyObject_TypeCheck(item, &pgf_ExprType)) {
                PyErr_SetString(PyExc_TypeError, "The list of arguments must contain other expressions");
                Py_DECREF(fun);
                return -1;
            }
            ExprAppObject *app_expr =
                (ExprAppObject *) pgf_ExprAppType.tp_alloc(&pgf_ExprAppType, 0);
            if (app_expr == NULL) {
                Py_DECREF(fun);
                return -1;
            }
            app_expr->fun = (ExprObject *) fun;
            app_expr->arg = (ExprObject *) item;
            Py_INCREF(item);
            fun = (PyObject *) app_expr;
        }

        arg = PyList_GetItem(arg, n_args-1);
        if (!PyObject_TypeCheck(arg, &pgf_ExprType)) {
            PyErr_SetString(PyExc_TypeError, "The list of arguments must contain other expressions");
            Py_DECREF(fun);
            return -1;
        }
        Py_INCREF(arg);

        self->fun = (ExprObject *)fun;
        self->arg = (ExprObject *)arg;

        return 0;
    }

    PyErr_SetString(PyExc_TypeError, "The arguments must be two expressions");
    return -1;
}

static void
ExprApp_dealloc(ExprAppObject *self)
{
    Py_XDECREF(self->fun);
    Py_XDECREF(self->arg);
    Py_TYPE(self)->tp_free(self);
}

static Py_hash_t
ExprApp_hash(ExprAppObject *eapp)
{
    return PyObject_Hash((PyObject*) eapp->fun) * 101 +
           PyObject_Hash((PyObject*) eapp->arg);
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
    {"fun", T_OBJECT_EX, offsetof(ExprAppObject, fun), READONLY, "the function in a function application"},
    {"arg", T_OBJECT_EX, offsetof(ExprAppObject, arg), READONLY, "the argument in a function application"},
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
    (hashfunc) ExprApp_hash,   /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    Expr_subclass_new,         /*tp_new */
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

static Py_hash_t
ExprLit_hash(ExprLitObject *elit)
{
    return PyObject_Hash(elit->lit);
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
    {"val", T_OBJECT_EX, offsetof(ExprLitObject, lit), READONLY, "the value of the literal"},
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
    (hashfunc) ExprLit_hash,   /*tp_hash */
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
    Expr_subclass_new,         /*tp_new */
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

static Py_hash_t
ExprMeta_hash(ExprMetaObject *emeta)
{
    return PyObject_Hash(emeta->id);
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
    {"id", T_OBJECT_EX, offsetof(ExprMetaObject, id), READONLY, "the id of a meta variable"},
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
    (hashfunc) ExprMeta_hash,  /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    Expr_subclass_new,         /*tp_new */
};

// ----------------------------------------------------------------------------

static int
ExprFun_init(ExprFunObject *self, PyObject *args, PyObject *kwds)
{
    PyObject* list = NULL;
    if (!PyArg_ParseTuple(args, "U|O!", &self->name, &PyList_Type, &list)) {
        return -1;
    }
    Py_INCREF(self->name);
    if (list != NULL && PyList_Size(list) != 0) {
        PyErr_SetString(PyExc_TypeError, "The list of arguments must be empty");
        return -1;
    }
    return 0;
}

static void
ExprFun_dealloc(ExprFunObject *self)
{
    Py_XDECREF(self->name);
    Py_TYPE(self)->tp_free(self);
}

static Py_hash_t
ExprFun_hash(ExprFunObject *efun)
{
    return PyObject_Hash(efun->name);
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
    {"name", T_OBJECT_EX, offsetof(ExprFunObject, name), READONLY, "the name of the function"},
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
    (hashfunc) ExprFun_hash,   /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    Expr_subclass_new,         /*tp_new */
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

static Py_hash_t
ExprVar_hash(ExprVarObject *efun)
{
    return (Py_hash_t) efun->var;
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
    {"index", T_OBJECT_EX, offsetof(ExprVarObject, var), READONLY, "the de Bruijn index of a variable"},
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
    (hashfunc) ExprVar_hash,   /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    Expr_subclass_new,         /*tp_new */
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

static Py_hash_t
ExprTyped_hash(ExprTypedObject *etyped)
{
    return PyObject_Hash((PyObject*) etyped->expr);
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
    {"expr", T_OBJECT_EX, offsetof(ExprTypedObject, expr), READONLY, "the expression"},
    {"type", T_OBJECT_EX, offsetof(ExprTypedObject, type), READONLY, "the type"},
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
    (hashfunc) ExprTyped_hash, /*tp_hash */
    (ternaryfunc) Expr_call,   /*tp_call*/
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
    (initproc) ExprTyped_init, /*tp_init */
    0,                         /*tp_alloc */
    Expr_subclass_new,         /*tp_new */
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

static Py_hash_t
ExprImplArg_hash(ExprImplArgObject *eimpl)
{
    return PyObject_Hash((PyObject*) eimpl->expr);
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
    {"expr", T_OBJECT_EX, offsetof(ExprImplArgObject, expr), READONLY, "the inner expression"},
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
    (hashfunc)ExprImplArg_hash,/*tp_hash */
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
    Expr_subclass_new,         /*tp_new */
};
