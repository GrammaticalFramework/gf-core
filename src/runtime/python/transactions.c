#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <structmember.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"
#include "./transactions.h"

PyObject *
PGF_checkoutBranch(PGFObject *self, PyObject *args)
{
    const char *s = NULL;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *name = CString_AsPgfText(s, size);

    PgfExn err;
    PgfRevision rev = pgf_checkout_revision(self->db, name, &err);
    FreePgfText(name);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }
    if (rev == 0) {
        // is this possible?
        PyErr_SetString(PyExc_KeyError, "unknown branch name");
        return NULL;
    }

    pgf_free_revision(self->db, self->revision);
    self->revision = rev;

    Py_RETURN_TRUE;
}

TransactionObject *
PGF_newTransaction(PGFObject *self, PyObject *args)
{
    PgfText *name = NULL;
    const char *s = NULL;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "|s#", &s, &size))
        return NULL;

    if (s != NULL) {
        name = CString_AsPgfText(s, size);
    }

    PgfExn err;
    PgfRevision rev = pgf_clone_revision(self->db, self->revision, name, &err);
    if (name != NULL) {
        FreePgfText(name);
    }
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    TransactionObject *trans = (TransactionObject *)pgf_TransactionType.tp_alloc(&pgf_TransactionType, 0);
    trans->pgf = self;
    trans->revision = rev;

    return trans;
}

PyObject *
PGF_getGlobalFlag(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *flagname = CString_AsPgfText(s, size);

    PgfExn err;
    PgfLiteral lit = pgf_get_global_flag(self->db, self->revision, flagname, &unmarshaller, &err);
    FreePgfText(flagname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    } else if (lit == 0) {
        PyErr_Format(PyExc_KeyError, "unknown global flag '%s'", s);
        return NULL;
    }

    return (PyObject *)lit;
}


PyObject *
PGF_getAbstractFlag(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *flagname = CString_AsPgfText(s, size);

    PgfExn err;
    PgfLiteral lit = pgf_get_abstract_flag(self->db, self->revision, flagname, &unmarshaller, &err);
    FreePgfText(flagname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    } else if (lit == 0) {
        PyErr_Format(PyExc_KeyError, "unknown abstract flag '%s'", s);
        return NULL;
    }

    return (PyObject *)lit;
}

// ----------------------------------------------------------------------------

static void
Transaction_dealloc(TransactionObject *self)
{
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
Transaction_commit(TransactionObject *self, PyObject *args)
{
    PgfExn err;
    pgf_commit_revision(self->pgf->db, self->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    pgf_free_revision(self->pgf->db, self->pgf->revision);
    self->pgf->revision = self->revision;
    Py_INCREF(self->pgf);

    Py_RETURN_NONE;
}

static PyObject *
Transaction_createFunction(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    TypeObject *type;
    Py_ssize_t arity = 0;
    prob_t prob = 0.0;
    if (!PyArg_ParseTuple(args, "s#O!nf", &s, &size, &pgf_TypeType, &type, &arity, &prob))
        return NULL;

    PgfText *funname = CString_AsPgfText(s, size);

    PgfExn err;
    pgf_create_function(self->pgf->db, self->revision, funname, (PgfType) type, arity, prob, &marshaller, &err);
    FreePgfText(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject *
Transaction_dropFunction(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *funname = CString_AsPgfText(s, size);

    PgfExn err;
    pgf_drop_function(self->pgf->db, self->revision, funname, &err);
    FreePgfText(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject *
Transaction_createCategory(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    PyObject *hypos;
    prob_t prob = 0.0;
    if (!PyArg_ParseTuple(args, "s#Of", &s, &size, &hypos, &prob))
        return NULL;
    if (!PySequence_Check(hypos)) {
        PyErr_SetString(PyExc_TypeError, "context must be a sequence");
        return NULL;
    }

    PgfText *catname = CString_AsPgfText(s, size);

    Py_ssize_t n_hypos;
    PgfTypeHypo *context = PySequence_AsHypos(hypos, &n_hypos);
    if (PyErr_Occurred()) {
        FreePgfText(catname);
        return NULL;
    }

    PgfExn err;
    pgf_create_category(self->pgf->db, self->revision, catname, n_hypos, context, prob, &marshaller, &err);

    FreePgfText(catname);
    for (Py_ssize_t i = 0; i < n_hypos; i++) {
        FreePgfText(context[i].cid);
    }
    PyMem_Free(context);

    if (handleError(err) != PGF_EXN_NONE) {        
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject *
Transaction_dropCategory(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = CString_AsPgfText(s, size);

    PgfExn err;
    pgf_drop_category(self->pgf->db, self->revision, catname, &err);
    FreePgfText(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject *
Transaction_setGlobalFlag(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    PyObject *pyobj;
    if (!PyArg_ParseTuple(args, "s#O", &s, &size, &pyobj))
        return NULL;

    PgfText *flagname = CString_AsPgfText(s, size);
    if (!PyLong_Check(pyobj) && !PyFloat_Check(pyobj) && !PyUnicode_Check(pyobj)) {
        PyErr_SetString(PyExc_TypeError, "flag value must be integer, float, or string");
        return NULL;
    }

    PgfExn err;
    pgf_set_global_flag(self->pgf->db, self->revision, flagname, (PgfLiteral) pyobj, &marshaller, &err);
    FreePgfText(flagname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject *
Transaction_setAbstractFlag(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    PyObject *pyobj;
    if (!PyArg_ParseTuple(args, "s#O", &s, &size, &pyobj))
        return NULL;

    PgfText *flagname = CString_AsPgfText(s, size);
    if (!PyLong_Check(pyobj) && !PyFloat_Check(pyobj) && !PyUnicode_Check(pyobj)) {
        PyErr_SetString(PyExc_TypeError, "flag value must be integer, float, or string");
        return NULL;
    }

    PgfExn err;
    pgf_set_abstract_flag(self->pgf->db, self->revision, flagname, (PgfLiteral) pyobj, &marshaller, &err);
    FreePgfText(flagname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

static TransactionObject *
Transaction_enter(TransactionObject *self, PyObject *Py_UNUSED(ignored))
{
    Py_INCREF(self);
    return self;
}

static PyObject *
Transaction_exit_impl(TransactionObject *self, PyObject *exc_type, PyObject *exc_value, PyObject *exc_tb)
{
    if (exc_type == Py_None && exc_value == Py_None && exc_tb == Py_None) {
        return Transaction_commit(self, NULL);
    } else {
        PyErr_SetObject(exc_type, exc_value);
        return NULL;
    }
}

// cpython/Modules/_multiprocessing/clinic/semaphore.c.h
// cpython/Modules/_sqlite/connection.c
static PyObject *
Transaction_exit(TransactionObject *self, PyObject *const *args, Py_ssize_t nargs)
{
    PyObject *return_value = NULL;
    PyObject *exc_type = Py_None;
    PyObject *exc_value = Py_None;
    PyObject *exc_tb = Py_None;

    if (nargs < 0 || nargs > 3) {
        goto exit;
    }
    if (nargs < 1) {
        goto skip_optional;
    }
    exc_type = args[0];
    if (nargs < 2) {
        goto skip_optional;
    }
    exc_value = args[1];
    if (nargs < 3) {
        goto skip_optional;
    }
    exc_tb = args[2];
skip_optional:
    return_value = Transaction_exit_impl(self, exc_type, exc_value, exc_tb);

exit:
    return return_value;
}

static PyGetSetDef Transaction_getseters[] = {
    {NULL}  /* Sentinel */
};

static PyMemberDef Transaction_members[] = {
    {NULL}  /* Sentinel */
};

static PyMethodDef Transaction_methods[] = {
    {"commit", (PyCFunction)Transaction_commit, METH_VARARGS,
     "Commit transaction"
    },

    // used in 'with'-syntax
    {"__enter__", (PyCFunction)Transaction_enter, METH_NOARGS, ""},
    {"__exit__", (PyCFunction)(void(*)(void))Transaction_exit, METH_FASTCALL, ""},

    {"createFunction", (PyCFunction)Transaction_createFunction, METH_VARARGS,
     "Create function"
    },
    {"dropFunction", (PyCFunction)Transaction_dropFunction, METH_VARARGS,
     "Drop function"
    },
    {"createCategory", (PyCFunction)Transaction_createCategory, METH_VARARGS,
     "Create category"
    },
    {"dropCategory", (PyCFunction)Transaction_dropCategory, METH_VARARGS,
     "Drop category"
    },

    {"setGlobalFlag", (PyCFunction)Transaction_setGlobalFlag, METH_VARARGS,
     "Set a global flag"
    },
    {"setAbstractFlag", (PyCFunction)Transaction_setAbstractFlag, METH_VARARGS,
     "Set an abstract flag"
    },
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_TransactionType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Transaction",                 /*tp_name*/
    sizeof(TransactionObject),         /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) Transaction_dealloc,   /*tp_dealloc*/
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
    0, // (reprfunc) Transaction_str,        /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "Transaction object",              /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    0,                           /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    Transaction_methods,               /*tp_methods */
    Transaction_members,               /*tp_members */
    Transaction_getseters,             /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0,                         /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};
