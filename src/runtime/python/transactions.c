#define PY_SSIZE_T_CLEAN
#include <Python.h>
// #include <stdbool.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"
#include "./transactions.h"

TransactionObject *
PGF_newTransaction(PGFObject *self, PyObject *args)
{
    PgfText *name = NULL;
    // PgfText *name = PyUnicode_AsPgfText(PyUnicode_FromString("transient"));

    PgfExn err;
    PgfRevision rev = pgf_clone_revision(self->db, self->revision, name, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    TransactionObject *trans = (TransactionObject *)pgf_TransactionType.tp_alloc(&pgf_TransactionType, 0);
    trans->pgf = self;
    trans->revision = rev;

    return trans;
}

PyObject *
Transaction_commit(TransactionObject *self, PyObject *args)
{
    PgfExn err;
    pgf_commit_revision(self->pgf->db, self->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    self->pgf->revision = self->revision;

    Py_RETURN_TRUE;
}

PyObject *
Transaction_createFunction(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    TypeObject *type;
    Py_ssize_t arity = 0;
    float prob = 0.0;
    if (!PyArg_ParseTuple(args, "s#O!nf", &s, &size, &pgf_TypeType, &type, &arity, &prob))
        return NULL;

    PgfText *fname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(fname->text, s, size+1);
    fname->size = size;

    PgfExn err;
    pgf_create_function(self->pgf->db, self->revision, fname, (PgfType) type, arity, prob, &marshaller, &err);
    PyMem_Free(fname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
Transaction_dropFunction(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *fname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(fname->text, s, size+1);
    fname->size = size;

    PgfExn err;
    pgf_drop_category(self->pgf->db, self->revision, fname, &err);
    PyMem_Free(fname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
Transaction_createCategory(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    float prob = 0.0;
    if (!PyArg_ParseTuple(args, "s#f", &s, &size, prob))
        return NULL;

    PgfText *catname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(catname->text, s, size+1);
    catname->size = size;

    Py_ssize_t n_hypos = 0;
    PgfTypeHypo *context = NULL;

    PgfExn err;
    pgf_create_category(self->pgf->db, self->revision, catname, n_hypos, context, prob, &marshaller, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
Transaction_dropCategory(TransactionObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(catname->text, s, size+1);
    catname->size = size;

    PgfExn err;
    pgf_drop_function(self->pgf->db, self->revision, catname, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

// ----------------------------------------------------------------------------

// static void
// Transaction_dealloc(PGFObject* self)
// {
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }

static PyGetSetDef Transaction_getseters[] = {
    {NULL}  /* Sentinel */
};

// static PyMemberDef Transaction_members[] = {
//     {NULL}  /* Sentinel */
// };

static PyMethodDef Transaction_methods[] = {
    {"commit", (PyCFunction)Transaction_commit, METH_VARARGS,
     "Commit transaction"
    },

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
    {NULL}  /* Sentinel */
};

PyTypeObject pgf_TransactionType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Transaction",                 /*tp_name*/
    sizeof(PGFObject),         /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0, //(destructor)Transaction_dealloc,   /*tp_dealloc*/
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
    "PGF transaction",              /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    0,                           /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    Transaction_methods,               /*tp_methods */
    0, //Transaction_members,               /*tp_members */
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
