#define PY_SSIZE_T_CLEAN
#include <Python.h>
// #include <stdbool.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"

PyObject *
PGF_createFunction(PGFObject *self, PyObject *args)
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
    pgf_create_function(self->db, self->revision, fname, (PgfType) type, arity, prob, &marshaller, &err);
    PyMem_Free(fname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
PGF_dropFunction(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *fname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(fname->text, s, size+1);
    fname->size = size;

    PgfExn err;
    pgf_drop_category(self->db, self->revision, fname, &err);
    PyMem_Free(fname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
PGF_createCategory(PGFObject *self, PyObject *args)
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
    pgf_create_category(self->db, self->revision, catname, n_hypos, context, prob, &marshaller, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}

PyObject *
PGF_dropCategory(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(catname->text, s, size+1);
    catname->size = size;

    PgfExn err;
    pgf_drop_function(self->db, self->revision, catname, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    Py_RETURN_NONE;
}
