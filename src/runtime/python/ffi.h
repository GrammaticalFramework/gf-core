#ifndef PYPGF_FFI_H_
#define PYPGF_FFI_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

typedef struct {
    PyObject_HEAD
    PgfDB *db;
    PgfRevision revision;
} PGFObject;

PyObject *PGFError;
PgfExnType handleError(PgfExn err);

PgfText *PyUnicode_AsPgfText(PyObject *pystr);
PyObject *PyUnicode_FromPgfText(PgfText *text);

PgfUnmarshaller unmarshaller;
PgfMarshaller marshaller;

#endif // PYPGF_FFI_H_
