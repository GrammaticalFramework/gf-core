#ifndef PYPGF_MARSHALLER_H_
#define PYPGF_MARSHALLER_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

typedef struct {
    PyObject_HEAD
    PgfDB *db;
    PgfRevision revision;
} PGFObject;

extern PyObject *PGFError;
PgfExnType handleError(PgfExn err);

PgfText *PyUnicode_AsPgfText(PyObject *pystr);
PyObject *PyUnicode_FromPgfText(PgfText *text);

extern PgfUnmarshaller unmarshaller;
extern PgfMarshaller marshaller;

#endif // PYPGF_MARSHALLER_H_
