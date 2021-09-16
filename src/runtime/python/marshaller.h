#ifndef PYPGF_MARSHALLER_H_
#define PYPGF_MARSHALLER_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

PgfText *PyUnicode_AsPgfText(PyObject *pystr);
PyObject *PyUnicode_FromPgfText(PgfText *text);

extern PgfUnmarshaller unmarshaller;
extern PgfMarshaller marshaller;

#endif // PYPGF_MARSHALLER_H_
