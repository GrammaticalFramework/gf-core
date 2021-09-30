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

PgfText *CString_AsPgfText(const char *s, size_t size);

PgfText *PyUnicode_AsPgfText(PyObject *pystr);
PyObject *PyUnicode_FromPgfText(PgfText *text);

PgfTypeHypo *PySequence_AsHypos(PyObject *pyseq, Py_ssize_t *n_hypos);
PyObject *PyList_FromHypos(PgfTypeHypo *hypos, const size_t n_hypos);

PgfPrintContext *PyList_AsPgfPrintContext(PyObject *pylist);

void FreePgfText(PgfText *txt);
void FreeHypos(PgfTypeHypo *hypos, Py_ssize_t n_hypos);
void FreePgfPrintContext(PgfPrintContext *ctxt);

PgfUnmarshaller unmarshaller;
PgfMarshaller marshaller;

#endif // PYPGF_FFI_H_
