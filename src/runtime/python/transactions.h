#ifndef PYPGF_TRANSACTIONS_H_
#define PYPGF_TRANSACTIONS_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

typedef struct {
    PyObject_HEAD
    PGFObject *pgf; // original reference, gets updated on commit
    PgfRevision revision; // transient branch
} TransactionObject;

extern PyTypeObject pgf_TransactionType;

TransactionObject *
PGF_newTransaction(PGFObject *self, PyObject *args);

PyObject *
Transaction_commit(TransactionObject *self, PyObject *args);

PyObject *
Transaction_createFunction(TransactionObject *self, PyObject *args);

PyObject *
Transaction_dropFunction(TransactionObject *self, PyObject *args);

PyObject *
Transaction_createCategory(TransactionObject *self, PyObject *args);

PyObject *
Transaction_dropCategory(TransactionObject *self, PyObject *args);

#endif // PYPGF_TRANSACTIONS_H_
