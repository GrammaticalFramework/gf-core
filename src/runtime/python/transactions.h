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

PyTypeObject pgf_TransactionType;

PyObject *
PGF_checkoutBranch(PGFObject *self, PyObject *args);

TransactionObject *
PGF_newTransaction(PGFObject *self, PyObject *args);

#endif // PYPGF_TRANSACTIONS_H_
