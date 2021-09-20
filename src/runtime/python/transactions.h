#ifndef PYPGF_TRANSACTIONS_H_
#define PYPGF_TRANSACTIONS_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

// typedef struct {
//     PyObject_HEAD
//     PyObject *thing;
// } TransactionObject;

// modifyPGF
//
// branchPGF
//
// checkoutPGF

PyObject *
PGF_createFunction(PGFObject *self, PyObject *args);

PyObject *
PGF_dropFunction(PGFObject *self, PyObject *args);

PyObject *
PGF_createCategory(PGFObject *self, PyObject *args);

PyObject *
PGF_dropCategory(PGFObject *self, PyObject *args);

// setGlobalFlag
//
// setAbstractFlag



#endif // PYPGF_TRANSACTIONS_H_
