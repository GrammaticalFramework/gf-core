#ifndef PYPGF_EXPR_H_
#define PYPGF_EXPR_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

#include "./compat.h"

typedef struct {
    PyObject_HEAD
    PyObject *hypos; // PyListObject of PyTupleObject: (bind_type: int, cid: string, type: TypeObject)
    PyObject *cat;   // PyStringObject
    PyObject *exprs; // PyListObject of ExprObject
} TypeObject;

extern PyTypeObject pgf_TypeType;

// typedef struct {
//     PyObject_HEAD
//     PyObject* master;
//     GuPool* pool;
//     PgfExpr expr;
// } ExprObject;

#endif // PYPGF_EXPR_H_
