#ifndef PYPGF_EXPR_H_
#define PYPGF_EXPR_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

typedef struct {
    PyObject_HEAD
    PyObject *hypos; // PyListObject of HypoObject
    PyObject *name;  // PyUnicodeObject
    PyObject *exprs; // PyListObject of ExprObject
} TypeObject;

PyTypeObject pgf_TypeType;

typedef struct {
    PyObject_HEAD
    PyObject *bind_type; // PyBool
    PyObject *cid;       // PyUnicodeObject
    TypeObject *type;
} HypoObject;

PyTypeObject pgf_HypoType;

typedef struct {
    PyObject_HEAD
} ExprObject;

typedef struct {
    PyObject_HEAD
    PyObject *bind_type; // PyBool
    PyObject *name;      // PyUnicodeObject
    ExprObject *body;
} ExprAbsObject;

typedef struct {
    PyObject_HEAD
    ExprObject *fun; // ExprObject
    ExprObject *arg; // ExprObject
} ExprAppObject;

typedef struct {
    PyObject_HEAD
    PyObject *lit; // PyLongObject | PyFloatObject | PyUnicodeObject
} ExprLitObject;

typedef struct {
    PyObject_HEAD
    PyObject *id; // PyLongObject
} ExprMetaObject;

typedef struct {
    PyObject_HEAD
    PyObject *name; // PyUnicodeObject
} ExprFunObject;

typedef struct {
    PyObject_HEAD
    PyObject *var; // PyLongObject
} ExprVarObject;

typedef struct {
    PyObject_HEAD
    ExprObject *expr;
    TypeObject *type;
} ExprTypedObject;

typedef struct {
    PyObject_HEAD
    ExprObject *expr;
} ExprImplArgObject;

PyTypeObject pgf_ExprType;
PyTypeObject pgf_ExprAbsType;
PyTypeObject pgf_ExprAppType;
PyTypeObject pgf_ExprLitType;
PyTypeObject pgf_ExprMetaType;
PyTypeObject pgf_ExprFunType;
PyTypeObject pgf_ExprVarType;
PyTypeObject pgf_ExprTypedType;
PyTypeObject pgf_ExprImplArgType;

#endif // PYPGF_EXPR_H_
