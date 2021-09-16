#ifndef PYPGF_EXPR_H_
#define PYPGF_EXPR_H_

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <pgf/pgf.h>

typedef struct {
    PyObject_HEAD
    PyObject *hypos; // PyListObject of PyTupleObject: (bind_type: int, cid: string, type: TypeObject)
    PyObject *cat;   // PyUnicodeObject
    PyObject *exprs; // PyListObject of ExprObject
} TypeObject;

extern PyTypeObject pgf_TypeType;

typedef struct {
    PyObject_HEAD
} ExprObject;

typedef struct {
    PyObject_HEAD
    PyObject *bindType; // PyLongObject
    PyObject *var;      // PyUnicodeObject
    ExprObject *expr;
} ExprAbsObject;

typedef struct {
    PyObject_HEAD
    ExprObject *e1; // ExprObject
    ExprObject *e2; // ExprObject
} ExprAppObject;

typedef struct {
    PyObject_HEAD
    PyObject *value; // PyLongObject | PyFloatObject | PyUnicodeObject
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
    PyObject *index; // PyLongObject
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

extern PyTypeObject pgf_ExprType;
extern PyTypeObject pgf_ExprAbsType;
extern PyTypeObject pgf_ExprAppType;
extern PyTypeObject pgf_ExprLitType;
extern PyTypeObject pgf_ExprMetaType;
extern PyTypeObject pgf_ExprFunType;
extern PyTypeObject pgf_ExprVarType;
extern PyTypeObject pgf_ExprTypedType;
extern PyTypeObject pgf_ExprImplArgType;

#endif // PYPGF_EXPR_H_
