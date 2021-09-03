#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdbool.h>

#include "expr.h"

// static ExprObject*
// Expr_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
// {
//     ExprObject* self = (ExprObject *)type->tp_alloc(type, 0);
//     if (self != NULL) {
//         self->master = NULL;
//         self->pool   = NULL;
//         self->expr   = gu_null_variant;
//     }
//
//     return self;
// }
//
// static void
// Expr_dealloc(ExprObject* self)
// {
//     if (self->master != NULL) {
//         Py_DECREF(self->master);
//     }
//     if (self->pool != NULL) {
//         gu_pool_free(self->pool);
//     }
//
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }
//
// static PyObject*
// Expr_getattro(ExprObject *self, PyObject *attr_name);
//
// static int
// Expr_initMeta(ExprObject *self);
//
// static int
// Expr_initLiteral(ExprObject *self, PyObject *lit);
//
// static int
// Expr_initApp(ExprObject *self, const char* fname, PyObject *args);
//
// static ExprObject*
// Expr_call(ExprObject* e, PyObject* args, PyObject* kw);
//
// static PyObject*
// Expr_unpack(ExprObject* self, PyObject *args);
//
// static PyObject*
// Expr_visit(ExprObject* self, PyObject *args);
//
// static PyObject*
// Expr_reduce_ex(ExprObject* self, PyObject *args);
//
// static int
// Expr_init(ExprObject *self, PyObject *args, PyObject *kwds)
// {
//     Py_ssize_t tuple_size = PyTuple_Size(args);
//
//     if (tuple_size == 0) {
//         return Expr_initMeta(self);
//     } else if (tuple_size == 1) {
//         PyObject* lit = NULL;
//         if (!PyArg_ParseTuple(args, "O", &lit))
//             return -1;
//         return Expr_initLiteral(self, lit);
//     } else if (tuple_size == 2) {
//         const char* fname;
//         PyObject* list = NULL;
//         if (!PyArg_ParseTuple(args, "sO!", &fname, &PyList_Type, &list))
//             return -1;
//         return Expr_initApp(self, fname, list);
//     } else {
//         PyErr_Format(PyExc_TypeError, "function takes 0, 1 or 2 arguments (%d given)", (int) tuple_size);
//         return -1;
//     }
//
//     return 0;
// }
//
// static PyObject *
// Expr_repr(ExprObject *self)
// {
//     GuPool* tmp_pool = gu_local_pool();
//
//     GuExn* err = gu_exn(tmp_pool);
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//     pgf_print_expr(self->expr, NULL, 0, out, err);
//
//     PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                  gu_string_buf_length(sbuf));
//
//     gu_pool_free(tmp_pool);
//     return pystr;
// }
//
// static PyObject *
// Expr_richcompare(ExprObject *e1, ExprObject *e2, int op)
// {
//     bool cmp = pgf_expr_eq(e1->expr,e2->expr);
//
//     if (op == Py_EQ) {
//         if (cmp) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
//     } else if (op == Py_NE) {
//         if (cmp) Py_RETURN_FALSE; else Py_RETURN_TRUE;
//     } else {
//         PyErr_SetString(PyExc_TypeError, "the operation is not supported");
//         return NULL;
//     }
// }
//
// static long
// Expr_hash(ExprObject *e)
// {
//     return (long) pgf_expr_hash(0, e->expr);
// }
//
// static PyMethodDef Expr_methods[] = {
//     {"unpack", (PyCFunction)Expr_unpack, METH_VARARGS,
//      "Decomposes an expression into its components"
//     },
//     {"visit", (PyCFunction)Expr_visit, METH_VARARGS,
//      "Implementation of the visitor pattern for abstract syntax trees. "
//      "If e is an expression equal to f a1 .. an then "
//      "e.visit(self) calls method self.on_f(a1,..an). "
//      "If the method doesn't exist then the method self.default(e) "
//      "is called."
//     },
//     {"__reduce_ex__", (PyCFunction)Expr_reduce_ex, METH_VARARGS,
//      "This method allows for transparent pickling/unpickling of expressions."
//     },
//     {NULL}  /* Sentinel */
// };
//
// static PyGetSetDef Expr_getseters[] = {
//     {"fun",
//      NULL, NULL,
//      "this is the function in a function application",
//      NULL},
//     {"arg",
//      NULL, NULL,
//      "this is the argument in a function application",
//      NULL},
//     {"val",
//      NULL, NULL,
//      "this is the value of a literal",
//      NULL},
//     {"id",
//      NULL, NULL,
//      "this is the id of a meta variable",
//      NULL},
//     {"name",
//      NULL, NULL,
//      "this is the name of a function",
//      NULL},
//     {"index",
//      NULL, NULL,
//      "this is the de Bruijn index of a variable",
//      NULL},
//     {NULL}  /* Sentinel */
// };
//
// static PyTypeObject pgf_ExprType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                         /*ob_size*/
//     "pgf.Expr",                /*tp_name*/
//     sizeof(ExprObject),        /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     (destructor)Expr_dealloc,  /*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     (hashfunc) Expr_hash,      /*tp_hash */
//     (ternaryfunc) Expr_call,   /*tp_call*/
//     (reprfunc) Expr_repr,      /*tp_str*/
//     (getattrofunc) Expr_getattro,/*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "abstract syntax tree",    /*tp_doc*/
//     0,                           /*tp_traverse */
//     0,                           /*tp_clear */
//     (richcmpfunc) Expr_richcompare, /*tp_richcompare */
//     0,                           /*tp_weaklistoffset */
//     0,                           /*tp_iter */
//     0,                           /*tp_iternext */
//     Expr_methods,              /*tp_methods */
//     0,                         /*tp_members */
//     Expr_getseters,            /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     (initproc)Expr_init,       /*tp_init */
//     0,                         /*tp_alloc */
//     (newfunc) Expr_new,        /*tp_new */
// };
//
// static int
// Expr_initMeta(ExprObject *self)
// {
//     self->master = NULL;
//     self->pool = gu_new_pool();
//     PgfExprMeta* e =
//         gu_new_variant(PGF_EXPR_META,
//                        PgfExprMeta,
//                        &self->expr, self->pool);
//     e->id = 0;
//     return 0;
// }
//
// static int
// Expr_initLiteral(ExprObject *self, PyObject *lit)
// {
//     self->master = NULL;
//     self->pool   = gu_new_pool();
//     PgfExprLit* e =
//         gu_new_variant(PGF_EXPR_LIT,
//                        PgfExprLit,
//                        &self->expr, self->pool);
//     e->lit = gu_null_variant;
//
//     if (PyString_Check(lit)) {
//         char* s;
//         Py_ssize_t len;
//
// #if PY_MAJOR_VERSION >= 3
//         PyObject* bytes = PyUnicode_AsUTF8String(lit);
//         if (bytes == NULL)
//             return -1;
//         if (PyBytes_AsStringAndSize(bytes,&s,&len) < 0)
//             return -1;
// #else
//         if (PyString_AsStringAndSize(lit,&s,&len) < 0)
//             return -1;
// #endif
//
//         PgfLiteralStr* slit =
//             gu_new_flex_variant(PGF_LITERAL_STR,
//                                 PgfLiteralStr,
//                                 val, len+1,
//                                 &e->lit, self->pool);
//         memcpy(slit->val, s, len+1);
//
// #if PY_MAJOR_VERSION >= 3
//         Py_DECREF(bytes);
// #endif
//     } else if (PyInt_Check(lit)) {
//         PgfLiteralInt* ilit =
//             gu_new_variant(PGF_LITERAL_INT,
//                            PgfLiteralInt,
//                            &e->lit, self->pool);
//         ilit->val = PyInt_AsLong(lit);
//     } else if (PyFloat_Check(lit)) {
//         PgfLiteralFlt* flit =
//             gu_new_variant(PGF_LITERAL_FLT,
//                            PgfLiteralFlt,
//                            &e->lit, self->pool);
//         flit->val = PyFloat_AsDouble(lit);
//     } else {
//         PyErr_SetString(PyExc_TypeError, "the literal must be a string, an integer, or a float");
//         return -1;
//     }
//     return 0;
// }
//
// static int
// Expr_initApp(ExprObject *self, const char* fname, PyObject *args)
// {
//     Py_ssize_t n_args = PyList_Size(args);
//
//     self->master = PyTuple_New(n_args);
//     if (self->master == NULL)
//         return -1;
//
//     self->pool = gu_new_pool();
//     PgfExprFun* e =
//         gu_new_flex_variant(PGF_EXPR_FUN,
//                             PgfExprFun,
//                             fun, strlen(fname)+1,
//                             &self->expr, self->pool);
//     strcpy(e->fun, fname);
//
//     for (Py_ssize_t i = 0; i < n_args; i++) {
//         PyObject* obj = PyList_GetItem(args, i);
//         if (obj->ob_type != &pgf_ExprType) {
//             PyErr_SetString(PyExc_TypeError, "the arguments in the list must be expressions");
//             return -1;
//         }
//
//         PyTuple_SetItem(self->master, i, obj);
//         Py_INCREF(obj);
//
//         PgfExpr fun = self->expr;
//         PgfExpr arg = ((ExprObject*) obj)->expr;
//
//         PgfExprApp* e =
//             gu_new_variant(PGF_EXPR_APP,
//                            PgfExprApp,
//                            &self->expr, self->pool);
//         e->fun = fun;
//         e->arg = arg;
//     }
//
//     return 0;
// }
//
// static ExprObject*
// Expr_call(ExprObject* self, PyObject* args, PyObject* kw)
// {
//     ExprObject* pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (pyexpr == NULL)
//         return NULL;
//
//     size_t n_args = PyTuple_Size(args);
//
//     pyexpr->master = PyTuple_New(n_args+1);
//     if (pyexpr->master == NULL) {
//         Py_DECREF(pyexpr);
//         return NULL;
//     }
//
//     PyTuple_SetItem(pyexpr->master, 0, (PyObject*) self);
//     Py_INCREF(self);
//
//     pyexpr->pool = gu_new_pool();
//     pyexpr->expr = self->expr;
//
//     for (Py_ssize_t i = 0; i < n_args; i++) {
//         PyObject* obj = PyTuple_GetItem(args, i);
//         if (obj->ob_type != &pgf_ExprType) {
//             PyErr_SetString(PyExc_TypeError, "the arguments must be expressions");
//             return NULL;
//         }
//
//         PyTuple_SetItem(pyexpr->master, i+1, obj);
//         Py_INCREF(obj);
//
//         PgfExpr fun = pyexpr->expr;
//         PgfExpr arg = ((ExprObject*) obj)->expr;
//
//         PgfExprApp* e =
//             gu_new_variant(PGF_EXPR_APP,
//                            PgfExprApp,
//                            &pyexpr->expr, pyexpr->pool);
//         e->fun = fun;
//         e->arg = arg;
//     }
//
//     return pyexpr;
// }
//
// static PyObject*
// Expr_unpack(ExprObject* self, PyObject *fargs)
// {
//     PgfExpr expr = self->expr;
//     PyObject* args = PyList_New(0);
//
//     for (;;) {
//         GuVariantInfo i = gu_variant_open(expr);
//         switch (i.tag) {
//         case PGF_EXPR_ABS: {
//             PgfExprAbs* eabs = i.data;
//
//             ExprObject* py_body = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//             if (py_body == NULL) {
//                 Py_DECREF(args);
//                 return NULL;
//             }
//             py_body->pool   = NULL;
//             py_body->master = (self->master) ? self->master : (PyObject*) self;
//             py_body->expr   = eabs->body;
//             Py_INCREF(py_body->master);
//
//             PyObject* py_bindtype =
//                 (eabs->bind_type == PGF_BIND_TYPE_EXPLICIT) ? Py_True
//                                                             : Py_False;
//             PyObject* py_var = PyString_FromString(eabs->id);
//             PyObject* res =
//                 Py_BuildValue("OOOO", py_bindtype, py_var, py_body, args);
//             Py_DECREF(py_var);
//             Py_DECREF(py_body);
//             Py_DECREF(args);
//             return res;
//         }
//         case PGF_EXPR_APP: {
//             PgfExprApp* eapp = i.data;
//
//             ExprObject* pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//             if (pyexpr == NULL) {
//                 Py_DECREF(args);
//                 return NULL;
//             }
//             pyexpr->pool   = NULL;
//             pyexpr->master = (self->master) ? self->master : (PyObject*) self;
//             pyexpr->expr   = eapp->arg;
//             Py_INCREF(pyexpr->master);
//
//             if (PyList_Insert(args, 0, (PyObject*) pyexpr) == -1) {
//                 Py_DECREF(args);
//                 return NULL;
//             }
//
//             Py_DECREF((PyObject*) pyexpr);
//
//             expr = eapp->fun;
//             break;
//         }
//         case PGF_EXPR_LIT: {
//             PgfExprLit* elit = i.data;
//
//             Py_DECREF(args);
//
//             GuVariantInfo i = gu_variant_open(elit->lit);
//             switch (i.tag) {
//             case PGF_LITERAL_STR: {
//                 PgfLiteralStr* lstr = i.data;
//                 return PyString_FromString(lstr->val);
//             }
//             case PGF_LITERAL_INT: {
//                 PgfLiteralInt* lint = i.data;
//                 return PyInt_FromLong(lint->val);
//             }
//             case PGF_LITERAL_FLT: {
//                 PgfLiteralFlt* lflt = i.data;
//                 return PyFloat_FromDouble(lflt->val);
//             }
//             default:
//                 gu_impossible();
//                 return NULL;
//             }
//         }
//         case PGF_EXPR_META: {
//             PyObject* res = Py_BuildValue("OO", Py_None, args);
//             Py_DECREF(args);
//             return res;
//         }
//         case PGF_EXPR_FUN: {
//             PgfExprFun* efun = i.data;
//             PyObject* fun = PyString_FromString(efun->fun);
//             PyObject* res = Py_BuildValue("OO", fun, args);
//             Py_DECREF(fun);
//             Py_DECREF(args);
//             return res;
//         }
//         case PGF_EXPR_VAR: {
//             PgfExprVar* evar = i.data;
//             PyObject* res = Py_BuildValue("iO", evar->var, args);
//             Py_DECREF(args);
//             return res;
//         }
//         case PGF_EXPR_TYPED: {
//             PgfExprTyped* etyped = i.data;
//             expr = etyped->expr;
//             break;
//         }
//         case PGF_EXPR_IMPL_ARG: {
//             PgfExprImplArg* eimpl = i.data;
//             expr = eimpl->expr;
//             break;
//         }
//         default:
//             gu_impossible();
//             return NULL;
//         }
//     }
//     return NULL;
// }
//
// static PyObject*
// Expr_visit(ExprObject* self, PyObject *args)
// {
//     PyObject* py_visitor = NULL;
//     PgfExpr expr = self->expr;
//     if (!PyArg_ParseTuple(args, "O", &py_visitor))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//
//     PgfApplication* app = pgf_expr_unapply(expr, tmp_pool);
//     if (app != NULL) {
//         char* method_name = gu_malloc(tmp_pool, strlen(app->fun)+4);
//         strcpy(method_name, "on_");
//         strcat(method_name, app->fun);
//
//         if (PyObject_HasAttrString(py_visitor, method_name)) {
//             PyObject* method_args = PyTuple_New(app->n_args);
//             if (method_args == NULL) {
//                 gu_pool_free(tmp_pool);
//                 return NULL;
//             }
//
//             for (size_t i = 0; i < app->n_args; i++) {
//                 ExprObject* pyarg = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//                 if (pyarg == NULL) {
//                     Py_DECREF(args);
//                     gu_pool_free(tmp_pool);
//                     return NULL;
//                 }
//                 pyarg->pool   = NULL;
//                 pyarg->master = (self->master) ? self->master : (PyObject*) self;
//                 pyarg->expr   = app->args[i];
//                 Py_INCREF(pyarg->master);
//
//                 if (PyTuple_SetItem(method_args, i, (PyObject*) pyarg) == -1) {
//                     Py_DECREF(args);
//                     gu_pool_free(tmp_pool);
//                     return NULL;
//                 }
//             }
//
//             PyObject* method =
//                 PyObject_GetAttrString(py_visitor, method_name);
//             if (method == NULL) {
//                 Py_DECREF(args);
//                 gu_pool_free(tmp_pool);
//                 return NULL;
//             }
//
//             gu_pool_free(tmp_pool);
//
//             return PyObject_CallObject(method, method_args);
//         }
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return PyObject_CallMethod(py_visitor, "default", "O", self);
// }
//
// static PyObject*
// Expr_reduce_ex(ExprObject* self, PyObject *args)
// {
//     int protocol;
//     if (!PyArg_ParseTuple(args, "i", &protocol))
//         return NULL;
//
//     PyObject* myModule = PyImport_ImportModule("pgf");
//     if (myModule == NULL)
//         return NULL;
//     PyObject* py_readExpr = PyObject_GetAttrString(myModule, "readExpr");
//     Py_DECREF(myModule);
//     if (py_readExpr == NULL)
//         return NULL;
//
//     PyObject* py_str = Expr_repr(self);
//     if (py_str == NULL) {
//         Py_DECREF(py_readExpr);
//         return NULL;
//     }
//
//     PyObject* py_tuple =
//         Py_BuildValue("O(O)", py_readExpr, py_str);
//
//     Py_DECREF(py_str);
//     Py_DECREF(py_readExpr);
//
//     return py_tuple;
// }
//
// static PyObject*
// Expr_getattro(ExprObject *self, PyObject *attr_name) {
// #if PY_MAJOR_VERSION >= 3
// #define IS_ATTR(attr) (PyUnicode_CompareWithASCIIString(attr_name,attr) == 0)
// #else
//     const char* name = PyString_AsString(attr_name);
// #define IS_ATTR(attr) (strcmp(name, attr) == 0)
// #endif
//
//     PgfExpr expr = self->expr;
//
// redo:;
//     GuVariantInfo i = gu_variant_open(expr);
//     switch (i.tag) {
//     case PGF_EXPR_APP: {
//         PgfExprApp* eapp = i.data;
//
//         ExprObject* pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//         if (pyexpr == NULL)
//             return NULL;
//         pyexpr->pool   = NULL;
//         pyexpr->master = (self->master) ? self->master : (PyObject*) self;
//         pyexpr->expr   = gu_null_variant;
//         Py_INCREF(pyexpr->master);
//
//         if (IS_ATTR("fun")) {
//             pyexpr->expr = eapp->fun;
//             return ((PyObject*) pyexpr);
//         } else if (IS_ATTR("arg")) {
//             pyexpr->expr = eapp->arg;
//             return ((PyObject*) pyexpr);
//         } else {
//             Py_DECREF(pyexpr);
//         }
//         break;
//     }
//     case PGF_EXPR_LIT: {
//         PgfExprLit* elit = i.data;
//
//         if (IS_ATTR("val")) {
//             GuVariantInfo i = gu_variant_open(elit->lit);
//             switch (i.tag) {
//             case PGF_LITERAL_INT: {
//                 PgfLiteralInt* lint = i.data;
//                 return PyInt_FromLong(lint->val);
//             }
//             case PGF_LITERAL_FLT: {
//                 PgfLiteralFlt* lflt = i.data;
//                 return PyFloat_FromDouble(lflt->val);
//             }
//             case PGF_LITERAL_STR: {
//                 PgfLiteralStr* lstr = i.data;
//                 return PyString_FromString(lstr->val);
//             }
//             }
//         }
//         break;
//     }
//     case PGF_EXPR_META: {
//         PgfExprMeta* emeta = i.data;
//         if (IS_ATTR("id"))
//             return PyInt_FromLong(emeta->id);
//         break;
//     }
//     case PGF_EXPR_FUN: {
//         PgfExprFun* efun = i.data;
//         if (IS_ATTR("name")) {
//             return PyString_FromString(efun->fun);
//         }
//         break;
//     }
//     case PGF_EXPR_VAR: {
//         PgfExprVar* evar = i.data;
//         if (IS_ATTR("index")) {
//             return PyInt_FromLong(evar->var);
//         }
//         break;
//     }
//     case PGF_EXPR_TYPED: {
//         PgfExprTyped* etyped = i.data;
//         expr = etyped->expr;
//         goto redo;
//     }
//     case PGF_EXPR_IMPL_ARG: {
//         PgfExprImplArg* eimpl = i.data;
//         expr = eimpl->expr;
//         goto redo;
//     }
//     default:
//         gu_impossible();
//     }
//
//     return PyObject_GenericGetAttr((PyObject*)self, attr_name);
// }

// ----------------------------------------------------------------------------

static TypeObject*
Type_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    TypeObject* self = (TypeObject *)type->tp_alloc(type, 0);
    if (self != NULL) {
        self->master = NULL;
        // self->pool   = NULL;
        // self->type   = NULL;
        self->cat = NULL;
    }

    return self;
}

static void
Type_dealloc(TypeObject* self)
{
    if (self->master != NULL) {
        Py_DECREF(self->master);
    }
    // if (self->pool != NULL) {
    //     gu_pool_free(self->pool);
    // }

    Py_TYPE(self)->tp_free((PyObject*)self);
}

static int
Type_init(TypeObject *self, PyObject *args, PyObject *kwds)
{
//     PyObject* py_hypos;
//     const char* catname_s;
//     PyObject* py_exprs;
//     size_t n_exprs;
//     size_t n_hypos;
//
//     if (PyTuple_Size(args) == 1) {
//         py_hypos = NULL;
//         py_exprs = NULL;
//         n_exprs  = 0;
//         n_hypos  = 0;
//         if (!PyArg_ParseTuple(args, "s", &catname_s))
//             return -1;
//     } else {
//         if (!PyArg_ParseTuple(args, "O!sO!",
//                 &PyList_Type, &py_hypos,
//                 &catname_s,
//                 &PyList_Type, &py_exprs))
//             return -1;
//
//         n_exprs = PyList_Size(py_exprs);
//         n_hypos = PyList_Size(py_hypos);
//     }
//
//     self->pool = gu_new_pool();
//     self->master =
//         (n_exprs+n_hypos > 0) ? PyTuple_New(n_exprs+n_hypos) : NULL;
//
//     self->type = gu_new_flex(self->pool, PgfType, exprs, n_exprs);
//
//     self->type->hypos =
//         gu_new_seq(PgfHypo, n_hypos, self->pool);
//
//     for (size_t i = 0; i < n_hypos; i++) {
//         PyObject* obj = PyList_GetItem(py_hypos, i);
//         PyObject* py_bindtype;
//         PgfCId cid;
//         PyObject* py_type;
//
//         if (Py_TYPE(obj) == &pgf_TypeType) {
//             py_bindtype = Py_True;
//             cid = "_";
//             py_type = obj;
//         } else {
//             if (!PyTuple_Check(obj) ||
//                 PyTuple_GET_SIZE(obj) != 3) {
//                 PyErr_SetString(PyExc_TypeError, "the arguments in the first list must be triples of (boolean,string,pgf.Type)");
//                 return -1;
//             }
//
//             py_bindtype = PyTuple_GetItem(obj, 0);
//             if (!PyBool_Check(py_bindtype)) {
//                 PyErr_SetString(PyExc_TypeError, "the arguments in the first list must be triples of (boolean,string,pgf.Type)");
//                 return -1;
//             }
//
//             PyObject* py_var = PyTuple_GetItem(obj, 1);
//             if (!PyString_Check(py_var)) {
//                 PyErr_SetString(PyExc_TypeError, "the arguments in the first list must be triples of (boolean,string,pgf.Type)");
//                 return -1;
//             }
//
//             {
//                 char* s;
//                 Py_ssize_t len;
//
// #if PY_MAJOR_VERSION >= 3
//                 PyObject* bytes = PyUnicode_AsUTF8String(py_var);
//                 if (bytes == NULL)
//                     return -1;
//                 if (PyBytes_AsStringAndSize(bytes,&s,&len) < 0)
//                     return -1;
// #else
//                 if (PyString_AsStringAndSize(py_var,&s,&len) < 0)
//                     return -1;
// #endif
//
//                 cid = gu_malloc(self->pool, len+1);
//                 memcpy((char*)cid, s, len+1);
//
// #if PY_MAJOR_VERSION >= 3
//                 Py_DECREF(bytes);
// #endif
//             }
//
//             py_type = PyTuple_GetItem(obj, 2);
//             if (Py_TYPE(py_type) != &pgf_TypeType) {
//                 PyErr_SetString(PyExc_TypeError, "the arguments in the first list must be triples of (boolean,string,pgf.Type)");
//                 return -1;
//             }
//         }
//
//         PgfHypo* hypo = gu_seq_index(self->type->hypos, PgfHypo, i);
//         hypo->bind_type =
//             (py_bindtype == Py_True) ? PGF_BIND_TYPE_EXPLICIT
//                                      : PGF_BIND_TYPE_IMPLICIT;
//         hypo->cid = cid;
//         hypo->type = ((TypeObject*) py_type)->type;
//
//         PyTuple_SetItem(self->master, i, py_type);
//         Py_INCREF(py_type);
//     }
//
//     self->type->cid = gu_string_copy(catname_s, self->pool);
//
//     self->type->n_exprs = n_exprs;
//     for (Py_ssize_t i = 0; i < n_exprs; i++) {
//         PyObject* obj = PyList_GetItem(py_exprs, i);
//         if (Py_TYPE(obj) != &pgf_ExprType) {
//             PyErr_SetString(PyExc_TypeError, "the arguments in the second list must be expressions");
//             return -1;
//         }
//
//         PyTuple_SetItem(self->master, n_hypos+i, obj);
//         Py_INCREF(obj);
//
//         self->type->exprs[i] = ((ExprObject*) obj)->expr;
//     }

    return 0;
}

static PyObject *
Type_repr(TypeObject *self)
{
    // GuPool* tmp_pool = gu_local_pool();
    //
    // GuExn* err = gu_exn(tmp_pool);
    // GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
    // GuOut* out = gu_string_buf_out(sbuf);
    //
    // pgf_print_type(self->type, NULL, 0, out, err);
    //
    // PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
    //                                              gu_string_buf_length(sbuf));
    //
    // gu_pool_free(tmp_pool);
    // return pystr;

    PyErr_SetString(PyExc_TypeError, "Type_repr: not implemented");
    return NULL;
}

bool pgfTextEqual(PgfText *t1, PgfText *t2) {
    if (t1->size != t2->size) return false;
    for (size_t i = 0; i < t1->size; i++) {
        if (t1->text[i] != t2->text[i]) return false;
    }
    return true;
}

static PyObject *
Type_richcompare(TypeObject *t1, TypeObject *t2, int op)
{
    bool cmp = pgfTextEqual(t1->cat, t2->cat);

    if (op == Py_EQ) {
        if (cmp) Py_RETURN_TRUE;  else Py_RETURN_FALSE;
    } else if (op == Py_NE) {
        if (cmp) Py_RETURN_FALSE; else Py_RETURN_TRUE;
    } else {
        PyErr_SetString(PyExc_TypeError, "comparison operation not supported");
        return NULL;
    }
}

// static PyObject*
// Type_getHypos(TypeObject *self, void *closure)
// {
//     PgfType* type = self->type;
//
//     PyObject* py_hypos = PyList_New(0);
//     if (py_hypos == NULL)
//         return NULL;
//
//     size_t n_hypos = gu_seq_length(type->hypos);
//     for (size_t i = 0; i < n_hypos; i++) {
//         PgfHypo* hypo = gu_seq_index(type->hypos, PgfHypo, i);
//
//         PyObject* py_bindtype =
//             (hypo->bind_type == PGF_BIND_TYPE_EXPLICIT) ? Py_True
//                                                           : Py_False;
//
//         PyObject* py_var = PyString_FromString(hypo->cid);
//         if (py_var == NULL)
//             goto fail;
//
//         TypeObject* py_type = (TypeObject*) pgf_TypeType.tp_alloc(&pgf_TypeType, 0);
//         if (py_type == NULL) {
//             Py_DECREF(py_var);
//             goto fail;
//         }
//
//         py_type->pool   = NULL;
//         py_type->master = (PyObject*) self;
//         py_type->type   = hypo->type;
//         Py_INCREF(self);
//
//         PyObject* py_hypo =
//             Py_BuildValue("OOO", py_bindtype, py_var, py_type);
//         Py_DECREF(py_var);
//         Py_DECREF(py_type);
//
//         if (py_hypo == NULL)
//             goto fail;
//
//         if (PyList_Append(py_hypos, (PyObject*) py_hypo) == -1)
//             goto fail;
//
//         Py_DECREF(py_hypo);
//     }
//
//     return py_hypos;
//
// fail:
//     Py_DECREF(py_hypos);
//     return NULL;
// }
//
// static PyObject*
// Type_getCat(TypeObject *self, void *closure)
// {
//     return PyString_FromString(self->type->cid);
// }
//
// static PyObject*
// Type_getExprs(TypeObject *self, void *closure)
// {
//     PgfType* type = self->type;
//
//     PyObject* py_exprs = PyList_New(0);
//     if (py_exprs == NULL)
//         return NULL;
//
//     for (size_t i = 0; i < type->n_exprs; i++) {
//         ExprObject* py_expr =
//             (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//         if (py_expr == NULL)
//             goto fail;
//         py_expr->pool   = NULL;
//         py_expr->master = (PyObject*) self;
//         py_expr->expr   = type->exprs[i];
//         Py_INCREF(py_expr->master);
//
//         if (PyList_Append(py_exprs, (PyObject*) py_expr) == -1)
//             goto fail;
//
//         Py_DECREF((PyObject*) py_expr);
//     }
//
//     return py_exprs;
//
// fail:
//     Py_DECREF(py_exprs);
//     return NULL;
// }
//
// static PyObject*
// Type_unpack(TypeObject* self, PyObject *fargs)
// {
//     PyObject* res = NULL;
//     PyObject* py_hypos = NULL;
//     PyObject* py_cat = NULL;
//     PyObject* py_exprs = NULL;
//
//     py_hypos = Type_getHypos(self, NULL);
//     if (py_hypos == NULL)
//         goto fail;
//
//     py_cat = Type_getCat(self, NULL);
//     if (py_cat == NULL)
//         goto fail;
//
//     py_exprs = Type_getExprs(self, NULL);
//     if (py_exprs == NULL)
//         goto fail;
//
//     res = Py_BuildValue("OOO", py_hypos, py_cat, py_exprs);
//
// fail:
//     Py_XDECREF(py_hypos);
//     Py_XDECREF(py_cat);
//     Py_XDECREF(py_exprs);
//     return res;
// }
//
// static PyObject*
// Type_reduce_ex(TypeObject* self, PyObject *args)
// {
//     int protocol;
//     if (!PyArg_ParseTuple(args, "i", &protocol))
//         return NULL;
//
//     PyObject* myModule = PyImport_ImportModule("pgf");
//     if (myModule == NULL)
//         return NULL;
//     PyObject* py_readType = PyObject_GetAttrString(myModule, "readType");
//     Py_DECREF(myModule);
//     if (py_readType == NULL)
//         return NULL;
//
//     PyObject* py_str = Type_repr(self);
//     if (py_str == NULL) {
//         Py_DECREF(py_readType);
//         return NULL;
//     }
//
//     PyObject* py_tuple =
//         Py_BuildValue("O(O)", py_readType, py_str);
//
//     Py_DECREF(py_str);
//     Py_DECREF(py_readType);
//
//     return py_tuple;
// }
//
static PyMethodDef Type_methods[] = {
//     {"unpack", (PyCFunction)Type_unpack, METH_VARARGS,
//      "Decomposes a type into its components"
//     },
//     {"__reduce_ex__", (PyCFunction)Type_reduce_ex, METH_VARARGS,
//      "This method allows for transparent pickling/unpickling of types."
//     },
    {NULL}  /* Sentinel */
};

static PyGetSetDef Type_getseters[] = {
//     {"hypos",
//      (getter)Type_getHypos, NULL,
//      "this is the list of hypotheses in the type signature",
//      NULL},
//     {"cat",
//      (getter)Type_getCat, NULL,
//      "this is the name of the category",
//      NULL},
//     {"exprs",
//      (getter)Type_getExprs, NULL,
//      "this is the list of indices for the category",
//      NULL},
    {NULL}  /* Sentinel */
};

/* static */
PyTypeObject pgf_TypeType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Type",                /*tp_name*/
    sizeof(TypeObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) Type_dealloc, /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    (reprfunc) Type_repr,      /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "abstract syntax type",    /*tp_doc*/
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    (richcmpfunc) Type_richcompare, /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    Type_methods,              /*tp_methods */
    0,                         /*tp_members */
    Type_getseters,            /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc) Type_init,      /*tp_init */
    0,                         /*tp_alloc */
    (newfunc) Type_new,        /*tp_new */
};
