#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <structmember.h>

#include <pgf/pgf.h>
#include "./compat.h"
#include "./expr.h"
#include "./marshaller.h"

static PyObject *PGFError;

static
PgfExnType handleError(PgfExn err)
{
    if (err.type == PGF_EXN_SYSTEM_ERROR) {
        errno = err.code;
        PyErr_SetFromErrnoWithFilename(PyExc_IOError, err.msg);
    } else if (err.type == PGF_EXN_PGF_ERROR) {
        PyErr_SetString(PGFError, err.msg);
        free((char*) err.msg);
    } else if (err.type == PGF_EXN_OTHER_ERROR) {
        PyErr_SetString(PGFError, "an unknown error occured");
    }
    return err.type;
}

// static PyObject* ParseError;

// static PyObject* TypeError;

typedef struct {
    PyObject_HEAD
    PgfDB *db;
    PgfRevision revision;
} PGFObject;

// typedef struct IterObject {
//     PyObject_HEAD
//     PyObject* source;
//     PyObject* container;
//     GuPool* pool;
//     int max_count;
//     int counter;
//     GuEnum* res;
//     PyObject* (*fetch)(struct IterObject* self);
// } IterObject;
//
// static PyObject*
// Iter_fetch_expr(IterObject* self)
// {
//     PgfExprProb* ep = gu_next(self->res, PgfExprProb*, self->pool);
//     if (ep == NULL)
//         return NULL;
//
//     ExprObject* pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (pyexpr == NULL)
//         return NULL;
//     pyexpr->pool   = NULL;
//     pyexpr->expr   = ep->expr;
//     pyexpr->master = self->container;
//     Py_XINCREF(self->container);
//
//     PyObject* res = Py_BuildValue("(f,O)", ep->prob, pyexpr);
//     Py_DECREF(pyexpr);
//
//     return res;
// }
//
// static PyObject*
// Iter_fetch_token(IterObject* self)
// {
//     PgfTokenProb* tp = gu_next(self->res, PgfTokenProb*, self->pool);
//     if (tp == NULL)
//         return NULL;
//
//     PyObject* py_tok = PyString_FromString(tp->tok);
//     PyObject* py_cat = PyString_FromString(tp->cat);
//     PyObject* py_fun = PyString_FromString(tp->fun);
//     PyObject* res = Py_BuildValue("(f,O,O,O)", tp->prob, py_tok, py_cat, py_fun);
//     Py_DECREF(py_fun);
//     Py_DECREF(py_cat);
//     Py_DECREF(py_tok);
//
//     return res;
// }
//
//
// static IterObject*
// Iter_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
// {
//     IterObject* self = (IterObject *)type->tp_alloc(type, 0);
//     if (self != NULL) {
//         self->source = NULL;
//         self->container = NULL;
//         self->pool = NULL;
//         self->max_count = -1;
//         self->counter   = 0;
//         self->res  = NULL;
//     }
//
//     return self;
// }
//
// static void
// Iter_dealloc(IterObject* self)
// {
//     if (self->pool != NULL)
//         gu_pool_free(self->pool);
//
//     Py_XDECREF(self->source);
//
//     Py_XDECREF(self->container);
//
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }
//
// static int
// Iter_init(IterObject *self, PyObject *args, PyObject *kwds)
// {
//     return -1;
// }
//
// static PyObject*
// Iter_iter(IterObject *self)
// {
//     Py_INCREF(self);
//     return (PyObject*) self;
// }
//
// static PyObject*
// Iter_iternext(IterObject *self)
// {
//     if (self->max_count >= 0 && self->counter >= self->max_count) {
//         return NULL;
//     }
//     self->counter++;
//
//     return self->fetch(self);
// }
//
// static PyMethodDef Iter_methods[] = {
//     {NULL}  /* Sentinel */
// };
//
// static PyTypeObject pgf_IterType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                         /*ob_size*/
//     "pgf.Iter",                /*tp_name*/
//     sizeof(IterObject),        /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     (destructor)Iter_dealloc,  /*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     0,                         /*tp_hash */
//     0,                         /*tp_call*/
//     0,                         /*tp_str*/
//     0,                         /*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "an iterator over a sequence of expressions",/*tp_doc*/
//     0,                           /*tp_traverse */
//     0,                           /*tp_clear */
//     0,                           /*tp_richcompare */
//     0,                           /*tp_weaklistoffset */
//     (getiterfunc) Iter_iter,   /*tp_iter */
//     (iternextfunc) Iter_iternext, /*tp_iternext */
//     Iter_methods,              /*tp_methods */
//     0,                         /*tp_members */
//     0,                         /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     (initproc)Iter_init,       /*tp_init */
//     0,                         /*tp_alloc */
//     (newfunc) Iter_new,        /*tp_new */
// };
//
// typedef struct {
//     PyObject_HEAD
//     PGFObject* grammar;
//     PgfConcr* concr;
// } ConcrObject;
//
// static ConcrObject*
// Concr_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
// {
//     ConcrObject* self = (ConcrObject *)type->tp_alloc(type, 0);
//     if (self != NULL) {
//         self->grammar = NULL;
//         self->concr   = NULL;
//     }
//
//     return self;
// }
//
// static void
// Concr_dealloc(ConcrObject* self)
// {
//     Py_XDECREF(self->grammar);
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }
//
// static int
// Concr_init(ConcrObject *self, PyObject *args, PyObject *kwds)
// {
//     return -1;
// }
//
// static PyObject*
// Concr_printName(ConcrObject* self, PyObject *args)
// {
//     GuString id;
//     if (!PyArg_ParseTuple(args, "s", &id))
//         return NULL;
//
//     GuString name = pgf_print_name(self->concr, id);
//     if (name == NULL)
//         Py_RETURN_NONE;
//
//     return PyString_FromString(name);
// }
//
// #if (    (PY_VERSION_HEX <  0x02070000) \
//      || ((PY_VERSION_HEX >= 0x03000000) \
//       && (PY_VERSION_HEX <  0x03010000)) )
//
// #define PyPool_New(pool) \
//         PyCObject_FromVoidPtr(pool, gu_pool_free)
//
// #else
//
// #define PGF_CONTAINER_NAME "pgf.Container"
//
// static void pypgf_container_descructor(PyObject *capsule)
// {
//     GuPool* pool = PyCapsule_GetPointer(capsule, PGF_CONTAINER_NAME);
//     gu_pool_free(pool);
// }
//
// #define PyPool_New(pool) \
//         PyCapsule_New(pool, PGF_CONTAINER_NAME, \
//                       pypgf_container_descructor)
//
// #endif
//
// typedef struct {
//     PgfLiteralCallback callback;
//     PyObject* pycallback;
//     GuFinalizer fin;
// } PyPgfLiteralCallback;
//
// #if PY_MAJOR_VERSION >= 3
// static size_t
// utf8_to_unicode_offset(GuString sentence, size_t offset)
// {
//     const uint8_t* start = (uint8_t*) sentence;
//     const uint8_t* end   = start+offset;
//
//     size_t chars = 0;
//     while (start < end) {
//         gu_utf8_decode(&start);
//         chars++;
//     }
//
//     return chars;
// }
//
// static size_t
// unicode_to_utf8_offset(GuString sentence, size_t chars)
// {
//     const uint8_t* start = (uint8_t*) sentence;
//     const uint8_t* end   = start;
//
//     while (chars > 0) {
//         GuUCS ucs = gu_utf8_decode(&end);
//         if (ucs == 0)
//             break;
//         chars--;
//     }
//
//     return (end-start);
// }
// #endif
//
// static PgfExprProb*
// pypgf_literal_callback_match(PgfLiteralCallback* self, PgfConcr* concr,
//                              GuString ann,
//                              GuString sentence, size_t* poffset,
//                              GuPool *out_pool)
// {
//     PyPgfLiteralCallback* callback =
//         gu_container(self, PyPgfLiteralCallback, callback);
//
//     PyObject* result =
//         PyObject_CallFunction(callback->pycallback, "si",
//                               ann,
// #if PY_MAJOR_VERSION >= 3
//                               utf8_to_unicode_offset(sentence, *poffset)
// #else
//                               *poffset
// #endif
//                              );
//     if (result == NULL) {
//         PyErr_Print();
//         return NULL;
//     }
//
//     if (result == Py_None) {
//         Py_DECREF(result);
//         return NULL;
//     }
//
//     PgfExprProb* ep = gu_new(PgfExprProb, out_pool);
//
//     ExprObject* pyexpr;
// #if PY_MAJOR_VERSION >= 3
//     int chars;
//     if (!PyArg_ParseTuple(result, "Ofi", &pyexpr, &ep->prob, &chars))
//         return NULL;
//     *poffset = unicode_to_utf8_offset(sentence, chars);
// #else
//     if (!PyArg_ParseTuple(result, "Ofi", &pyexpr, &ep->prob, poffset))
//         return NULL;
// #endif
//
//     ep->expr = pgf_clone_expr(pyexpr->expr, out_pool);
//
//     Py_DECREF(result);
//
//     return ep;
// }
//
// static GuEnum*
// pypgf_literal_callback_predict(PgfLiteralCallback* self, PgfConcr* concr,
//                                GuString ann,
//                                GuString prefix,
//                                GuPool *out_pool)
// {
//     return NULL;
// }
//
// static void
// pypgf_literal_callback_fin(GuFinalizer* self)
// {
//     PyPgfLiteralCallback* callback =
//         gu_container(self, PyPgfLiteralCallback, fin);
//
//     Py_XDECREF(callback->pycallback);
// }
//
// static PgfCallbacksMap*
// pypgf_new_callbacks_map(PgfConcr* concr, PyObject *py_callbacks,
//                         GuPool* pool)
// {
//     PgfCallbacksMap* callbacks =
//         pgf_new_callbacks_map(concr, pool);
//
//     if (py_callbacks == NULL)
//         return callbacks;
//
//     size_t n_callbacks = PyList_Size(py_callbacks);
//     for (size_t i = 0; i < n_callbacks; i++) {
//         PyObject* item =
//             PyList_GetItem(py_callbacks, i);
//
//         PyObject* pycallback = NULL;
//         const char* cat = NULL;
//         if (!PyArg_ParseTuple(item, "sO", &cat, &pycallback))
//             return NULL;
//
//         PyPgfLiteralCallback* callback = gu_new(PyPgfLiteralCallback, pool);
//         callback->callback.match   = pypgf_literal_callback_match;
//         callback->callback.predict = pypgf_literal_callback_predict;
//         callback->pycallback = pycallback;
//         callback->fin.fn = pypgf_literal_callback_fin;
//
//         Py_XINCREF(callback->pycallback);
//
//         gu_pool_finally(pool, &callback->fin);
//
//         pgf_callbacks_map_add_literal(concr, callbacks,
//                                       cat, &callback->callback);
//     }
//
//     return callbacks;
// }
//
// static PgfType*
// pgf_type_from_object(PyObject* obj, GuPool* pool) {
//     if (PyString_Check(obj)) {
//         PgfType* type = gu_new_flex(pool, PgfType, exprs, 0);
//         type->hypos   = gu_empty_seq();
//         type->cid     = "";
//         type->n_exprs = 0;
//         return type;
//     } else if (obj->ob_type == &pgf_TypeType) {
//         return ((TypeObject*) obj)->type;
//     } else {
//         PyErr_SetString(PyExc_TypeError, "the start category should be a string or a type");
//         return NULL;
//     }
// }
//
// static IterObject*
// Concr_parse(ConcrObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"sentence", "cat", "n", "heuristics", "callbacks", NULL};
//
//     const char *sentence = NULL;
//     PyObject* start = NULL;
//     int max_count = -1;
//     double heuristics = -1;
//     PyObject* py_callbacks = NULL;
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "s|OidO!", kwlist,
//                                      &sentence, &start, &max_count,
//                                      &heuristics,
//                                      &PyList_Type, &py_callbacks))
//         return NULL;
//
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         return NULL;
//     }
//
//     pyres->source = (PyObject*) self->grammar;
//     Py_XINCREF(pyres->source);
//
//     GuPool* out_pool = gu_new_pool();
//
//     PyObject* py_pool = PyPool_New(out_pool);
//     pyres->container = PyTuple_Pack(2, pyres->source, py_pool);
//     Py_DECREF(py_pool);
//
//     pyres->pool      = gu_new_pool();
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_expr;
//
//     GuExn* parse_err = gu_exn(pyres->pool);
//
//     PgfCallbacksMap* callbacks =
//         pypgf_new_callbacks_map(self->concr, py_callbacks, pyres->pool);
//     if (callbacks == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     sentence = gu_string_copy(sentence, pyres->pool);
//
//     PgfType* type;
//     if (start == NULL) {
//         type = pgf_start_cat(self->grammar->pgf, pyres->pool);
//     } else {
//         type = pgf_type_from_object(start, pyres->pool);
//     }
//     if (type == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     pyres->res =
//         pgf_parse_with_heuristics(self->concr, type, sentence,
//                                   heuristics, callbacks, parse_err,
//                                   pyres->pool, out_pool);
//
//     if (!gu_ok(parse_err)) {
//         if (gu_exn_caught(parse_err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(parse_err);
//             PyErr_SetString(PGFError, msg);
//         } else if (gu_exn_caught(parse_err, PgfParseError)) {
//             PgfParseError* err = (PgfParseError*) gu_exn_caught_data(parse_err);
//             PyObject* py_offset = PyInt_FromLong(err->offset);
//             if (err->incomplete) {
//                 PyObject_SetAttrString(ParseError, "incomplete",  Py_True);
//                 PyObject_SetAttrString(ParseError, "offset",      py_offset);
//                 PyErr_Format(ParseError, "The sentence is incomplete");
//             } else {
//                 PyObject* py_tok    = PyString_FromStringAndSize(err->token_ptr,
//                                                                  err->token_len);
//                 PyObject_SetAttrString(ParseError, "incomplete",  Py_False);
//                 PyObject_SetAttrString(ParseError, "offset",      py_offset);
//                 PyObject_SetAttrString(ParseError, "token",       py_tok);
// #if PY_MAJOR_VERSION >= 3
//                 PyErr_Format(ParseError, "Unexpected token: \"%U\"", py_tok);
// #else
//                 PyErr_Format(ParseError, "Unexpected token: \"%s\"", PyString_AsString(py_tok));
// #endif
//                 Py_DECREF(py_tok);
//             }
//             Py_DECREF(py_offset);
//         }
//
//         Py_DECREF(pyres);
//         pyres = NULL;
//     }
//
//     return pyres;
// }
//
// static IterObject*
// Concr_complete(ConcrObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"sentence", "cat", "prefix", "n", NULL};
//
//     const char *sentence = NULL;
//     PyObject* start = NULL;
//     GuString prefix = "";
//     int max_count = -1;
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "s|Osi", kwlist,
//                                      &sentence, &start,
//                                      &prefix, &max_count))
//         return NULL;
//
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         return NULL;
//     }
//
//     pyres->source = (PyObject*) self->grammar;
//     Py_XINCREF(pyres->source);
//
//     pyres->container = NULL;
//
//     pyres->pool = gu_new_pool();
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_token;
//
//     GuPool *tmp_pool = gu_local_pool();
//
//     GuExn* parse_err = gu_new_exn(tmp_pool);
//
//     PgfType* type;
//     if (start == NULL) {
//         type = pgf_start_cat(self->grammar->pgf, pyres->pool);
//     } else {
//         type = pgf_type_from_object(start, pyres->pool);
//     }
//     if (type == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     pyres->res =
//         pgf_complete(self->concr, type, sentence, prefix, parse_err, pyres->pool);
//
//     if (!gu_ok(parse_err)) {
//         Py_DECREF(pyres);
//         pyres = NULL;
//
//         if (gu_exn_caught(parse_err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(parse_err);
//             PyErr_SetString(PGFError, msg);
//         } else if (gu_exn_caught(parse_err, PgfParseError)) {
//             GuString tok = (GuString) gu_exn_caught_data(parse_err);
//             PyObject* py_tok = PyString_FromString(tok);
//             PyObject_SetAttrString(ParseError, "token", py_tok);
//             PyErr_Format(ParseError, "Unexpected token: \"%s\"", tok);
//             Py_DECREF(py_tok);
//         }
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return pyres;
// }
//
// static PyObject*
// Concr_parseval(ConcrObject* self, PyObject *args) {
//     ExprObject* pyexpr = NULL;
//     PyObject* start = NULL;
//     if (!PyArg_ParseTuple(args, "O!s", &pgf_ExprType, &pyexpr, &start))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//
//     double precision = 0;
//     double recall = 0;
//     double exact = 0;
//
//     PgfType* type = pgf_type_from_object(start, tmp_pool);
//     if (type == NULL) {
//         return NULL;
//     }
//
//     if (!pgf_parseval(self->concr, pyexpr->expr, type,
//                       &precision, &recall, &exact))
//         return NULL;
//
//     gu_pool_free(tmp_pool);
//
//     return Py_BuildValue("ddd", precision, recall, exact);
// }
//
// static IterObject*
// Concr_lookupSentence(ConcrObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"sentence", "cat", NULL};
//
//     const char *sentence = NULL;
//     PyObject* start = NULL;
//     int max_count = -1;
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "s|O", kwlist,
//                                      &sentence, &start, &max_count))
//         return NULL;
//
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         return NULL;
//     }
//
//     pyres->source = (PyObject*) self->grammar;
//     Py_XINCREF(pyres->source);
//
//     GuPool* out_pool = gu_new_pool();
//
//     PyObject* py_pool = PyPool_New(out_pool);
//     pyres->container = PyTuple_Pack(2, pyres->source, py_pool);
//     Py_DECREF(py_pool);
//
//     pyres->pool      = gu_new_pool();
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_expr;
//
//     sentence = gu_string_copy(sentence, pyres->pool);
//
//     PgfType* type;
//     if (start == NULL) {
//         type = pgf_start_cat(self->grammar->pgf, pyres->pool);
//     } else {
//         type = pgf_type_from_object(start, pyres->pool);
//     }
//     if (type == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     pyres->res =
//         pgf_lookup_sentence(self->concr, type, sentence,
//                                   pyres->pool, out_pool);
//
//     return pyres;
// }
//
// static PyObject*
// Concr_linearize(ConcrObject* self, PyObject *args)
// {
//     ExprObject* pyexpr;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//     pgf_linearize(self->concr, pyexpr->expr, out, err);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfLinNonExist)) {
//             gu_pool_free(tmp_pool);
//             Py_RETURN_NONE;
//         }
//         else if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//             gu_pool_free(tmp_pool);
//             return NULL;
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be linearized");
//             gu_pool_free(tmp_pool);
//             return NULL;
//         }
//     }
//
//     PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                  gu_string_buf_length(sbuf));
//
//     gu_pool_free(tmp_pool);
//     return pystr;
// }
//
// static PyObject*
// Iter_fetch_linearization(IterObject* self)
// {
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_new_exn(tmp_pool);
//
// restart:;
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//
//     PgfCncTree ctree = gu_next(self->res, PgfCncTree, tmp_pool);
//     if (gu_variant_is_null(ctree)) {
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//     ctree = pgf_lzr_wrap_linref(ctree, tmp_pool); // to reduce tuple of strings to a single string;
//
//     // Linearize the concrete tree as a simple sequence of strings.
//     ConcrObject* pyconcr = (ConcrObject*)self->container;
//     pgf_lzr_linearize_simple(pyconcr->concr, ctree, 0, out, err, tmp_pool);
//
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfLinNonExist)) {
//             // encountered nonExist. Unfortunately there
//             // might be some output printed already. The
//             // right solution should be to use GuStringBuf.
//             gu_exn_clear(err);
//             goto restart;
//         }
//         else if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//             gu_pool_free(tmp_pool);
//             return NULL;
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be linearized");
//             gu_pool_free(tmp_pool);
//             return NULL;
//         }
//     }
//
//     PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                  gu_string_buf_length(sbuf));
//     gu_pool_free(tmp_pool);
//     return pystr;
// }
//
// static PyObject*
// Concr_linearizeAll(ConcrObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"expression", "n", NULL};
//     ExprObject* pyexpr = NULL;
//     int max_count = -1;
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "O!|i", kwlist,
//                                      &pgf_ExprType, &pyexpr, &max_count))
//         return NULL;
//
//     GuPool* pool = gu_new_pool();
//
//     GuExn* err = gu_exn(pool);
//     GuEnum* cts = pgf_lzr_concretize(self->concr, pyexpr->expr, err, pool);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//             gu_pool_free(pool);
//             return NULL;
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be linearized");
//             gu_pool_free(pool);
//             return NULL;
//         }
//     }
//
//     IterObject* pyres = (IterObject*) pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         gu_pool_free(pool);
//         return NULL;
//     }
//
//     pyres->source = (PyObject*)pyexpr;
//     Py_INCREF(pyres->source);
//     pyres->container = (PyObject*)self;
//     Py_INCREF(pyres->container);
//     pyres->pool = pool;
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_linearization;
//     pyres->res       = cts;
//
//     return (PyObject*)pyres;
// }
//
// static PyObject*
// Concr_tabularLinearize(ConcrObject* self, PyObject *args)
// {
//     ExprObject* pyexpr;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
//         return NULL;
//
//     PyObject* table = PyDict_New();
//     if (table == NULL)
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//
//     GuEnum* cts =
//         pgf_lzr_concretize(self->concr,
//                            pyexpr->expr,
//                            err,
//                            tmp_pool);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfLinNonExist))
//             Py_RETURN_NONE;
//         else if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//             return NULL;
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be linearized");
//             return NULL;
//         }
//     }
//
//     PgfCncTree ctree = gu_next(cts, PgfCncTree, tmp_pool);
//     if (gu_variant_is_null(ctree)) {
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     size_t n_lins;
//     GuString* labels;
//     pgf_lzr_get_table(self->concr, ctree, &n_lins, &labels);
//
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//     for (size_t lin_idx = 0; lin_idx < n_lins; lin_idx++) {
//
//
//         pgf_lzr_linearize_simple(self->concr, ctree, lin_idx, out, err, tmp_pool);
//
//         PyObject* pystr = NULL;
//         if (gu_ok(err)) {
//             pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                gu_string_buf_length(sbuf));
//         } else {
//             gu_exn_clear(err);
//             pystr = Py_None;
//             Py_INCREF(pystr);
//         }
//
//         gu_string_buf_flush(sbuf);
//
//         if (PyDict_SetItemString(table, labels[lin_idx], pystr) < 0)
//             return NULL;
//
//         Py_XDECREF(pystr);
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return table;
// }
//
//
// typedef struct {
//     PyObject_HEAD
//     PyObject* cat;
//     int fid;
//     PyObject* ann;
//     PyObject* fun;
//     PyObject* children;
// } BracketObject;
//
// static void
// Bracket_dealloc(BracketObject* self)
// {
//     Py_XDECREF(self->cat);
//     Py_XDECREF(self->fun);
//     Py_XDECREF(self->children);
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }
//
// static PyObject *
// Bracket_repr(BracketObject *self)
// {
//     PyObject *repr =
// #if PY_MAJOR_VERSION >= 3
//         PyString_FromFormat("(%U:%d", self->cat, self->fid);
// #else
//         PyString_FromFormat("(%s:%d", PyString_AsString(self->cat), self->fid);
// #endif
//     if (repr == NULL) {
//         return NULL;
//     }
//
//     PyObject *space = PyString_FromString(" ");
//
//     size_t len = PyList_Size(self->children);
//     for (size_t i = 0; i < len; i++) {
//         PyObject *child = PyList_GetItem(self->children, i);
//
//         PyString_Concat(&repr, space);
//         if (repr == NULL) {
//             Py_DECREF(space);
//             return NULL;
//         }
//
//         PyObject *child_str = Py_TYPE(child)->tp_str(child);
//         if (child_str == NULL) {
//             Py_DECREF(repr);
//             Py_DECREF(space);
//             return NULL;
//         }
//
//         PyString_Concat(&repr, child_str);
//         if (repr == NULL) {
//             Py_DECREF(space);
//             return NULL;
//         }
//
//         Py_DECREF(child_str);
//     }
//
//     Py_DECREF(space);
//
//     PyObject *str = PyString_FromString(")");
//     PyString_Concat(&repr, str);
//     if (repr == NULL) {
//         Py_DECREF(str);
//         return NULL;
//     }
//     Py_DECREF(str);
//
//     return repr;
// }
//
// static PyMemberDef Bracket_members[] = {
//     {"cat", T_OBJECT_EX, offsetof(BracketObject, cat), 0,
//      "the syntactic category for this bracket"},
//     {"fun", T_OBJECT_EX, offsetof(BracketObject, fun), 0,
//      "the abstract function for this bracket"},
//     {"fid", T_INT, offsetof(BracketObject, fid), 0,
//      "an id which identifies this bracket in the bracketed string. If there are discontinuous phrases this id will be shared for all brackets belonging to the same phrase."},
//     {"ann", T_OBJECT_EX, offsetof(BracketObject, ann), 0,
//      "the analysis of the constituent"},
//     {"children", T_OBJECT_EX, offsetof(BracketObject, children), 0,
//      "a list with the children of this bracket"},
//     {NULL}  /* Sentinel */
// };
//
// static PyTypeObject pgf_BracketType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                         /*ob_size*/
//     "pgf.Bracket",             /*tp_name*/
//     sizeof(BracketObject),     /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     (destructor)Bracket_dealloc,/*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     0,                         /*tp_hash */
//     0,                         /*tp_call*/
//     (reprfunc) Bracket_repr,   /*tp_str*/
//     0,                         /*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "a linearization bracket", /*tp_doc*/
//     0,                           /*tp_traverse */
//     0,                           /*tp_clear */
//     0,                           /*tp_richcompare */
//     0,                           /*tp_weaklistoffset */
//     0,                           /*tp_iter */
//     0,                           /*tp_iternext */
//     0,                         /*tp_methods */
//     Bracket_members,           /*tp_members */
//     0,                         /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     0,                         /*tp_init */
//     0,                         /*tp_alloc */
//     0,                         /*tp_new */
// };
//
// typedef struct {
//     PyObject_HEAD
// } BINDObject;
//
// static PyObject *
// BIND_repr(BINDObject *self)
// {
//     return PyString_FromString("&+");
// }
//
// static PyTypeObject pgf_BINDType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                       /*ob_size*/
//     "pgf.BIND",                /*tp_name*/
//     sizeof(BINDObject),        /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     0,                         /*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     0,                         /*tp_hash */
//     0,                         /*tp_call*/
//     (reprfunc) BIND_repr,      /*tp_str*/
//     0,                         /*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "a marker for BIND in a bracketed string", /*tp_doc*/
//     0,                           /*tp_traverse */
//     0,                           /*tp_clear */
//     0,                           /*tp_richcompare */
//     0,                           /*tp_weaklistoffset */
//     0,                           /*tp_iter */
//     0,                           /*tp_iternext */
//     0,                         /*tp_methods */
//     0,                         /*tp_members */
//     0,                         /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     0,                         /*tp_init */
//     0,                         /*tp_alloc */
//     0,                         /*tp_new */
// };
//
// typedef struct {
//     PgfLinFuncs* funcs;
//     GuBuf* stack;
//     PyObject* list;
// } PgfBracketLznState;
//
// static void
// pgf_bracket_lzn_symbol_token(PgfLinFuncs** funcs, PgfToken tok)
// {
//     PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);
//
//     PyObject* str = PyString_FromString(tok);
//     PyList_Append(state->list, str);
//     Py_DECREF(str);
// }
//
// static void
// pgf_bracket_lzn_begin_phrase(PgfLinFuncs** funcs, PgfCId cat, int fid, GuString ann, PgfCId fun)
// {
//     PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);
//
//     gu_buf_push(state->stack, PyObject*, state->list);
//     state->list = PyList_New(0);
// }
//
// static void
// pgf_bracket_lzn_end_phrase(PgfLinFuncs** funcs, PgfCId cat, int fid, GuString ann, PgfCId fun)
// {
//     PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);
//
//     PyObject* parent = gu_buf_pop(state->stack, PyObject*);
//
//     if (PyList_Size(state->list) > 0) {
//         BracketObject* bracket = (BracketObject *)
//             pgf_BracketType.tp_alloc(&pgf_BracketType, 0);
//         if (bracket != NULL) {
//             bracket->cat = PyString_FromString(cat);
//             bracket->fid = fid;
//             bracket->ann = PyString_FromString(ann);
//             bracket->fun = PyString_FromString(fun);
//             bracket->children = state->list;
//             PyList_Append(parent, (PyObject*) bracket);
//             Py_DECREF(bracket);
//         }
//     } else {
//         Py_DECREF(state->list);
//     }
//
//     state->list = parent;
// }
//
// static void
// pgf_bracket_lzn_symbol_bind(PgfLinFuncs** funcs)
// {
//     PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);
//
//     PyObject* bind = pgf_BINDType.tp_alloc(&pgf_BINDType, 0);
//     PyList_Append(state->list, bind);
//     Py_DECREF(bind);
// }
//
// static void
// pgf_bracket_lzn_symbol_meta(PgfLinFuncs** funcs, PgfMetaId meta_id)
// {
//     pgf_bracket_lzn_symbol_token(funcs, "?");
// }
//
// static PgfLinFuncs pgf_bracket_lin_funcs = {
//     .symbol_token  = pgf_bracket_lzn_symbol_token,
//     .begin_phrase  = pgf_bracket_lzn_begin_phrase,
//     .end_phrase    = pgf_bracket_lzn_end_phrase,
//     .symbol_ne     = NULL,
//     .symbol_bind   = pgf_bracket_lzn_symbol_bind,
//     .symbol_capit  = NULL,
//     .symbol_meta   = pgf_bracket_lzn_symbol_meta
// };
//
// static PyObject*
// Concr_bracketedLinearize(ConcrObject* self, PyObject *args)
// {
//     ExprObject* pyexpr;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//
//     GuEnum* cts =
//         pgf_lzr_concretize(self->concr, pyexpr->expr, err, tmp_pool);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//             return NULL;
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be concretized");
//         }
//     }
//
//     PgfCncTree ctree = gu_next(cts, PgfCncTree, tmp_pool);
//     if (gu_variant_is_null(ctree)) {
//         PyErr_SetString(PGFError, "The abstract tree cannot be concretized");
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     PyObject* list = PyList_New(0);
//
//     ctree = pgf_lzr_wrap_linref(ctree, tmp_pool);
//
//     PgfBracketLznState state;
//     state.funcs = &pgf_bracket_lin_funcs;
//     state.stack = gu_new_buf(PyObject*, tmp_pool);
//     state.list  = list;
//     pgf_lzr_linearize(self->concr, ctree, 0, &state.funcs, tmp_pool);
//
//     gu_pool_free(tmp_pool);
//
//     return list;
// }
//
// static PyObject*
// Iter_fetch_bracketedLinearization(IterObject* self)
// {
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//
// restart:;
//
//     PgfCncTree ctree = gu_next(self->res, PgfCncTree, tmp_pool);
//     if (gu_variant_is_null(ctree)) {
//     gu_pool_free(tmp_pool);
//     return NULL;
//     }
//
//     PyObject* list = PyList_New(0);
//     ctree = pgf_lzr_wrap_linref(ctree, tmp_pool);
//
//     ConcrObject* pyconcr = (ConcrObject*) self->container;
//
//     PgfBracketLznState state;
//     state.funcs = &pgf_bracket_lin_funcs;
//     state.stack = gu_new_buf(PyObject*, tmp_pool);
//     state.list  = list;
//     pgf_lzr_linearize(pyconcr->concr, ctree, 0, &state.funcs, tmp_pool);
//
//     if (!gu_ok(err)) {
//     if (gu_exn_caught(err, PgfLinNonExist)) {
//         // encountered nonExist. Unfortunately there
//         // might be some output printed already. The
//         // right solution should be to use GuStringBuf.
//         gu_exn_clear(err);
//         goto restart;
//     }
//     else if (gu_exn_caught(err, PgfExn)) {
//         GuString msg = (GuString) gu_exn_caught_data(err);
//         PyErr_SetString(PGFError, msg);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     } else {
//         PyErr_SetString(PGFError, "The abstract tree cannot be linearized");
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//     }
//
//     gu_pool_free(tmp_pool);
//     return list;
// }
//
// static PyObject*
// Concr_bracketedLinearizeAll(ConcrObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"expression", "n", NULL};
//     ExprObject* pyexpr = NULL;
//     int max_count = -1;
//
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "O!|i", kwlist,
//                                      &pgf_ExprType, &pyexpr, &max_count))
//         return NULL;
//
//     GuPool* pool = gu_new_pool();
//     GuExn* err = gu_exn(pool);
//
//     GuEnum* cts =
//         pgf_lzr_concretize(self->concr, pyexpr->expr, err, pool);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//         } else {
//             PyErr_SetString(PGFError, "The abstract tree cannot be concretized");
//         }
//         return NULL;
//     }
//
//     IterObject* pyres = (IterObject*) pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         gu_pool_free(pool);
//         return NULL;
//     }
//
//     pyres->source = (PyObject*)pyexpr;
//     Py_INCREF(pyres->source);
//     pyres->container = (PyObject*)self;
//     Py_INCREF(pyres->container);
//     pyres->pool = pool;
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_bracketedLinearization;
//     pyres->res       = cts;
//
//     return (PyObject*)pyres;
// }
//
// static PyObject*
// Concr_hasLinearization(ConcrObject* self, PyObject *args)
// {
//     PgfCId id;
//     if (!PyArg_ParseTuple(args, "s", &id))
//         return NULL;
//
//     if (pgf_has_linearization(self->concr, id))
//         Py_RETURN_TRUE;
//     else
//         Py_RETURN_FALSE;
// }
//
// static PyObject*
// Concr_getName(ConcrObject *self, void *closure)
// {
//     return PyString_FromString(pgf_concrete_name(self->concr));
// }
//
// static PyObject*
// Concr_getLanguageCode(ConcrObject *self, void *closure)
// {
//     GuString code = pgf_language_code(self->concr);
//     if (code == NULL)
//         Py_RETURN_NONE;
//     return PyString_FromString(code);
// }
//
// static PyObject*
// Concr_graphvizParseTree(ConcrObject* self, PyObject *args) {
//     ExprObject* pyexpr;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//     pgf_graphviz_parse_tree(self->concr, pyexpr->expr, pgf_default_graphviz_options, out, err);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//         } else {
//             PyErr_SetString(PGFError, "The parse tree cannot be visualized");
//         }
//         return NULL;
//     }
//
//     PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                  gu_string_buf_length(sbuf));
//
//     gu_pool_free(tmp_pool);
//     return pystr;
// }
//
// typedef struct {
//     PgfMorphoCallback fn;
//     PyObject* analyses;
// } PyMorphoCallback;
//
// static void
// pypgf_collect_morpho(PgfMorphoCallback* self,
//                      PgfCId lemma, GuString analysis, prob_t prob,
//                      GuExn* err)
// {
//     PyMorphoCallback* callback = (PyMorphoCallback*) self;
//
//     PyObject* py_lemma = PyString_FromString(lemma);
//     PyObject* py_analysis = PyString_FromString(analysis);
//     PyObject* res =
//         Py_BuildValue("OOf", py_lemma, py_analysis, prob);
//
//     if (PyList_Append(callback->analyses, res) != 0) {
//         gu_raise(err, PgfExn);
//     }
//
//     Py_DECREF(py_lemma);
//     Py_DECREF(py_analysis);
//     Py_DECREF(res);
// }
//
// static PyObject*
// Concr_lookupMorpho(ConcrObject* self, PyObject *args) {
//     GuString sent;
//     if (!PyArg_ParseTuple(args, "s", &sent))
//         return NULL;
//
//     GuPool *tmp_pool = gu_local_pool();
//     GuExn* err = gu_exn(tmp_pool);
//
//     PyObject* analyses = PyList_New(0);
//
//     PyMorphoCallback callback = { { pypgf_collect_morpho }, analyses };
//     pgf_lookup_morpho(self->concr, sent, &callback.fn, err);
//     if (!gu_ok(err)) {
//         if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//         } else {
//             PyErr_SetString(PGFError, "The lookup failed");
//         }
//         Py_DECREF(analyses);
//         analyses = NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return analyses;
// }
//
// #define PGF_MORPHOCALLBACK_NAME "pgf.MorphoCallback"
//
// static void
// pypgf_morphocallback_destructor(PyObject *capsule)
// {
//     PyMorphoCallback* callback =
//         PyCapsule_GetPointer(capsule, PGF_MORPHOCALLBACK_NAME);
//     Py_XDECREF(callback->analyses);
// }
//
// static PyObject*
// Iter_fetch_cohort(IterObject* self)
// {
//     PgfCohortRange range =
//         gu_next(self->res, PgfCohortRange, self->pool);
//     if (range.buf == NULL)
//         return NULL;
//
//     PyObject* py_start = PyLong_FromSize_t(range.start.pos);
//     if (py_start == NULL)
//         return NULL;
//     PyObject* py_end = PyLong_FromSize_t(range.end.pos);
//     if (py_end == NULL) {
//         Py_DECREF(py_start);
//         return NULL;
//     }
//
//     PyMorphoCallback* callback =
//         PyCapsule_GetPointer(PyTuple_GetItem(self->container, 0),
//                              PGF_MORPHOCALLBACK_NAME);
//
//     PyObject* py_slice =
//         PySlice_New(py_start, py_end, NULL);
//     if (py_slice == NULL) {
//         Py_DECREF(py_start);
//         Py_DECREF(py_end);
//         return NULL;
//     }
//
//     PyObject* py_w =
//         PyObject_GetItem(PyTuple_GetItem(self->container, 1), py_slice);
//
//     PyObject* res =
//         PyTuple_Pack(4, py_start, py_w, callback->analyses, py_end);
//
//     Py_DECREF(callback->analyses);
//     callback->analyses = PyList_New(0);
//
//     Py_DECREF(py_w);
//     Py_DECREF(py_slice);
//     Py_DECREF(py_end);
//     Py_DECREF(py_start);
//
//     return res;
// }
//
// static PyObject*
// Concr_lookupCohorts(ConcrObject* self, PyObject *args)
// {
//     PyObject* py_sent = NULL;
//     if (!PyArg_ParseTuple(args, "U", &py_sent))
//         return NULL;
//
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL)
//         return NULL;
//
//     pyres->pool   = gu_new_pool();
//     pyres->source = (PyObject*) self->grammar;
//     Py_XINCREF(pyres->source);
//
//     PyMorphoCallback* callback = gu_new(PyMorphoCallback,pyres->pool);
//     callback->fn.callback = pypgf_collect_morpho;
//     callback->analyses    = PyList_New(0);
//     PyObject* capsule =
//         PyCapsule_New(callback, PGF_MORPHOCALLBACK_NAME,
//                       pypgf_morphocallback_destructor);
//     if (capsule == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
// #if PY_MAJOR_VERSION >= 3
//     PyObject* bytes = PyUnicode_AsUTF8String(py_sent);
//     if (!bytes)
//         return NULL;
//     GuString sent = PyBytes_AsString(bytes);
//     if (!sent) {
//         Py_DECREF(bytes);
//         return NULL;
//     }
// #else
//     GuString sent = PyString_AsString(py_sent);
//     if (!sent)
//         return NULL;
// #endif
//
//
//     pyres->container =
// #if PY_MAJOR_VERSION >= 3
//         PyTuple_Pack(3, capsule, py_sent, bytes);
//     Py_DECREF(bytes);
// #else
//         PyTuple_Pack(2, capsule, py_sent);
// #endif
//     pyres->max_count = -1;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_cohort;
//
//     Py_DECREF(capsule);
//
//     GuExn* err = gu_new_exn(pyres->pool);
//     pyres->res = pgf_lookup_cohorts(self->concr, sent,
//                                     &callback->fn, pyres->pool, err);
//     if (pyres->res == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     return (PyObject*) pyres;
// }
//
// static PyObject*
// Iter_fetch_fullform(IterObject* self)
// {
//     PgfFullFormEntry* entry =
//         gu_next(self->res, PgfFullFormEntry*, self->pool);
//     if (entry == NULL)
//         return NULL;
//
//     PyObject* res = NULL;
//     PyObject* py_tokens = NULL;
//     PyObject* py_analyses = NULL;
//
//     GuString tokens =
//         pgf_fullform_get_string(entry);
//
//     py_tokens = PyString_FromString(tokens);
//     if (py_tokens == NULL)
//         goto done;
//
//     py_analyses = PyList_New(0);
//     if (py_analyses == NULL)
//         goto done;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_new_exn(tmp_pool);
//
//     PyMorphoCallback callback = { { pypgf_collect_morpho }, py_analyses };
//     pgf_fullform_get_analyses(entry, &callback.fn, err);
//
//     if (!gu_ok(err))
//         goto done;
//
//     res = Py_BuildValue("OO", py_tokens, py_analyses);
//
// done:
//     Py_XDECREF(py_tokens);
//     Py_XDECREF(py_analyses);
//
//     return res;
// }
//
// static PyObject*
// Concr_fullFormLexicon(ConcrObject* self, PyObject *args)
// {
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL)
//         return NULL;
//
//     pyres->source = (PyObject*) self->grammar;
//     Py_XINCREF(pyres->source);
//
//     pyres->container = NULL;
//     pyres->pool      = gu_new_pool();
//     pyres->max_count = -1;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_fullform;
//
//     pyres->res = pgf_fullform_lexicon(self->concr, pyres->pool);
//     if (pyres->res == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     return (PyObject*) pyres;
// }
//
// static PyObject*
// Concr_load(ConcrObject* self, PyObject *args)
// {
//     const char *fpath;
//     if (!PyArg_ParseTuple(args, "s", &fpath))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//
//     // Create an exception frame that catches all errors.
//     GuExn* err = gu_new_exn(tmp_pool);
//
//     FILE* infile = fopen(fpath, "rb");
//     if (infile == NULL) {
//         PyErr_SetFromErrnoWithFilename(PyExc_IOError, fpath);
//         return NULL;
//     }
//
//     // Create an input stream from the input file
//     GuIn* in = gu_file_in(infile, tmp_pool);
//
//     // Read the PGF grammar.
//     pgf_concrete_load(self->concr, in, err);
//     if (!gu_ok(err)) {
//         fclose(infile);
//         if (gu_exn_caught(err, GuErrno)) {
//             errno = *((GuErrno*) gu_exn_caught_data(err));
//             PyErr_SetFromErrnoWithFilename(PyExc_IOError, fpath);
//         } else if (gu_exn_caught(err, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(err);
//             PyErr_SetString(PGFError, msg);
//         } else {
//             PyErr_SetString(PGFError, "The language cannot be loaded");
//         }
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     fclose(infile);
//
//     Py_RETURN_NONE;
// }
//
// static PyObject*
// Concr_unload(ConcrObject* self, PyObject *args)
// {
//     if (!PyArg_ParseTuple(args, ""))
//         return NULL;
//
//     pgf_concrete_unload(self->concr);
//
//     Py_RETURN_NONE;
// }
//
// static PyGetSetDef Concr_getseters[] = {
//     {"name",
//      (getter)Concr_getName, NULL,
//      "the name of the concrete syntax",
//     },
//     {"languageCode",
//      (getter)Concr_getLanguageCode, NULL,
//      "the language code for this concrete syntax",
//     },
//     {NULL}  /* Sentinel */
// };
//
// static PyMethodDef Concr_methods[] = {
//     {"printName", (PyCFunction)Concr_printName, METH_VARARGS,
//      "Returns the print name of a function or category"
//     },
//     {"parse", (PyCFunction)Concr_parse, METH_VARARGS | METH_KEYWORDS,
//      "Parses a string and returns an iterator over the abstract trees for this sentence\n\n"
//      "Named arguments:\n"
//      "- sentence (string)\n"
//      "- cat (string); OPTIONAL, default: the startcat of the grammar\n"
//      "- n (int), max. trees; OPTIONAL, default: extract all trees\n"
//      "- heuristics (double >= 0.0); OPTIONAL, default: taken from the flags in the grammar\n"
//      "- callbacks (list of category and callback); OPTIONAL, default: built-in callbacks only for Int, String and Float"
//     },
//     {"complete", (PyCFunction)Concr_complete, METH_VARARGS | METH_KEYWORDS,
//      "Parses a partial string and returns a list with the top n possible next tokens"
//     },
//     {"parseval", (PyCFunction)Concr_parseval, METH_VARARGS,
//      "Computes precision, recall and exact match for the parser on a given abstract tree"
//     },
//     {"lookupSentence", (PyCFunction)Concr_lookupSentence, METH_VARARGS | METH_KEYWORDS,
//      "Looks up a sentence from the grammar by a sequence of keywords\n\n"
//      "Named arguments:\n"
//      "- sentence (string) or tokens (list of strings)\n"
//      "- cat (string); OPTIONAL, default: the startcat of the grammar\n"
//      "- n (int), max. trees; OPTIONAL, default: extract all trees"
//     },
//     {"linearize", (PyCFunction)Concr_linearize, METH_VARARGS,
//      "Takes an abstract tree and linearizes it to a string"
//     },
//     {"linearizeAll", (PyCFunction)Concr_linearizeAll, METH_VARARGS | METH_KEYWORDS,
//      "Takes an abstract tree and linearizes with all variants"
//     },
//     {"tabularLinearize", (PyCFunction)Concr_tabularLinearize, METH_VARARGS,
//      "Takes an abstract tree and linearizes it to a table containing all fields"
//     },
//     {"bracketedLinearize", (PyCFunction)Concr_bracketedLinearize, METH_VARARGS,
//      "Takes an abstract tree and linearizes it to a bracketed string"
//     },
//     {"bracketedLinearizeAll", (PyCFunction)Concr_bracketedLinearizeAll, METH_VARARGS | METH_KEYWORDS,
//      "Takes an abstract tree and linearizes all variants into bracketed strings"
//     },
//     {"hasLinearization", (PyCFunction)Concr_hasLinearization, METH_VARARGS,
//      "hasLinearization(f) returns true if the function f has linearization in the concrete syntax"
//     },
//     {"graphvizParseTree", (PyCFunction)Concr_graphvizParseTree, METH_VARARGS,
//      "Renders an abstract syntax tree as a parse tree in Graphviz format"
//     },
//     {"lookupMorpho", (PyCFunction)Concr_lookupMorpho, METH_VARARGS,
//      "Looks up a word in the lexicon of the grammar"
//     },
//     {"lookupCohorts", (PyCFunction)Concr_lookupCohorts, METH_VARARGS,
//      "Takes a sentence and returns all matches for lexical items from the grammar in that sentence"
//     },
//     {"fullFormLexicon", (PyCFunction)Concr_fullFormLexicon, METH_VARARGS,
//      "Enumerates all words in the lexicon (useful for extracting full form lexicons)"
//     },
//     {"load", (PyCFunction)Concr_load, METH_VARARGS,
//      "Loads the concrete syntax from a .pgf_c file"
//     },
//     {"unload", (PyCFunction)Concr_unload, METH_VARARGS,
//      "Unloads the concrete syntax"
//     },
//     {NULL}  /* Sentinel */
// };
//
// static PyTypeObject pgf_ConcrType = {
//     PyVarObject_HEAD_INIT(NULL, 0)
//     //0,                         /*ob_size*/
//     "pgf.Concr",               /*tp_name*/
//     sizeof(ConcrObject),       /*tp_basicsize*/
//     0,                         /*tp_itemsize*/
//     (destructor)Concr_dealloc, /*tp_dealloc*/
//     0,                         /*tp_print*/
//     0,                         /*tp_getattr*/
//     0,                         /*tp_setattr*/
//     0,                         /*tp_compare*/
//     0,                         /*tp_repr*/
//     0,                         /*tp_as_number*/
//     0,                         /*tp_as_sequence*/
//     0,                         /*tp_as_mapping*/
//     0,                         /*tp_hash */
//     0,                         /*tp_call*/
//     0,                         /*tp_str*/
//     0,                         /*tp_getattro*/
//     0,                         /*tp_setattro*/
//     0,                         /*tp_as_buffer*/
//     Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
//     "concrete syntax",         /*tp_doc*/
//     0,                           /*tp_traverse */
//     0,                           /*tp_clear */
//     0,                           /*tp_richcompare */
//     0,                           /*tp_weaklistoffset */
//     0,                           /*tp_iter */
//     0,                           /*tp_iternext */
//     Concr_methods,             /*tp_methods */
//     0,                         /*tp_members */
//     Concr_getseters,           /*tp_getset */
//     0,                         /*tp_base */
//     0,                         /*tp_dict */
//     0,                         /*tp_descr_get */
//     0,                         /*tp_descr_set */
//     0,                         /*tp_dictoffset */
//     (initproc)Concr_init,      /*tp_init */
//     0,                         /*tp_alloc */
//     (newfunc)Concr_new,        /*tp_new */
// };

static void
PGF_dealloc(PGFObject* self)
{
    pgf_free(self->db);
    Py_TYPE(self)->tp_free((PyObject*)self);
}

typedef struct {
    PgfItor fn;
    PGFObject* grammar;
    void* collection;
} PyPGFClosure;

// static void
// pgf_collect_langs_seq(GuMapItor* fn, const void* key, void* value, GuExn* err)
// {
//     PgfConcr* concr = *((PgfConcr**) value);
//     PyPGFClosure* clo = (PyPGFClosure*) fn;
//
//     gu_buf_push((GuBuf*) clo->collection, PgfConcr*, concr);
// }

static PyObject*
PGF_str(PGFObject *self)
{
    // GuPool* tmp_pool = gu_local_pool();
    //
    // GuExn* err = gu_exn(tmp_pool);
    // GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
    // GuOut* out = gu_string_buf_out(sbuf);
    //
    // GuBuf* languages = gu_new_buf(PgfConcr*, tmp_pool);
    //
    // PyPGFClosure clo = { { pgf_collect_langs_seq }, self, languages };
    // pgf_iter_languages(self->pgf, &clo.fn, err);
    //
    // pgf_print(self->pgf, gu_buf_length(languages),
    //                      gu_buf_data(languages),
    //                      out, err);
    //
    // PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
    //                                              gu_string_buf_length(sbuf));
    //
    // gu_pool_free(tmp_pool);
    // return pystr;
    return NULL;
}

static PyObject*
PGF_getAbstractName(PGFObject *self, void *closure)
{
    PgfExn err;
    PgfText* txt = pgf_abstract_name(self->db, self->revision, &err);

    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    PyObject *name = PyUnicode_FromStringAndSize(txt->text, txt->size);
    free(txt);
    return name;
}

// static void
// pgf_collect_langs_dict(GuMapItor* fn, const void* key, void* value, GuExn* err)
// {
//     PgfCId name = (PgfCId) key;
//     PgfConcr* concr = *((PgfConcr**) value);
//     PyPGFClosure* clo = (PyPGFClosure*) fn;
//
//     PyObject* py_name = NULL;
//     PyObject* py_lang = NULL;
//
//     py_name = PyString_FromString(name);
//     if (py_name == NULL) {
//         gu_raise(err, PgfExn);
//         goto end;
//     }
//
//     py_lang = pgf_ConcrType.tp_alloc(&pgf_ConcrType, 0);
//     if (py_lang == NULL) {
//         gu_raise(err, PgfExn);
//         goto end;
//     }
//
//     ((ConcrObject *) py_lang)->concr = concr;
//     ((ConcrObject *) py_lang)->grammar = clo->grammar;
//     Py_INCREF(clo->grammar);
//
//     if (PyDict_SetItem((PyObject*) clo->collection, py_name, py_lang) != 0) {
//         gu_raise(err, PgfExn);
//         goto end;
//     }
//
// end:
//     Py_XDECREF(py_lang);
//     Py_XDECREF(py_name);
// }
//
// static PyObject*
// PGF_getLanguages(PGFObject *self, void *closure)
// {
//     PyObject* languages = PyDict_New();
//     if (languages == NULL)
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//
//     // Create an exception frame that catches all errors.
//     GuExn* err = gu_new_exn(tmp_pool);
//
//     PyPGFClosure clo = { { pgf_collect_langs_dict }, self, languages };
//     pgf_iter_languages(self->pgf, &clo.fn, err);
//     if (!gu_ok(err)) {
//         Py_DECREF(languages);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     PyObject* proxy = PyDictProxy_New(languages);
//
//     Py_DECREF(languages);
//     gu_pool_free(tmp_pool);
//
//     return proxy;
// }

static void
pgf_collect_cats(PgfItor* fn, PgfText* key, void* value, PgfExn *err)
{
    PgfText* name = key;
    PyPGFClosure* clo = (PyPGFClosure*) fn;

    PyObject* py_name = PyUnicode_FromStringAndSize(name->text, name->size);
    if (py_name == NULL) {
        err->type = PGF_EXN_OTHER_ERROR;
        err->msg = "unable to create string from category";
        return;
    }

    if (PyList_Append((PyObject*) clo->collection, py_name) != 0) {
        err->type = PGF_EXN_OTHER_ERROR;
        err->msg = "unable append category to list";
        Py_DECREF(py_name);
    }
}

static PyObject*
PGF_getCategories(PGFObject *self, void *closure)
{
    PyObject* categories = PyList_New(0);
    if (categories == NULL)
        return NULL;

    PgfExn err;
    PyPGFClosure clo = { { pgf_collect_cats }, self, categories };
    pgf_iter_categories(self->db, self->revision, &clo.fn, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(categories);
        return NULL;
    }

    return categories;
}

static PyObject*
PGF_categoryContext(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(catname->text, s, size+1);
    catname->size = size;

    PgfExn err;
    size_t n_hypos;
    PgfTypeHypo *hypos = pgf_category_context(self->db, self->revision, catname, &n_hypos, &unmarshaller, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    PyObject *contexts = PyList_New(n_hypos);
    if (contexts == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < n_hypos; i++) {
        PyObject *tup = PyTuple_New(3);
        PyTuple_SetItem(tup, 0, PyLong_FromLong(hypos[i].bind_type));
        PyTuple_SetItem(tup, 1, PyUnicode_FromStringAndSize(hypos[i].cid->text, hypos[i].cid->size));
        PyTuple_SetItem(tup, 2, (PyObject *)hypos[i].type);
        Py_INCREF(hypos[i].type);
        PyList_SetItem(contexts, i, tup);
    }
    if (PyErr_Occurred()) {
        Py_DECREF(contexts);
        return NULL;
    }

    return contexts;
}

static TypeObject *
PGF_getStartCat(PGFObject *self, void *closure)
{
    PgfExn err;
    PgfType type = pgf_start_cat(self->db, self->revision, &unmarshaller, &err);

    if (type == 0) {
        PyErr_SetString(PGFError, "start category cannot be found");
        return NULL;
    }
    else if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    return (TypeObject *)type;
}

static void
pgf_collect_funs(PgfItor* fn, PgfText* key, void* value, PgfExn *err)
{
    PgfText* name = key;
    PyPGFClosure* clo = (PyPGFClosure*) fn;

    PyObject* py_name = PyUnicode_FromStringAndSize(name->text, name->size);
    if (py_name == NULL) {
        err->type = PGF_EXN_OTHER_ERROR;
        err->msg = "unable to create string from function";
    }

    if (PyList_Append((PyObject*) clo->collection, py_name) != 0) {
        err->type = PGF_EXN_OTHER_ERROR;
        err->msg = "unable append function to list";
        Py_DECREF(py_name);
    }
}

static PyObject*
PGF_getFunctions(PGFObject *self, void *closure)
{
    PyObject* functions = PyList_New(0);
    if (functions == NULL)
        return NULL;

    PgfExn err;
    PyPGFClosure clo = { { pgf_collect_funs }, self, functions };
    pgf_iter_functions(self->db, self->revision, &clo.fn, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(functions);
        return NULL;
    }

    return functions;
}

static PyObject *
PGF_functionsByCat(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(catname->text, s, size+1);
    catname->size = size;

    PyObject *functions = PyList_New(0);
    if (functions == NULL) {
        return NULL;
    }

    PgfExn err;
    PyPGFClosure clo = { { pgf_collect_funs }, self, functions };
    pgf_iter_functions_by_cat(self->db, self->revision, catname, &clo.fn, &err);
    PyMem_Free(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(functions);
        return NULL;
    }

    return functions;
}

static TypeObject *
PGF_functionType(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *funname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(funname->text, s, size+1);
    funname->size = size;

    PgfExn err;
    PgfType type = pgf_function_type(self->db, self->revision, funname, &unmarshaller, &err);
    PyMem_Free(funname);
    if (type == 0) {
        PyErr_Format(PyExc_KeyError, "function '%s' is not defined", s);
        return NULL;
    }
    else if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    return (TypeObject *)type;
}

static PyObject *
PGF_functionIsConstructor(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *funname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(funname->text, s, size+1);
    funname->size = size;

    PgfExn err;
    int isCon = pgf_function_is_constructor(self->db, self->revision, funname, &err);
    PyMem_Free(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    return PyBool_FromLong(isCon);
}

// static IterObject*
// PGF_generateAll(PGFObject* self, PyObject *args, PyObject *keywds)
// {
//     static char *kwlist[] = {"cat", "n", NULL};
//
//     PyObject* start = NULL;
//     int max_count = -1;
//     if (!PyArg_ParseTupleAndKeywords(args, keywds, "O|i", kwlist,
//                                      &start, &max_count))
//         return NULL;
//
//     IterObject* pyres = (IterObject*)
//         pgf_IterType.tp_alloc(&pgf_IterType, 0);
//     if (pyres == NULL) {
//         return NULL;
//     }
//
//     pyres->source = (PyObject*) self;
//     Py_INCREF(self);
//
//     GuPool* out_pool = gu_new_pool();
//
//     PyObject* py_pool = PyPool_New(out_pool);
//     pyres->container = PyTuple_Pack(2, pyres->source, py_pool);
//     Py_DECREF(py_pool);
//
//     pyres->pool = gu_new_pool();
//     pyres->max_count = max_count;
//     pyres->counter   = 0;
//     pyres->fetch     = Iter_fetch_expr;
//
//     GuExn* err = gu_exn(pyres->pool);
//
//     PgfType* type = pgf_type_from_object(start, pyres->pool);
//     if (type == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     pyres->res =
//         pgf_generate_all(self->pgf, type, err, pyres->pool, out_pool);
//     if (pyres->res == NULL) {
//         Py_DECREF(pyres);
//         return NULL;
//     }
//
//     return pyres;
// }
//
// static ExprObject*
// PGF_compute(PGFObject* self, PyObject *args)
// {
//     ExprObject* py_expr = NULL;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &py_expr))
//         return NULL;
//
//     ExprObject* py_expr_res = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (py_expr_res == NULL)
//         return NULL;
//
//     GuPool* tmp_pool = gu_new_pool();
//     GuExn* err = gu_new_exn(tmp_pool);
//
//     py_expr_res->pool = gu_new_pool();
//     py_expr_res->expr = pgf_compute(self->pgf, py_expr->expr, err,
//                                     tmp_pool, py_expr_res->pool);
//     py_expr_res->master = (PyObject*) self;
//     Py_INCREF(py_expr_res->master);
//
//     if (!gu_ok(err)) {
//         GuString msg = (GuString) gu_exn_caught_data(err);
//         PyErr_SetString(PGFError, msg);
//
//         Py_DECREF(py_expr_res);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//     return py_expr_res;
// }
//
// static ExprObject*
// PGF_checkExpr(PGFObject* self, PyObject *args)
// {
//     ExprObject* py_expr = NULL;
//     TypeObject* py_type = NULL;
//     if (!PyArg_ParseTuple(args, "O!O!", &pgf_ExprType, &py_expr, &pgf_TypeType, &py_type))
//         return NULL;
//
//     ExprObject* new_pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (new_pyexpr == NULL)
//         return NULL;
//
//     new_pyexpr->pool = gu_new_pool();
//     new_pyexpr->expr = py_expr->expr;
//     new_pyexpr->master = NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* exn = gu_new_exn(tmp_pool);
//
//     pgf_check_expr(self->pgf, &new_pyexpr->expr, py_type->type,
//                    exn, new_pyexpr->pool);
//     if (!gu_ok(exn)) {
//         if (gu_exn_caught(exn, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(PGFError, msg);
//         } else if (gu_exn_caught(exn, PgfTypeError)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(TypeError, msg);
//         }
//
//         Py_DECREF(new_pyexpr);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return new_pyexpr;
// }
//
// static PyObject*
// PGF_inferExpr(PGFObject* self, PyObject *args)
// {
//     ExprObject* py_expr = NULL;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &py_expr))
//         return NULL;
//
//     ExprObject* new_pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (new_pyexpr == NULL)
//         return NULL;
//
//     new_pyexpr->pool = gu_new_pool();
//     new_pyexpr->expr = py_expr->expr;
//     new_pyexpr->master = NULL;
//
//     TypeObject* new_pytype = (TypeObject*) pgf_TypeType.tp_alloc(&pgf_TypeType, 0);
//     if (new_pytype == NULL) {
//         Py_DECREF(new_pyexpr);
//         return NULL;
//     }
//
//     new_pytype->pool = NULL;
//     new_pytype->type = NULL;
//     new_pytype->master = (PyObject*) new_pyexpr;
//     Py_INCREF(new_pyexpr);
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* exn = gu_new_exn(tmp_pool);
//
//     new_pytype->type =
//         pgf_infer_expr(self->pgf, &new_pyexpr->expr,
//                        exn, new_pyexpr->pool);
//     if (!gu_ok(exn)) {
//         if (gu_exn_caught(exn, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(PGFError, msg);
//         } else if (gu_exn_caught(exn, PgfTypeError)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(TypeError, msg);
//         }
//
//         Py_DECREF(new_pyexpr);
//         Py_DECREF(new_pytype);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     PyObject* res =
//         Py_BuildValue("OO", new_pyexpr, new_pytype);
//
//     Py_DECREF(new_pyexpr);
//     Py_DECREF(new_pytype);
//
//     return res;
// }
//
// static TypeObject*
// PGF_checkType(PGFObject* self, PyObject *args)
// {
//     TypeObject* py_type = NULL;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_TypeType, &py_type))
//         return NULL;
//
//     TypeObject* new_pytype = (TypeObject*) pgf_TypeType.tp_alloc(&pgf_TypeType, 0);
//     if (new_pytype == NULL) {
//         return NULL;
//     }
//
//     new_pytype->pool   = gu_new_pool();
//     new_pytype->type   = py_type->type;
//     new_pytype->master = NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* exn = gu_new_exn(tmp_pool);
//
//     pgf_check_type(self->pgf, &new_pytype->type,
//                    exn, new_pytype->pool);
//     if (!gu_ok(exn)) {
//         if (gu_exn_caught(exn, PgfExn)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(PGFError, msg);
//         } else if (gu_exn_caught(exn, PgfTypeError)) {
//             GuString msg = (GuString) gu_exn_caught_data(exn);
//             PyErr_SetString(TypeError, msg);
//         }
//
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     return new_pytype;
// }
//
// static PyObject*
// PGF_graphvizAbstractTree(PGFObject* self, PyObject *args) {
//     ExprObject* pyexpr;
//     if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//     GuExn* err = gu_new_exn(tmp_pool);
//     GuStringBuf* sbuf = gu_new_string_buf(tmp_pool);
//     GuOut* out = gu_string_buf_out(sbuf);
//
//     pgf_graphviz_abstract_tree(self->pgf, pyexpr->expr, pgf_default_graphviz_options, out, err);
//     if (!gu_ok(err)) {
//         PyErr_SetString(PGFError, "The abstract tree cannot be visualized");
//         return NULL;
//     }
//
//     PyObject* pystr = PyString_FromStringAndSize(gu_string_buf_data(sbuf),
//                                                  gu_string_buf_length(sbuf));
//
//     gu_pool_free(tmp_pool);
//     return pystr;
// }
//
// static void
// pgf_embed_funs(GuMapItor* fn, const void* key, void* value, GuExn* err)
// {
//     PyPGFClosure* clo = (PyPGFClosure*) fn;
//
//     PgfCId name = (PgfCId) key;
//
//     ExprObject* pyexpr = (ExprObject*) pgf_ExprType.tp_alloc(&pgf_ExprType, 0);
//     if (pyexpr == NULL) {
//         gu_raise(err, PgfExn);
//         return;
//     }
//
//     pyexpr->master = (PyObject*) clo->grammar;
//     pyexpr->expr   = pgf_fun_get_ep(value)->expr;
//
//     Py_INCREF(pyexpr->master);
//
//     if (PyModule_AddObject((PyObject*) clo->collection, name, (PyObject*) pyexpr) != 0) {
//         Py_DECREF(pyexpr);
//         gu_raise(err, PgfExn);
//     }
// }
//
// static PyObject*
// PGF_embed(PGFObject* self, PyObject *args)
// {
//     PgfCId modname;
//     if (!PyArg_ParseTuple(args, "s", &modname))
//         return NULL;
//
//     PyObject *m = PyImport_AddModule(modname);
//     if (m == NULL)
//         return NULL;
//
//     GuPool* tmp_pool = gu_local_pool();
//
//     // Create an exception frame that catches all errors.
//     GuExn* err = gu_new_exn(tmp_pool);
//
//     PyPGFClosure clo = { { pgf_embed_funs }, self, m };
//     pgf_iter_functions(self->pgf, &clo.fn, err);
//     if (!gu_ok(err)) {
//         Py_DECREF(m);
//         gu_pool_free(tmp_pool);
//         return NULL;
//     }
//
//     gu_pool_free(tmp_pool);
//
//     Py_INCREF(m);
//     return m;
// }

static PyGetSetDef PGF_getseters[] = {
    {"abstractName",
     (getter)PGF_getAbstractName, NULL,
     "the abstract syntax name",
     NULL},
    // {"languages",
    //  (getter)PGF_getLanguages, NULL,
    //  "a map containing all concrete languages in the grammar",
    //  NULL},
    {"categories",
     (getter)PGF_getCategories, NULL,
     "a list containing all categories in the grammar",
     NULL},
    {"startCat",
     (getter)PGF_getStartCat, NULL,
     "the start category for the grammar",
     NULL},
    {"functions",
     (getter)PGF_getFunctions, NULL,
     "a list containing all functions in the grammar",
     NULL},
    {NULL}  /* Sentinel */
};

static PyMemberDef PGF_members[] = {
    {NULL}  /* Sentinel */
};

static PyMethodDef PGF_methods[] = {
    {"categoryContext", (PyCFunction)PGF_categoryContext, METH_VARARGS,
     "Returns the context for a given category"
    },
    {"functionsByCat", (PyCFunction)PGF_functionsByCat, METH_VARARGS,
     "Returns the list of functions for a given category"
    },
    {"functionType", (PyCFunction)PGF_functionType, METH_VARARGS,
     "Returns the type of a function"
    },
    {"functionIsConstructor", (PyCFunction)PGF_functionIsConstructor, METH_VARARGS,
     "Checks whether a function is a constructor"
    },
    // {"generateAll", (PyCFunction)PGF_generateAll, METH_VARARGS | METH_KEYWORDS,
    //  "Generates abstract syntax trees of given category in decreasing probability order"
    // },
    // {"compute", (PyCFunction)PGF_compute, METH_VARARGS,
    //  "Computes the normal form of an abstract syntax tree"
    // },
    // {"checkExpr", (PyCFunction)PGF_checkExpr, METH_VARARGS,
    //  "Type checks an abstract syntax expression and returns the updated expression"
    // },
    // {"inferExpr", (PyCFunction)PGF_inferExpr, METH_VARARGS,
    //  "Type checks an abstract syntax expression and returns the updated expression"
    // },
    // {"checkType", (PyCFunction)PGF_checkType, METH_VARARGS,
    //  "Type checks an abstract syntax type and returns the updated type"
    // },
    // {"graphvizAbstractTree", (PyCFunction)PGF_graphvizAbstractTree, METH_VARARGS,
    //  "Renders an abstract syntax tree in a Graphviz format"
    // },
    // {"embed", (PyCFunction)PGF_embed, METH_VARARGS,
    //  "embed(mod_name) creates a Python module with name mod_name, which "
    //  "contains one Python object for every abstract function in the grammar. "
    //  "The module can be imported to make it easier to construct abstract "
    //  "syntax trees."
    // },
    {NULL}  /* Sentinel */
};

static PyTypeObject pgf_PGFType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.PGF",                 /*tp_name*/
    sizeof(PGFObject),         /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor)PGF_dealloc,   /*tp_dealloc*/
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
    (reprfunc) PGF_str,        /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "PGF object",              /*tp_doc*/
    0,                           /*tp_traverse */
    0,                           /*tp_clear */
    0,                           /*tp_richcompare */
    0,                           /*tp_weaklistoffset */
    0,                           /*tp_iter */
    0,                           /*tp_iternext */
    PGF_methods,               /*tp_methods */
    PGF_members,               /*tp_members */
    PGF_getseters,             /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0,                         /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

static PGFObject*
pgf_readPGF(PyObject *self, PyObject *args)
{
    const char *fpath;
    if (!PyArg_ParseTuple(args, "s", &fpath))
        return NULL;

    PGFObject* py_pgf = (PGFObject*) pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the PGF grammar.
    PgfExn err;
    py_pgf->db = pgf_read_pgf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject*
pgf_bootNGF(PyObject *self, PyObject *args)
{
    const char *fpath; // pgf
    const char *npath; // ngf
    if (!PyArg_ParseTuple(args, "ss", &fpath, &npath))
        return NULL;

    PGFObject* py_pgf = (PGFObject*) pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the PGF grammar.
    PgfExn err;
    py_pgf->db = pgf_boot_ngf(fpath, npath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject*
pgf_readNGF(PyObject *self, PyObject *args)
{
const char *fpath;
    if (!PyArg_ParseTuple(args, "s", &fpath))
        return NULL;

    PGFObject* py_pgf = (PGFObject*) pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the NGF grammar.
    PgfExn err;
    py_pgf->db = pgf_read_ngf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static ExprObject*
pgf_readExpr(PyObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *input = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(input->text, s, size+1);
    input->size = size;

    PgfExpr expr = pgf_read_expr(input, &unmarshaller);
    PyMem_Free(input);
    if (expr == 0) {
        PyErr_SetString(PGFError, "expression cannot be parsed");
        return NULL;
    }

    return (ExprObject *)expr;
}

static TypeObject *
pgf_readType(PyObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *input = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(input->text, s, size+1);
    input->size = size;

    PgfType type = pgf_read_type(input, &unmarshaller);
    PyMem_Free(input);
    if (type == 0) {
        PyErr_SetString(PGFError, "type cannot be parsed");
        return NULL;
    }

    return (TypeObject *)type;
}

static PyMethodDef module_methods[] = {
    {"readPGF",  (void*)pgf_readPGF,  METH_VARARGS,
     "Reads a PGF file into memory"},
    {"bootNGF",  (void*)pgf_bootNGF,  METH_VARARGS,
     "Reads a PGF file into memory and stores the unpacked data in an NGF file"},
    {"readNGF",  (void*)pgf_readNGF,  METH_VARARGS,
     "Reads an NGF file into memory"},
    {"readExpr", (void*)pgf_readExpr, METH_VARARGS,
     "Parses a string as an abstract tree"},
    {"readType", (void*)pgf_readType, METH_VARARGS,
     "Parses a string as an abstract type"},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

#if PY_MAJOR_VERSION >= 3
  #define MOD_ERROR_VAL NULL
  #define MOD_SUCCESS_VAL(val) val
  #define MOD_INIT(name) PyMODINIT_FUNC PyInit_##name(void)
  #define MOD_DEF(ob, name, doc, methods) \
              static struct PyModuleDef moduledef = { \
                              PyModuleDef_HEAD_INIT, name, doc, -1, methods, }; \
          ob = PyModule_Create(&moduledef);
#else
  #define MOD_ERROR_VAL
  #define MOD_SUCCESS_VAL(val)
  #define MOD_INIT(name) void init##name(void)
  #define MOD_DEF(ob, name, doc, methods) \
              ob = Py_InitModule3(name, methods, doc);
#endif

MOD_INIT(pgf)
{
    PyObject *m;

    if (PyType_Ready(&pgf_PGFType) < 0)
        return MOD_ERROR_VAL;

    // if (PyType_Ready(&pgf_ConcrType) < 0)
    //     return MOD_ERROR_VAL;
    //
    // if (PyType_Ready(&pgf_BracketType) < 0)
    //     return MOD_ERROR_VAL;
    //
    // if (PyType_Ready(&pgf_BINDType) < 0)
    //     return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprLitType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprMetaType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprVarType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_TypeType) < 0)
        return MOD_ERROR_VAL;

    // if (PyType_Ready(&pgf_IterType) < 0)
    //     return MOD_ERROR_VAL;

    MOD_DEF(m, "pgf", "The Runtime for Portable Grammar Format in Python",
            module_methods);
    if (m == NULL)
        return MOD_ERROR_VAL;

    PGFError = PyErr_NewException("pgf.PGFError", NULL, NULL);
    PyModule_AddObject(m, "PGFError", PGFError);
    Py_INCREF(PGFError);

    // PyObject *dict = PyDict_New();
    // PyDict_SetItemString(dict, "token", PyString_FromStringAndSize("", 0));
    // ParseError = PyErr_NewException("pgf.ParseError", NULL, dict);
    // PyModule_AddObject(m, "ParseError", ParseError);
    // Py_INCREF(ParseError);

    // TypeError = PyErr_NewException("pgf.TypeError", NULL, NULL);
    // PyModule_AddObject(m, "TypeError", TypeError);
    // Py_INCREF(TypeError);

    PyModule_AddObject(m, "Expr", (PyObject *) &pgf_ExprType);
    Py_INCREF(&pgf_ExprType);

    PyModule_AddObject(m, "ExprLit", (PyObject *) &pgf_ExprLitType);
    Py_INCREF(&pgf_ExprLitType);

    PyModule_AddObject(m, "ExprMeta", (PyObject *) &pgf_ExprMetaType);
    Py_INCREF(&pgf_ExprMetaType);

    PyModule_AddObject(m, "ExprVar", (PyObject *) &pgf_ExprVarType);
    Py_INCREF(&pgf_ExprVarType);

    PyModule_AddObject(m, "Type", (PyObject *) &pgf_TypeType);
    Py_INCREF(&pgf_TypeType);

    PyModule_AddObject(m, "PGF", (PyObject *) &pgf_PGFType);
    Py_INCREF(&pgf_PGFType);

    // PyModule_AddObject(m, "Concr", (PyObject *) &pgf_ConcrType);
    // Py_INCREF(&pgf_ConcrType);
    //
    // PyModule_AddObject(m, "Iter", (PyObject *) &pgf_IterType);
    // Py_INCREF(&pgf_IterType);
    //
    // PyModule_AddObject(m, "Bracket", (PyObject *) &pgf_BracketType);
    // Py_INCREF(&pgf_BracketType);
    //
    // PyModule_AddObject(m, "BIND", (PyObject *) &pgf_BINDType);
    // Py_INCREF(&pgf_BINDType);

    return MOD_SUCCESS_VAL(m);
}
