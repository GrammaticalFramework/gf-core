#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <structmember.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"
#include "./transactions.h"

// ----------------------------------------------------------------------------
// PGF: the grammar object

static void
PGF_dealloc(PGFObject *self)
{
    if (self->db != NULL && self->revision != 0)
        pgf_free_revision(self->db, self->revision);
    Py_TYPE(self)->tp_free((PyObject *)self);
}

typedef struct {
    PgfItor fn;
    PGFObject *grammar;
    void *collection;
} PyPGFClosure;

// static void
// pgf_collect_langs_seq(GuMapItor* fn, const void* key, void* value, GuExn* err)
// {
//     PgfConcr* concr = *((PgfConcr**) value);
//     PyPGFClosure* clo = (PyPGFClosure*) fn;
//
//     gu_buf_push((GuBuf*) clo->collection, PgfConcr*, concr);
// }

// static PyObject*
// PGF_str(PGFObject *self)
// {
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
//     return NULL;
// }

static PyObject *
PGF_getAbstractName(PGFObject *self, void *closure)
{
    PgfExn err;
    PgfText *txt = pgf_abstract_name(self->db, self->revision, &err);

    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    PyObject *name = PyUnicode_FromStringAndSize(txt->text, txt->size);
    free(txt);
    return name;
}

static void
_collect_cats(PgfItor *fn, PgfText *key, void *value, PgfExn *err)
{
    PgfText *name = key;
    PyPGFClosure *clo = (PyPGFClosure*) fn;

    PyObject *py_name = PyUnicode_FromStringAndSize(name->text, name->size);
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

static PyObject *
PGF_getCategories(PGFObject *self, void *closure)
{
    PyObject *categories = PyList_New(0);
    if (categories == NULL)
        return NULL;

    PgfExn err;
    PyPGFClosure clo = { { _collect_cats }, self, categories };
    pgf_iter_categories(self->db, self->revision, &clo.fn, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(categories);
        return NULL;
    }

    return categories;
}

static PyObject *
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

    if (hypos == NULL) {
        Py_RETURN_NONE;
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
_collect_funs(PgfItor *fn, PgfText *key, void *value, PgfExn *err)
{
    PgfText *name = key;
    PyPGFClosure *clo = (PyPGFClosure*) fn;

    PyObject *py_name = PyUnicode_FromStringAndSize(name->text, name->size);
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

static PyObject *
PGF_getFunctions(PGFObject *self, void *closure)
{
    PyObject *functions = PyList_New(0);
    if (functions == NULL)
        return NULL;

    PgfExn err;
    PyPGFClosure clo = { { _collect_funs }, self, functions };
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
    PyPGFClosure clo = { { _collect_funs }, self, functions };
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

static PyObject *
PGF_functionProbability(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *funname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(funname->text, s, size+1);
    funname->size = size;

    PgfExn err;
    prob_t prob = pgf_function_prob(self->db, self->revision, funname, &err);
    PyMem_Free(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    return PyFloat_FromDouble((double)prob);
}

static PyGetSetDef PGF_getseters[] = {
    {"abstractName",
     (getter)PGF_getAbstractName, NULL,
     "the abstract syntax name",
     NULL},
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
    // {"revision", T_PYSSIZET, offsetof(PGFObject, revision), READONLY,
    //  "the revision number of this PGF"},
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
    {"functionProbability", (PyCFunction)PGF_functionProbability, METH_VARARGS,
     "Returns the probability of a function"
    },

    {"checkoutBranch", (PyCFunction)PGF_checkoutBranch, METH_VARARGS,
     "Switch to a branch"
    },
    {"newTransaction", (PyCFunction)PGF_newTransaction, METH_VARARGS,
     "Create new transaction"
    },

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
    0, // (reprfunc) PGF_str,        /*tp_str*/
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

// ----------------------------------------------------------------------------
// pgf: the module

static PGFObject *
pgf_readPGF(PyObject *self, PyObject *args)
{
    const char *fpath;
    if (!PyArg_ParseTuple(args, "s", &fpath))
        return NULL;

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the PGF grammar.
    PgfExn err;
    py_pgf->db = pgf_read_pgf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject *
pgf_bootNGF(PyObject *self, PyObject *args)
{
    const char *fpath; // pgf
    const char *npath; // ngf
    if (!PyArg_ParseTuple(args, "ss", &fpath, &npath))
        return NULL;

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the PGF grammar.
    PgfExn err;
    py_pgf->db = pgf_boot_ngf(fpath, npath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject *
pgf_readNGF(PyObject *self, PyObject *args)
{
    const char *fpath;
    if (!PyArg_ParseTuple(args, "s", &fpath))
        return NULL;

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the NGF grammar.
    PgfExn err;
    py_pgf->db = pgf_read_ngf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject *
pgf_newNGF(PyObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    const char *fpath = NULL;
    if (!PyArg_ParseTuple(args, "s#|s", &s, &size, &fpath))
        return NULL;

    PgfText *absname = (PgfText *)PyMem_Malloc(sizeof(PgfText)+size+1);
    memcpy(absname->text, s, size+1);
    absname->size = size;

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    // Read the NGF grammar.
    PgfExn err;
    py_pgf->db = pgf_new_ngf(absname, fpath, &py_pgf->revision, &err);
    PyMem_Free(absname);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static ExprObject *
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

static PyObject *
pgf_showExpr(PyObject *self, PyObject *args)
{
    PyObject *pylist;
    PyObject *pyexpr;
    if (!PyArg_ParseTuple(args, "O!O!", &PyList_Type, &pylist, &pgf_ExprType, &pyexpr))
        return NULL;

    PgfPrintContext *ctxt = NULL;
    for (Py_ssize_t i = PyList_Size(pylist); i > 0 ; i--) {
        PyObject *item = PyList_GetItem(pylist, i-1);
        if (!PyUnicode_Check(item)) {
            PyErr_SetString(PyExc_TypeError, "invalid variable argument in showExpr");
            return NULL;
        }
        PgfText *input = PyUnicode_AsPgfText(item);

        // TODO a better way to copy into this->name?
        PgfPrintContext *this = (PgfPrintContext *)PyMem_Malloc(sizeof(PgfPrintContext *) + sizeof(PgfText) + input->size + 1);
        this->next = ctxt;
        memcpy(&this->name, input, sizeof(PgfText) + input->size + 1);
        ctxt = this;
    }

    PgfText *s = pgf_print_expr((PgfExpr) pyexpr, ctxt, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
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

static PyObject *
pgf_showType(PyObject *self, PyObject *args)
{
    PyObject *pylist;
    PyObject *pytype;
    if (!PyArg_ParseTuple(args, "O!O!", &PyList_Type, &pylist, &pgf_TypeType, &pytype))
        return NULL;

    PgfPrintContext *ctxt = NULL;
    for (Py_ssize_t i = PyList_Size(pylist); i > 0 ; i--) {
        PyObject *item = PyList_GetItem(pylist, i-1);
        if (!PyUnicode_Check(item)) {
            PyErr_SetString(PyExc_TypeError, "invalid variable argument in showType");
            return NULL;
        }
        PgfText *input = PyUnicode_AsPgfText(item);

        // TODO a better way to copy into this->name?
        PgfPrintContext *this = (PgfPrintContext *)PyMem_Malloc(sizeof(PgfPrintContext *) + sizeof(PgfText) + input->size + 1);
        this->next = ctxt;
        memcpy(&this->name, input, sizeof(PgfText) + input->size + 1);
        ctxt = this;
    }

    PgfText *s = pgf_print_type((PgfType) pytype, ctxt, 0, &marshaller);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    free(s);
    return str;
}

static PyObject *
pgf_mkHypo(PyObject *self, PyObject *args)
{
    PyObject *type;
    if (!PyArg_ParseTuple(args, "O!", &pgf_TypeType, &type))
        return NULL;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, PyLong_FromLong(0)); // explicit
    PyTuple_SetItem(tup, 1, PyUnicode_FromStringAndSize("_", 1));
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(type);

    return tup;
}

static PyObject *
pgf_mkDepHypo(PyObject *self, PyObject *args)
{
    PyObject *var;
    PyObject *type;
    if (!PyArg_ParseTuple(args, "UO!", &var, &pgf_TypeType, &type))
        return NULL;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, PyLong_FromLong(0)); // explicit
    PyTuple_SetItem(tup, 1, var);
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(var);
    Py_INCREF(type);

    return tup;
}

static PyObject *
pgf_mkImplHypo(PyObject *self, PyObject *args)
{
    PyObject *var;
    PyObject *type;
    if (!PyArg_ParseTuple(args, "UO!", &var, &pgf_TypeType, &type))
        return NULL;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, PyLong_FromLong(1)); // implicit
    PyTuple_SetItem(tup, 1, var);
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(var);
    Py_INCREF(type);

    return tup;
}

static PyMethodDef module_methods[] = {
    {"readPGF",  (void*)pgf_readPGF,  METH_VARARGS,
     "Reads a PGF file into memory"},
    {"bootNGF",  (void*)pgf_bootNGF,  METH_VARARGS,
     "Reads a PGF file into memory and stores the unpacked data in an NGF file"},
    {"readNGF",  (void*)pgf_readNGF,  METH_VARARGS,
     "Reads an NGF file into memory"},
    {"newNGF",   (void*)pgf_newNGF,  METH_VARARGS,
     "Creates a new NGF file with the given name"},

    {"readExpr", (void*)pgf_readExpr, METH_VARARGS,
     "Parses a string as an abstract tree"},
    {"showExpr", (void*)pgf_showExpr, METH_VARARGS,
     "Renders an expression as a string"},
    {"readType", (void*)pgf_readType, METH_VARARGS,
     "Parses a string as an abstract type"},
    {"showType", (void*)pgf_showType, METH_VARARGS,
     "Renders a type as a string"},

    {"mkHypo", (void*)pgf_mkHypo, METH_VARARGS,
     "Creates hypothesis for non-dependent type i.e. A"},
    {"mkDepHypo", (void*)pgf_mkDepHypo, METH_VARARGS,
     "Creates hypothesis for dependent type i.e. (x : A)"},
    {"mkImplHypo", (void*)pgf_mkImplHypo, METH_VARARGS,
     "Creates hypothesis for dependent type with implicit argument i.e. ({x} : A)"},
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

    if (PyType_Ready(&pgf_TransactionType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprAbsType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprAppType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprLitType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprMetaType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprFunType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprVarType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprTypedType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_ExprImplArgType) < 0)
        return MOD_ERROR_VAL;

    if (PyType_Ready(&pgf_TypeType) < 0)
        return MOD_ERROR_VAL;

    MOD_DEF(m, "pgf", "The Runtime for Portable Grammar Format in Python", module_methods);
    if (m == NULL)
        return MOD_ERROR_VAL;

    PGFError = PyErr_NewException("pgf.PGFError", NULL, NULL);
    PyModule_AddObject(m, "PGFError", PGFError);
    Py_INCREF(PGFError);

    PyModule_AddObject(m, "PGF", (PyObject *) &pgf_PGFType);
    Py_INCREF(&pgf_PGFType);

    PyModule_AddObject(m, "Transaction", (PyObject *) &pgf_TransactionType);
    Py_INCREF(&pgf_TransactionType);

    PyModule_AddObject(m, "Expr", (PyObject *) &pgf_ExprType);
    Py_INCREF(&pgf_ExprType);

    PyModule_AddObject(m, "ExprAbs", (PyObject *) &pgf_ExprAbsType);
    Py_INCREF(&pgf_ExprAbsType);

    PyModule_AddObject(m, "ExprApp", (PyObject *) &pgf_ExprAppType);
    Py_INCREF(&pgf_ExprAppType);

    PyModule_AddObject(m, "ExprLit", (PyObject *) &pgf_ExprLitType);
    Py_INCREF(&pgf_ExprLitType);

    PyModule_AddObject(m, "ExprMeta", (PyObject *) &pgf_ExprMetaType);
    Py_INCREF(&pgf_ExprMetaType);

    PyModule_AddObject(m, "ExprFun", (PyObject *) &pgf_ExprFunType);
    Py_INCREF(&pgf_ExprFunType);

    PyModule_AddObject(m, "ExprVar", (PyObject *) &pgf_ExprVarType);
    Py_INCREF(&pgf_ExprVarType);

    PyModule_AddObject(m, "ExprTyped", (PyObject *) &pgf_ExprTypedType);
    Py_INCREF(&pgf_ExprTypedType);

    PyModule_AddObject(m, "ExprImplArg", (PyObject *) &pgf_ExprImplArgType);
    Py_INCREF(&pgf_ExprImplArgType);

    PyModule_AddObject(m, "Type", (PyObject *) &pgf_TypeType);
    Py_INCREF(&pgf_TypeType);

    PyModule_AddIntConstant(m, "BIND_TYPE_EXPLICIT", 0);

    PyModule_AddIntConstant(m, "BIND_TYPE_IMPLICIT", 1);

    return MOD_SUCCESS_VAL(m);
}
