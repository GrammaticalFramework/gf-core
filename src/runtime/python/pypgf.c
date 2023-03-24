#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <structmember.h>

#include <pgf/pgf.h>
#include "./expr.h"
#include "./ffi.h"
#include "./transactions.h"
#ifndef _WIN32
#include <dirent.h>
#endif

static ConcrObject*
Concr_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    ConcrObject* self = (ConcrObject *)type->tp_alloc(type, 0);
    if (self != NULL) {
        self->grammar = 0;
        self->concr   = 0;
    }

    return self;
}

static void
Concr_dealloc(ConcrObject* self)
{
    if (self->grammar != NULL && self->concr != 0)
        pgf_free_concr_revision(self->grammar->db, self->concr);
    Py_XDECREF(self->grammar);
}

static int
Concr_init(ConcrObject *self, PyObject *args, PyObject *kwds)
{
    return -1;
}

static PyObject*
Concr_getName(ConcrObject *self, void *closure)
{
    PgfExn err;
    PgfText *name = pgf_concrete_name(self->grammar->db, self->concr, &err);
    PyObject *pyname = PyUnicode_FromStringAndSize(name->text, name->size);
    free(name);
    return pyname;
}

/*
static PyObject*
Concr_getLanguageCode(ConcrObject *self, void *closure)
{
    PgfExn err;
    PgfText *code = pgf_language_code(self->grammar->db, self->concr, &err);
    PyObject *pycode = PyUnicode_FromStringAndSize(code->text, code->size);
    free(code);
    return pycode;
}
*/

static PyObject*
Concr_linearize(ConcrObject* self, PyObject *args)
{
	ExprObject* pyexpr;
	if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
        return NULL;

	PgfExn err;
	PgfText *text = 
        pgf_linearize(self->grammar->db, self->concr, (PgfExpr) pyexpr, NULL,
                      &marshaller, &err);
	if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
	}

    if (text == NULL) {
        Py_RETURN_NONE;
    }

	PyObject* pystr = PyUnicode_FromStringAndSize(text->text, text->size);

	free(text);
	return pystr;
}

typedef struct {
	PyObject_HEAD
	PyObject* cat;
	int fid;
	PyObject* ann;
	PyObject* fun;
	PyObject* children;
} BracketObject;

static void
Bracket_dealloc(BracketObject* self)
{
	Py_XDECREF(self->cat);
	Py_XDECREF(self->fun);
	Py_XDECREF(self->children);
    Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject *
Bracket_repr(BracketObject *self)
{
	PyObject *repr =
		PyUnicode_FromFormat("(%U:%d", self->cat, self->fid);
	if (repr == NULL) {
		return NULL;
	}

	PyObject *space = PyUnicode_FromFormat(" ");

	PyObject *new_repr;
    size_t len = PyList_Size(self->children);
	for (size_t i = 0; i < len; i++) {
        PyObject *child = PyList_GetItem(self->children, i);

		new_repr = PyUnicode_Concat(repr, space); Py_DECREF(repr);
		if (new_repr == NULL) {
			Py_DECREF(space);
			return NULL;
		}
        repr = new_repr;

		PyObject *child_str = Py_TYPE(child)->tp_str(child);
		if (child_str == NULL) {
			Py_DECREF(repr);
			Py_DECREF(space);
			return NULL;
		}

		new_repr = PyUnicode_Concat(repr, child_str); Py_DECREF(repr);
		if (new_repr == NULL) {
			Py_DECREF(space);
			return NULL;
		}
        repr = new_repr;

		Py_DECREF(child_str);
	}

	Py_DECREF(space);

	PyObject *str = PyUnicode_FromFormat(")");
	new_repr = PyUnicode_Concat(repr, str); Py_DECREF(repr); Py_DECREF(str);
    repr = new_repr;

	return repr;
}

static PyMemberDef Bracket_members[] = {
    {"cat", T_OBJECT_EX, offsetof(BracketObject, cat), 0,
     "the syntactic category for this bracket"},
    {"fun", T_OBJECT_EX, offsetof(BracketObject, fun), 0,
     "the abstract function for this bracket"},
    {"fid", T_INT, offsetof(BracketObject, fid), 0,
     "an id which identifies this bracket in the bracketed string. If there are discontinuous phrases this id will be shared for all brackets belonging to the same phrase."},
    {"ann", T_OBJECT_EX, offsetof(BracketObject, ann), 0,
     "the analysis of the constituent"},
    {"children", T_OBJECT_EX, offsetof(BracketObject, children), 0,
     "a list with the children of this bracket"},
    {NULL}  /* Sentinel */
};

static PyTypeObject pgf_BracketType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Bracket",             /*tp_name*/
    sizeof(BracketObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor)Bracket_dealloc,/*tp_dealloc*/
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
    (reprfunc) Bracket_repr,   /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "a linearization bracket", /*tp_doc*/
    0,		                   /*tp_traverse */
    0,		                   /*tp_clear */
    0,		                   /*tp_richcompare */
    0,		                   /*tp_weaklistoffset */
    0,		                   /*tp_iter */
    0,		                   /*tp_iternext */
    0,                         /*tp_methods */
    Bracket_members,           /*tp_members */
    0,                         /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0,                         /*tp_init */
    0,                         /*tp_alloc */
    0,                         /*tp_new */
};

typedef struct {
	PyObject_HEAD
} BINDObject;

static PyObject *BIND_instance = NULL;

static void
BIND_dealloc(PyTypeObject *self)
{
    BIND_instance = NULL;
}

static PyObject *
BIND_repr(BINDObject *self)
{
	return PyUnicode_FromString("pgf.BIND");
}

static PyObject *
BIND_str(BINDObject *self)
{
	return PyUnicode_FromString("&+");
}

static PyObject *
BIND_alloc(PyTypeObject *self, Py_ssize_t nitems)
{
    if (BIND_instance == NULL)
        BIND_instance = PyType_GenericAlloc(self, nitems);
    else
        Py_INCREF(BIND_instance); 
    return BIND_instance;
}

static PyTypeObject pgf_BINDType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                       /*ob_size*/
    "pgf.BINDType",            /*tp_name*/
    sizeof(BINDObject),        /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor) BIND_dealloc, /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    (reprfunc) BIND_repr,      /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    (reprfunc) BIND_str,       /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "a marker for BIND in a bracketed string", /*tp_doc*/
    0,		                   /*tp_traverse */
    0,		                   /*tp_clear */
    0,		                   /*tp_richcompare */
    0,		                   /*tp_weaklistoffset */
    0,		                   /*tp_iter */
    0,		                   /*tp_iternext */
    0,                         /*tp_methods */
    0,                         /*tp_members */
    0,                         /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    0,                         /*tp_init */
    BIND_alloc,                /*tp_alloc */
    0,                         /*tp_new */
};

typedef struct
{
    PgfLinearizationOutputIface iface;
    int non_exist;
    PyObject *stack;
    PyObject *bs;
} PyPgfLinearizationOutput;

static void
pypgf_lin_out_symbol_token(PyPgfLinearizationOutput *this, PgfText *tok)
{
    PyObject *py_tok = PyUnicode_FromPgfText(tok);
    PyList_Append(this->bs, py_tok);
    Py_DECREF(py_tok);
}

static void
pypgf_lin_out_begin_phrase(PyPgfLinearizationOutput *this, PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
    PyList_Append(this->stack, this->bs);
    this->bs = PyList_New(0);
}

static void
pypgf_lin_out_end_phrase(PyPgfLinearizationOutput *this, PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
    PyObject *parent = PyObject_CallMethod(this->stack, "pop", "");
    if (PyList_Size(this->bs) > 0) {
        BracketObject* bracket = (BracketObject *)
			pgf_BracketType.tp_alloc(&pgf_BracketType, 0);
		if (bracket != NULL) {
            bracket->cat = PyUnicode_FromPgfText(cat);
			bracket->fid = fid;
			bracket->ann = PyUnicode_FromPgfText(ann);
			bracket->fun = PyUnicode_FromPgfText(fun);
			bracket->children = this->bs;
			PyList_Append(parent, (PyObject*) bracket);
			Py_DECREF(bracket);
        }
    } else {
        Py_DECREF(this->bs);
    }

    this->bs = parent;
}

static void
pypgf_lin_out_symbol_ne(PyPgfLinearizationOutput *this)
{
    this->non_exist = 1;
}

static void
pypgf_lin_out_symbol_bind(PyPgfLinearizationOutput *this)
{
    PyList_Append(this->bs, pgf_BINDType.tp_alloc(&pgf_BINDType, 0));
}

static void
pypgf_lin_out_flush(PyPgfLinearizationOutput *this)
{
}

PgfLinearizationOutputIfaceVtbl pypgf_lin_out_iface_vtbl =
{
    (void*) pypgf_lin_out_symbol_token,
    (void*) pypgf_lin_out_begin_phrase,
    (void*) pypgf_lin_out_end_phrase,
    (void*) pypgf_lin_out_symbol_ne,
    (void*) pypgf_lin_out_symbol_bind,
    (void*) pypgf_lin_out_flush
};

static PyObject*
Concr_bracketedLinearize(ConcrObject* self, PyObject *args)
{
	ExprObject* pyexpr;
	if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &pyexpr))
        return NULL;

    PyPgfLinearizationOutput lin_out;
    lin_out.iface.vtbl = &pypgf_lin_out_iface_vtbl;
    lin_out.non_exist = 0;
    lin_out.stack = PyList_New(0);
    lin_out.bs    = PyList_New(0);

	PgfExn err;
    pgf_bracketed_linearize(self->grammar->db, self->concr, (PgfExpr) pyexpr, NULL,
                            &marshaller, &lin_out.iface, &err);
    Py_DECREF(lin_out.stack);
	if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
	}

    if (lin_out.non_exist) {
        Py_DECREF(lin_out.bs);
        Py_RETURN_NONE;
    }

	return lin_out.bs;
}

static PyGetSetDef Concr_getseters[] = {
    {"name", 
     (getter)Concr_getName, NULL,
     "the name of the concrete syntax",
    },
/*    {"languageCode", 
     (getter)Concr_getLanguageCode, NULL,
     "the language code for this concrete syntax",
    },*/
    {NULL}  /* Sentinel */
};

static PyMethodDef Concr_methods[] = {
/*    {"printName", (PyCFunction)Concr_printName, METH_VARARGS,
     "Returns the print name of a function or category"
    },
    {"parse", (PyCFunction)Concr_parse, METH_VARARGS | METH_KEYWORDS,
     "Parses a string and returns an iterator over the abstract trees for this sentence\n\n"
     "Named arguments:\n"
     "- sentence (string)\n"
     "- cat (string); OPTIONAL, default: the startcat of the grammar\n"
     "- n (int), max. trees; OPTIONAL, default: extract all trees\n"
     "- heuristics (double >= 0.0); OPTIONAL, default: taken from the flags in the grammar\n"
     "- callbacks (list of category and callback); OPTIONAL, default: built-in callbacks only for Int, String and Float"
    },
    {"complete", (PyCFunction)Concr_complete, METH_VARARGS | METH_KEYWORDS,
     "Parses a partial string and returns a list with the top n possible next tokens"
     "Named arguments:\n"
     "- sentence (string or a (string,pgf.BIND) tuple. The later indicates that the sentence ends with a BIND token)\n"
     "- cat (string); OPTIONAL, default: the startcat of the grammar\n"
     "- prefix (string); OPTIONAL, the prefix of predicted tokens"
     "- n (int), max. number of predicted tokens"
    },
    {"parseval", (PyCFunction)Concr_parseval, METH_VARARGS,
     "Computes precision, recall and exact match for the parser on a given abstract tree"
    },
    {"lookupSentence", (PyCFunction)Concr_lookupSentence, METH_VARARGS | METH_KEYWORDS,
     "Looks up a sentence from the grammar by a sequence of keywords\n\n"
     "Named arguments:\n"
     "- sentence (string) or tokens (list of strings)\n"
     "- cat (string); OPTIONAL, default: the startcat of the grammar\n"
     "- n (int), max. trees; OPTIONAL, default: extract all trees"
    },*/
    {"linearize", (PyCFunction)Concr_linearize, METH_VARARGS,
     "Takes an abstract tree and linearizes it to a string"
    },
    /*{"linearizeAll", (PyCFunction)Concr_linearizeAll, METH_VARARGS | METH_KEYWORDS,
     "Takes an abstract tree and linearizes with all variants"
    },
    {"tabularLinearize", (PyCFunction)Concr_tabularLinearize, METH_VARARGS,
     "Takes an abstract tree and linearizes it to a table containing all fields"
	},*/
    {"bracketedLinearize", (PyCFunction)Concr_bracketedLinearize, METH_VARARGS,
     "Takes an abstract tree and linearizes it to a bracketed string"
    },
/*    {"bracketedLinearizeAll", (PyCFunction)Concr_bracketedLinearizeAll, METH_VARARGS | METH_KEYWORDS,
     "Takes an abstract tree and linearizes all variants into bracketed strings"
    },
    {"hasLinearization", (PyCFunction)Concr_hasLinearization, METH_VARARGS,
     "hasLinearization(f) returns true if the function f has linearization in the concrete syntax"
    },
    {"graphvizParseTree", (PyCFunction)Concr_graphvizParseTree, METH_VARARGS,
     "Renders an abstract syntax tree as a parse tree in Graphviz format"
    },
    {"lookupMorpho", (PyCFunction)Concr_lookupMorpho, METH_VARARGS,
     "Looks up a word in the lexicon of the grammar"
    },
    {"lookupCohorts", (PyCFunction)Concr_lookupCohorts, METH_VARARGS,
     "Takes a sentence and returns all matches for lexical items from the grammar in that sentence"
    },
    {"fullFormLexicon", (PyCFunction)Concr_fullFormLexicon, METH_VARARGS,
     "Enumerates all words in the lexicon (useful for extracting full form lexicons)"
    },
    {"load", (PyCFunction)Concr_load, METH_VARARGS,
     "Loads the concrete syntax from a .pgf_c file"
    },
    {"unload", (PyCFunction)Concr_unload, METH_VARARGS,
     "Unloads the concrete syntax"
    },*/
    {NULL}  /* Sentinel */
};

static PyTypeObject pgf_ConcrType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    //0,                         /*ob_size*/
    "pgf.Concr",               /*tp_name*/
    sizeof(ConcrObject),       /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    (destructor)Concr_dealloc, /*tp_dealloc*/
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
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "concrete syntax",         /*tp_doc*/
    0,		                   /*tp_traverse */
    0,		                   /*tp_clear */
    0,		                   /*tp_richcompare */
    0,		                   /*tp_weaklistoffset */
    0,		                   /*tp_iter */
    0,		                   /*tp_iternext */
    Concr_methods,             /*tp_methods */
    0,                         /*tp_members */
    Concr_getseters,           /*tp_getset */
    0,                         /*tp_base */
    0,                         /*tp_dict */
    0,                         /*tp_descr_get */
    0,                         /*tp_descr_set */
    0,                         /*tp_dictoffset */
    (initproc)Concr_init,      /*tp_init */
    0,                         /*tp_alloc */
    (newfunc)Concr_new,        /*tp_new */
};

// ----------------------------------------------------------------------------
// PGF: the grammar object

static void
PGF_dealloc(PGFObject *self)
{
    if (self->db != NULL && self->revision != 0)
        pgf_free_revision(self->db, self->revision);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
PGF_writePGF(PGFObject *self, PyObject *args, PyObject *kwargs)
{
    char *kwds[] = {"","langs",NULL};

    const char *fpath;
    PyObject *py_langs = NULL;
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "s|O!", kwds, &fpath, &PyList_Type, &py_langs))
        return NULL;

    PgfText **langs = NULL;
    if (py_langs != NULL) {
        size_t len = PyList_Size(py_langs);
        langs = (PgfText **) alloca((len+1)*sizeof(PgfText*));
        for (size_t i = 0; i < len; i++) {
            langs[i] = PyUnicode_AsPgfText(PyList_GetItem(py_langs,i));
            if (!langs[i]) {
                while (i > 0) {
                    FreePgfText(langs[--i]);
                }
                return NULL;
            }
        }
        langs[len] = NULL;
    }

    PgfExn err;
    pgf_write_pgf(fpath, self->db, self->revision, langs, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    if (langs != NULL) {
        PgfText **p = langs;
        while (*p) {
            FreePgfText(*p);
            p++;
        }
    }

    Py_RETURN_NONE;
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
pgf_collect_langs(PgfItor *fn, PgfText *name, PgfConcrRevision concr, PgfExn *err)
{
    PyPGFClosure* clo = (PyPGFClosure*) fn;
    
    PyObject* py_name = NULL;
    PyObject* py_lang = NULL;

	py_name = PyUnicode_FromStringAndSize(name->text, name->size);
	if (py_name == NULL) {
		err->type = PGF_EXN_OTHER_ERROR;
		goto end;
	}

	py_lang = pgf_ConcrType.tp_alloc(&pgf_ConcrType, 0);
	if (py_lang == NULL) {
		err->type = PGF_EXN_OTHER_ERROR;
		goto end;
	}

	((ConcrObject *) py_lang)->concr = concr;
	((ConcrObject *) py_lang)->grammar = clo->grammar;
	Py_INCREF(clo->grammar);

    if (PyDict_SetItem((PyObject*) clo->collection, py_name, py_lang) != 0) {
		err->type = PGF_EXN_OTHER_ERROR;
		goto end;
	}

end:
    Py_XDECREF(py_lang);
    Py_XDECREF(py_name);
}

static PyObject*
PGF_getLanguages(PGFObject *self, void *closure)
{
	PyObject* languages = PyDict_New();
	if (languages == NULL)
		return NULL;

	PgfExn err;

	PyPGFClosure clo = { { pgf_collect_langs }, self, languages };

	pgf_iter_concretes(self->db, self->revision, &clo.fn, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(languages);
        return NULL;
    }

	PyObject* proxy = PyDictProxy_New(languages);
	
	Py_DECREF(languages);

    return proxy;
}

static void
_collect_cats(PgfItor *fn, PgfText *key, object value, PgfExn *err)
{
    PgfText *name = key;
    PyPGFClosure *clo = (PyPGFClosure*) fn;

    PyObject *py_name = PyUnicode_FromStringAndSize(name->text, name->size);
    if (py_name == NULL) {
        err->type = PGF_EXN_OTHER_ERROR;
        return;
    }

    if (PyList_Append((PyObject*) clo->collection, py_name) != 0) {
        err->type = PGF_EXN_OTHER_ERROR;
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

    PgfText *catname = CString_AsPgfText(s, size);

    PgfExn err;
    size_t n_hypos;
    PgfTypeHypo *hypos = pgf_category_context(self->db, self->revision, catname, &n_hypos, &unmarshaller, &err);
    FreePgfText(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    if (hypos == NULL) {
        Py_RETURN_NONE;
    }

    PyObject *contexts = PyTuple_FromHypos(hypos, n_hypos);

    for (size_t i = 0; i < n_hypos; i++) {
        free(hypos[i].cid);
        Py_DECREF((PyObject *)hypos[i].type);
    }
    free(hypos);

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
_collect_funs(PgfItor *fn, PgfText *key, object value, PgfExn *err)
{
    PgfText *name = key;
    PyPGFClosure *clo = (PyPGFClosure*) fn;

    PyObject *py_name = PyUnicode_FromStringAndSize(name->text, name->size);
    if (py_name == NULL) {
        err->type = PGF_EXN_OTHER_ERROR;
        return;
    }

    if (PyList_Append((PyObject*) clo->collection, py_name) != 0) {
        err->type = PGF_EXN_OTHER_ERROR;
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

    PgfText *catname = CString_AsPgfText(s, size);

    PyObject *functions = PyList_New(0);
    if (functions == NULL) {
        FreePgfText(catname);
        return NULL;
    }

    PgfExn err;
    PyPGFClosure clo = { { _collect_funs }, self, functions };
    pgf_iter_functions_by_cat(self->db, self->revision, catname, &clo.fn, &err);
    FreePgfText(catname);
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

    PgfText *funname = CString_AsPgfText(s, size);

    PgfExn err;
    PgfType type = pgf_function_type(self->db, self->revision, funname, &unmarshaller, &err);
    FreePgfText(funname);
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

    PgfText *funname = CString_AsPgfText(s, size);

    PgfExn err;
    int isCon = pgf_function_is_constructor(self->db, self->revision, funname, &err);
    FreePgfText(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    return PyBool_FromLong(isCon);
}

static PyObject *
PGF_categoryProbability(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *catname = CString_AsPgfText(s, size);

    PgfExn err;
    prob_t prob = pgf_category_prob(self->db, self->revision, catname, &err);
    FreePgfText(catname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    double dprob = (double) prob;
    if (dprob == INFINITY) {
        PyErr_Format(PyExc_KeyError, "category '%s' is not defined", s);
        return NULL;
    }
    return PyFloat_FromDouble(dprob);
}

static PyObject *
PGF_functionProbability(PGFObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *funname = CString_AsPgfText(s, size);

    PgfExn err;
    prob_t prob = pgf_function_prob(self->db, self->revision, funname, &err);
    FreePgfText(funname);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    double dprob = (double) prob;
    if (dprob == INFINITY) {
        PyErr_Format(PyExc_KeyError, "function '%s' is not defined", s);
        return NULL;
    }
    return PyFloat_FromDouble(dprob);
}

static PyObject *
PGF_exprProbability(PGFObject *self, PyObject *args)
{
    ExprObject *expr;
    if (!PyArg_ParseTuple(args, "O!", &pgf_ExprType, &expr))
        return NULL;

    PgfExn err;
    prob_t prob = pgf_expr_prob(self->db, self->revision, (PgfExpr) expr, &marshaller, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        return NULL;
    }

    double dprob = (double) prob;
    return PyFloat_FromDouble(dprob);
}

static PyGetSetDef PGF_getseters[] = {
    {"abstractName",
     (getter)PGF_getAbstractName, NULL,
     "the abstract syntax name",
     NULL},
    {"languages", 
     (getter)PGF_getLanguages, NULL,
     "a map containing all concrete languages in the grammar",
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

#if PY_VERSION_HEX < 0x03080000

static void
pgf_embed_funs(PgfItor* fn, PgfText* name, object value, PgfExn* err)
{
    PyPGFClosure *clo = (PyPGFClosure*) fn;

	ExprFunObject *pyexpr = (ExprFunObject *)
        pgf_ExprFunType.tp_alloc(&pgf_ExprFunType, 0);
	if (pyexpr == NULL) {
		err->type = PGF_EXN_OTHER_ERROR;
		return;
	}

	pyexpr->name = PyUnicode_FromStringAndSize(name->text, name->size);
    if (pyexpr->name == NULL) {
        Py_DECREF(pyexpr);
        err->type = PGF_EXN_OTHER_ERROR;
        return;
    }

    if (PyModule_AddObject(clo->collection, name->text, (PyObject*) pyexpr) != 0) {
		Py_DECREF(pyexpr);
        err->type = PGF_EXN_OTHER_ERROR;
	}
}

static PyObject*
PGF_embed(PGFObject* self, PyObject *modname)
{
    PyObject *m = PyImport_Import(modname);
    if (m == NULL) {
        PyObject *globals = PyEval_GetBuiltins();
        if (globals != NULL) {
            PyObject *exc = PyDict_GetItemString(globals, "ModuleNotFoundError");
            if (exc != NULL) {
                if (PyErr_ExceptionMatches(exc)) {
                    PyErr_Clear();
                    m = PyImport_AddModuleObject(modname);
                    Py_INCREF(m);
                }
            }
        }
    }
    if (m == NULL)
        return NULL;

    Py_INCREF(self);
    if (PyModule_AddObject(m, "__pgf__", (PyObject*) self) != 0) {
        Py_DECREF(self);
        Py_DECREF(m);
        return NULL;
    }

    PgfExn err;
    PyPGFClosure clo = { { pgf_embed_funs }, self, m };
    pgf_iter_functions(self->db, self->revision, &clo.fn, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(m);
        return NULL;
    }

    return m;
}

#else

typedef struct {
    PyObject_HEAD
    PyObject* dict;
    PyObject* modname;
    PyObject* package_path;
    PyObject* grammar_path;
    PGFObject* grammar;
} EmbeddedGrammarObject;

static PyObject *EmbeddedGrammar_getattro(EmbeddedGrammarObject *self, PyObject *py_attr)
{
    PgfText *name = PyUnicode_AsPgfText(py_attr);
    if (!name)
        return NULL;

    PgfExn err;
    prob_t prob = pgf_function_prob(self->grammar->db, self->grammar->revision, name, &err);
    PyMem_RawFree(name);

    if (handleError(err) != PGF_EXN_NONE)
        return NULL;

    if (prob != INFINITY) {
        ExprFunObject *pyexpr = (ExprFunObject *)
            pgf_ExprFunType.tp_alloc(&pgf_ExprFunType, 0);
        if (pyexpr == NULL)
            return NULL;

        pyexpr->name = py_attr; Py_INCREF(py_attr);
        return (PyObject*) pyexpr;
    }

    return PyObject_GenericGetAttr((PyObject*) self, py_attr);
}

static PyObject*
EmbeddedGrammar_str(EmbeddedGrammarObject *self)
{
    if (self->grammar_path == Py_None) {
        return PyUnicode_FromFormat("<embedded grammar '%U'>", self->modname);
    } else {
        return PyUnicode_FromFormat("<embedded grammar '%U' from '%U'>", self->modname, self->grammar_path);
    }
}

static void
EmbeddedGrammar_dealloc(EmbeddedGrammarObject* self)
{
    Py_XDECREF(self->dict);
    Py_XDECREF(self->modname);
    Py_XDECREF(self->package_path);
    Py_XDECREF(self->grammar_path);
    Py_XDECREF(self->grammar);
}

static PyMemberDef EmbeddedGrammar_members[] = {
    {"__name__", T_OBJECT, offsetof(EmbeddedGrammarObject, modname), READONLY, NULL},
    {"__path__", T_OBJECT, offsetof(EmbeddedGrammarObject, package_path), READONLY, NULL},
    {"__file__", T_OBJECT, offsetof(EmbeddedGrammarObject, grammar_path), READONLY, NULL},
    {"__dict__", T_OBJECT, offsetof(EmbeddedGrammarObject, dict),    READONLY, NULL},
    {"__pgf__",  T_OBJECT, offsetof(EmbeddedGrammarObject, grammar), READONLY, NULL},
    {NULL}
};

static PyTypeObject pgf_EmbeddedGrammarType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "pgf.EmbeddedGrammar",                    /*tp_name*/
    sizeof(EmbeddedGrammarObject),            /*tp_basicsize*/
    0,                                        /*tp_itemsize*/
    (destructor)EmbeddedGrammar_dealloc,      /*tp_dealloc*/
    0,                                        /*tp_print*/
    0,                                        /*tp_getattr*/
    0,                                        /*tp_setattr*/
    0,                                        /*tp_compare*/
    (reprfunc) EmbeddedGrammar_str,           /*tp_repr*/
    0,                                        /*tp_as_number*/
    0,                                        /*tp_as_sequence*/
    0,                                        /*tp_as_mapping*/
    0,                                        /*tp_hash */
    0,                                        /*tp_call*/
    (reprfunc) EmbeddedGrammar_str,           /*tp_str*/
    (getattrofunc) EmbeddedGrammar_getattro,  /*tp_getattro*/
    0,                                        /*tp_setattro*/
    0,                                        /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "a grammar embeded in Python as a module",/*tp_doc*/
    0,		                                  /*tp_traverse */
    0,	            	                      /*tp_clear */
    0,		                                  /*tp_richcompare */
    0,		                                  /*tp_weaklistoffset */
    0,		                                  /*tp_iter */
    0,		                                  /*tp_iternext */
    0,                                        /*tp_methods */
    EmbeddedGrammar_members,                  /*tp_members */
    0,                                        /*tp_getset */
    0,                                        /*tp_base */
    0,                                        /*tp_dict */
    0,                                        /*tp_descr_get */
    0,                                        /*tp_descr_set */
    offsetof(EmbeddedGrammarObject,dict),     /*tp_dictoffset */
    0,                                        /*tp_init */
    0,                                        /*tp_alloc */
    0,                                        /*tp_new */
};

static PyObject*
PGF_embed(PGFObject* self, PyObject *modname)
{
    PyObject *module = PyImport_Import(modname);
    if (module == NULL) {
        PyObject *globals = PyEval_GetBuiltins();
        if (globals == NULL)
            return NULL;

        PyObject *exc = PyDict_GetItemString(globals, "ModuleNotFoundError");
        if (exc == NULL)
            return NULL;

        if (!PyErr_ExceptionMatches(exc))
            return NULL;

        PyErr_Clear();
    }

    EmbeddedGrammarObject *py_embedding =
        (EmbeddedGrammarObject *) pgf_EmbeddedGrammarType.tp_alloc(&pgf_EmbeddedGrammarType, 0);
	if (py_embedding == NULL) {
		return NULL;
	}

    py_embedding->modname = modname; Py_INCREF(modname);
    py_embedding->grammar = self;    Py_INCREF(self);

    const char *fpath = pgf_file_path(self->db);
    if (fpath == NULL) {
        py_embedding->grammar_path = Py_None; Py_INCREF(Py_None);
    } else {
        py_embedding->grammar_path = PyUnicode_FromString(fpath);
    }

    if (module == NULL) {
        py_embedding->package_path = PyList_New(0);
    } else {
        py_embedding->package_path = PyObject_GetAttrString(module, "__path__");
    }

    if (module == NULL) {
        py_embedding->dict = PyDict_New();
    } else {
        py_embedding->dict = PyModule_GetDict(module);
        Py_INCREF(py_embedding->dict);
    }

    if (_PyImport_SetModule(modname, (PyObject*) py_embedding) < 0) {
        return NULL;
    }

    return (PyObject*) py_embedding;
}

typedef struct {
    PyObject_HEAD
    PyObject *package_path;
    PyObject *grammar_path;
    int is_pgf;
} GrammarImporterObject;

static void
GrammarImporter_dealloc(GrammarImporterObject* self)
{
    Py_DECREF(self->package_path);
    Py_DECREF(self->grammar_path);
}

static PyObject *get_package_path(PyObject *dir,PyObject *name)
{
    PyObject *py_fpath = 
        (PyUnicode_GET_LENGTH(dir) == 0) ? PyUnicode_FromFormat("%U", name)
                                         : PyUnicode_FromFormat("%U/%U", dir, name);
    const char *fpath = PyUnicode_AsUTF8(py_fpath);

    int exists = 0;
#ifdef _WIN32
    if (_access(fpath, 0) == 0) {
        struct _stat status;
        _stat(fpath, &status);
        exists = ((status.st_mode & S_IFDIR) != 0);
    }
#else
    DIR* dirent = opendir(fpath);
    if (dirent) {
        exists = 1;
        closedir(dirent);
    }
#endif

    if (exists) {
        PyObject *path = PyList_New(1);
        PyList_SET_ITEM(path, 0, py_fpath);
        return path;
    } else {
        Py_DECREF(py_fpath);
        return PyList_New(0);
    }
}

static PyObject *
GrammarImporter_find_spec(PyTypeObject *class, PyObject *args)
{
    PyObject *name, *path, *target;
    if (!PyArg_ParseTuple(args, "UOO", &name, &path, &target))
        return NULL;

    PyObject *parts = PyObject_CallMethod(name, "rpartition", "s", ".");
    if (parts == NULL)
        return NULL;
    name = PyTuple_GET_ITEM(parts, 2);

    if (path == Py_None) {
        PyObject *sys = PyImport_ImportModule("sys");
        if (sys == NULL) {
            Py_DECREF(parts);
            return NULL;
        }
        path = PyObject_GetAttrString(sys, "path");
        Py_DECREF(sys);
    } else {
        Py_INCREF(path);
    }

    PyObject *iterator = PyObject_GetIter(path);
    if (iterator == NULL) {
        Py_DECREF(parts);
        Py_DECREF(path);
        return NULL;
    }

    GrammarImporterObject *py_importer = NULL;

    for (;;) {
        PyObject *dir = PyIter_Next(iterator);
        if (dir == NULL)
            break;

        {
            PyObject *py_fpath = 
                (PyUnicode_GET_LENGTH(dir) == 0) ? PyUnicode_FromFormat("%U.ngf", name)
                                                 : PyUnicode_FromFormat("%U/%U.ngf", dir, name);
            const char *fpath = PyUnicode_AsUTF8(py_fpath);
#ifdef _WIN32
            if (access(fpath, 0) == 0) {
#else
            if (access(fpath, F_OK) == 0) {
#endif
                py_importer = (GrammarImporterObject *)class->tp_alloc(class, 0);
                py_importer->package_path = get_package_path(dir,name);
                py_importer->grammar_path = py_fpath;
                py_importer->is_pgf = 0;
                Py_DECREF(dir);
                break;
            }
            Py_DECREF(py_fpath);
        }

        {
            PyObject *py_fpath = 
                (PyUnicode_GET_LENGTH(dir) == 0) ? PyUnicode_FromFormat("%U.pgf", name)
                                                 : PyUnicode_FromFormat("%U/%U.pgf", dir, name);
            const char *fpath = PyUnicode_AsUTF8(py_fpath);
#ifdef _WIN32
            if (access(fpath, 0) == 0) {
#else
            if (access(fpath, F_OK) == 0) {
#endif
                py_importer = (GrammarImporterObject *)class->tp_alloc(class, 0);
                py_importer->package_path = get_package_path(dir,name);
                py_importer->grammar_path = py_fpath;
                py_importer->is_pgf = 1;
                Py_DECREF(dir);
                break;
            }
            Py_DECREF(py_fpath);
        }

        Py_DECREF(dir);
    }

    Py_DECREF(iterator);
    Py_DECREF(path);

    if (py_importer == NULL) {
        Py_DECREF(parts);
        Py_RETURN_NONE;
    }

    PyObject *machinery = PyImport_ImportModule("importlib.machinery");
    if (machinery == NULL) {
        Py_DECREF(parts);
        return NULL;
    }

    PyObject *py_spec =
        PyObject_CallMethod(machinery, "ModuleSpec", "OO", name, py_importer);

    Py_DECREF(py_importer);
    Py_DECREF(parts);

    return py_spec;
}

static PyObject *
GrammarImporter_create_module(GrammarImporterObject *self, PyObject *spec);

static PyObject *
GrammarImporter_exec_module(PyTypeObject *class, PyObject *module)
{
    Py_RETURN_NONE;
}

static PyMethodDef GrammarImporter_methods[] = {
    {"find_spec", (PyCFunction)GrammarImporter_find_spec, METH_VARARGS | METH_CLASS,
     "Finds a grammar and returns a module specification"},
    {"create_module", (PyCFunction)GrammarImporter_create_module, METH_O,
     "Creates a module"},
    {"exec_module", (PyCFunction)GrammarImporter_exec_module, METH_O,
     "Executes a module"},
    {NULL}  /* Sentinel */
};

static PyTypeObject pgf_GrammarImporterType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "pgf.GrammarImporter",                    /*tp_name*/
    sizeof(GrammarImporterObject),            /*tp_basicsize*/
    0,                                        /*tp_itemsize*/
    (destructor)GrammarImporter_dealloc,      /*tp_dealloc*/
    0,                                        /*tp_print*/
    0,                                        /*tp_getattr*/
    0,                                        /*tp_setattr*/
    0,                                        /*tp_compare*/
    0,                                        /*tp_repr*/
    0,                                        /*tp_as_number*/
    0,                                        /*tp_as_sequence*/
    0,                                        /*tp_as_mapping*/
    0,                                        /*tp_hash */
    0,                                        /*tp_call*/
    0,                                        /*tp_str*/
    0,                                        /*tp_getattro*/
    0,                                        /*tp_setattro*/
    0,                                        /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "a finder for grammars as Python modules",/*tp_doc*/
    0,		                                  /*tp_traverse */
    0,	            	                      /*tp_clear */
    0,		                                  /*tp_richcompare */
    0,		                                  /*tp_weaklistoffset */
    0,		                                  /*tp_iter */
    0,		                                  /*tp_iternext */
    GrammarImporter_methods,                  /*tp_methods */
    0,                                        /*tp_members */
    0,                                        /*tp_getset */
    0,                                        /*tp_base */
    0,                                        /*tp_dict */
    0,                                        /*tp_descr_get */
    0,                                        /*tp_descr_set */
    0,                                        /*tp_dictoffset */
    0,                                        /*tp_init */
    0,                                        /*tp_alloc */
    0,                                        /*tp_new */
};

#endif

static PyMethodDef PGF_methods[] = {
    {"writePGF", (PyCFunction)PGF_writePGF, METH_VARARGS | METH_KEYWORDS,
     "Writes to a PGF file"},
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
    {"categoryProbability", (PyCFunction)PGF_categoryProbability, METH_VARARGS,
     "Returns the probability of a category"
    },
    {"functionProbability", (PyCFunction)PGF_functionProbability, METH_VARARGS,
     "Returns the probability of a function"
    },
    {"exprProbability", (PyCFunction)PGF_exprProbability, METH_VARARGS,
     "Returns the probability of an expression"
    },
    {"checkoutBranch", (PyCFunction)PGF_checkoutBranch, METH_VARARGS,
     "Switch to a branch"
    },
    {"newTransaction", (PyCFunction)PGF_newTransaction, METH_VARARGS,
     "Create new transaction"
    },
    {"getGlobalFlag", (PyCFunction)PGF_getGlobalFlag, METH_VARARGS,
     "Get the value of a global flag"
    },
    {"getAbstractFlag", (PyCFunction)PGF_getAbstractFlag, METH_VARARGS,
     "Get the value of an abstract flag"
    },
    {"embed", (PyCFunction)PGF_embed, METH_O,
     "embed(mod_name) creates a Python module with name mod_name, which "
     "contains one Python object for every abstract function in the grammar. "
     "The module can be imported to make it easier to construct abstract "
     "syntax trees."
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
    0,                         /*tp_traverse */
    0,                         /*tp_clear */
    0,                         /*tp_richcompare */
    0,                         /*tp_weaklistoffset */
    0,                         /*tp_iter */
    0,                         /*tp_iternext */
    PGF_methods,               /*tp_methods */
    0,                         /*tp_members */
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

#if PY_VERSION_HEX >= 0x03080000
static PyObject *
GrammarImporter_create_module(GrammarImporterObject *self, PyObject *spec)
{
    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    PgfExn err;
    const char *fpath = PyUnicode_AsUTF8(self->grammar_path);
    py_pgf->db = self->is_pgf ? pgf_read_pgf(fpath, &py_pgf->revision, NULL, &err)
                              : pgf_read_ngf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    EmbeddedGrammarObject *py_embedding =
        (EmbeddedGrammarObject *) pgf_EmbeddedGrammarType.tp_alloc(&pgf_EmbeddedGrammarType, 0);
	if (py_embedding == NULL) {
        Py_DECREF(py_pgf);
		return NULL;
	}

    py_embedding->modname = PyObject_GetAttrString(spec, "name");
    py_embedding->package_path = self->package_path; Py_INCREF(self->package_path);
    py_embedding->grammar_path = self->grammar_path; Py_INCREF(self->grammar_path);
    py_embedding->grammar = py_pgf;
    py_embedding->dict = PyDict_New();

    return (PyObject*) py_embedding;
}
#endif

// ----------------------------------------------------------------------------
// pgf: the module

static PGFObject *
pgf_readPGF(PyObject *self, PyObject *args)
{
    const char *fpath;
    if (!PyArg_ParseTuple(args, "s", &fpath))
        return NULL;

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    PgfExn err;
    py_pgf->db = pgf_read_pgf(fpath, &py_pgf->revision, NULL, &err);
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

    PgfExn err;
    py_pgf->db = pgf_boot_ngf(fpath, npath, &py_pgf->revision, NULL, &err);
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

    PgfExn err;
    py_pgf->db = pgf_read_ngf(fpath, &py_pgf->revision, &err);
    if (handleError(err) != PGF_EXN_NONE) {
        Py_DECREF(py_pgf);
        return NULL;
    }

    return py_pgf;
}

static PGFObject *
pgf_newNGF(PyObject *self, PyObject *args, PyObject *kwargs)
{
    char *kwds[] = {"","file","size",NULL};

    const char *s;
    Py_ssize_t size;
    const char *fpath = NULL;
    Py_ssize_t init_size = 0;
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "s#|sn", kwds, &s, &size, &fpath, &init_size))
        return NULL;
    PgfText *absname = CString_AsPgfText(s, size);

    PGFObject *py_pgf = (PGFObject *)pgf_PGFType.tp_alloc(&pgf_PGFType, 0);

    PgfExn err;
    py_pgf->db = pgf_new_ngf(absname, fpath, init_size, &py_pgf->revision, &err);
    FreePgfText(absname);
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

    PgfText *input = CString_AsPgfText(s, size);

    PgfExpr expr = pgf_read_expr(input, &unmarshaller);
    FreePgfText(input);
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
    ExprObject *expr;
    if (!PyArg_ParseTuple(args, "O!O!", &PyList_Type, &pylist, &pgf_ExprType, &expr))
        return NULL;

    PgfPrintContext *ctxt = PyList_AsPgfPrintContext(pylist);
    PgfText *s = pgf_print_expr((PgfExpr) expr, ctxt, 0, &marshaller);
    FreePgfPrintContext(ctxt);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    FreePgfText(s);
    return str;
}

static TypeObject *
pgf_readType(PyObject *self, PyObject *args)
{
    const char *s;
    Py_ssize_t size;
    if (!PyArg_ParseTuple(args, "s#", &s, &size))
        return NULL;

    PgfText *input = CString_AsPgfText(s, size);

    PgfType type = pgf_read_type(input, &unmarshaller);
    FreePgfText(input);
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
    TypeObject *type;
    if (!PyArg_ParseTuple(args, "O!O!", &PyList_Type, &pylist, &pgf_TypeType, &type))
        return NULL;

    PgfPrintContext *ctxt = PyList_AsPgfPrintContext(pylist);
    PgfText *s = pgf_print_type((PgfType) type, ctxt, 0, &marshaller);
    FreePgfPrintContext(ctxt);
    PyObject *str = PyUnicode_FromStringAndSize(s->text, s->size);
    FreePgfText(s);
    return str;
}

static PyObject *
pgf_mkHypo(PyObject *self, PyObject *args)
{
    PyObject *type;
    if (!PyArg_ParseTuple(args, "O!", &pgf_TypeType, &type))
        return NULL;

    // HypoObject *hypo = PyObject_New(HypoObject, &pgf_HypoType);
    // hypo->bind_type = Py_True; // explicit
    // hypo->cid = PyUnicode_FromStringAndSize("_", 1);
    // hypo->type = type;
    // Py_INCREF(hypo->bind_type);
    // Py_INCREF(hypo->cid);
    // Py_INCREF(hypo->type);
    // return hypo;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, Py_True); // explicit
    PyTuple_SetItem(tup, 1, PyUnicode_FromStringAndSize("_", 1));
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(Py_True);
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

    // HypoObject *hypo = PyObject_New(HypoObject, &pgf_HypoType);
    // hypo->bind_type = Py_True; // explicit
    // hypo->cid = var;
    // hypo->type = type;
    // Py_INCREF(hypo->bind_type);
    // Py_INCREF(hypo->cid);
    // Py_INCREF(hypo->type);
    // return hypo;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, Py_True); // explicit
    PyTuple_SetItem(tup, 1, var);
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(Py_True);
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

    // HypoObject *hypo = PyObject_New(HypoObject, &pgf_HypoType);
    // hypo->bind_type = Py_False; // implicit
    // hypo->cid = var;
    // hypo->type = type;
    // Py_INCREF(hypo->bind_type);
    // Py_INCREF(hypo->cid);
    // Py_INCREF(hypo->type);
    // return hypo;

    PyObject *tup = PyTuple_New(3);
    PyTuple_SetItem(tup, 0, Py_False); // implicit
    PyTuple_SetItem(tup, 1, var);
    PyTuple_SetItem(tup, 2, type);
    Py_INCREF(Py_True);
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
    {"newNGF",   (void*)pgf_newNGF,  METH_VARARGS | METH_KEYWORDS,
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

#define MOD_ERROR_VAL NULL
#define MOD_SUCCESS_VAL(val) val
#define MOD_INIT(name) PyMODINIT_FUNC PyInit_##name(void)
#define MOD_DEF(ob, name, doc, methods) \
    static struct PyModuleDef moduledef = { \
        PyModuleDef_HEAD_INIT, name, doc, -1, methods, }; \
        ob = PyModule_Create(&moduledef);

#define TYPE_READY(type) \
    if (PyType_Ready(&type) < 0) \
        return MOD_ERROR_VAL;

#define ADD_TYPE(name, type) \
    if (PyModule_AddObject(m, name, (PyObject *)&type) < 0) { \
        Py_DECREF(&type); \
        Py_DECREF(m); \
        return NULL; \
    }

#define ADD_TYPE_DIRECT(name, type) \
    if (PyModule_AddObject(m, name, (PyObject *)type) < 0) { \
        Py_DECREF(type); \
        Py_DECREF(m); \
        return NULL; \
    }

MOD_INIT(pgf)
{
    PyObject *m;

    TYPE_READY(pgf_PGFType);
    TYPE_READY(pgf_ConcrType);
    TYPE_READY(pgf_TransactionType);
    TYPE_READY(pgf_ExprType);
    TYPE_READY(pgf_ExprAbsType);
    TYPE_READY(pgf_ExprAppType);
    TYPE_READY(pgf_ExprLitType);
    TYPE_READY(pgf_ExprMetaType);
    TYPE_READY(pgf_ExprFunType);
    TYPE_READY(pgf_ExprVarType);
    TYPE_READY(pgf_ExprTypedType);
    TYPE_READY(pgf_ExprImplArgType);
    TYPE_READY(pgf_TypeType);
#if PY_VERSION_HEX >= 0x03080000
    TYPE_READY(pgf_EmbeddedGrammarType);
    TYPE_READY(pgf_GrammarImporterType);
#endif
    TYPE_READY(pgf_BracketType);
    TYPE_READY(pgf_BINDType);

    MOD_DEF(m, "pgf", "The Runtime for the Portable Grammar Format in Python", module_methods);
    if (m == NULL)
        return MOD_ERROR_VAL;

    PGFError = PyErr_NewException("pgf.PGFError", NULL, NULL);
    ADD_TYPE_DIRECT("PGFError", PGFError);

    ADD_TYPE("PGF", pgf_PGFType);
    ADD_TYPE("Concr", pgf_ConcrType);
    ADD_TYPE("Transaction", pgf_TransactionType);
    ADD_TYPE("Expr", pgf_ExprType);
    ADD_TYPE("ExprAbs", pgf_ExprAbsType);
    ADD_TYPE("ExprApp", pgf_ExprAppType);
    ADD_TYPE("ExprLit", pgf_ExprLitType);
    ADD_TYPE("ExprMeta", pgf_ExprMetaType);
    ADD_TYPE("ExprFun", pgf_ExprFunType);
    ADD_TYPE("ExprVar", pgf_ExprVarType);
    ADD_TYPE("ExprTyped", pgf_ExprTypedType);
    ADD_TYPE("ExprImplArg", pgf_ExprImplArgType);
    ADD_TYPE("Type", pgf_TypeType);
    ADD_TYPE("Bracket", pgf_BracketType);
    ADD_TYPE("BIND", pgf_BINDType);

    Py_INCREF(Py_True);
    ADD_TYPE_DIRECT("BIND_TYPE_EXPLICIT", Py_True);
    Py_INCREF(Py_False);
    ADD_TYPE_DIRECT("BIND_TYPE_IMPLICIT", Py_False);

#if PY_VERSION_HEX >= 0x03080000
    PyObject *sys = PyImport_ImportModule("sys");
    if (sys != NULL) {
        PyObject *meta_path = PyObject_GetAttrString(sys, "meta_path");
        Py_DECREF(sys);
        if (meta_path != NULL) {
            PyList_Append(meta_path, (PyObject*) &pgf_GrammarImporterType);
            Py_DECREF(meta_path);
        }
    }
#endif

    return MOD_SUCCESS_VAL(m);
}
