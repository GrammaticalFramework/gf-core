#include <fcntl.h>
#include <math.h>
#include "data.h"
#include "reader.h"
#include "printer.h"

static void
pgf_exn_clear(PgfExn* err)
{
    err->type = PGF_EXN_NONE;
    err->code = 0;
    err->msg  = NULL;
}

PGF_API
PgfDB *pgf_read_pgf(const char* fpath,
                    PgfRevision *revision,
                    PgfExn* err)
{
    PgfDB *db = NULL;

    pgf_exn_clear(err);

    try {
        db = new PgfDB(NULL, 0, 0);

        std::ifstream in(fpath, std::ios::binary);
        if (in.fail()) {
            throw std::system_error(errno, std::generic_category());
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(&in);
            ref<PgfPGF> pgf = rdr.read_pgf();

            PgfDB::set_root(pgf);
            *revision = pgf.as_object();
        }

        return db;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (db != NULL)
        delete db;

    return NULL;
}

PGF_API
PgfDB *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                    PgfRevision *revision,
                    PgfExn* err)
{
    PgfDB *db = NULL;

    pgf_exn_clear(err);

    try {
        db = new PgfDB(ngf_path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);

        std::ifstream in(pgf_path, std::ios::binary);
        if (in.fail()) {
            throw std::system_error(errno, std::generic_category());
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(&in);
            ref<PgfPGF> pgf = rdr.read_pgf();

            db->set_root<PgfPGF>(pgf);
            *revision = pgf.as_object();

            PgfDB::sync();
        }

        return db;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (db != NULL) {
        delete db;
        remove(ngf_path);
    }

    return NULL;
}

PGF_API
PgfDB *pgf_read_ngf(const char *fpath,
                     PgfRevision *revision,
                     PgfExn* err)
{
    PgfDB *db = NULL;

    pgf_exn_clear(err);

    bool is_new = false;
    try {
        db = new PgfDB(fpath, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);

        {
            DB_scope scope(db, WRITER_SCOPE);

            if (PgfDB::get_root<PgfPGF>() == 0) {
                is_new = true;
                ref<PgfPGF> pgf = PgfDB::malloc<PgfPGF>();
                pgf->major_version = 2;
                pgf->minor_version = 0;
                pgf->gflags = 0;
                pgf->abstract.name = PgfDB::malloc<PgfText>();
                pgf->abstract.name->size = 0;
                pgf->abstract.aflags = 0;
                pgf->abstract.funs = 0;
                pgf->abstract.cats = 0;
                PgfDB::set_root<PgfPGF>(pgf);
                *revision = pgf.as_object();
            } else {
                *revision = PgfDB::get_root<PgfPGF>().as_object();
            }
        }

        return db;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (db != NULL) {
        delete db;
        if (is_new)
            remove(fpath);
    }

    return NULL;
}

PGF_API
void pgf_free(PgfDB *db)
{
    delete db;
}

PGF_API_DECL
void pgf_free_revision(PgfDB *pgf, PgfRevision revision)
{
}

PGF_API
PgfText *pgf_abstract_name(PgfDB *db, PgfRevision revision)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    return textdup(&(*pgf->abstract.name));
}

PGF_API
void pgf_iter_categories(PgfDB *db, PgfRevision revision,
                         PgfItor *itor, PgfExn *err)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    err->type = PGF_EXN_NONE;
    namespace_iter(pgf->abstract.cats, itor, err);
}

PGF_API
PgfType pgf_start_cat(PgfDB *db, PgfRevision revision,
                      PgfUnmarshaller *u)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    PgfText *startcat = (PgfText *)
        alloca(sizeof(PgfText)+9);
    startcat->size = 8;
    strcpy(startcat->text, "startcat");

	ref<PgfFlag> flag =
		namespace_lookup(pgf->abstract.aflags, startcat);

	if (flag != 0) {
		switch (ref<PgfLiteral>::get_tag(flag->value)) {
		case PgfLiteralStr::tag: {
			auto lstr = ref<PgfLiteralStr>::untagged(flag->value);

            PgfType type = pgf_read_type(&lstr->val, u);
			if (type == 0)
				break;
			return type;
		}
		}
	}

    PgfText *s = (PgfText *)
        alloca(sizeof(PgfText)+2);
    s->size = 1;
    s->text[0] = 'S';
    s->text[1] = 0;
	return u->dtyp(0,NULL,s,0,NULL);
}

PGF_API
PgfTypeHypo *pgf_category_context(PgfDB *db, PgfRevision revision,
                                  PgfText *catname, size_t *n_hypos, PgfUnmarshaller *u)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    ref<PgfAbsCat> abscat =
        namespace_lookup(pgf->abstract.cats, catname);
	if (abscat == 0) {
        *n_hypos = 0;
		return NULL;
    }

    PgfDBMarshaller m;

    PgfTypeHypo *hypos = (PgfTypeHypo *)
        malloc(abscat->context->len * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < abscat->context->len; i++) {
        hypos[i].bind_type = abscat->context->data[i].bind_type;
        hypos[i].cid = textdup(abscat->context->data[i].cid);
        hypos[i].type = m.match_type(u, abscat->context->data[i].type.as_object());
    }

    *n_hypos = abscat->context->len;
    return hypos;
}

PGF_API
prob_t pgf_category_prob(PgfDB *db, PgfRevision revision,
                         PgfText *catname)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    ref<PgfAbsCat> abscat =
        namespace_lookup(pgf->abstract.cats, catname);
	if (abscat == 0) {
		return 0;
    }

    return abscat->prob;
}

PGF_API
void pgf_iter_functions(PgfDB *db, PgfRevision revision,
                        PgfItor *itor, PgfExn *err)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    err->type = PGF_EXN_NONE;
    namespace_iter(pgf->abstract.funs, itor, err);
}

struct PgfItorHelper : PgfItor
{
    PgfText *cat;
    PgfItor *itor;
};

static
void iter_by_cat_helper(PgfItor *itor, PgfText *key, void *value,
		                PgfExn *err)
{
    PgfItorHelper* helper = (PgfItorHelper*) itor;
    PgfAbsFun* absfun = (PgfAbsFun*) value;
    if (textcmp(helper->cat, &absfun->type->name) == 0)
        helper->itor->fn(helper->itor, key, value, err);
}

PGF_API
void pgf_iter_functions_by_cat(PgfDB *db, PgfRevision revision,
                               PgfText *cat, PgfItor *itor, PgfExn *err)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    PgfItorHelper helper;
    helper.fn   = iter_by_cat_helper;
    helper.cat  = cat;
    helper.itor = itor;

    err->type = PGF_EXN_NONE;
    namespace_iter(pgf->abstract.funs, &helper, err);
}

PGF_API
PgfType pgf_function_type(PgfDB *db, PgfRevision revision,
                          PgfText *funname, PgfUnmarshaller *u)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->abstract.funs, funname);
	if (absfun == 0)
		return 0;

	return PgfDBMarshaller().match_type(u, absfun->type.as_object());
}

PGF_API
int pgf_function_is_constructor(PgfDB *db, PgfRevision revision,
                                PgfText *funname)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->abstract.funs, funname);
	if (absfun == 0)
		return false;

	return (absfun->defns == 0);
}

PGF_API
prob_t pgf_function_prob(PgfDB *db, PgfRevision revision,
                         PgfText *funname)
{
    DB_scope scope(db, READER_SCOPE);
    ref<PgfPGF> pgf = revision;

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->abstract.funs, funname);
	if (absfun == 0)
		return INFINITY;

	return absfun->ep.prob;
}

PGF_API
PgfText *pgf_print_expr(PgfExpr e,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m)
{
    PgfPrinter printer(ctxt,prio,m);
    m->match_expr(&printer, e);
    return printer.get_text();
}

PGF_API PgfExpr
pgf_read_expr(PgfText *input, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    PgfExpr res = parser.parse_expr();
    if (!parser.eof()) {
        u->free_ref(res);
        return 0;
    }
    return res;
}

PGF_API
PgfText *pgf_print_type(PgfType ty,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m)
{
    PgfPrinter printer(ctxt,prio,m);
    m->match_type(&printer, ty);
    return printer.get_text();
}

PGF_API
PgfType pgf_read_type(PgfText *input, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    PgfType res = parser.parse_type();
    if (!parser.eof()) {
        u->free_ref(res);
        return 0;
    }
    return res;
}

PGF_API
void pgf_create_function(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfType ty, prob_t prob,
                         PgfMarshaller *m,
                         PgfExn *err)
{
    DB_scope scope(db, WRITER_SCOPE);

    PgfDBUnmarshaller u(m);

    ref<PgfPGF> pgf = revision;
    ref<PgfAbsFun> absfun = PgfDB::malloc<PgfAbsFun>(sizeof(PgfAbsFun)+name->size+1);
    absfun->type  = m->match_type(&u, ty);
    absfun->arity = 0;
    absfun->defns = 0;
    absfun->ep.prob = prob;
    ref<PgfExprFun> efun =
        ref<PgfExprFun>::from_ptr((PgfExprFun*) &absfun->name);
    absfun->ep.expr = ref<PgfExprFun>::tagged(efun);
    memcpy(&absfun->name, name, sizeof(PgfText)+name->size+1);
    
    Namespace<PgfAbsFun> nmsp =
        namespace_insert(pgf->abstract.funs, absfun);
    namespace_release(pgf->abstract.funs);
    pgf->abstract.funs = nmsp;
}
