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
PgfPGF *pgf_read_pgf(const char* fpath,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    try {
        pgf = new PgfPGF(NULL, 0, 0, unmarshaller);

        std::ifstream in(fpath, std::ios::binary);
        if (in.fail()) {
            throw std::system_error(errno, std::generic_category());
        }

        {
            DB_scope scope(pgf, WRITER_SCOPE);

            PgfReader rdr(&in);
            ref<PgfPGFRoot> pgf_root = rdr.read_pgf();

            pgf->set_root(pgf_root);
        }

        return pgf;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (pgf != NULL)
        delete pgf;

    return NULL;
}

PGF_API
PgfPGF *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    try {
        pgf = new PgfPGF(ngf_path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR, unmarshaller);

        std::ifstream in(pgf_path, std::ios::binary);
        if (in.fail()) {
            throw std::system_error(errno, std::generic_category());
        }

        {
            DB_scope scope(pgf, WRITER_SCOPE);

            PgfReader rdr(&in);
            ref<PgfPGFRoot> pgf_root = rdr.read_pgf();

            pgf->set_root(pgf_root);

            DB::sync();
        }

        return pgf;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (pgf != NULL) {
        delete pgf;
        remove(ngf_path);
    }

    return NULL;
}

PGF_API
PgfPGF *pgf_read_ngf(const char *fpath,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    bool is_new = false;
    try {
        pgf = new PgfPGF(fpath, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR, unmarshaller);

        {
            DB_scope scope(pgf, WRITER_SCOPE);

            if (DB::get_root<PgfPGFRoot>() == 0) {
                is_new = true;
                ref<PgfPGFRoot> root = DB::malloc<PgfPGFRoot>();
                root->major_version = 2;
                root->minor_version = 0;
                root->gflags = 0;
                root->abstract.name = DB::malloc<PgfText>();
                root->abstract.name->size = 0;
                root->abstract.aflags = 0;
                root->abstract.funs = 0;
                root->abstract.cats = 0;
                DB::set_root<PgfPGFRoot>(root);
            }
        }

        return pgf;
    } catch (std::system_error& e) {
        err->type = PGF_EXN_SYSTEM_ERROR;
        err->code = e.code().value();
    } catch (pgf_error& e) {
        err->type = PGF_EXN_PGF_ERROR;
        err->msg  = strdup(e.what());
    }

    if (pgf != NULL) {
        delete pgf;
        if (is_new)
            remove(fpath);
    }

    return NULL;
}

PGF_API
void pgf_free(PgfPGF *pgf)
{
    delete pgf;
}

PGF_API
PgfText *pgf_abstract_name(PgfPGF* pgf)
{
    DB_scope scope(pgf, READER_SCOPE);

    return textdup(&(*pgf->get_root<PgfPGFRoot>()->abstract.name));
}

PGF_API
void pgf_iter_categories(PgfPGF *pgf, PgfItor *itor)
{
    DB_scope scope(pgf, READER_SCOPE);

    namespace_iter(pgf->get_root<PgfPGFRoot>()->abstract.cats, itor);
}

PGF_API
uintptr_t pgf_start_cat(PgfPGF *pgf)
{
    DB_scope scope(pgf, READER_SCOPE);

    PgfText *startcat = (PgfText *)
        alloca(sizeof(PgfText)+9);
    startcat->size = 8;
    strcpy(startcat->text, "startcat");

	ref<PgfFlag> flag =
		namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.aflags, startcat);

	if (flag != 0) {
		switch (ref<PgfLiteral>::get_tag(flag->value)) {
		case PgfLiteralStr::tag: {
			auto lstr = ref<PgfLiteralStr>::untagged(flag->value);

            uintptr_t type = pgf_read_type(&lstr->val, pgf->u);
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
	return pgf->u->dtyp(0,NULL,s,0,NULL);
}

PGF_API
PgfTypeHypo *pgf_category_context(PgfPGF *pgf, PgfText *catname, size_t *n_hypos)
{
    DB_scope scope(pgf, READER_SCOPE);

    ref<PgfAbsCat> abscat =
        namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.cats, catname);
	if (abscat == 0) {
        *n_hypos = 0;
		return NULL;
    }

    PgfTypeHypo *hypos = (PgfTypeHypo *)
        malloc(abscat->context->len * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < abscat->context->len; i++) {
        hypos[i].bind_type = abscat->context->data[i].bind_type;
        hypos[i].cid = textdup(abscat->context->data[i].cid);
        hypos[i].type = pgf_unmarshall_type(pgf->u, abscat->context->data[i].type);
    }

    *n_hypos = abscat->context->len;
    return hypos;
}

PGF_API
prob_t pgf_category_prob(PgfPGF *pgf, PgfText *catname)
{
    DB_scope scope(pgf, READER_SCOPE);

    ref<PgfAbsCat> abscat =
        namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.cats, catname);
	if (abscat == 0) {
		return 0;
    }

    return abscat->prob;
}

PGF_API
void pgf_iter_functions(PgfPGF *pgf, PgfItor *itor)
{
    DB_scope scope(pgf, READER_SCOPE);

    namespace_iter(pgf->get_root<PgfPGFRoot>()->abstract.funs, itor);
}

struct PgfItorHelper : PgfItor
{
    PgfText *cat;
    PgfItor *itor;
};

static
void iter_by_cat_helper(PgfItor *itor, PgfText *key, void *value)
{
    PgfItorHelper* helper = (PgfItorHelper*) itor;
    PgfAbsFun* absfun = (PgfAbsFun*) value;
    if (textcmp(helper->cat, &absfun->type->name) == 0)
        helper->itor->fn(helper->itor, key, value);
}

PGF_API
void pgf_iter_functions_by_cat(PgfPGF *pgf, PgfText *cat, PgfItor *itor)
{
    DB_scope scope(pgf, READER_SCOPE);

    PgfItorHelper helper;
    helper.fn   = iter_by_cat_helper;
    helper.cat  = cat;
    helper.itor = itor;
    namespace_iter(pgf->get_root<PgfPGFRoot>()->abstract.funs, &helper);
}

PGF_API
uintptr_t pgf_function_type(PgfPGF *pgf, PgfText *funname)
{
    DB_scope scope(pgf, READER_SCOPE);

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.funs, funname);
	if (absfun == 0)
		return 0;

	return pgf_unmarshall_type(pgf->u, absfun->type);
}

PGF_API
int pgf_function_is_constructor(PgfPGF *pgf, PgfText *funname)
{
    DB_scope scope(pgf, READER_SCOPE);

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.funs, funname);
	if (absfun == 0)
		return false;

	return (absfun->defns == 0);
}

PGF_API
prob_t pgf_function_prob(PgfPGF *pgf, PgfText *funname)
{
    DB_scope scope(pgf, READER_SCOPE);

    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->get_root<PgfPGFRoot>()->abstract.funs, funname);
	if (absfun == 0)
		return INFINITY;

	return absfun->ep.prob;
}

PGF_API
PgfText *pgf_print_expr(uintptr_t e,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m)
{
    PgfPrinter printer(ctxt,prio,m);
    m->match_expr(&printer, e);
    return printer.get_text();
}

PGF_API uintptr_t
pgf_read_expr(PgfText *input, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    uintptr_t res = parser.parse_expr();
    if (!parser.eof()) {
        u->free_ref(res);
        return 0;
    }
    return res;
}

PGF_API
PgfText *pgf_print_type(uintptr_t ty,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m)
{
    PgfPrinter printer(ctxt,prio,m);
    m->match_type(&printer, ty);
    return printer.get_text();
}

PGF_API uintptr_t
pgf_read_type(PgfText *input, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    uintptr_t res = parser.parse_type();
    if (!parser.eof()) {
        u->free_ref(res);
        return 0;
    }
    return res;
}
