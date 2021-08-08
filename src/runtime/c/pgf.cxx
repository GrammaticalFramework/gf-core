#include <fcntl.h>
#include "data.h"
#include "reader.h"

static void
pgf_exn_clear(PgfExn* err)
{
    err->type = PGF_EXN_NONE;
    err->code = 0;
    err->msg  = NULL;
}

PGF_API
PgfPGF *pgf_read_pgf(const char* fpath, PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    try {
        pgf = new PgfPGF(NULL, 0, 0);

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
PgfPGF *pgf_boot_ngf(const char* pgf_path, const char* ngf_path, PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    try {
        pgf = new PgfPGF(ngf_path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);

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
PgfPGF *pgf_read_ngf(const char *fpath, PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    bool is_new = false;
    try {
        pgf = new PgfPGF(fpath, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);

        if (DB::get_root<PgfPGFRoot>() == 0) {
            is_new = true;

            DB_scope scope(pgf, WRITER_SCOPE);

            ref<PgfPGFRoot> root = DB::malloc<PgfPGFRoot>();
            root->major_version = 2;
            root->minor_version = 0;
            root->gflags = 0;
            root->abstract.name = DB::malloc<PgfText>();
            root->abstract.name->size = 0;
            root->abstract.aflags = 0;
            root->abstract.funs = 0;
            root->abstract.cats = 0;
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
            remove(ngf_path);
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
void pgf_iter_categories(PgfPGF* pgf, PgfItor* itor)
{
    DB_scope scope(pgf, READER_SCOPE);

    namespace_iter(pgf->get_root<PgfPGFRoot>()->abstract.cats, itor);
}

PGF_API
void pgf_iter_functions(PgfPGF* pgf, PgfItor* itor)
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
void iter_by_cat_helper(PgfItor* itor, PgfText* key, void* value)
{
    PgfItorHelper* helper = (PgfItorHelper*) itor;
    PgfAbsFun* absfun = (PgfAbsFun*) value;
    if (textcmp(helper->cat, &absfun->type->name) == 0)
        helper->itor->fn(helper->itor, key, value);
}

PGF_API
void pgf_iter_functions_by_cat(PgfPGF* pgf, PgfText* cat, PgfItor* itor)
{
    DB_scope scope(pgf, READER_SCOPE);

    PgfItorHelper helper;
    helper.fn   = iter_by_cat_helper;
    helper.cat  = cat;
    helper.itor = itor;
    namespace_iter(pgf->get_root<PgfPGFRoot>()->abstract.funs, &helper);
}
