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
PgfPGF *pgf_read(const char* fpath, PgfExn* err)
{
    PgfPGF *pgf = NULL;

    pgf_exn_clear(err);

    try {
        std::string fpath_n = fpath;
        size_t len = fpath_n.length();
        if (len > 4 && fpath_n.substr(len-4) == ".pgf")
            fpath_n[len-3] = 'n';
        else if (!(len > 4 && fpath_n.substr(len-4) == ".ngf"))
            fpath_n += ".ngf";

        pgf = new PgfPGF(fpath_n.c_str());

        if (DB::get_root<PgfPGFRoot>() == 0) {
            std::ifstream in(fpath, std::ios::binary);
            if (in.fail()) {
                throw std::system_error(errno, std::generic_category());
            }

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

    if (pgf != NULL)
        delete pgf;

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
	return textdup(&(*pgf->get_root<PgfPGFRoot>()->abstract.name));
}
