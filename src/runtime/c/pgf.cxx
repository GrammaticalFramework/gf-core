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
            rdr.read_pgf(pgf);

            pgf->set_root();
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

void PgfPGF::set_root() {
    ref<PgfPGFRoot> root = DB::malloc<PgfPGFRoot>();
    root->major_version = major_version;
    root->minor_version = minor_version;
    DB::set_root(root);
}

PGF_API
void pgf_free(PgfPGF *pgf)
{
    delete pgf;
}
