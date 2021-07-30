#include "data.h"
#include "reader.h"

PGF_API
PgfPGF *pgf_read(const char* fpath, PgfExn* err)
{
    PgfPGF *pgf = NULL;

    try {
        std::string fpath_n = fpath;
        size_t len = fpath_n.length();
        if (len > 4 && fpath_n.substr(len-4) == ".pgf")
            fpath_n[len-3] = 'n';
        else if (!(len > 4 && fpath_n.substr(len-4) == ".ngf"))
            fpath_n += ".ngf";

        pgf = new PgfPGF(fpath_n.c_str());

        if (pgf->db.get_root<PgfPGFRoot>() == 0) {
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
        err->type = "system_error";
        err->msg  = NULL;
    } catch (pgf_error& e) {
        err->type = "pgf_error";
        err->msg  = e.what();
    }

    if (pgf != NULL)
        delete pgf;

    return NULL;
}

void PgfPGF::set_root() {
    ref<PgfPGFRoot> root = db.malloc<PgfPGFRoot>();
    root->major_version = major_version;
    root->minor_version = minor_version;
    db.set_root(root);
}
