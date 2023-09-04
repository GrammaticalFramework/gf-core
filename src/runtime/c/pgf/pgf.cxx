#include <fcntl.h>
#include <math.h>
#include <errno.h>
#include <limits.h>
#ifdef _WIN32
#include <sys\stat.h>
#endif

#include "data.h"
#include "reader.h"
#include "writer.h"
#include "printer.h"
#include "typechecker.h"
#include "linearizer.h"
#include "parser.h"
#include "graphviz.h"
#include "aligner.h"
#include "generator.h"

static void
pgf_exn_clear(PgfExn* err)
{
    err->type = PGF_EXN_NONE;
    err->code = 0;
    err->msg  = NULL;
}

#define PGF_API_BEGIN \
    pgf_exn_clear(err); \
\
    try \

#define PGF_API_END \
    catch (pgf_systemerror& e) { \
        err->type = PGF_EXN_SYSTEM_ERROR; \
        err->code = e.code(); \
        err->msg  = e.filepath(); \
    } catch (pgf_error& e) { \
        err->type = PGF_EXN_PGF_ERROR; \
        err->msg  = strdup(e.what()); \
    }

PGF_API
PgfDB *pgf_read_pgf(const char* fpath, PgfRevision *revision,
                    PgfProbsCallback *probs_callback,
                    PgfExn* err)
{
    PgfDB *db = NULL;
    FILE *in = NULL;

    PGF_API_BEGIN {
        in = fopen(fpath, "rb");
        if (!in) {
            throw pgf_systemerror(errno, fpath);
        }

        fseek(in, 0, SEEK_END);
        size_t pgf_size = ftell(in);
        fseek(in, 0, SEEK_SET);
        db = new PgfDB(NULL, 0, 0, pgf_size*7);

        {
            DB_scope scope(db, WRITER_SCOPE);

            db->start_transaction();

            PgfReader rdr(in,probs_callback);
            ref<PgfPGF> pgf = rdr.read_pgf();
            fclose(in);

            db->set_transaction_object(pgf.as_object());

            *revision = db->register_revision(pgf.tagged(), PgfDB::get_txn_id());
            db->commit(pgf.as_object());
        }

        db->ref_count++;
        return db;
    } PGF_API_END

    if (db != NULL)
        delete db;

    if (in != NULL)
        fclose(in);

    return NULL;
}

PGF_API
PgfDB *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                    PgfRevision *revision,
                    PgfProbsCallback *probs_callback,
                    PgfExn* err)
{
    PgfDB *db = NULL;
    FILE *in = NULL;

    PGF_API_BEGIN {
        in = fopen(pgf_path, "rb");
        if (!in) {
            throw pgf_systemerror(errno, pgf_path);
        }

        fseek(in, 0, SEEK_END);
        size_t pgf_size = ftell(in);
        fseek(in, 0, SEEK_SET);

        db = new PgfDB(ngf_path, O_CREAT | O_EXCL | O_RDWR,
#ifndef _WIN32
                       S_IRUSR | S_IWUSR,
#else
                       _S_IREAD | _S_IWRITE,
#endif
                       pgf_size*7);

        {
            DB_scope scope(db, WRITER_SCOPE);

            db->start_transaction();

            PgfReader rdr(in,probs_callback);
            ref<PgfPGF> pgf = rdr.read_pgf();
            fclose(in);

            db->set_transaction_object(pgf.as_object());

            *revision = db->register_revision(pgf.tagged(), PgfDB::get_txn_id());
            db->commit(pgf.as_object());
        }

        db->ref_count++;
        return db;
    } PGF_API_END

    if (in != NULL)
        fclose(in);

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

    PGF_API_BEGIN {
        db = new PgfDB(fpath, O_RDWR, 0, 0);

        {
            DB_scope scope(db, READER_SCOPE);
            ref<PgfPGF> pgf = db->get_active_revision();
            *revision = 0;
            if (pgf != 0) {
                *revision = db->register_revision(pgf.tagged(), PgfDB::get_txn_id()-1);
                db->ref_count++;
            }
        }

        return db;
    } PGF_API_END

    if (db != NULL)
        delete db;

    return NULL;
}

PGF_API
PgfDB *pgf_new_ngf(PgfText *abstract_name,
                   const char *fpath,
                   size_t init_size,
                   PgfRevision *revision,
                   PgfExn* err)
{
    PgfDB *db = NULL;

    PGF_API_BEGIN {
        db = new PgfDB(fpath, O_CREAT | O_EXCL | O_RDWR,
#ifndef _WIN32
                       S_IRUSR | S_IWUSR,
#else
                       _S_IREAD | _S_IWRITE,
#endif
                       init_size);

        {
            DB_scope scope(db, WRITER_SCOPE);

            db->start_transaction();

            ref<PgfPGF> pgf = PgfDB::malloc<PgfPGF>();
            pgf->major_version = PGF_MAJOR_VERSION;
            pgf->minor_version = PGF_MINOR_VERSION;
            pgf->gflags = 0;
            pgf->abstract.name = PgfDB::malloc<PgfText>(abstract_name->size+1);
            memcpy(&(*pgf->abstract.name), abstract_name, sizeof(PgfText)+abstract_name->size+1);
            pgf->abstract.aflags = 0;
            pgf->abstract.funs = 0;
            pgf->abstract.cats = 0;
            pgf->concretes = 0;

            db->set_transaction_object(pgf.as_object());

            *revision = db->register_revision(pgf.tagged(), PgfDB::get_txn_id());
            db->commit(pgf.as_object());
        }

        db->ref_count++;
        return db;
    } PGF_API_END

    if (db != NULL) {
        delete db;
        if (fpath != NULL)
            remove(fpath);
    }

    return NULL;
}

PGF_API
void pgf_merge_pgf(PgfDB *db, PgfRevision revision,
                   const char* fpath,
                   PgfExn* err)
{
    FILE *in = NULL;

    PGF_API_BEGIN {
        in = fopen(fpath, "rb");
        if (!in) {
            throw pgf_systemerror(errno, fpath);
        }

        {
            DB_scope scope(db, WRITER_SCOPE);
            ref<PgfPGF> pgf = db->revision2pgf(revision);

            PgfReader rdr(in,NULL);
            rdr.merge_pgf(pgf);
        }
    } PGF_API_END

    if (in != NULL)
        fclose(in);
}

PGF_API
void pgf_write_pgf(const char* fpath,
                   PgfDB *db, PgfRevision revision,
                   PgfText **langs,
                   PgfExn* err)
{
    FILE *out = NULL;

    PGF_API_BEGIN {
        out = fopen(fpath, "wb");
        if (!out) {
            throw pgf_systemerror(errno, fpath);
        }

        {
            DB_scope scope(db, READER_SCOPE);
            ref<PgfPGF> pgf = db->revision2pgf(revision);

            PgfWriter wtr(langs, out);
            wtr.write_pgf(pgf);
        }
    } PGF_API_END

end:
    if (out != NULL)
        fclose(out);
}

#ifdef _GNU_SOURCE
PGF_API
void pgf_write_pgf_cookie
                  (void *cookie, cookie_io_functions_t *io_funcs,
                   PgfDB *db, PgfRevision revision,
                   PgfText **langs, // null terminated list or null
                   PgfExn* err)
{
    FILE *out = NULL;

    PGF_API_BEGIN {
        out = fopencookie(cookie, "wb", *io_funcs);
        if (!out) {
            throw pgf_systemerror(errno, "<cookie>");
        }

        {
            DB_scope scope(db, READER_SCOPE);
            ref<PgfPGF> pgf = db->revision2pgf(revision);

            PgfWriter wtr(langs, out);
            wtr.write_pgf(pgf);
        }
    } PGF_API_END

    if (out != NULL)
        fclose(out);
}
#endif

PGF_API
const char *pgf_file_path(PgfDB *db)
{
    return db->get_file_path();
}

PGF_API
void pgf_free_revision(PgfDB *db, PgfRevision revision)
{
    try {
        ref<PgfPGF> pgf = db->revision2pgf(revision);
        db->rollback(pgf.as_object());
        db->unregister_revision(revision);
        db->ref_count--;
    } catch (std::runtime_error& e) {
        // silently ignore and hope for the best
    }

    if (!db->ref_count)
        delete db;
}

PGF_API
void pgf_free_concr_revision(PgfDB *db, PgfConcrRevision revision)
{
    try {
        DB_scope scope(db, READER_SCOPE);
        db->unregister_revision(revision);
        db->ref_count--;
    } catch (std::runtime_error& e) {
        // silently ignore and hope for the best
    }

    if (!db->ref_count)
        delete db;
}

PGF_API
PgfText *pgf_abstract_name(PgfDB *db, PgfRevision revision,
                           PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        return textdup(&(*pgf->abstract.name));
    } PGF_API_END

    return NULL;
}

PGF_API
void pgf_iter_categories(PgfDB *db, PgfRevision revision,
                         PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        namespace_iter(pgf->abstract.cats, itor, err);
    } PGF_API_END
}

struct PgfItorConcrHelper : PgfItor
{
    PgfDB *db;
    txn_t txn_id;
    PgfItor *itor;
};

static
void iter_concretes_helper(PgfItor *itor, PgfText *key, object value, PgfExn *err)
{
    PgfItorConcrHelper* helper = (PgfItorConcrHelper*) itor;
    ref<PgfConcr> concr = value;
    object rev = helper->db->register_revision(concr.tagged(), helper->txn_id);
    helper->db->ref_count++;
    helper->itor->fn(helper->itor, key, rev, err);
}

PGF_API
void pgf_iter_concretes(PgfDB *db, PgfRevision revision,
                        PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        size_t txn_id;

        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision, &txn_id);

        PgfItorConcrHelper helper;
        helper.fn     = iter_concretes_helper;
        helper.db     = db;
        helper.txn_id = txn_id;
        helper.itor   = itor;

        namespace_iter(pgf->concretes, &helper, err);
    } PGF_API_END
}

PGF_API
PgfType pgf_start_cat(PgfDB *db, PgfRevision revision,
                      PgfUnmarshaller *u,
                      PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

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
    } PGF_API_END

    return 0;
}

PGF_API
PgfTypeHypo *pgf_category_context(PgfDB *db, PgfRevision revision,
                                  PgfText *catname, size_t *n_hypos, PgfUnmarshaller *u,
                                  PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

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
    } PGF_API_END

    *n_hypos = 0;
    return NULL;
}

PGF_API
prob_t pgf_category_prob(PgfDB *db, PgfRevision revision,
                         PgfText *catname,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsCat> abscat =
            namespace_lookup(pgf->abstract.cats, catname);
        if (abscat == 0) {
            return INFINITY;
        }

        return abscat->prob;
    } PGF_API_END

    return INFINITY;
}

PGF_API
void pgf_iter_functions(PgfDB *db, PgfRevision revision,
                        PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        pgf_exn_clear(err);
        namespace_iter(pgf->abstract.funs, itor, err);
    } PGF_API_END
}

PGF_API
void pgf_iter_functions_by_prefix(PgfDB *db, PgfRevision revision,
                                  PgfText *prefix, PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        pgf_exn_clear(err);
        namespace_iter_prefix(pgf->abstract.funs, prefix, itor, err);
    } PGF_API_END
}

PGF_API
void pgf_iter_functions_by_cat(PgfDB *db, PgfRevision revision,
                               PgfText *cat, PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        std::function<bool(ref<PgfAbsFun>)> f =
            [itor,err](ref<PgfAbsFun> fun) {
                itor->fn(itor, &fun->name, fun.as_object(), err);
                return (err->type == PGF_EXN_NONE);
            };
        probspace_iter(pgf->abstract.funs_by_cat, cat, f, false);
    } PGF_API_END
}

PGF_API
PgfType pgf_function_type(PgfDB *db, PgfRevision revision,
                          PgfText *funname, PgfUnmarshaller *u,
                          PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, funname);
        if (absfun == 0)
            return 0;

        return PgfDBMarshaller().match_type(u, absfun->type.as_object());
    } PGF_API_END

    return 0;
}

PGF_API
int pgf_function_is_constructor(PgfDB *db, PgfRevision revision,
                                PgfText *funname,
                                PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, funname);
        if (absfun == 0)
            return false;

        return (absfun->bytecode == 0);
    } PGF_API_END

    return false;
}

PGF_API
prob_t pgf_function_prob(PgfDB *db, PgfRevision revision,
                         PgfText *funname,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, funname);
        if (absfun == 0)
            return INFINITY;

        return absfun->prob;
    } PGF_API_END

    return INFINITY;
}

PGF_API
PgfText *pgf_concrete_name(PgfDB *db, PgfConcrRevision revision,
                           PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(revision);

        return textdup(&concr->name);
    } PGF_API_END

    return NULL;
}

PGF_API
PgfText *pgf_concrete_language_code(PgfDB *db, PgfConcrRevision revision,
                                    PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        size_t size = strlen("language");
        PgfText *language = (PgfText *) alloca(sizeof(PgfText)+size+1);
        language->size = size;
        strcpy((char*) &language->text, "language");

        ref<PgfFlag> flag =
            namespace_lookup(concr->cflags, language);
        if (flag != 0 &&
            ref<PgfLiteral>::get_tag(flag->value) == PgfLiteralStr::tag) {
            ref<PgfLiteralStr> lstr = ref<PgfLiteralStr>::untagged(flag->value);
            return textdup(&lstr->val);
        }
    } PGF_API_END

    return NULL;
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

PGF_API
PgfText *pgf_print_ident(PgfText *name)
{
    PgfPrinter printer(NULL,0,NULL);
    printer.efun(name);
    return printer.get_text();
}

PGF_API
PgfExpr pgf_read_expr(PgfText *input, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    PgfExpr res = parser.parse_expr();
    if (!parser.eof()) {
        if (res != 0)
            u->free_ref(res);
        return 0;
    }
    return res;
}

PGF_API
PgfExpr pgf_read_expr_ex(PgfText *input, const char **end_pos, PgfUnmarshaller *u)
{
    PgfExprParser parser(input, u);
    PgfExpr expr = parser.parse_expr();
    *end_pos = parser.get_token_pos();
    return expr;
}

PGF_API
prob_t pgf_expr_prob(PgfDB *db, PgfRevision revision,
                     PgfExpr e,
                     PgfMarshaller *m,
                     PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfExprProbEstimator estimator(pgf, m);
        m->match_expr(&estimator, e);
        return estimator.get_prob();
    } PGF_API_END

    return 0;
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
PgfText *pgf_print_context(size_t n_hypos, PgfTypeHypo *hypos,
                           PgfPrintContext *ctxt, int prio,
                           PgfMarshaller *m)
{
    PgfPrinter printer(ctxt,prio,m);
    for (size_t i = 0; i < n_hypos; i++) {
        if (i > 0)
            printer.puts(" ");
        printer.hypo(&hypos[i],4);
    }
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
PgfTypeHypo *pgf_read_context(PgfText *input, PgfUnmarshaller *u, size_t *n_hypos)
{
    PgfExprParser parser(input, u);
    PgfTypeHypo *res = parser.parse_context(n_hypos);
    if (!parser.eof()) {
        for (size_t i = 0; i < *n_hypos; i++) {
            free(res[i].cid);
            u->free_ref(res[i].type);
        }
        *n_hypos = (size_t) -1;
        return NULL;
    }
    return res;
}

PGF_API
PgfText *pgf_print_category_internal(object o)
{
    ref<PgfAbsCat> abscat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    printer.puts("cat ");
    printer.efun(&abscat->name);

    for (size_t i = 0; i < abscat->context->len; i++) {
        printer.puts(" ");

        PgfTypeHypo hypo;
        hypo.bind_type = abscat->context->data[i].bind_type;
        hypo.cid = abscat->context->data[i].cid;
        hypo.type = abscat->context->data[i].type.as_object();
        printer.hypo(&hypo,4);
    }

    printer.nprintf(32, " ; -- %g", abscat->prob);

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_start_cat_internal(PgfDB *db, PgfRevision revision, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfText *startcat = (PgfText *)
            alloca(sizeof(PgfText)+9);
        startcat->size = 8;
        strcpy(startcat->text, "startcat");

        ref<PgfFlag> flag =
            namespace_lookup(pgf->abstract.aflags, startcat);

        if (flag != 0) {
            PgfInternalMarshaller m;
            PgfPrinter printer(NULL,0,&m);
            printer.puts("startcat = ");
            m.match_lit(&printer, flag->value);
            return printer.get_text();
        }
    } PGF_API_END

    return NULL;
}

PGF_API
PgfText *pgf_print_function_internal(object o)
{
    ref<PgfAbsFun> absfun = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    printer.puts(absfun->bytecode != 0 ? "fun " : "data ");
    printer.efun(&absfun->name);
    printer.puts(" : ");
    m.match_type(&printer, absfun->type.as_object());
    printer.nprintf(32, " ; -- %g", absfun->prob);

    return printer.get_text();
}

PGF_API
void pgf_iter_lincats(PgfDB *db, PgfConcrRevision cnc_revision,
                      PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        namespace_iter(concr->lincats, itor, err);
    } PGF_API_END
}

PGF_API
void pgf_iter_lins(PgfDB *db, PgfConcrRevision cnc_revision,
                   PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        namespace_iter(concr->lins, itor, err);
    } PGF_API_END
}

PGF_INTERNAL bool
pgf_is_case_sensitive(ref<PgfConcr> concr)
{
    PgfText *case_sensitive = (PgfText *)
        alloca(sizeof(PgfText)+15);
    case_sensitive->size = 14;
    strcpy(case_sensitive->text, "case_sensitive");

    ref<PgfFlag> flag =
        namespace_lookup(concr->cflags, case_sensitive);
    if (flag != 0) {
        switch (ref<PgfLiteral>::get_tag(flag->value)) {
        case PgfLiteralStr::tag: {
			auto lstr = ref<PgfLiteralStr>::untagged(flag->value);
			if (lstr->val.size == 3 && strcmp(lstr->val.text, "off") == 0)
				return false;
		}
        }
	}
	return true;
}

class PGF_INTERNAL_DECL PgfMorphoScanner : public PgfPhraseScanner {
public:
    PgfMorphoScanner(PgfMorphoCallback* callback) {
        this->callback = callback;
    }

	virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)
    {
    }

	virtual void start_matches(PgfTextSpot *end, PgfExn* err)
    {
    }

    virtual void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)
    {
        ref<PgfText> field =
            *vector_elem(lin->lincat->fields, seq_index % lin->lincat->fields->len);
        callback->fn(callback, &lin->absfun->name, field, lin->lincat->abscat->prob+lin->absfun->prob, err);
    }

	virtual void end_matches(PgfTextSpot *end, PgfExn* err)
    {
    }

private:
    PgfMorphoCallback* callback;
};

PGF_API
void pgf_lookup_morpho(PgfDB *db, PgfConcrRevision cnc_revision,
                       PgfText *sentence,
                       PgfMorphoCallback* callback, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        bool case_sensitive = pgf_is_case_sensitive(concr);

        PgfMorphoScanner scanner(callback);
        phrasetable_lookup(concr->phrasetable,
                           sentence, case_sensitive,
                           &scanner, err);
    } PGF_API_END
}

class PGF_INTERNAL_DECL PgfCohortsScanner : public PgfPhraseScanner {
public:
    PgfCohortsScanner(PgfCohortsCallback* callback) {
        this->callback = callback;
    }

	virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)
    {
        match_start = end->pos;
    }

	virtual void start_matches(PgfTextSpot *end, PgfExn* err)
    {
    }

    virtual void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)
    {
        ref<PgfText> field =
            *vector_elem(lin->lincat->fields, seq_index % lin->lincat->fields->len);
        callback->morpho.fn(&callback->morpho, &lin->absfun->name, field, lin->lincat->abscat->prob+lin->absfun->prob, err);
    }

	virtual void end_matches(PgfTextSpot *end, PgfExn* err)
    {
        callback->fn(callback, match_start, end->pos, err);
    }

private:
    size_t match_start;
    PgfCohortsCallback* callback;
};

PGF_API
void pgf_lookup_cohorts(PgfDB *db, PgfConcrRevision cnc_revision,
                        PgfText *sentence,
                        PgfCohortsCallback* callback, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        bool case_sensitive = pgf_is_case_sensitive(concr);

        PgfCohortsScanner scanner(callback);
        phrasetable_lookup_cohorts(concr->phrasetable,
                                   sentence, case_sensitive,
                                   &scanner, err);
    } PGF_API_END
}

PGF_API
PgfPhrasetableIds *pgf_iter_sequences(PgfDB *db, PgfConcrRevision cnc_revision,
                                      PgfSequenceItor *itor,
                                      PgfMorphoCallback *callback,
                                      PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        PgfPhrasetableIds *seq_ids = new PgfPhrasetableIds();
        seq_ids->start(concr);

        phrasetable_iter(concr, concr->phrasetable, itor, callback, seq_ids, err);

        return seq_ids;
    } PGF_API_END

    return NULL;
}

PGF_API
void pgf_get_lincat_counts_internal(object o, size_t *counts)
{
    ref<PgfConcrLincat> lincat = o;
    counts[0] = lincat->fields->len;
    counts[1] = lincat->n_lindefs;
    counts[2] = lincat->res->len - lincat->n_lindefs;
}

PGF_API
PgfText *pgf_get_lincat_field_internal(object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;
    return &**vector_elem(lincat->fields, i);
}

PGF_API
size_t pgf_get_lin_get_prod_count(object o)
{
    ref<PgfConcrLin> lin = o;
    return lin->res->len;
}

PGF_API
PgfText *pgf_print_lindef_internal(PgfPhrasetableIds *seq_ids, object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    ref<PgfPResult> res = *vector_elem(lincat->res, i);
    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    printer.efun(&lincat->name);
    printer.puts("(");
    printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
    printer.puts(") -> ");
    printer.efun(&lincat->name);
    printer.puts("[String(0)] = [");

    size_t n_seqs = lincat->fields->len;
    for (size_t j = 0; j < n_seqs; j++) {
		if (j > 0)
			printer.puts(",");

		ref<PgfSequence> seq = *vector_elem(lincat->seqs, i*n_seqs + j);
		printer.seq_id(seq_ids, seq);
	}

	printer.puts("]");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_linref_internal(PgfPhrasetableIds *seq_ids, object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    ref<PgfPResult> res = *vector_elem(lincat->res, lincat->n_lindefs+i);
    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    printer.puts("String(0) -> ");
    printer.efun(&lincat->name);
    printer.puts("[");
    printer.efun(&lincat->name);
    printer.puts("(");
    printer.lparam(vector_elem(lincat->args, lincat->n_lindefs+i)->param);
    printer.puts(")] = [");

	size_t n_seqs = lincat->fields->len;
	ref<PgfSequence> seq = *vector_elem(lincat->seqs, lincat->n_lindefs*n_seqs+i);
	printer.seq_id(seq_ids, seq);

	printer.puts("]");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_lin_internal(PgfPhrasetableIds *seq_ids, object o, size_t i)
{
    ref<PgfConcrLin> lin = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    ref<PgfPResult> res = *vector_elem(lin->res, i);
    ref<PgfDTyp> ty = lin->absfun->type;

    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    printer.efun(&ty->name);
    printer.puts("(");
    printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
    printer.puts(") -> ");

    printer.efun(&lin->name);
    printer.puts("[");
    size_t n_args = lin->args->len / lin->res->len;
    for (size_t j = 0; j < n_args; j++) {
        if (j > 0)
            printer.puts(",");
        printer.parg(vector_elem(ty->hypos, j)->type,
                     vector_elem(lin->args, i*n_args + j));
    }
    printer.puts("] = [");

    size_t n_seqs = lin->seqs->len / lin->res->len;
    for (size_t j = 0; j < n_seqs; j++) {
		if (j > 0)
			printer.puts(",");

		ref<PgfSequence> seq = *vector_elem(lin->seqs, i*n_seqs + j);
		printer.seq_id(seq_ids, seq);
	}

	printer.puts("]");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_sequence_internal(size_t seq_id, object o)
{
    ref<PgfSequence> seq = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

	printer.nprintf(10,"S%zu = ", seq_id);
	printer.sequence(seq);

    return printer.get_text();
}

PGF_API
PgfText *pgf_sequence_get_text_internal(object o)
{
    ref<PgfSequence> seq = o;

    PgfPrinter printer(NULL,0,NULL);
    for (size_t i = 0; i < seq->syms.len; i++) {
        if (i > 0)
            printer.puts(" ");

        PgfSymbol sym = *vector_elem(&seq->syms, i);
        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolKS::tag: {
            auto sym_ks = ref<PgfSymbolKS>::untagged(sym);
            printer.puts(&sym_ks->token);
            break;
        }
        default:
            return NULL;
        }
    }

    return printer.get_text();
}

PGF_API_DECL
void pgf_release_phrasetable_ids(PgfPhrasetableIds *seq_ids)
{
	delete seq_ids;
}

PGF_API
void pgf_check_expr(PgfDB *db, PgfRevision revision,
                    PgfExpr* pe, PgfType ty,
                    PgfMarshaller *m, PgfUnmarshaller *u,
                    PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfTypechecker checker(pgf,m,u);
        *pe = m->match_expr(&checker, *pe);
    } PGF_API_END
}

PGF_API
PgfType pgf_infer_expr(PgfDB *db, PgfRevision revision,
                       PgfExpr* pe,
                       PgfMarshaller *m, PgfUnmarshaller *u,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfTypechecker checker(pgf,m,u);
        *pe = m->match_expr(&checker, *pe);
    } PGF_API_END

    PgfText *cat = (PgfText *) alloca(sizeof(PgfText)+2);
    cat->size = 1;
    cat->text[0] = 'S';
    cat->text[1] = 0;
    return u->dtyp(0,NULL,cat,0,NULL);
}

PGF_API
void pgf_check_type(PgfDB *db, PgfRevision revision,
                    PgfType* pty,
                    PgfMarshaller *m, PgfUnmarshaller *u,
                    PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfTypechecker checker(pgf,m,u);
        *pty = m->match_type(&checker, *pty);
    } PGF_API_END
}

PGF_API
PgfExpr pgf_generate_random(PgfDB *db, PgfRevision revision,
                            PgfConcrRevision *concr_revisions, size_t n_concr_revisions,
                            PgfType type, size_t depth,
                            uint64_t *seed, prob_t *prob,
                            PgfMarshaller *m, PgfUnmarshaller *u,
                            PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfRandomGenerator gen(pgf, depth, seed, m, u);
        for (size_t i = 0; i < n_concr_revisions; i++) {
            gen.addConcr(db->revision2concr(concr_revisions[i]));
        }
        PgfExpr expr = m->match_type(&gen, type);
        if (expr != 0) {
            *prob = gen.getProb();
            return expr;
        }
    } PGF_API_END

    return 0;
}

PGF_API
PgfExpr pgf_generate_random_from
                           (PgfDB *db, PgfRevision revision,
                            PgfConcrRevision *concr_revisions, size_t n_concr_revisions,
                            PgfExpr expr, size_t depth,
                            uint64_t *seed, prob_t *prob,
                            PgfMarshaller *m, PgfUnmarshaller *u,
                            PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfRandomGenerator gen(pgf, depth, seed, m, u);
        for (size_t i = 0; i < n_concr_revisions; i++) {
            gen.addConcr(db->revision2concr(concr_revisions[i]));
        }
        PgfExpr new_expr = m->match_expr(&gen, expr);
        if (new_expr != 0) {
            *prob = gen.getProb();
            return new_expr;
        }
    } PGF_API_END

    return 0;
}

PGF_API
PgfExprEnum *pgf_generate_all(PgfDB *db, PgfRevision revision,
                              PgfConcrRevision *concr_revisions, size_t n_concr_revisions,
                              PgfType type, size_t depth,
                              PgfMarshaller *m, PgfUnmarshaller *u,
                              PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfExhaustiveGenerator *gen = new PgfExhaustiveGenerator(pgf, depth, m, u);
        for (size_t i = 0; i < n_concr_revisions; i++) {
            gen->addConcr(db->revision2concr(concr_revisions[i]));
        }
        m->match_type(gen, type);
        return gen;
    } PGF_API_END

    return NULL;
}

PGF_API
PgfRevision pgf_start_transaction(PgfDB *db, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->get_active_revision();

        db->start_transaction();

        ref<PgfPGF> new_pgf = PgfDB::malloc<PgfPGF>();
        new_pgf->major_version = pgf->major_version;
        new_pgf->minor_version = pgf->minor_version;
        new_pgf->gflags = pgf->gflags;
        new_pgf->abstract.name = pgf->abstract.name;
        new_pgf->abstract.aflags = pgf->abstract.aflags;
        new_pgf->abstract.funs = pgf->abstract.funs;
        new_pgf->abstract.cats = pgf->abstract.cats;
        new_pgf->abstract.funs_by_cat = pgf->abstract.funs_by_cat;
        new_pgf->concretes = pgf->concretes;

        db->set_transaction_object(new_pgf.as_object());

        object rev = db->register_revision(new_pgf.tagged(), PgfDB::get_txn_id());

        PgfDB::free(pgf);

        db->ref_count++;
        return rev;
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_commit_transaction(PgfDB *db, PgfRevision revision,
                            PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> new_pgf = db->revision2pgf(revision);
        db->commit(new_pgf.as_object());
    } PGF_API_END
}

PGF_API
PgfRevision pgf_checkout_revision(PgfDB *db, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = db->get_active_revision();
        object rev = 0;
        if (pgf != 0) {
            rev = db->register_revision(pgf.tagged(), PgfDB::get_txn_id()-1);
            db->ref_count++;
        }
        return rev;
    } PGF_API_END

    return 0;
}

PGF_API
PgfText *pgf_create_function(PgfDB *db, PgfRevision revision,
                             PgfText *name_pattern,
                             PgfType ty, size_t arity, char *bytecode,
                             prob_t prob,
                             PgfMarshaller *m,
                             PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfNameAllocator<PgfAbsFun> nalloc(name_pattern);
        Namespace<PgfAbsFun> funs =
            nalloc.allocate(pgf->abstract.funs);
        if (funs == 0)
            throw pgf_error("A function with that name already exists");

        PgfText *name; ref<PgfAbsFun> absfun;
        nalloc.fetch_name_value(&name, &absfun);

        absfun->type  = m->match_type(&u, ty);
        absfun->arity = arity;
        absfun->bytecode = bytecode ? PgfDB::malloc<char>(0) : 0;
        absfun->prob = prob;

        pgf->abstract.funs = funs;

        PgfProbspace funs_by_cat =
            probspace_insert(pgf->abstract.funs_by_cat, absfun);
        pgf->abstract.funs_by_cat = funs_by_cat;

        return name;
    } PGF_API_END

    return NULL;
}

static
ref<PgfConcr> clone_concrete(ref<PgfPGF> pgf, ref<PgfConcr> concr)
{
    ref<PgfConcr> clone = concr;
    if (!current_db->is_transient_object(clone.as_object())) {
        clone = PgfDB::malloc<PgfConcr>(concr->name.size+1);
        clone->cflags = concr->cflags;
        clone->lins = concr->lins;
        clone->lincats = concr->lincats;
        clone->phrasetable = concr->phrasetable;
        clone->printnames = concr->printnames;
        memcpy(&clone->name, &concr->name, sizeof(PgfText)+concr->name.size+1);

        ref<PgfConcr> old_concr;
        Namespace<PgfConcr> concrs =
            namespace_replace(pgf->concretes, clone, &old_concr);
        pgf->concretes = concrs;

        PgfDB::free(concr, concr->name.size+1);
    }
    return clone;
}

static
void drop_lin(ref<PgfConcr> concr, PgfText *name)
{
    ref<PgfConcrLin> lin;
    Namespace<PgfConcrLin> lins =
        namespace_delete(concr->lins, name, &lin);
    if (lin != 0) {
        object container = lin.tagged();
        for (size_t i = 0; i < lin->seqs->len; i++) {
            ref<PgfSequence> seq = *vector_elem(lin->seqs, i);
            PgfPhrasetable phrasetable =
                phrasetable_delete(concr->phrasetable,container,i,seq);
            concr->phrasetable = phrasetable;
        }
        PgfConcrLin::release(lin);
    }
    concr->lins = lins;
}

PGF_API
void pgf_drop_function(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsFun> fun;
        Namespace<PgfAbsFun> funs =
            namespace_delete(pgf->abstract.funs, name, &fun);
        if (fun != 0) {
            PgfProbspace funs_by_cat =
                probspace_delete(pgf->abstract.funs_by_cat, fun);
            pgf->abstract.funs_by_cat = funs_by_cat;

            std::function<ref<PgfConcr>(ref<PgfConcr>)> f =
                            [name,pgf](ref<PgfConcr> concr){
                concr = clone_concrete(pgf, concr);
                drop_lin(concr,name);
                return concr;
            };
            namespace_map(pgf->concretes, f);

            PgfAbsFun::release(fun);
        }
        pgf->abstract.funs = funs;
    } PGF_API_END
}

PGF_API
void pgf_create_category(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         size_t n_hypos, PgfTypeHypo *context, prob_t prob,
                         PgfMarshaller *m,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        ref<PgfAbsCat> abscat = PgfDB::malloc<PgfAbsCat>(name->size+1);
        abscat->context = vector_new<PgfHypo>(n_hypos);
        abscat->prob    = prob;
        memcpy(&abscat->name, name, sizeof(PgfText)+name->size+1);

        for (size_t i = 0; i < n_hypos; i++) {
            vector_elem(abscat->context, i)->bind_type = context[i].bind_type;
            vector_elem(abscat->context, i)->cid = textdup_db(context[i].cid);
            vector_elem(abscat->context, i)->type = m->match_type(&u, context[i].type);
        }

        Namespace<PgfAbsCat> cats =
            namespace_insert(pgf->abstract.cats, abscat);
        if (cats == 0) {
            throw pgf_error("A category with that name already exists");
        }
        pgf->abstract.cats = cats;
    } PGF_API_END
}

struct PGF_INTERNAL_DECL PgfDropItor : PgfItor
{
    ref<PgfPGF> pgf;
    ref<PgfConcr> concrete;
    PgfText *name;
};

static
void iter_drop_cat_helper2(PgfItor *itor, PgfText *key, object value, PgfExn *err)
{
    ref<PgfConcr> concr = value;
    PgfText* name = ((PgfDropItor*) itor)->name;

    drop_lin(concr, name);
}

static
void iter_drop_cat_helper(PgfItor *itor, PgfText *key, object value, PgfExn *err)
{
    ref<PgfPGF> pgf = ((PgfDropItor*) itor)->pgf;

    PgfDropItor itor2;
    itor2.fn       = iter_drop_cat_helper2;
    itor2.pgf      = 0;
    itor2.concrete = 0;
    itor2.name     = key;
    namespace_iter(pgf->concretes, &itor2, err);

    ref<PgfAbsFun> fun;
    Namespace<PgfAbsFun> funs =
        namespace_delete(pgf->abstract.funs, key, &fun);
    fun = value;
    PgfAbsFun::release(fun);
    pgf->abstract.funs = funs;
}

PGF_API
void pgf_drop_category(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfAbsCat> cat;
        Namespace<PgfAbsCat> cats =
            namespace_delete(pgf->abstract.cats, name, &cat);
        if (cat != 0) {
            std::function<ref<PgfConcr>(ref<PgfConcr>)> f =
                                        [name,pgf](ref<PgfConcr> concr){
                concr = clone_concrete(pgf, concr);

                ref<PgfConcrLincat> lincat;
                Namespace<PgfConcrLincat> lincats =
                    namespace_delete(concr->lincats, name, &lincat);
                concr->lincats = lincats;

                return concr;
            };
            namespace_map(pgf->concretes, f);

            PgfDropItor itor;
            itor.fn       = iter_drop_cat_helper;
            itor.pgf      = pgf;
            itor.concrete = 0;
            itor.name     = name;
            PgfProbspace funs_by_cat =
                probspace_delete_by_cat(pgf->abstract.funs_by_cat, &cat->name,
                                        &itor, err);
            pgf->abstract.funs_by_cat = funs_by_cat;
            PgfAbsCat::release(cat);
        }
        pgf->abstract.cats = cats;
    } PGF_API_END
}

PGF_API
PgfConcrRevision pgf_create_concrete(PgfDB *db, PgfRevision revision,
                                     PgfText *name,
                                     PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfConcr> concr =
            namespace_lookup(pgf->concretes, name);
        if (concr != 0)
            throw pgf_error("The concrete syntax already exists");

        concr = PgfDB::malloc<PgfConcr>(name->size+1);
        concr->cflags = 0;
        concr->lins = 0;
        concr->lincats = 0;
        concr->phrasetable = 0;
        concr->printnames = 0;
        memcpy(&concr->name, name, sizeof(PgfText)+name->size+1);

        object rev = db->register_revision(concr.tagged(), PgfDB::get_txn_id());

        Namespace<PgfConcr> concrs =
            namespace_insert(pgf->concretes, concr);
        if (concrs == 0) {
            throw pgf_error("A concrete language with that name already exists");
        }
        pgf->concretes = concrs;

        db->ref_count++;
        return rev;
    } PGF_API_END
    return 0;
}

PGF_API
PgfConcrRevision pgf_clone_concrete(PgfDB *db, PgfRevision revision,
                                    PgfText *name,
                                    PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        
        ref<PgfConcr> concr =
            namespace_lookup(pgf->concretes, name);
        if (concr == 0)
            throw pgf_error("Unknown concrete syntax");

        concr = clone_concrete(pgf, concr);

        object rev = db->register_revision(concr.tagged(), PgfDB::get_txn_id());
        db->ref_count++;
        return rev;
    } PGF_API_END
    return 0;
}

PGF_API
void pgf_drop_concrete(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfConcr> concr;
        Namespace<PgfConcr> concrs =
            namespace_delete(pgf->concretes, name, &concr);
        if (concr != 0)
            PgfConcr::release(concr);
        pgf->concretes = concrs;
    } PGF_API_END
}

class PGF_INTERNAL PgfLinBuilder : public PgfLinBuilderIface
{
	ref<PgfConcr> concr;

    ref<Vector<PgfPArg>> args;
    ref<Vector<ref<PgfPResult>>> res;
    ref<Vector<ref<PgfSequence>>> seqs;

    object container; // what are we building?
    ref<PgfConcrLincat> container_lincat;

    size_t var_index;
    size_t arg_index;
    size_t res_index;
    size_t seq_index;
    size_t sym_index;
    size_t alt_index;

    size_t n_lindefs;
    size_t n_linrefs;

    ref<PgfSequence> seq;

    size_t pre_sym_index;

    const char *builder_error_msg =
        "Detected incorrect use of the linearization builder";

public:
    PgfLinBuilder(ref<PgfConcr> concr)
    {
		this->concr = concr;

        this->args = 0;
        this->res  = 0;
        this->seqs = 0;
        this->var_index = 0;
        this->arg_index = 0;
        this->res_index = 0;
        this->seq_index = 0;
        this->sym_index = (size_t) -1;
        this->alt_index = (size_t) -1;
        this->n_lindefs = 0;
        this->n_linrefs = 0;
        this->seq = 0;
        this->pre_sym_index = (size_t) -1;
    }

    ref<PgfConcrLincat> build(ref<PgfAbsCat> abscat,
                              size_t n_fields, PgfText **fields,
                              size_t n_lindefs, size_t n_linrefs,
                              PgfBuildLinIface *build, PgfExn *err)
    {
        size_t n_prods = n_lindefs+n_linrefs;
        this->args = vector_new<PgfPArg>(n_prods);
        this->res  = vector_new<ref<PgfPResult>>(n_prods);
        this->seqs = vector_new<ref<PgfSequence>>(n_lindefs*n_fields+n_linrefs);
        this->n_lindefs = n_lindefs;
        this->n_linrefs = n_linrefs;

        ref<PgfConcrLincat> lincat = PgfDB::malloc<PgfConcrLincat>(abscat->name.size+1);
        memcpy(&lincat->name, &abscat->name, sizeof(PgfText)+abscat->name.size+1);
        lincat->abscat = abscat;
        lincat->args = args;
        lincat->res  = res;
        lincat->seqs = seqs;
        lincat->n_lindefs = n_lindefs;

        ref<Vector<ref<PgfText>>> db_fields = vector_new<ref<PgfText>>(n_fields);
        for (size_t i = 0; i < n_fields; i++) {
            ref<PgfText> name = textdup_db(fields[i]);
            *vector_elem(db_fields, i) = name;
        }
        lincat->fields = db_fields;

        this->container = lincat.tagged();
        this->container_lincat = 0;

        build->build(this, err);
        if (err->type == PGF_EXN_NONE && res_index != res->len) {
            err->type = PGF_EXN_PGF_ERROR;
            err->msg  = builder_error_msg;
        }

        if (err->type != PGF_EXN_NONE) {
            return 0;
        }

        return lincat;
    }

    ref<PgfConcrLin> build(ref<PgfAbsFun> absfun, size_t n_prods,
                           PgfBuildLinIface *build, PgfExn *err)
    {
        ref<PgfConcrLincat> lincat =
            namespace_lookup(concr->lincats, &absfun->type->name);
        if (lincat == 0) {
            throw pgf_error("Missing linearization category");
        }

        this->args = vector_new<PgfPArg>(n_prods*absfun->type->hypos->len);
        this->res  = vector_new<ref<PgfPResult>>(n_prods);
        this->seqs = vector_new<ref<PgfSequence>>(n_prods*lincat->fields->len);
        this->n_lindefs = n_prods;

        ref<PgfConcrLin> lin = PgfDB::malloc<PgfConcrLin>(absfun->name.size+1);
        memcpy(&lin->name, &absfun->name, sizeof(PgfText)+absfun->name.size+1);
        lin->absfun = absfun;
        lin->lincat = lincat;
        lin->args = args;
        lin->res  = res;
        lin->seqs = seqs;

        this->container = lin.tagged();
        this->container_lincat = lincat;

        build->build(this, err);
        if (err->type == PGF_EXN_NONE && res_index != res->len) {
            err->type = PGF_EXN_PGF_ERROR;
            err->msg  = builder_error_msg;
        }

        if (err->type != PGF_EXN_NONE) {
            return 0;
        }

        return lin;
    }

    void start_production(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (res_index >= res->len)
                throw pgf_error(builder_error_msg);
            var_index = 0;
            *vector_elem(res, res_index) = 0;
        } PGF_API_END
    }

    void add_argument(size_t n_hypos, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (arg_index >= args->len)
                throw pgf_error(builder_error_msg);

            ref<PgfLParam> param = PgfDB::malloc<PgfLParam>(n_terms*2*sizeof(size_t));
            param->i0 = i0;
            param->n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                param->terms[i].factor = terms[2*i];
                param->terms[i].var    = terms[2*i+1];
            }

            ref<PgfPArg> parg = vector_elem(args, arg_index);
            parg->param = param;

            arg_index++;
        } PGF_API_END
    }

    void set_result(size_t n_vars, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (res_index >= res->len)
                throw pgf_error(builder_error_msg);

            ref<Vector<PgfVariableRange>> vars =
                (n_vars > 0) ? vector_new<PgfVariableRange>(n_vars)
                             : 0;

            ref<PgfPResult> res_elem = PgfDB::malloc<PgfPResult>(n_terms*2*sizeof(size_t));
            res_elem->vars = vars;
            res_elem->param.i0 = i0;
            res_elem->param.n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                res_elem->param.terms[i].factor = terms[2*i];
                res_elem->param.terms[i].var    = terms[2*i+1];
            }

            *vector_elem(res, res_index) = res_elem;
        } PGF_API_END
    }

    void add_variable(size_t var, size_t range, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (res_index >= res->len)
                throw pgf_error(builder_error_msg);

            ref<PgfPResult> res_elem =
                *vector_elem(res, res_index);

            if (res_elem->vars == 0 || var_index >= res_elem->vars->len)
                throw pgf_error(builder_error_msg);

            ref<PgfVariableRange> var_range =
                vector_elem(res_elem->vars, var_index);
            var_range->var   = var;
            var_range->range = range;

            var_index++;
        } PGF_API_END
    }

    void start_sequence(size_t n_syms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq_index >= seqs->len)
                throw pgf_error(builder_error_msg);

			seq = PgfDB::malloc<PgfSequence>(n_syms*sizeof(PgfSymbol));
            seq->syms.len  = n_syms;

            *vector_elem(seqs, seq_index) = seq;
            sym_index = 0;
        } PGF_API_END
    }

    void add_symcat(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolCat> symcat = PgfDB::malloc<PgfSymbolCat>(n_terms*2*sizeof(size_t));
            symcat->d  = d;
            symcat->r.i0 = i0;
            symcat->r.n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                symcat->r.terms[i].factor = terms[2*i];
                symcat->r.terms[i].var    = terms[2*i+1];
            }

            *vector_elem(&seq->syms, sym_index) = symcat.tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symlit(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolLit> symlit = PgfDB::malloc<PgfSymbolLit>(n_terms*2*sizeof(size_t));
            symlit->d  = d;
            symlit->r.i0 = i0;
            symlit->r.n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                symlit->r.terms[i].factor = terms[2*i];
                symlit->r.terms[i].var    = terms[2*i+1];
            }

            *vector_elem(&seq->syms, sym_index) = symlit.tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symvar(size_t d, size_t r, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolVar> symvar = PgfDB::malloc<PgfSymbolVar>();
            symvar->d = d;
            symvar->r = r;

            *vector_elem(&seq->syms, sym_index) = symvar.tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symks(PgfText *token, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolKS> symtok = PgfDB::malloc<PgfSymbolKS>(token->size+1);
            memcpy(&symtok->token, token, sizeof(PgfText)+token->size+1);

            *vector_elem(&seq->syms, sym_index) = symtok.tagged();
            sym_index++;
        } PGF_API_END
    }

    void start_symkp(size_t n_syms, size_t n_alts, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len || pre_sym_index != (size_t) -1)
                throw pgf_error(builder_error_msg);

			ref<PgfSequence> def = PgfDB::malloc<PgfSequence>(n_syms*sizeof(PgfSymbol));
            def->syms.len  = n_syms;

            ref<PgfSymbolKP> symkp = PgfDB::malloc<PgfSymbolKP>(n_alts*sizeof(PgfAlternative));
            symkp->default_form = def;
            symkp->alts.len = n_alts;

            *vector_elem(&seq->syms, sym_index) = symkp.tagged();

            pre_sym_index = sym_index;
            seq = def;
            sym_index = 0;
            alt_index = 0;
        } PGF_API_END
    }

    void start_symkp_alt(size_t n_syms, size_t n_prefs, PgfText **prefs, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (pre_sym_index == (size_t) -1)
                throw pgf_error(builder_error_msg);

			ref<PgfSequence> form = PgfDB::malloc<PgfSequence>(n_syms*sizeof(PgfSymbol));
            form->syms.len  = n_syms;

            ref<Vector<ref<PgfText>>> prefixes = vector_new<ref<PgfText>>(n_prefs);
            for (size_t i = 0; i < n_prefs; i++) {
                ref<PgfText> pref = textdup_db(prefs[i]);
                *vector_elem(prefixes, i) = pref;
            }

            seq = *vector_elem(seqs, seq_index);
            ref<PgfSymbolKP> symkp = ref<PgfSymbolKP>::untagged(*vector_elem(&seq->syms, pre_sym_index));
            ref<PgfAlternative> alt = ref<PgfAlternative>::from_ptr(&symkp->alts.data[alt_index]);

            alt->form     = form;
            alt->prefixes = prefixes;

            seq = form;
            sym_index = 0;
        } PGF_API_END
    }

    void end_symkp_alt(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (pre_sym_index == (size_t) -1)
                throw pgf_error(builder_error_msg);

            seq = *vector_elem(seqs, seq_index);
            ref<PgfSymbolKP> symkp = ref<PgfSymbolKP>::untagged(*vector_elem(&seq->syms, pre_sym_index));            
            if (alt_index >= symkp->alts.len)
                throw pgf_error(builder_error_msg);

            alt_index++;
        } PGF_API_END
    }

    void end_symkp(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (pre_sym_index == (size_t) -1)
                throw pgf_error(builder_error_msg);

            seq = *vector_elem(seqs, seq_index);
            sym_index = pre_sym_index+1;
            alt_index = 0;
            pre_sym_index = (size_t) -1;
        } PGF_API_END
    }

    void add_symbind(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolBIND>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symsoftbind(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolSOFTBIND>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symne(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolNE>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symsoftspace(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolSOFTSPACE>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symcapit(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolCAPIT>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    void add_symallcapit(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index == (size_t) -1 || sym_index >= seq->syms.len)
                throw pgf_error(builder_error_msg);

            *vector_elem(&seq->syms, sym_index) = ref<PgfSymbolALLCAPIT>(0).tagged();
            sym_index++;
        } PGF_API_END
    }

    object end_sequence(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return 0;

        ref<PgfPhrasetableEntry> entry = 0;

        PGF_API_BEGIN {
            if (seq == 0 || sym_index != seq->syms.len)
                throw pgf_error(builder_error_msg);

            PgfPhrasetable phrasetable =
				phrasetable_internalize(concr->phrasetable,
                                        seq, container_lincat, container, seq_index,
                                        &entry);
            concr->phrasetable = phrasetable;
            *vector_elem(seqs, seq_index) = entry->seq;

            sym_index = (size_t) -1;
            seq = 0;
            seq_index++;
        } PGF_API_END

        return entry.as_object();
    }

    void add_sequence_id(object seq_id, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (seq_index >= seqs->len)
                throw pgf_error(builder_error_msg);

            ref<PgfPhrasetableEntry> entry = seq_id;

            size_t len = entry->backrefs->len;
            ref<Vector<PgfSequenceBackref>> backrefs =
                vector_resize<PgfSequenceBackref>(entry->backrefs, len+1, PgfDB::get_txn_id());
            backrefs->data[len].container = container;
            backrefs->data[len].seq_index = seq_index;
            entry->backrefs = backrefs;

            *vector_elem(seqs, seq_index) = entry->seq;

            seq_index++;
        } PGF_API_END
	}

    void end_production(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            size_t n_args = (args->len/res->len);
            if (arg_index != (res_index+1)*n_args)
                throw pgf_error(builder_error_msg);

            if (*vector_elem(res, res_index) == 0)
                throw pgf_error(builder_error_msg);

            size_t n_seqs = ((seqs->len-n_linrefs)/(res->len-n_linrefs));
            size_t exp_index =
                     (res_index < n_lindefs) ? (res_index+1)*n_seqs
                                             : n_seqs * n_lindefs + (res_index-n_lindefs+1) ;
            if (seq_index != exp_index)
                throw pgf_error(builder_error_msg);

            res_index++;
        } PGF_API_END
    }
};


PGF_API
void pgf_create_lincat(PgfDB *db,
                       PgfRevision revision, PgfConcrRevision cnc_revision,
                       PgfText *name, size_t n_fields, PgfText **fields,
                       size_t n_lindefs, size_t n_linrefs, PgfBuildLinIface *build,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        ref<PgfAbsCat> abscat =
            namespace_lookup(pgf->abstract.cats, name);
        if (abscat == 0) {
            throw pgf_error("There is no corresponding category in the abstract syntax");
        }

        ref<PgfConcrLincat> lincat =
            PgfLinBuilder(concr).build(abscat, n_fields, fields, n_lindefs, n_linrefs, build, err);
        if (lincat != 0) {
            Namespace<PgfConcrLincat> lincats =
                namespace_insert(concr->lincats, lincat);
            if (lincats == 0) {
                throw pgf_error("A linearization category with that name already exists");
            }
            concr->lincats = lincats;
        }
    } PGF_API_END
}

PGF_API
void pgf_drop_lincat(PgfDB *db,
                     PgfRevision revision,PgfConcrRevision cnc_revision,
                     PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF>   pgf   = db->revision2pgf(revision);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        ref<PgfConcrLincat> lincat;
        Namespace<PgfConcrLincat> lincats =
            namespace_delete(concr->lincats, name, &lincat);
        if (lincat != 0) {
            // The lincat was indeed in the concrete syntax.

            // Remove the linearizations of all functions that
            // depend on it.
            std::function<bool(ref<PgfAbsFun>)> f =
                [concr](ref<PgfAbsFun> fun) {
                    drop_lin(concr, &fun->name);
                    return true;
                };
            probspace_iter(pgf->abstract.funs_by_cat, name, f, true);

            // Remove the sequences comprizing the lindef and linref
            object container = lincat.tagged();
            PgfPhrasetable phrasetable = concr->phrasetable;
            for (size_t i = 0; i < lincat->seqs->len; i++) {
                ref<PgfSequence> seq = *vector_elem(lincat->seqs, i);
                phrasetable =
                    phrasetable_delete(phrasetable,container,i,seq);
            }
            concr->phrasetable = phrasetable;

            // Finaly remove the lincat object itself.
            PgfConcrLincat::release(lincat);
        }
        concr->lincats = lincats;
    } PGF_API_END
}

PGF_API
void pgf_create_lin(PgfDB *db,
                    PgfRevision revision, PgfConcrRevision cnc_revision,
                    PgfText *name, size_t n_prods,
                    PgfBuildLinIface *build,
                    PgfExn *err)
{
    if (n_prods == 0)
        return;

    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, name);
        if (absfun == 0) {
            throw pgf_error("There is no corresponding function in the abstract syntax");
        }

        ref<PgfConcrLin> lin =
            PgfLinBuilder(concr).build(absfun, n_prods, build, err);
        if (lin != 0) {
            Namespace<PgfConcrLin> lins =
                namespace_insert(concr->lins, lin);
            if (lins == 0) {
                throw pgf_error("A linearization function with that name already exists");
            }
            concr->lins = lins;
        }
    } PGF_API_END
}

PGF_API
void pgf_alter_lin(PgfDB *db,
                   PgfRevision revision, PgfConcrRevision cnc_revision,
                   PgfText *name, size_t n_prods,
                   PgfBuildLinIface *build,
                   PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, name);
        if (absfun == 0) {
            throw pgf_error("There is no corresponding function in the abstract syntax");
        }

        ref<PgfConcrLin> lin =
            PgfLinBuilder(concr).build(absfun, n_prods, build, err);
        if (lin != 0) {
            ref<PgfConcrLin> old_lin;
            Namespace<PgfConcrLin> lins =
                namespace_replace(concr->lins, lin, &old_lin);
            concr->lins = lins;
            if (old_lin != 0) {
                object container = old_lin.tagged();
                PgfPhrasetable phrasetable = concr->phrasetable;
                for (size_t i = 0; i < old_lin->seqs->len; i++) {
                    ref<PgfSequence> seq = *vector_elem(old_lin->seqs, i);
                    phrasetable =
                        phrasetable_delete(phrasetable,container,i,seq);
                }
                concr->phrasetable = phrasetable;
                PgfConcrLin::release(old_lin);
            }
        }
    } PGF_API_END
}

PGF_API
void pgf_drop_lin(PgfDB *db,
                  PgfRevision revision, PgfConcrRevision cnc_revision,
                  PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);
        ref<PgfConcr> concr = db->revision2concr(cnc_revision);

        drop_lin(concr, name);
    } PGF_API_END
}

PGF_API
int pgf_has_linearization(PgfDB *db, PgfConcrRevision revision,
                          PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        ref<PgfConcrLin> lin =
            namespace_lookup(concr->lins, name);

        return (lin != 0);
    } PGF_API_END

    return 0;
}

PGF_API
PgfText **pgf_category_fields(PgfDB *db, PgfConcrRevision revision,
                              PgfText *name, size_t *p_n_fields,
                              PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        ref<PgfConcrLincat> lincat =
            namespace_lookup(concr->lincats, name);

        if (lincat == 0) {
			*p_n_fields = 0;
			return NULL;
		} else {
			size_t n_fields = lincat->fields->len;
			PgfText **fields = (PgfText **) malloc(sizeof(PgfText*)*n_fields);
			if (fields == 0)
				throw pgf_systemerror(ENOMEM);
			for (size_t i = 0; i < n_fields; i++) {
				fields[i] = textdup(*vector_elem(lincat->fields, i));
			}
			*p_n_fields = n_fields;
			return fields;
		}
    } PGF_API_END

    return NULL;
}

PGF_API
PgfText *pgf_linearize(PgfDB *db, PgfConcrRevision revision,
                       PgfExpr expr, PgfPrintContext *ctxt,
                       PgfMarshaller *m,
                       PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        if (linearizer.resolve()) {
            linearizer.linearize(&out, 0);
            return out.get_text();
        }
    } PGF_API_END

    return NULL;
}

PGF_API
PgfText **pgf_linearize_all(PgfDB *db, PgfConcrRevision revision,
                            PgfExpr expr, PgfPrintContext *ctxt,
                            PgfMarshaller *m, size_t *n_variants,
                            PgfExn* err)
{
    *n_variants = 0;
    PgfText **variants = NULL;

    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);

        while (linearizer.resolve()) {
            linearizer.linearize(&out, 0);
            PgfText *text = out.get_text();
            if (text != NULL) {
                variants = (PgfText **) realloc(variants, ((*n_variants)+1)*sizeof(PgfText **));
                variants[(*n_variants)++] = text;
            }
        }

        return variants;
    } PGF_API_END

    free(variants);
    return NULL;
}

PGF_API
PgfText **pgf_tabular_linearize(PgfDB *db, PgfConcrRevision revision,
                                PgfExpr expr, PgfPrintContext *ctxt,
                                PgfMarshaller *m, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(false);
        if (linearizer.resolve()) {
            ref<PgfConcrLincat> lincat = linearizer.get_lincat();
            if (lincat != 0) {
                PgfText **res = (PgfText **)
                    malloc((lincat->fields->len+1)*2*sizeof(PgfText*));
                if (res == NULL)
                    throw pgf_systemerror(ENOMEM);
                size_t pos = 0;
                for (size_t i = 0; i < lincat->fields->len; i++) {
                    linearizer.linearize(&out, i);

                    PgfText *text = out.get_text();
                    if (text != NULL) {
                        res[pos++] = textdup(&**vector_elem(lincat->fields,i));
                        res[pos++] = text;
                    }
                }
                res[pos++] = NULL;
                res[pos++] = NULL;
                return res;
            }
        }
    } PGF_API_END

    return NULL;
}

PGF_API
PgfText **pgf_tabular_linearize_all(PgfDB *db, PgfConcrRevision revision,
                                    PgfExpr expr, PgfPrintContext *ctxt,
                                    PgfMarshaller *m, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(false);

        size_t pos = 0;
        PgfText **res = NULL;
        while (linearizer.resolve()) {
            ref<PgfConcrLincat> lincat = linearizer.get_lincat();
            res = (PgfText **)
                realloc(res, (pos+(lincat->fields->len+1)*2)*sizeof(PgfText*));
            for (size_t i = 0; i < lincat->fields->len; i++) {
                linearizer.linearize(&out, i);

                PgfText *text = out.get_text();
                if (text != NULL) {
                    res[pos++] = textdup(&**vector_elem(lincat->fields, i));
                    res[pos++] = text;
                }
            }
            res[pos++] = NULL;
        }
        res[pos++] = NULL;
        return res;
    } PGF_API_END

    return NULL;
}

PGF_API
void pgf_bracketed_linearize(PgfDB *db, PgfConcrRevision revision,
                             PgfExpr expr, PgfPrintContext *ctxt,
                             PgfMarshaller *m,
                             PgfLinearizationOutputIface *out,
                             PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        if (linearizer.resolve()) {
            linearizer.linearize(out, 0);
        }
    } PGF_API_END
}

PGF_API
void pgf_bracketed_linearize_all(PgfDB *db, PgfConcrRevision revision,
                                 PgfExpr expr, PgfPrintContext *ctxt,
                                 PgfMarshaller *m,
                                 PgfLinearizationOutputIface *out,
                                 PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        while (linearizer.resolve()) {
            linearizer.linearize(out, 0);
            out->flush();
        }
    } PGF_API_END
}

struct PGF_INTERNAL_DECL PgfLincatUnmarshaller : PgfUnmarshaller {
    PgfLincatUnmarshaller(ref<PgfConcr> concr) {
        this->concr  = concr;
        this->lincat = 0;
    }

    virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body) { return 0; }
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg) { return 0; }
    virtual PgfExpr elit(PgfLiteral lit) { return 0; }
    virtual PgfExpr emeta(PgfMetaId meta) { return 0; }
    virtual PgfExpr efun(PgfText *name) { return 0; }
    virtual PgfExpr evar(int index) { return 0; }
    virtual PgfExpr etyped(PgfExpr expr, PgfType typ) { return 0; }
    virtual PgfExpr eimplarg(PgfExpr expr) { return 0; }
    virtual PgfLiteral lint(size_t size, uintmax_t *v) { return 0; }
    virtual PgfLiteral lflt(double v) { return 0; }
    virtual PgfLiteral lstr(PgfText *v) { return 0; }
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs) {
        lincat =
            namespace_lookup(concr->lincats, cat);
        return 0;
    }
    virtual void free_ref(object x) {};

    ref<PgfConcr> concr;
    ref<PgfConcrLincat> lincat;
};

PGF_API
PgfExprEnum *pgf_parse(PgfDB *db, PgfConcrRevision revision,
                       PgfType ty, PgfMarshaller *m, PgfUnmarshaller *u,
                       PgfText *sentence,
                       PgfExn * err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        bool case_sensitive = pgf_is_case_sensitive(concr);

        PgfLincatUnmarshaller lincat_u(concr);
        m->match_type(&lincat_u, ty);
        if (lincat_u.lincat == 0)
            return 0;

        PgfParser *parser = new PgfParser(concr, lincat_u.lincat, sentence, m, u);
        phrasetable_lookup_cohorts(concr->phrasetable,
                                   sentence, case_sensitive,
                                   parser, err);
        parser->prepare();
        return parser;
    } PGF_API_END

    return NULL;
}

PGF_API
void pgf_free_expr_enum(PgfExprEnum *en)
{
    delete en;
}

PGF_API
PgfText *pgf_get_printname(PgfDB *db, PgfConcrRevision revision,
                           PgfText *fun, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);
        PgfText *printname = namespace_lookup(concr->printnames, fun)->printname;
        return textdup(printname);
    } PGF_API_END

    return NULL;
}

PGF_API
void pgf_set_printname(PgfDB *db, PgfConcrRevision revision,
                       PgfText *fun, PgfText *name, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        ref<PgfConcrPrintname> printname = PgfDB::malloc<PgfConcrPrintname>(fun->size+1);
        memcpy(&printname->name, fun, sizeof(PgfText)+fun->size+1);
        printname->printname = textdup_db(name);

        ref<PgfConcrPrintname> old_printname;
        Namespace<PgfConcrPrintname> printnames =
            namespace_replace(concr->printnames, printname, &old_printname);
        if (old_printname != 0) {
            PgfConcrPrintname::release(old_printname);
        }
        concr->printnames = printnames;
    } PGF_API_END
}

PGF_API
PgfLiteral pgf_get_global_flag(PgfDB *db, PgfRevision revision,
                               PgfText *name,
                               PgfUnmarshaller *u,
                               PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfFlag> flag =
            namespace_lookup(pgf->gflags, name);
        if (flag != 0) {
            return PgfDBMarshaller().match_lit(u, flag->value);
        }
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_set_global_flag(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfLiteral value,
                         PgfMarshaller *m,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;

        ref<PgfFlag> old_flag;
        Namespace<PgfFlag> gflags =
            namespace_replace(pgf->gflags, flag, &old_flag);
        if (old_flag != 0) {
            PgfFlag::release(old_flag);
        }
        pgf->gflags = gflags;
    } PGF_API_END
}

PGF_API
PgfLiteral pgf_get_abstract_flag(PgfDB *db, PgfRevision revision,
                                 PgfText *name,
                                 PgfUnmarshaller *u,
                                 PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfFlag> flag =
            namespace_lookup(pgf->abstract.aflags, name);
        if (flag != 0) {
            return PgfDBMarshaller().match_lit(u, flag->value);
        }
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_set_abstract_flag(PgfDB *db, PgfRevision revision,
                           PgfText *name,
                           PgfLiteral value,
                           PgfMarshaller *m,
                           PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;

        ref<PgfFlag> old_flag;
        Namespace<PgfFlag> aflags =
            namespace_replace(pgf->abstract.aflags, flag, &old_flag);
        if (old_flag != 0) {
            PgfFlag::release(old_flag);
        }
        pgf->abstract.aflags = aflags;
    } PGF_API_END
}

PGF_API
PgfLiteral pgf_get_concrete_flag(PgfDB *db, PgfConcrRevision revision,
                                 PgfText *name,
                                 PgfUnmarshaller *u,
                                 PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        ref<PgfFlag> flag =
            namespace_lookup(concr->cflags, name);
        if (flag != 0) {
            return PgfDBMarshaller().match_lit(u, flag->value);
        }
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_set_concrete_flag(PgfDB *db, PgfConcrRevision revision,
                           PgfText *name,
                           PgfLiteral value,
                           PgfMarshaller *m,
                           PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfConcr> concr = db->revision2concr(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;

        ref<PgfFlag> old_flag;
        Namespace<PgfFlag> cflags =
            namespace_replace(concr->cflags, flag, &old_flag);
        if (old_flag != 0) {
            PgfFlag::release(old_flag);
        }
        concr->cflags = cflags;
    } PGF_API_END
}

PGF_API PgfText *
pgf_graphviz_abstract_tree(PgfDB *db, PgfRevision revision,
                           PgfExpr expr, PgfMarshaller *m,
                           PgfGraphvizOptions* opts,
                           PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = db->revision2pgf(revision);

        PgfAbstractGraphvizOutput out(&pgf->abstract, opts, m);
        return out.generate_graphviz(expr);
    } PGF_API_END

    return NULL;
}

PGF_API PgfText *
pgf_graphviz_parse_tree(PgfDB *db, PgfConcrRevision revision,
                        PgfExpr expr, PgfPrintContext *ctxt,
                        PgfMarshaller *m,
                        PgfGraphvizOptions* opts,
                        PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        PgfLinearizationGraphvizOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        if (linearizer.resolve()) {
            linearizer.linearize(&out, 0);
            return out.generate_graphviz(opts);
        }
    } PGF_API_END

    return NULL;
}

PGF_API PgfText *
pgf_graphviz_word_alignment(PgfDB *db, PgfConcrRevision* revisions, size_t n_revisions,
                            PgfExpr expr, PgfPrintContext *ctxt,
                            PgfMarshaller *m,
                            PgfGraphvizOptions* opts,
                            PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        PgfPrinter printer(NULL, 0, NULL);
        
        printer.puts("digraph {\n");
        printer.puts("rankdir=LR ;\n");
        printer.puts("node [shape = record");
        if (opts->leafFont != NULL && *opts->leafFont)
            printer.nprintf(40, ", fontname = \"%s\"", opts->leafFont);
        if (opts->leafColor != NULL && *opts->leafColor)
            printer.nprintf(40, ", fontcolor = \"%s\"", opts->leafColor);
        printer.puts("] ;\n\n");
        if (opts->leafEdgeStyle != NULL && *opts->leafEdgeStyle)
            printer.nprintf(40, "edge [style = %s];\n", opts->leafEdgeStyle);
        printer.puts("\n");

        size_t last_n_phrases = 0;
        PgfAlignmentPhrase **last_phrases = NULL;
        for (size_t i = 0; i < n_revisions; i++) {
            ref<PgfConcr> concr = db->revision2concr(revisions[i]);

            PgfAlignerOutput out;
            PgfLinearizer linearizer(ctxt, concr, m);
            m->match_expr(&linearizer, expr);
            linearizer.reverse_and_label(true);
            if (linearizer.resolve()) {
                linearizer.linearize(&out, 0);
                out.flush();

                printer.nprintf(40, "  struct%zu[label=\"", i);

                size_t n_phrases;
                PgfAlignmentPhrase **phrases = 
                    out.get_phrases(&n_phrases);

                for (size_t j = 0; j < n_phrases; j++) {
                    PgfAlignmentPhrase* phrase = phrases[j];
                    if (j > 0)
                        printer.puts(" | ");
                    printer.nprintf(16, "<n%zu> ", j);
                    printer.puts(phrase->phrase);
                }

                printer.puts("\"] ;\n");

                if (last_phrases != NULL) {
                    for (size_t j = 0; j < n_phrases; j++) {
                        PgfAlignmentPhrase* phrase = phrases[j];

                        for (size_t k = 0; k < phrase->n_fids; k++) {
                            int fid = phrase->fids[k];

                            for (size_t l = 0; l < last_n_phrases; l++) {
                                PgfAlignmentPhrase* last_phrase = last_phrases[l];

                                for (size_t r = 0; r < last_phrase->n_fids; r++) {
                                    int last_fid = last_phrase->fids[r];
                                    if (fid == last_fid) {
                                        printer.nprintf(50, "struct%zu:n%zu:e -> struct%zu:n%zu:w ;\n",i-1,l,i,j);
                                    }
                                }
                            }
                        }
                    }
                }

                PgfAlignerOutput::free_phrases(last_phrases, last_n_phrases);

                last_n_phrases = n_phrases;
                last_phrases = phrases;
            }
        }

        PgfAlignerOutput::free_phrases(last_phrases, last_n_phrases);

        printer.puts("}");

        return printer.get_text();
    } PGF_API_END

    return NULL;
}

PGF_API
PgfAlignmentPhrase **
pgf_align_words(PgfDB *db, PgfConcrRevision revision,
                PgfExpr expr, PgfPrintContext *ctxt,
                PgfMarshaller *m,
                size_t *n_phrases /* out */,
                PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = db->revision2concr(revision);

        PgfAlignerOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        if (linearizer.resolve()) {
            linearizer.linearize(&out, 0);
            out.flush();
            return out.get_phrases(n_phrases);
        }
    } PGF_API_END

    return NULL;
}
