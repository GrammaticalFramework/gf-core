#include <fcntl.h>
#include <math.h>
#include <errno.h>
#ifdef _WIN32
#include <sys\stat.h>
#endif

#include "data.h"
#include "reader.h"
#include "writer.h"
#include "printer.h"
#include "typechecker.h"
#include "linearizer.h"
#include "graphviz.h"

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

PGF_INTERNAL size_t master_size = 6;
PGF_INTERNAL char master_text[] = {'m','a','s','t','e','r',0};

PGF_API
PgfDB *pgf_read_pgf(const char* fpath,
                    PgfRevision *revision,
                    PgfExn* err)
{
    PgfDB *db = NULL;
    FILE *in = NULL;

    PGF_API_BEGIN {
        db = new PgfDB(NULL, 0, 0);
        in = fopen(fpath, "rb");
        if (!in) {
            throw pgf_systemerror(errno, fpath);
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(in);
            ref<PgfPGF> pgf = rdr.read_pgf();

            PgfDB::set_revision(pgf);
            *revision = pgf.as_object();
        }

        db->ref_count++;
        return db;
    } PGF_API_END

end:
    if (in != NULL)
        fclose(in);

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
    FILE *in = NULL;

    PGF_API_BEGIN {
        db = new PgfDB(ngf_path, O_CREAT | O_EXCL | O_RDWR,
#ifndef _WIN32
         S_IRUSR | S_IWUSR
#else
         _S_IREAD | _S_IWRITE
#endif
         );

        in = fopen(pgf_path, "rb");
        if (!in) {
            throw pgf_systemerror(errno, pgf_path);
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(in);
            ref<PgfPGF> pgf = rdr.read_pgf();

            PgfDB::set_revision(pgf);
            *revision = pgf.as_object();

            PgfDB::sync();
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

    bool is_new = false;
    PGF_API_BEGIN {
        db = new PgfDB(fpath, O_RDWR, 0);

        PgfText *master = (PgfText *)alloca(sizeof(PgfText)+master_size+1);
        master->size = master_size;
        memcpy(&master->text, master_text, master_size+1);

        {
            DB_scope scope(db, WRITER_SCOPE);

            ref<PgfPGF> pgf = PgfDB::get_revision(master);
            Node<PgfPGF>::add_value_ref(pgf);
            *revision = pgf.as_object();
        }

        db->ref_count++;
        return db;
    } PGF_API_END

    if (db != NULL) {
        delete db;
        if (is_new)
            remove(fpath);
    }

    return NULL;
}

PGF_API
PgfDB *pgf_new_ngf(PgfText *abstract_name,
                   const char *fpath,
                   PgfRevision *revision,
                   PgfExn* err)
{
    PgfDB *db = NULL;

    PGF_API_BEGIN {
        db = new PgfDB(fpath, O_CREAT | O_EXCL | O_RDWR,
#ifndef _WIN32
         S_IRUSR | S_IWUSR
#else
         0
#endif
);

        {
            DB_scope scope(db, WRITER_SCOPE);

            ref<PgfPGF> pgf = PgfDB::malloc<PgfPGF>(master_size+1);
            pgf->ref_count = 1;
            pgf->major_version = PGF_MAJOR_VERSION;
            pgf->minor_version = PGF_MINOR_VERSION;
            pgf->gflags = 0;
            pgf->abstract.name = PgfDB::malloc<PgfText>(abstract_name->size+1);
            memcpy(&(*pgf->abstract.name), abstract_name, sizeof(PgfText)+abstract_name->size+1);
            pgf->abstract.aflags = 0;
            pgf->abstract.funs = 0;
            pgf->abstract.cats = 0;
            pgf->concretes = 0;
            pgf->prev = 0;
            pgf->next = 0;
            pgf->name.size = master_size;
            memcpy(&pgf->name.text, master_text, master_size+1);
            PgfDB::set_revision(pgf);
            *revision = pgf.as_object();

            PgfDB::sync();
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
void pgf_write_pgf(const char* fpath,
                   PgfDB *db, PgfRevision revision,
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
            ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

            PgfWriter wtr(out);
            wtr.write_pgf(pgf);
        }
    } PGF_API_END

end:
    if (out != NULL)
        fclose(out);
}

PGF_API_DECL
void pgf_free_revision(PgfDB *db, PgfRevision revision)
{
    try {
        DB_scope scope(db, WRITER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        if (pgf->ref_count == 1 && PgfDB::is_persistant_revision(pgf)) {
            // Someone is trying to release the last reference count
            // to a persistant revision. Most likely this is an
            // error in the reference counting for one of the clients.
            // The best that we can do is to ignore the request.
            return;
        }

        if (!(--pgf->ref_count)) {
            PgfDB::unlink_transient_revision(pgf);
            PgfPGF::release(pgf);
            PgfDB::free(pgf);
        }

        db->ref_count--;
    } catch (std::runtime_error& e) {
        // silently ignore and hope for the best
    }

    if (!db->ref_count)
        delete db;
}

PGF_API_DECL
void pgf_free_concr_revision(PgfDB *db, PgfConcrRevision revision)
{
    try {
        DB_scope scope(db, WRITER_SCOPE);
        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        if (!(--concr->ref_count_ex)) {
            PgfDB::unlink_transient_revision(concr);
        }

        if (!(--concr->ref_count)) {
            PgfConcr::release(concr);
            PgfDB::free(concr);
        }

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        namespace_iter(pgf->abstract.cats, itor, err);
    } PGF_API_END
}

struct PgfItorConcrHelper : PgfItor
{
    PgfDB *db;
    PgfItor *itor;
};

static
void iter_concretes_helper(PgfItor *itor, PgfText *key, object value,
		                   PgfExn *err)
{
    PgfItorConcrHelper* helper = (PgfItorConcrHelper*) itor;
    ref<PgfConcr> concr = value;
    concr->ref_count++;
    concr->ref_count_ex++;
    helper->db->ref_count++;
    helper->itor->fn(helper->itor, key, value, err);
}

PGF_API
void pgf_iter_concretes(PgfDB *db, PgfRevision revision,
                        PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfItorConcrHelper helper;
        helper.fn   = iter_concretes_helper;
        helper.db   = db;
        helper.itor = itor;

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        pgf_exn_clear(err);
        namespace_iter(pgf->abstract.funs, itor, err);
    } PGF_API_END
}

struct PgfItorCatHelper : PgfItor
{
    PgfText *cat;
    PgfItor *itor;
};

static
void iter_by_cat_helper(PgfItor *itor, PgfText *key, object value,
		                PgfExn *err)
{
    PgfItorCatHelper* helper = (PgfItorCatHelper*) itor;
    ref<PgfAbsFun> absfun = value;
    if (textcmp(helper->cat, &absfun->type->name) == 0)
        helper->itor->fn(helper->itor, key, value, err);
}

PGF_API
void pgf_iter_functions_by_cat(PgfDB *db, PgfRevision revision,
                               PgfText *cat, PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfItorCatHelper helper;
        helper.fn   = iter_by_cat_helper;
        helper.cat  = cat;
        helper.itor = itor;

        namespace_iter(pgf->abstract.funs, &helper, err);
    } PGF_API_END
}

PGF_API
PgfType pgf_function_type(PgfDB *db, PgfRevision revision,
                          PgfText *funname, PgfUnmarshaller *u,
                          PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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
        ref<PgfConcr> concr = PgfDB::revision2concr(cnc_revision);

        namespace_iter(concr->lincats, itor, err);
    } PGF_API_END
}

PGF_API
void pgf_iter_lins(PgfDB *db, PgfConcrRevision cnc_revision,
                   PgfItor *itor, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfConcr> concr = PgfDB::revision2concr(cnc_revision);

        namespace_iter(concr->lins, itor, err);
    } PGF_API_END
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
    return &(**vector_elem(lincat->fields, i));
}

PGF_API
void pgf_get_lin_counts_internal(object o, size_t *counts)
{
    ref<PgfConcrLin> lin = o;
    counts[0] = lin->res->len;
    counts[1] = lin->seqs->len / lin->res->len;
}

PGF_API
PgfText *pgf_print_lindef_sig_internal(object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    printer.efun(&lincat->name);
    printer.puts(" : ");

    ref<PgfPResult> res = *vector_elem(lincat->res, i);

    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    printer.puts(" String(0) -> ");

    printer.efun(&lincat->name);
    printer.puts("(");
    printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
    printer.puts(")");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_lindef_seq_internal(object o, size_t i, size_t j)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    size_t n_seqs = lincat->fields->len;
    ref<Vector<PgfSymbol>> syms = *vector_elem(lincat->seqs, i*n_seqs + j);

    printer.symbols(syms);

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_linref_sig_internal(object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    printer.efun(&lincat->name);
    printer.puts(" : ");

    ref<PgfPResult> res = *vector_elem(lincat->res, lincat->n_lindefs+i);

    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    printer.efun(&lincat->name);
    printer.puts("(");
    printer.lparam(vector_elem(lincat->args, lincat->n_lindefs+i)->param);
    printer.puts(") -> String(0)");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_linref_seq_internal(object o, size_t i)
{
    ref<PgfConcrLincat> lincat = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    size_t n_seqs = lincat->fields->len;
    ref<Vector<PgfSymbol>> syms = *vector_elem(lincat->seqs, lincat->n_lindefs*n_seqs+i);

    printer.symbols(syms);

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_lin_sig_internal(object o, size_t i)
{
    ref<PgfConcrLin> lin = o;
    ref<PgfDTyp> ty = lin->absfun->type;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    printer.efun(&lin->name);
    printer.puts(" : ");

    ref<PgfPResult> res = *vector_elem(lin->res, i);

    if (res->vars != 0) {
        printer.lvar_ranges(res->vars);
        printer.puts(" . ");
    }

    size_t n_args = lin->args->len / lin->res->len;
    for (size_t j = 0; j < n_args; j++) {
        if (j > 0)
            printer.puts(" * ");

        printer.parg(vector_elem(ty->hypos, j)->type,
                     vector_elem(lin->args, i*n_args + j));
    }

    if (n_args > 0)
        printer.puts(" -> ");

    printer.efun(&ty->name);
    printer.puts("(");
    printer.lparam(ref<PgfLParam>::from_ptr(&res->param));
    printer.puts(")");

    return printer.get_text();
}

PGF_API
PgfText *pgf_print_lin_seq_internal(object o, size_t i, size_t j)
{
    ref<PgfConcrLin> lin = o;

    PgfInternalMarshaller m;
    PgfPrinter printer(NULL,0,&m);

    size_t n_seqs = lin->seqs->len / lin->res->len;
    ref<Vector<PgfSymbol>> syms = *vector_elem(lin->seqs, i*n_seqs + j);

    printer.symbols(syms);

    return printer.get_text();
}

PGF_API
void pgf_check_expr(PgfDB *db, PgfRevision revision,
                    PgfExpr* pe, PgfType ty,
                    PgfMarshaller *m, PgfUnmarshaller *u,
                    PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfTypechecker checker(pgf,u);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfTypechecker checker(pgf,u);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfTypechecker checker(pgf,u);
        *pty = m->match_type(&checker, *pty);
    } PGF_API_END
}

PGF_API
PgfRevision pgf_clone_revision(PgfDB *db, PgfRevision revision,
                               PgfText *name,
                               PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        size_t name_size =
            (name == NULL) ? pgf->name.size : name->size;

        ref<PgfPGF> new_pgf = PgfDB::malloc<PgfPGF>(name_size+1);
        new_pgf->ref_count = 1;
        new_pgf->major_version = pgf->major_version;
        new_pgf->minor_version = pgf->minor_version;

        new_pgf->gflags = pgf->gflags;
        if (pgf->gflags != 0)
            Node<PgfFlag>::add_node_ref(pgf->gflags);

        new_pgf->abstract.name = textdup_db(&(*pgf->abstract.name));

        new_pgf->abstract.aflags = pgf->abstract.aflags;
        if (pgf->abstract.aflags != 0)
            Node<PgfFlag>::add_node_ref(pgf->abstract.aflags);

        new_pgf->abstract.funs = pgf->abstract.funs;
        if (pgf->abstract.funs != 0)
            Node<PgfAbsFun>::add_node_ref(pgf->abstract.funs);

        new_pgf->abstract.cats = pgf->abstract.cats;
        if (pgf->abstract.cats != 0)
            Node<PgfAbsCat>::add_node_ref(pgf->abstract.cats);

        new_pgf->concretes = pgf->concretes;
        if (pgf->concretes != 0)
            Node<PgfConcr>::add_node_ref(pgf->concretes);

        new_pgf->prev = 0;
        new_pgf->next = 0;
        PgfDB::link_transient_revision(new_pgf);

        memcpy(&new_pgf->name, ((name == NULL) ? &pgf->name : name),
                   sizeof(PgfText)+name_size+1);

        db->ref_count++;
        return new_pgf.as_object();
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_commit_revision(PgfDB *db, PgfRevision revision,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> new_pgf = PgfDB::revision2pgf(revision);
        ref<PgfPGF> old_pgf = PgfDB::get_revision(&new_pgf->name);

        PgfDB::unlink_transient_revision(new_pgf);
        PgfDB::set_revision(new_pgf);

        if (old_pgf != 0)
            PgfDB::link_transient_revision(old_pgf);

        PgfDB::sync();
    } PGF_API_END
}

PGF_API
PgfRevision pgf_checkout_revision(PgfDB *db, PgfText *name,
                                  PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::get_revision(name);
        if (pgf != 0) {
            Node<PgfPGF>::add_value_ref(pgf);
            db->ref_count++;
        }
        return pgf.as_object();
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_create_function(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfType ty, size_t arity, char *bytecode,
                         prob_t prob,
                         PgfMarshaller *m,
                         PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        PgfDBUnmarshaller u(m);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);
        ref<PgfAbsFun> absfun = PgfDB::malloc<PgfAbsFun>(name->size+1);
        absfun->ref_count = 1;
        absfun->type  = m->match_type(&u, ty);
        absfun->arity = arity;
        absfun->bytecode = bytecode ? PgfDB::malloc<char>(0) : 0;
        absfun->prob = prob;
        memcpy(&absfun->name, name, sizeof(PgfText)+name->size+1);

        Namespace<PgfAbsFun> funs =
            namespace_insert(pgf->abstract.funs, absfun);
        namespace_release(pgf->abstract.funs);
        pgf->abstract.funs = funs;
    } PGF_API_END
}

PGF_API
void pgf_drop_function(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        Namespace<PgfAbsFun> funs =
            namespace_delete(pgf->abstract.funs, name);
        namespace_release(pgf->abstract.funs);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);
        ref<PgfAbsCat> abscat = PgfDB::malloc<PgfAbsCat>(name->size+1);
        abscat->ref_count = 1;
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
        namespace_release(pgf->abstract.cats);
        pgf->abstract.cats = cats;
    } PGF_API_END
}

PGF_API
void pgf_drop_category(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        Namespace<PgfAbsCat> cats =
            namespace_delete(pgf->abstract.cats, name);
        namespace_release(pgf->abstract.cats);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        ref<PgfConcr> concr =
            namespace_lookup(pgf->concretes, name);
        if (concr != 0)
            throw pgf_error("The concrete syntax already exists");

        concr = PgfDB::malloc<PgfConcr>(name->size+1);
        concr->ref_count    = 2;
        concr->ref_count_ex = 1;
        concr->cflags = 0;
        concr->lins = 0;
        concr->lincats = 0;
        concr->printnames = 0;
        concr->prev = 0;
        concr->next = 0;
        memcpy(&concr->name, name, sizeof(PgfText)+name->size+1);

        PgfDB::link_transient_revision(concr);

        Namespace<PgfConcr> concrs =
            namespace_insert(pgf->concretes, concr);
        namespace_release(pgf->concretes);
        pgf->concretes = concrs;

        db->ref_count++;
        return concr.as_object();
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);
        
        ref<PgfConcr> concr =
            namespace_lookup(pgf->concretes, name);
        if (concr == 0)
            throw pgf_error("Unknown concrete syntax");

        ref<PgfConcr> clone = PgfDB::malloc<PgfConcr>(name->size+1);
        clone->ref_count    = 2;
        clone->ref_count_ex = 1;
        clone->cflags = concr->cflags;
        clone->lins = concr->lins;
        clone->lincats = concr->lincats;
        clone->printnames = concr->printnames;
        clone->prev = 0;
        clone->next = 0;
        memcpy(&clone->name, name, sizeof(PgfText)+name->size+1);

        if (clone->cflags != 0)
            Node<PgfFlag>::add_node_ref(clone->cflags);

        PgfDB::link_transient_revision(clone);

        Namespace<PgfConcr> concrs =
            namespace_insert(pgf->concretes, clone);
        namespace_release(pgf->concretes);
        pgf->concretes = concrs;

        db->ref_count++;
        return clone.as_object();
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        Namespace<PgfConcr> concrs =
            namespace_delete(pgf->concretes, name);
        namespace_release(pgf->concretes);
        pgf->concretes = concrs;
    } PGF_API_END
}

class PGF_INTERNAL PgfLinBuilder : public PgfLinBuilderIface
{
    ref<Vector<PgfPArg>> args;
    ref<Vector<ref<PgfPResult>>> res;
    ref<Vector<ref<Vector<PgfSymbol>>>> seqs;

    size_t var_index;
    size_t arg_index;
    size_t res_index;
    size_t seq_index;
    size_t sym_index;
    size_t alt_index;

    size_t n_lindefs;
    size_t n_linrefs;

    ref<Vector<PgfSymbol>> syms;

    size_t pre_sym_index;

    const char *builder_error_msg =
        "Detected incorrect use of the linearization builder";

public:
    PgfLinBuilder()
    {
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
        this->syms = 0;
        this->pre_sym_index = (size_t) -1;
    }

    ref<PgfConcrLincat> build(ref<PgfAbsCat> abscat, PgfConcr *concr,
                              size_t n_fields, PgfText **fields,
                              size_t n_lindefs, size_t n_linrefs,
                              PgfBuildLinIface *build, PgfExn *err)
    {
        size_t n_prods = n_lindefs+n_linrefs;
        this->args = vector_new<PgfPArg>(n_prods);
        this->res  = vector_new<ref<PgfPResult>>(n_prods);
        this->seqs = vector_new<ref<Vector<PgfSymbol>>>(n_lindefs*n_fields+n_linrefs);
        this->n_lindefs = n_lindefs;
        this->n_linrefs = n_linrefs;

        ref<Vector<ref<PgfText>>> db_fields = vector_new<ref<PgfText>>(n_fields);
        for (size_t i = 0; i < n_fields; i++) {
            ref<PgfText> field = textdup_db(fields[i]);
            *vector_elem(db_fields, i) = field;
        }

        ref<PgfConcrLincat> lincat = PgfDB::malloc<PgfConcrLincat>(abscat->name.size+1);
        memcpy(&lincat->name, &abscat->name, sizeof(PgfText)+abscat->name.size+1);
        lincat->ref_count = 1;
        lincat->abscat = abscat;
        lincat->args = args;
        lincat->res  = res;
        lincat->seqs = seqs;
        lincat->fields = db_fields;
        lincat->n_lindefs = n_lindefs;

        build->build(this, err);
        if (err->type == PGF_EXN_NONE && res_index != res->len) {
            err->type = PGF_EXN_PGF_ERROR;
            err->msg  = builder_error_msg;
        }

        if (err->type != PGF_EXN_NONE) {
            failed();
            for (size_t i = 0; i < n_fields; i++) {
                PgfDB::free(*vector_elem(db_fields, i));
            }
            PgfDB::free(db_fields);
            PgfDB::free(lincat);
            return 0;
        }

        return lincat;
    }

    ref<PgfConcrLin> build(ref<PgfAbsFun> absfun, PgfConcr *concr, size_t n_prods,
                           PgfBuildLinIface *build, PgfExn *err)
    {
        ref<PgfConcrLincat> lincat =
            namespace_lookup(concr->lincats, &absfun->type->name);
        if (lincat == 0) {
            throw pgf_error("Missing linearization category");
        }

        this->args = vector_new<PgfPArg>(n_prods*absfun->type->hypos->len);
        this->res  = vector_new<ref<PgfPResult>>(n_prods);
        this->seqs = vector_new<ref<Vector<PgfSymbol>>>(n_prods*lincat->fields->len);
        this->n_lindefs = n_prods;

        ref<PgfConcrLin> lin = PgfDB::malloc<PgfConcrLin>(absfun->name.size+1);
        memcpy(&lin->name, &absfun->name, sizeof(PgfText)+absfun->name.size+1);
        lin->ref_count = 1;
        lin->absfun = absfun;
        lin->args = args;
        lin->res  = res;
        lin->seqs = seqs;

        build->build(this, err);
        if (err->type == PGF_EXN_NONE && res_index != res->len) {
            err->type = PGF_EXN_PGF_ERROR;
            err->msg  = builder_error_msg;
        }

        if (err->type != PGF_EXN_NONE) {
            failed();
            PgfDB::free(lin);
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

            syms = vector_new<PgfSymbol>(n_syms);
            *vector_elem(seqs, seq_index) = syms;
            sym_index = 0;
        } PGF_API_END
    }

    void add_symcat(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolCat> symcat = PgfDB::malloc<PgfSymbolCat>(n_terms*2*sizeof(size_t));
            symcat->d  = d;
            symcat->r.i0 = i0;
            symcat->r.n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                symcat->r.terms[i].factor = terms[2*i];
                symcat->r.terms[i].var    = terms[2*i+1];
            }

            *vector_elem(syms, sym_index) = ref<PgfSymbolCat>::tagged(symcat);
            sym_index++;
        } PGF_API_END
    }

    void add_symlit(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolLit> symlit = PgfDB::malloc<PgfSymbolLit>(n_terms*2*sizeof(size_t));
            symlit->d  = d;
            symlit->r.i0 = i0;
            symlit->r.n_terms = n_terms;

            for (size_t i = 0; i < n_terms; i++) {
                symlit->r.terms[i].factor = terms[2*i];
                symlit->r.terms[i].var    = terms[2*i+1];
            }

            *vector_elem(syms, sym_index) = ref<PgfSymbolLit>::tagged(symlit);
            sym_index++;
        } PGF_API_END
    }

    void add_symvar(size_t d, size_t r, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolVar> symvar = PgfDB::malloc<PgfSymbolVar>();
            symvar->d = d;
            symvar->r = r;

            *vector_elem(syms, sym_index) = ref<PgfSymbolVar>::tagged(symvar);
            sym_index++;
        } PGF_API_END
    }

    void add_symks(PgfText *token, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            ref<PgfSymbolKS> symtok = PgfDB::malloc<PgfSymbolKS>(token->size+1);
            memcpy(&symtok->token, token, sizeof(PgfText)+token->size+1);

            *vector_elem(syms, sym_index) = ref<PgfSymbolKS>::tagged(symtok);
            sym_index++;
        } PGF_API_END
    }

    void start_symkp(size_t n_syms, size_t n_alts, PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len || pre_sym_index != (size_t) -1)
                throw pgf_error(builder_error_msg);

            ref<Vector<PgfSymbol>> def = vector_new<PgfSymbol>(n_syms);

            ref<PgfSymbolKP> symkp = PgfDB::malloc<PgfSymbolKP>(n_alts*sizeof(PgfAlternative));
            symkp->default_form = def;
            symkp->alts.len = n_alts;

            *vector_elem(syms, sym_index) = ref<PgfSymbolKP>::tagged(symkp);

            pre_sym_index = sym_index;
            syms = def;
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

            ref<Vector<PgfSymbol>> form = vector_new<PgfSymbol>(n_syms);

            ref<Vector<ref<PgfText>>> prefixes = vector_new<ref<PgfText>>(n_prefs);
            for (size_t i = 0; i < n_prefs; i++) {
                ref<PgfText> pref = textdup_db(prefs[i]);
                *vector_elem(prefixes, i) = pref;
            }

            syms = *vector_elem(seqs, seq_index);
            ref<PgfSymbolKP> symkp = ref<PgfSymbolKP>::untagged(*vector_elem(syms, pre_sym_index));
            ref<PgfAlternative> alt = ref<PgfAlternative>::from_ptr(&symkp->alts.data[alt_index]);

            alt->form     = form;
            alt->prefixes = prefixes;

            syms = form;
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

            syms = *vector_elem(seqs, seq_index);
            ref<PgfSymbolKP> symkp = ref<PgfSymbolKP>::untagged(*vector_elem(syms, pre_sym_index));            
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

            syms = *vector_elem(seqs, seq_index);
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
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolBIND>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void add_symsoftbind(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolSOFTBIND>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void add_symne(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolNE>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void add_symsoftspace(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolSOFTSPACE>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void add_symcapit(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolCAPIT>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void add_symallcapit(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index == (size_t) -1 || sym_index >= syms->len)
                throw pgf_error(builder_error_msg);

            *vector_elem(syms, sym_index) = ref<PgfSymbolALLCAPIT>::tagged(0);
            sym_index++;
        } PGF_API_END
    }

    void end_sequence(PgfExn *err)
    {
        if (err->type != PGF_EXN_NONE)
            return;

        PGF_API_BEGIN {
            if (syms == 0 || sym_index != syms->len)
                throw pgf_error(builder_error_msg);
            sym_index = (size_t) -1;
            syms = 0;
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
    
    void failed()
    {
        for (size_t i = 0; i < arg_index; i++) {
            PgfDB::free(vector_elem(args, i)->param);
        }
        PgfDB::free(args);

        for (size_t i = 0; i < res_index; i++) {
            ref<PgfPResult> res_elem = *vector_elem(res, i);
            PgfDB::free(res_elem->vars);
            PgfDB::free(res_elem);
        }
        PgfDB::free(res);

        for (size_t i = 0; i < seq_index; i++) {
            ref<Vector<PgfSymbol>> syms = *vector_elem(seqs, i);
            pgf_symbols_free(syms);
        }
        
        if (sym_index != (size_t) -1) {
            ref<Vector<PgfSymbol>> syms = *vector_elem(seqs, seq_index);

            if (pre_sym_index != (size_t) -1) {
                auto sym_kp = ref<PgfSymbolKP>::untagged(*vector_elem(syms, pre_sym_index));

                if (this->syms == sym_kp->default_form) {
                    for (size_t i = 0; i < sym_index; i++) {
                        PgfSymbol sym = *vector_elem(syms, i);
                        pgf_symbol_free(sym);
                    }
                    PgfDB::free(syms);
                } else {
                    pgf_symbols_free(sym_kp->default_form);
                    for (size_t i = 0; i < alt_index; i++) {
                        pgf_symbols_free(sym_kp->alts.data[i].form);
                        for (size_t j = 0; j < sym_kp->alts.data[i].prefixes->len; j++) {
                            ref<PgfText> prefix = *vector_elem(sym_kp->alts.data[i].prefixes, j);
                            PgfDB::free(prefix);
                        }
                    }
                    for (size_t i = 0; i < sym_index; i++) {
                        PgfSymbol sym = *vector_elem(sym_kp->alts.data[alt_index].form, i);
                        pgf_symbol_free(sym);
                    }
                    PgfDB::free(sym_kp->alts.data[alt_index].form);
                    for (size_t i = 0; i < sym_kp->alts.data[alt_index].prefixes->len; i++) {
                        ref<PgfText> prefix = *vector_elem(sym_kp->alts.data[alt_index].prefixes, i);
                        PgfDB::free(prefix);
                    }
                }
                PgfDB::free(sym_kp);
        
                sym_index = pre_sym_index;
            }

            for (size_t j = 0; j < sym_index; j++) {
                PgfSymbol sym = *vector_elem(syms, j);
                pgf_symbol_free(sym);
            }
            PgfDB::free(syms);
        }

        PgfDB::free(seqs);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);
        ref<PgfConcr> concr = PgfDB::revision2concr(cnc_revision);

        ref<PgfAbsCat> abscat =
            namespace_lookup(pgf->abstract.cats, name);
        if (abscat == 0) {
            throw pgf_error("There is no corresponding category in the abstract syntax");
        }

        ref<PgfConcrLincat> lincat =
            PgfLinBuilder().build(abscat, concr, n_fields, fields, n_lindefs, n_linrefs, build, err);
        if (lincat != 0) {
            Namespace<PgfConcrLincat> lincats =
                namespace_insert(concr->lincats, lincat);
            namespace_release(concr->lincats);
            concr->lincats = lincats;
        }
    } PGF_API_END
}

PGF_API
void pgf_drop_lincat(PgfDB *db,
                     PgfConcrRevision revision,
                     PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        Namespace<PgfConcrLincat> lincats =
            namespace_delete(concr->lincats, name);
        namespace_release(concr->lincats);
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
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);
        ref<PgfConcr> concr = PgfDB::revision2concr(cnc_revision);

        ref<PgfAbsFun> absfun =
            namespace_lookup(pgf->abstract.funs, name);
        if (absfun == 0) {
            throw pgf_error("There is no corresponding function in the abstract syntax");
        }

        ref<PgfConcrLin> lin =
            PgfLinBuilder().build(absfun, concr, n_prods, build, err);
        if (lin != 0) {
            Namespace<PgfConcrLin> lins =
                namespace_insert(concr->lins, lin);
            namespace_release(concr->lins);
            concr->lins = lins;
        }
    } PGF_API_END
}

PGF_API
void pgf_drop_lin(PgfDB *db,
                  PgfConcrRevision revision,
                  PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        Namespace<PgfConcrLin> lins =
            namespace_delete(concr->lins, name);
        namespace_release(concr->lins);
        concr->lins = lins;
    } PGF_API_END
}

PGF_API
int pgf_has_linearization(PgfDB *db, PgfConcrRevision revision,
                          PgfText *name, PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        ref<PgfConcrLin> lin =
            namespace_lookup(concr->lins, name);

        return (lin != 0);
    } PGF_API_END

    return 0;
}

PGF_API
PgfText *pgf_linearize(PgfDB *db, PgfConcrRevision revision,
                       PgfExpr expr, PgfPrintContext *ctxt,
                       PgfMarshaller *m,
                       PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);
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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);

        while (linearizer.resolve()) {
            linearizer.linearize(&out, 0);
            variants = (PgfText **) realloc(variants, ((*n_variants)+1)*sizeof(PgfText **));
            variants[(*n_variants)++] = out.get_text();
        }

        return variants;
    } PGF_API_END

    free(variants);
    return NULL;
}

PGF_API
PgfText **pgf_tabular_linearize(PgfDB *db, PgfConcrRevision revision,
                                PgfExpr expr, PgfPrintContext *ctxt,
                                PgfMarshaller *m, size_t *n_fields,
                                PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);
        PgfLinearizationOutput out;
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(false);
        if (linearizer.resolve()) {
            ref<PgfConcrLincat> lincat = linearizer.get_lincat();
            PgfText **res = (PgfText **)
                malloc(lincat->fields->len*2*sizeof(PgfText*));
            size_t pos = 0;
            for (size_t i = 0; i < lincat->fields->len; i++) {
                linearizer.linearize(&out, i);

                PgfText *text = out.get_text();
                if (text != NULL) {
                    res[pos]   = textdup(&(*lincat->fields->data[i]));
                    res[pos+1] = text;
                    pos += 2;
                }
            }
            *n_fields = pos/2;
            return res;
        }
    } PGF_API_END

    return NULL;
}

PGF_API_DECL
void pgf_bracketed_linearize(PgfDB *db, PgfConcrRevision revision,
                             PgfExpr expr, PgfPrintContext *ctxt,
                             PgfMarshaller *m,
                             PgfLinearizationOutputIface *out,
                             PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);
        PgfLinearizer linearizer(ctxt, concr, m);
        m->match_expr(&linearizer, expr);
        linearizer.reverse_and_label(true);
        if (linearizer.resolve()) {
            linearizer.linearize(out, 0);
        }
    } PGF_API_END
}

PGF_API
PgfText *pgf_get_printname(PgfDB *db, PgfConcrRevision revision,
                           PgfText *fun, PgfExn* err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);
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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        ref<PgfConcrPrintname> printname = PgfDB::malloc<PgfConcrPrintname>(fun->size+1);
        printname->ref_count = 1;
        memcpy(&printname->name, fun, sizeof(PgfText)+fun->size+1);
        printname->printname = textdup_db(name);

        Namespace<PgfConcrPrintname> printnames =
            namespace_insert(concr->printnames, printname);
        namespace_release(concr->printnames);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        flag->ref_count = 1;
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;
        Namespace<PgfFlag> gflags =
            namespace_insert(pgf->gflags, flag);
        namespace_release(pgf->gflags);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        flag->ref_count = 1;
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;
        Namespace<PgfFlag> aflags =
            namespace_insert(pgf->abstract.aflags, flag);
        namespace_release(pgf->abstract.aflags);
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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

        ref<PgfFlag> flag = PgfDB::malloc<PgfFlag>(name->size+1);
        flag->ref_count = 1;
        memcpy(&flag->name, name, sizeof(PgfText)+name->size+1);
        PgfLiteral lit = m->match_lit(&u, value);
        flag->value = lit;
        Namespace<PgfFlag> cflags =
            namespace_insert(concr->cflags, flag);
        namespace_release(concr->cflags);
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

        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

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

        ref<PgfConcr> concr = PgfDB::revision2concr(revision);

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
