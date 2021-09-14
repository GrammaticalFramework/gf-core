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

PGF_INTERNAL
PgfText master = {size: 6, text: {'m','a','s','t','e','r',0}};

PGF_API
PgfDB *pgf_read_pgf(const char* fpath,
                    PgfRevision *revision,
                    PgfExn* err)
{
    PgfDB *db = NULL;

    PGF_API_BEGIN {
        db = new PgfDB(NULL, 0, 0);
        std::ifstream in(fpath, std::ios::binary);
        if (in.fail()) {
            throw pgf_systemerror(errno, fpath);
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(&in, fpath);
            ref<PgfPGF> pgf = rdr.read_pgf();

            PgfDB::set_revision(pgf);
            *revision = pgf.as_object();
        }

        return db;
    } PGF_API_END

end:
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

    PGF_API_BEGIN {
        db = new PgfDB(ngf_path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);

        std::ifstream in(pgf_path, std::ios::binary);
        if (in.fail()) {
            throw pgf_systemerror(errno, pgf_path);
        }

        {
            DB_scope scope(db, WRITER_SCOPE);

            PgfReader rdr(&in, pgf_path);
            ref<PgfPGF> pgf = rdr.read_pgf();

            PgfDB::set_revision(pgf);
            *revision = pgf.as_object();
        }

        return db;
    } PGF_API_END

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
        db = new PgfDB(fpath, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);

        {
            DB_scope scope(db, WRITER_SCOPE);

            ref<PgfPGF> pgf = PgfDB::get_revision(&master);
            if (pgf == 0) {
                is_new = true;
                pgf = PgfDB::malloc<PgfPGF>(master.size+1);
                pgf->ref_count = 1;
                pgf->major_version = PGF_MAJOR_VERSION;
                pgf->minor_version = PGF_MINOR_VERSION;
                pgf->gflags = 0;
                pgf->abstract.name = PgfDB::malloc<PgfText>();
                pgf->abstract.name->size = 0;
                pgf->abstract.aflags = 0;
                pgf->abstract.funs = 0;
                pgf->abstract.cats = 0;
                pgf->prev = 0;
                pgf->next = 0;
                memcpy(&pgf->name, &master, sizeof(PgfText)+master.size+1);
                PgfDB::set_revision(pgf);
            } else {
                Node<PgfPGF>::add_value_ref(pgf);
            }
            *revision = pgf.as_object();
        }

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
void pgf_free(PgfDB *db)
{
    delete db;
}

PGF_API_DECL
void pgf_free_revision(PgfDB *db, PgfRevision revision)
{
    try {
        DB_scope scope(db, WRITER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        if (pgf->ref_count == 1 && PgfDB::is_persistant_revision(pgf)) {
            // Someone is trying to release the last reference count
            // to a persistant revision. Mostly likely this is an
            // error in the reference counting for one of the clients.
            // The best that we can do is to ignore the request.
            return;
        }

        if (!(--pgf->ref_count)) {
            PgfDB::unlink_transient_revision(pgf);
            PgfPGF::release(pgf);
            PgfDB::free(pgf);
        }
    } catch (std::runtime_error& e) {
        // silently ignore and hope for the best
    }
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
            return 0;
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
    PGF_API_BEGIN {
        DB_scope scope(db, READER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::revision2pgf(revision);

        PgfItorHelper helper;
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

        return (absfun->defns == 0);
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

        return absfun->ep.prob;
    } PGF_API_END

    return INFINITY;
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

        new_pgf->prev = 0;
        new_pgf->next = 0;
        PgfDB::link_transient_revision(new_pgf);

        memcpy(&new_pgf->name, ((name == NULL) ? &pgf->name : name),
                   sizeof(PgfText)+name_size+1);

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
    } PGF_API_END
}

PGF_API
PgfRevision pgf_checkout_revision(PgfDB *db, PgfText *name,
                                  PgfExn *err)
{
    PGF_API_BEGIN {
        DB_scope scope(db, WRITER_SCOPE);
        ref<PgfPGF> pgf = PgfDB::get_revision(name);
        Node<PgfPGF>::add_value_ref(pgf);
        return pgf.as_object();
    } PGF_API_END

    return 0;
}

PGF_API
void pgf_create_function(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfType ty, prob_t prob,
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
        absfun->arity = 0;
        absfun->defns = 0;
        absfun->ep.prob = prob;
        ref<PgfExprFun> efun =
            ref<PgfExprFun>::from_ptr((PgfExprFun*) &absfun->name);
        absfun->ep.expr = ref<PgfExprFun>::tagged(efun);
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
        flag->value = m->match_lit(&u, value);
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
        flag->value = m->match_lit(&u, value);
        Namespace<PgfFlag> aflags =
            namespace_insert(pgf->abstract.aflags, flag);
        namespace_release(pgf->abstract.aflags);
        pgf->abstract.aflags = aflags;
    } PGF_API_END
}
