#include <stdarg.h>
#include "data.h"
#include "printer.h"

PgfPrinter::PgfPrinter(PgfPrintContext *context, int priority,
                       PgfMarshaller *marshaller)
{
    ctxt             = context;
    printing_lambdas = false;

    prio = priority;

    res      = NULL;

    m    = marshaller;
}

void PgfPrinter::puts(PgfText *s)
{
    if (res) {
        size_t size = res->size+s->size;

        res = (PgfText *) realloc(res, sizeof(PgfText)+size+1);
        memcpy(res->text+res->size, s->text, s->size+1);
        res->size = size;
    } else {
        res = textdup(s);
    }
}

void PgfPrinter::puts(const char *s)
{
    size_t len = strlen(s);

    if (res) {
        size_t size = res->size+len;

        res = (PgfText *) realloc(res, sizeof(PgfText)+size+1);
        memcpy(res->text+res->size, s, len+1);
        res->size = size;
    } else {
        res = (PgfText *) malloc(sizeof(PgfText)+len+1);
        memcpy(res->text, s, len+1);
        res->size = len;
    }
}

void PgfPrinter::nprintf(size_t buf_size, const char *format, ...)
{
again: {
        if (res) {
            size_t size = res->size+buf_size;
            res = (PgfText *) realloc(res, sizeof(PgfText)+size+1);
        } else {
            res = (PgfText *) malloc(sizeof(PgfText)+buf_size+1);
            res->size = 0;
        }

        va_list ap;
        va_start(ap, format);
        size_t out =
            vsnprintf(res->text+res->size, buf_size, format, ap);
        va_end(ap);
        if (out >= buf_size) {
            buf_size = out+1;
            goto again;
        }
        res->size += out;
    }
}

PgfText *PgfPrinter::get_text()
{
    return res;
}

void PgfPrinter::flush_lambdas()
{
    if (!printing_lambdas)
        return;

    if (this->btype == PGF_BIND_TYPE_IMPLICIT) {
        puts("}");
    }
    puts("->");
    printing_lambdas = false;
}

void PgfPrinter::push_variable(PgfText *name)
{
    PgfPrintContext *c = (PgfPrintContext *)
        malloc(sizeof(PgfPrintContext)+name->size+1);
    c->next = ctxt;
    c->name.size = name->size;
    memcpy(c->name.text, name->text, name->size+1);

    int i = 0;
    bool clash;
    do {
        clash = false;
        PgfPrintContext *c2 = ctxt;
        while (c2 != NULL) {
            if (textcmp(&c->name, &c2->name) == 0) {
                clash = true;
                if (i == 0) {
                    // the first time when we encounter a clash,
                    // we ensure enough space to add a number to the name.
                    c =  (PgfPrintContext *)
                        realloc(c,sizeof(PgfPrintContext)+name->size+1+15);
                }
                i++;
                char *buffer = c->name.text+name->size;
                snprintf(buffer,15,"%d",i);
                c->name.size = name->size+strlen(buffer);
                break;
            }
            c2 = c2->next;
        }
    } while (clash);

	ctxt = c;
}

void PgfPrinter::pop_variable()
{
    PgfPrintContext *tmp = ctxt;
    ctxt = ctxt->next;
    free(tmp);
}

PgfExpr PgfPrinter::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    bool p = (prio > 1);
    if (p) puts("(");

    push_variable(name);

    if (!printing_lambdas) {
        // This is the outermost lambda possibly in a chain of nested lambdas
        printing_lambdas = true;
        puts("\\");
        if (btype == PGF_BIND_TYPE_IMPLICIT) {
            puts("{");
        }
    } else {
        if (btype       == PGF_BIND_TYPE_EXPLICIT &&
            this->btype == PGF_BIND_TYPE_IMPLICIT) {
            puts("}");
        }
        puts(",");
        if (btype       == PGF_BIND_TYPE_IMPLICIT &&
            this->btype == PGF_BIND_TYPE_EXPLICIT) {
            puts("{");
        }
    }
    this->btype = btype;
    puts(&ctxt->name);

    prio = 1;
    m->match_expr(this, body);

    pop_variable();

    if (p) puts(")");

    return 0;
}

PgfExpr PgfPrinter::eapp(PgfExpr fun, PgfExpr arg)
{
    flush_lambdas();

    bool p = (prio > 3);
    if (p) puts("(");

    prio = 3;
    m->match_expr(this, fun);

    puts(" ");

    prio = 4;
    m->match_expr(this, arg);

    if (p) puts(")");

    return 0;
}

PgfExpr PgfPrinter::elit(PgfLiteral lit)
{
    flush_lambdas();
    return m->match_lit(this, lit);
}

PgfExpr PgfPrinter::emeta(PgfMetaId meta)
{
    flush_lambdas();

    if (meta == 0) {
        puts("?");
    } else {
        nprintf(4, "?%d", meta);
    }

    return 0;
}

PgfExpr PgfPrinter::efun(PgfText *name)
{
    flush_lambdas();

    bool normal_name = true;
    
    const uint8_t* start = (uint8_t*) name->text;
    const uint8_t* end   = start + name->size;

    const uint8_t* s = start;
    while (s < end) {
        uint32_t ucs = pgf_utf8_decode(&s);

        if (!((s == (uint8_t*) start)
                  ? pgf_is_ident_first(ucs)
                  : pgf_is_ident_rest (ucs))) {
            normal_name = false;
            break;
        }
    }

    if (normal_name) {
        puts(name);
    } else {
        PgfText *charbuf = (PgfText *) alloca(sizeof(PgfText)+7);

        puts("'");
        while (start < end) {
            const uint8_t* s = start;
            uint32_t ucs = pgf_utf8_decode(&s);

            switch (ucs) {
            case '\\':
                puts("\\\\");
                break;
            case '\'':
                puts("\\'");
                break;
            case '\n':
                puts("\\n");
                break;
            case '\r':
                puts("\\r");
                break;
            case '\b':
                puts("\\b");
                break;
            case '\t':
                puts("\\t");
                break;
            case '\0':
                puts("\\0");
                break;
            default:
                charbuf->size = s-start;
                memcpy(charbuf->text, start, charbuf->size);
                charbuf->text[charbuf->size] = 0;
                puts(charbuf);
            }
            start = s;
        }
        puts("'");
    }

    return 0;
}

PgfExpr PgfPrinter::evar(int index)
{
    PgfPrintContext *var = ctxt;
    for (int i = 0; i < index; i++) {
        if (var == NULL)
            break;
        var = var->next;
    }
    if (var == NULL) {
        flush_lambdas();
        nprintf(4, "#%d", index);
    } else {
        efun(&var->name);
    }
    return 0;
}

PgfExpr PgfPrinter::etyped(PgfExpr expr, PgfType ty)
{
    flush_lambdas();

    puts("<");
    prio = 0;
    m->match_expr(this, expr);
    puts(" : ");
    prio = 0;
    m->match_type(this, ty);
    puts(">");
    return 0;
}

PgfExpr PgfPrinter::eimplarg(PgfExpr expr)
{
    flush_lambdas();

    puts("{");
    prio = 0;
    m->match_expr(this, expr);
    puts("}");
    return 0;
}

#define xstr(s) str(s)
#define str(s) #s

PgfLiteral PgfPrinter::lint(size_t size, uintmax_t *v)
{
    if (size == 0)
        puts("0");
    else {
        nprintf(LINT_BASE_LOG+2, "%jd", v[0]);
        for (size_t i = 1; i < size; i++) {
            nprintf(LINT_BASE_LOG+1, "%0" xstr(LINT_BASE_LOG) "ju", v[i]);
        }
    }
    return 0;
}

PgfLiteral PgfPrinter::lflt(double v)
{
    nprintf(32,"%lg",v);
    return 0;
}

PgfLiteral PgfPrinter::lstr(PgfText *v)
{
    PgfText *charbuf = (PgfText *) alloca(sizeof(PgfText)+7);

    puts("\"");
    const uint8_t* start = (uint8_t*) v->text;
    const uint8_t* end   = start + v->size;
    while (start < end) {
        const uint8_t* s = start;
        uint32_t c = pgf_utf8_decode(&s);
        switch (c) {
        case '\\':
            puts("\\\\");
            break;
        case '"':
            puts("\\\"");
            break;
        case '\n':
            puts("\\n");
            break;
        case '\r':
            puts("\\r");
            break;
        case '\b':
            puts("\\b");
            break;
        case '\t':
            puts("\\t");
            break;
        case '\0':
            puts("\\0");
            break;
        default:
            charbuf->size = s-start;
            memcpy(charbuf->text, start, charbuf->size);
            charbuf->text[charbuf->size] = 0;
            puts(charbuf);
        }
        start = s;
    }
    puts("\"");
    return 0;
}

void PgfPrinter::hypo(PgfTypeHypo *hypo, int prio)
{
    if (hypo->cid->size == 1 && strcmp(hypo->cid->text, "_") == 0) {
        this->prio = prio;
        m->match_type(this, hypo->type);
    } else {
        puts("(");
        if (hypo->bind_type == PGF_BIND_TYPE_IMPLICIT)
            puts("{");
        puts(hypo->cid);
        if (hypo->bind_type == PGF_BIND_TYPE_IMPLICIT)
            puts("}");
        puts(" : ");
        this->prio = 0;
        m->match_type(this, hypo->type);
        puts(")");

        push_variable(hypo->cid);
    }
}

PgfType PgfPrinter::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs)
{
    bool p = (prio > 0 && n_hypos > 0) ||
             (prio > 3 && n_exprs > 0);
    if (p) puts("(");

    PgfPrintContext *save_ctxt = ctxt;

    for (int i = 0; i < n_hypos; i++) {
        hypo(&hypos[i],1);
        puts(" -> ");
    }

    efun(cat);

    for (int i = 0; i < n_exprs; i++) {
        puts(" ");
        prio = 4;
        m->match_expr(this, exprs[i]);
    }

    while (ctxt != save_ctxt) {
        pop_variable();
    }

    if (p) puts(")");

    return 0;
}

void PgfPrinter::free_ref(object x)
{
}
