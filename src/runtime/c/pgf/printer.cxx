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
    if (res) {
        size_t size = res->size+buf_size;
        res = (PgfText *) realloc(res, sizeof(PgfText)+size+1);
    } else {
        res = (PgfText *) malloc(sizeof(PgfText)+buf_size+1);
        res->size = 0;
    }

    va_list ap;
    va_start(ap, format);
    res->size +=
        vsnprintf(res->text+res->size, buf_size, format, ap);
    va_end(ap);
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

uintptr_t PgfPrinter::eabs(PgfBindType btype, PgfText *name, uintptr_t body)
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

uintptr_t PgfPrinter::eapp(uintptr_t fun, uintptr_t arg)
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

uintptr_t PgfPrinter::elit(uintptr_t lit)
{
    flush_lambdas();
    return m->match_lit(this, lit);
}

uintptr_t PgfPrinter::emeta(PgfMetaId meta)
{
    flush_lambdas();

    if (meta == 0) {
        puts("?");
    } else {
        nprintf(16, "?%d", meta);
    }

    return 0;
}

uintptr_t PgfPrinter::efun(PgfText *name)
{
    flush_lambdas();

    puts(name);
    return 0;
}

uintptr_t PgfPrinter::evar(int index)
{
    flush_lambdas();

    PgfPrintContext *var = ctxt;
    for (int i = 0; i < index; i++) {
        if (var == NULL)
            break;
        var = var->next;
    }
    if (var == NULL) {
        nprintf(16, "#%d", index);
    } else {
        puts(&var->name);
    }
    return 0;
}

uintptr_t PgfPrinter::etyped(uintptr_t expr, uintptr_t ty)
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

uintptr_t PgfPrinter::eimplarg(uintptr_t expr)
{
    flush_lambdas();

    puts("{");
    prio = 0;
    m->match_expr(this, expr);
    puts("}");
    return 0;
}

uintptr_t PgfPrinter::lint(int v)
{
    nprintf(16, "%d", v);
    return 0;
}

uintptr_t PgfPrinter::lflt(double v)
{
    nprintf(16,"%lg",v);
    return 0;
}

uintptr_t PgfPrinter::lstr(PgfText *v)
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

uintptr_t PgfPrinter::dtyp(int n_hypos, PgfTypeHypo *hypos,
                           PgfText *cat,
                           int n_exprs, uintptr_t *exprs)
{
    bool p = (prio > 0 && n_hypos > 0) ||
             (prio > 3 && n_exprs > 0);
    if (p) puts("(");

    PgfPrintContext *save_ctxt = ctxt;

    for (int i = 0; i < n_hypos; i++) {
        if (textcmp(hypos[i].cid, &wildcard) == 0) {
            prio = 1;
            m->match_type(this, hypos[i].type);
        } else {
            push_variable(hypos[i].cid);

            puts("(");
            if (hypos[i].bind_type == PGF_BIND_TYPE_IMPLICIT)
                puts("{");
            puts(&ctxt->name);
            if (hypos[i].bind_type == PGF_BIND_TYPE_IMPLICIT)
                puts("}");
            puts(" : ");
            m->match_type(this, hypos[i].type);
            puts(")");
        }

        puts(" -> ");
    }

    puts(cat);

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

void PgfPrinter::free_ref(uintptr_t x)
{
}

void PgfPrinter::free_me()
{
}
