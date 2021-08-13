#include "pgf/data.h"

PGF_INTERNAL
hobject pgf_unmarshall_literal(PgfBridge *b, PgfLiteral l)
{
    switch (ref<PgfLiteral>::get_tag(l)) {
    case PgfLiteralInt::tag: {
        return b->lint(ref<PgfLiteralInt>::untagged(l)->val);
    }
    case PgfLiteralFlt::tag: {
        return b->lflt(ref<PgfLiteralInt>::untagged(l)->val);
    }
    case PgfLiteralStr::tag: {
        return b->lstr(&ref<PgfLiteralStr>::untagged(l)->val);
    }
	default:
		throw pgf_error("Unknown literal tag");
    }
}

PGF_INTERNAL
hobject pgf_unmarshall_expr(PgfBridge *b, PgfExpr e)
{
    switch (ref<PgfExpr>::get_tag(e)) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(e);
        hobject body = pgf_unmarshall_expr(b,eabs->body);
        hobject res = b->eabs(eabs->bind_type,&eabs->name,body);
        b->free_ref(body);
        return res;
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(e);
        hobject fun = pgf_unmarshall_expr(b,eapp->fun);
        hobject arg = pgf_unmarshall_expr(b,eapp->arg);
        hobject res = b->eapp(fun,arg);
        b->free_ref(arg);
        b->free_ref(fun);
        return res;
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(e);
        hobject lit = pgf_unmarshall_literal(b,elit->lit);
        hobject res = b->elit(lit);
        b->free_ref(lit);
        return res;
    }
    case PgfExprMeta::tag: {
        return b->emeta(ref<PgfExprMeta>::untagged(e)->id);
    }
	case PgfExprFun::tag: {
        return b->efun(&ref<PgfExprFun>::untagged(e)->name);
    }
	case PgfExprVar::tag: {
        return b->evar(ref<PgfExprVar>::untagged(e)->var);
	}
	case PgfExprTyped::tag: {
        auto etyped = ref<PgfExprTyped>::untagged(e);
        hobject expr = pgf_unmarshall_expr(b,etyped->expr);
        hobject type = pgf_unmarshall_type(b,etyped->type);
        hobject res = b->etyped(expr,type);
        b->free_ref(type);
        b->free_ref(expr);
        return res;
	}
	case PgfExprImplArg::tag: {
        auto eimpl = ref<PgfExprImplArg>::untagged(e);
        hobject expr = pgf_unmarshall_expr(b,eimpl->expr);
        hobject res = b->eimplarg(expr);
        b->free_ref(expr);
        return res;
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

PGF_INTERNAL
hobject pgf_unmarshall_type(PgfBridge *b, PgfType *tp)
{
    PgfTypeHypo *hypos = (PgfTypeHypo *)
        alloca(tp->hypos->len * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < tp->hypos->len; i++) {
        hypos[i].bind_type = tp->hypos->data[i].bind_type;
        hypos[i].cid = &(*tp->hypos->data[i].cid);
        hypos[i].type = pgf_unmarshall_type(b, tp->hypos->data[i].type);
    }

    hobject *exprs = (hobject *)
        alloca(tp->exprs->len * sizeof(hobject));
    for (size_t i = 0; i < tp->exprs->len; i++) {
        exprs[i] = pgf_unmarshall_expr(b, tp->exprs->data[i]);
    }

    hobject res = b->dtyp(tp->hypos->len, hypos,
                            &tp->name,
                            tp->exprs->len, exprs);

    for (size_t i = 0; i < tp->hypos->len; i++) {
        b->free_ref(hypos[i].type);
    }

    for (size_t i = 0; i < tp->exprs->len; i++) {
        b->free_ref(exprs[i]);
    }

    return res;
}

PgfExprParser::PgfExprParser(PgfText *input, PgfBridge *bridge)
{
    inp = input;
    pos = (const char*) &inp->text;
	ch  = ' ';
    b   = bridge;
    token_value = NULL;

	token();
}

PgfExprParser::~PgfExprParser()
{
    free(token_value);
}

uint32_t PgfExprParser::getc()
{
    uint32_t ch = pgf_utf8_decode((const uint8_t **) &pos);
	if (pos - ((const char*) &inp->text) > inp->size) {
		ch = EOF;
	}
    return ch;
}

void PgfExprParser::putc(uint32_t ch)
{
    size_t ch_len =
             (ch < 0xe0 ? 1 :
	          ch < 0xf0 ? 2 :
	          ch < 0xf8 ? 3 :
	          ch < 0xfc ? 4 :
	                      5
	         );
    size_t len = token_value ? token_value->size : 0;

    token_value = (PgfText*)
        realloc(token_value, sizeof(PgfText)+len+ch_len+1);
    uint8_t *p = (uint8_t*) (token_value->text+len);
    pgf_utf8_encode(ch, &p);
    token_value->size = p - ((uint8_t*) token_value->text);
    *(p++) = 0;
}

bool PgfExprParser::eof()
{
    return (token_tag == PGF_TOKEN_EOF);
}

static bool
pgf_is_ident_first(uint32_t ucs)
{
	return (ucs == '_') ||
           (ucs >= 'a' && ucs <= 'z') ||
           (ucs >= 'A' && ucs <= 'Z') ||
           (ucs >= 192 && ucs <= 255 && ucs != 247 && ucs != 215);
}

static bool
pgf_is_ident_rest(uint32_t ucs)
{
	return (ucs == '_') ||
           (ucs == '\'') ||
           (ucs >= '0' && ucs <= '9') ||
           (ucs >= 'a' && ucs <= 'z') ||
           (ucs >= 'A' && ucs <= 'Z') ||
           (ucs >= 192 && ucs <= 255 && ucs != 247 && ucs != 215);
}

static bool
pgf_is_normal_ident(PgfText *id)
{
    if (id->size == 0)
        return false;

	const uint8_t* p = (const uint8_t*) id->text;
	uint32_t ucs = pgf_utf8_decode(&p);
	if (!pgf_is_ident_first(ucs))
		return false;

	while (p - (const uint8_t*) id->text < id->size) {
		ucs = pgf_utf8_decode(&p);
		if (!pgf_is_ident_rest(ucs))
			return false;
	}

	return true;
}

void PgfExprParser::str_char()
{
    if (ch == '\\') {
        ch = getc();
        switch (ch) {
        case '\\':
            putc('\\');
            break;
        case '"':
            putc('\"');
            break;
        case 'n':
            putc('\n');
            break;
        case 'r':
            putc('\r');
            break;
        case 'b':
            putc('\b');
            break;
        case 't':
            putc('\t');
            break;
        default:
            return;
        }
    } else {
        putc(ch);
    }
    ch = getc();
}

void PgfExprParser::token()
{
	while (isspace(ch)) {
		ch = getc();
	}

    if (token_value != NULL)
        free(token_value);

	token_tag   = PGF_TOKEN_UNKNOWN;
	token_value = NULL;

	switch (ch) {
	case EOF:
		ch = getc();
		token_tag = PGF_TOKEN_EOF;
		break;
	case '(':
		ch = getc();
		token_tag = PGF_TOKEN_LPAR;
		break;
	case ')':
		ch = getc();
		token_tag = PGF_TOKEN_RPAR;
		break;
	case '{':
		ch = getc();
		token_tag = PGF_TOKEN_LCURLY;
		break;
	case '}':
		ch = getc();
		token_tag = PGF_TOKEN_RCURLY;
		break;
	case '<':
		ch = getc();
		token_tag = PGF_TOKEN_LTRIANGLE;
		break;
	case '>':
		ch = getc();
		token_tag = PGF_TOKEN_RTRIANGLE;
		break;
	case '?':
		ch = getc();
		token_tag = PGF_TOKEN_QUESTION;
		break;
	case '\\':
		ch = getc();
		token_tag = PGF_TOKEN_LAMBDA;
		break;
	case '-':
		ch = getc();
		if (ch == '>') {
			ch = getc();
			token_tag = PGF_TOKEN_RARROW;
		}
		break;
	case ',':
		ch = getc();
		token_tag = PGF_TOKEN_COMMA;
		break;
	case ':':
		ch = getc();
		token_tag = PGF_TOKEN_COLON;
		break;
	case ';':
		ch = getc();
		token_tag = PGF_TOKEN_SEMI;
		break;
	case '\'': {
		ch = getc();
		while (ch != '\'' && ch != EOF) {
			str_char();
		}
		if (ch == '\'') {
			ch = getc();
			token_tag = PGF_TOKEN_IDENT;
		}
		break;
    }
    case '"': {
        ch = getc();
        while (ch != '"' && ch != EOF) {
            str_char();
        }
        if (ch == '"') {
            ch = getc();
            token_tag = PGF_TOKEN_STR;
        }
        break;
    }
	default: {
		if (pgf_is_ident_first(ch)) {
			do {
				putc(ch);
				ch = getc();
			} while (pgf_is_ident_rest(ch));
			token_tag = PGF_TOKEN_IDENT;
		} else if (isdigit(ch)) {
			while (isdigit(ch)) {
				putc(ch);
				ch = getc();
			}

			if (ch == '.') {
				putc(ch);
				ch = getc();

				while (isdigit(ch)) {
					putc(ch);
					ch = getc();
				}
				token_tag   = PGF_TOKEN_FLT;
			} else {
				token_tag   = PGF_TOKEN_INT;
			}
		}
		break;
	}
	}
}

bool PgfExprParser::lookahead(int ch)
{
	while (isspace(this->ch)) {
		this->ch = getc();
	}

	return (this->ch == ch);
}

hobject PgfExprParser::parse_term()
{
	switch (token_tag) {
	case PGF_TOKEN_LPAR: {
		token();
		hobject expr = parse_expr();
		if (token_tag == PGF_TOKEN_RPAR) {
			token();
			return expr;
		} else {
            if (expr != 0)
                b->free_ref(expr);
			return 0;
		}
	}
	case PGF_TOKEN_LTRIANGLE: {
		token();
		hobject expr = parse_expr();
		if (expr == 0)
			return 0;

		if (token_tag != PGF_TOKEN_COLON) {
            b->free_ref(expr);
			return 0;
		}
		token();

		hobject type = parse_type();
		if (type == 0) {
            b->free_ref(expr);
			return 0;
        }

		if (token_tag != PGF_TOKEN_RTRIANGLE) {
            b->free_ref(expr);
            b->free_ref(type);
			return 0;
		}
		token();

		hobject e = b->etyped(expr,type);
        b->free_ref(expr);
        b->free_ref(type);
        return e;
	}
	case PGF_TOKEN_QUESTION: {
		token();

		PgfMetaId id = 0;
		if (token_tag == PGF_TOKEN_INT) {
			id = atoi((const char*) &token_value->text);
			token();
		}
		return b->emeta(id);
	}
	case PGF_TOKEN_IDENT: {
        hobject e = b->efun(token_value);
		token();
        return e;
	}
	case PGF_TOKEN_INT: {
		int n = atoi((const char*) &token_value->text);
        uint32_t lit = b->lint(n);
        uint32_t e = b->elit(lit);
        b->free_ref(lit);
		token();
		return e;
	}
	case PGF_TOKEN_STR: {
        uint32_t lit = b->lstr(token_value);
        uint32_t e = b->elit(lit);
        b->free_ref(lit);
		token();
		return e;
	}
	case PGF_TOKEN_FLT: {
		double d = atof((const char*) &token_value->text);
        uint32_t lit = b->lflt(d);
        uint32_t e = b->elit(lit);
        b->free_ref(lit);
		token();
		return e;
	}
	default:
		return 0;
	}
}

hobject PgfExprParser::parse_arg()
{
	hobject arg;

	if (token_tag == PGF_TOKEN_LCURLY) {
		token();

		arg = parse_expr();
		if (arg == 0)
			return 0;

		if (token_tag != PGF_TOKEN_RCURLY) {
            b->free_ref(arg);
			return 0;
		}
		token();

		hobject e = b->eimplarg(arg);
        b->free_ref(arg);
        arg = e;
	} else {
		arg = parse_term();
	}

	return arg;
}

PgfText wildcard = {size: 1, text: {'_','0'}};

PgfBind *PgfExprParser::parse_bind(PgfBind *next)
{
    PgfBind *last = next;
	PgfBindType bind_type = PGF_BIND_TYPE_EXPLICIT;

	if (token_tag == PGF_TOKEN_LCURLY) {
		bind_type = PGF_BIND_TYPE_IMPLICIT;
		token();
	}

	for (;;) {
        PgfText *var;
		if (token_tag == PGF_TOKEN_IDENT) {
			var = token_value;
		} else if (token_tag == PGF_TOKEN_WILD) {
			var = &wildcard;
		} else {
			goto error;
		}

        PgfBind *bind = (PgfBind *) malloc(sizeof(PgfBind)+var->size+1);
        bind->bind_type = bind_type;
        bind->next      = last;
        bind->var.size  = var->size;
        memcpy(bind->var.text, var->text, var->size+1);
        last = bind;

        token();

		if (bind->bind_type == PGF_BIND_TYPE_EXPLICIT ||
		    token_tag != PGF_TOKEN_COMMA) {
			break;
		}

		token();
	}

	if (bind_type == PGF_BIND_TYPE_IMPLICIT) {
		if (token_tag != PGF_TOKEN_RCURLY)
			goto error;
		token();
	}

	return last;

error:
    while (last != next) {
        PgfBind *tmp = last;
        last = last->next;
        free(tmp);
    }
    return NULL;
}

PgfBind *PgfExprParser::parse_binds(PgfBind *next)
{
	for (;;) {
        PgfBind *binds = parse_bind(next);
        if (binds == NULL) {
            while (next != NULL) {
                PgfBind *tmp = next;
                next = next->next;
                free(tmp);
            }
            break;
        }

        next = binds;

		if (token_tag != PGF_TOKEN_COMMA)
			break;

		token();
	}

	return next;
}

hobject PgfExprParser::parse_expr()
{
    hobject expr;

	if (token_tag == PGF_TOKEN_LAMBDA) {
		token();

		PgfBind* bs = parse_binds(NULL);
		if (bs == NULL)
			return 0;

		if (token_tag != PGF_TOKEN_RARROW) {
			goto error;
		}
		token();

		expr = parse_expr();
		if (expr == 0)
			goto error;

		while (bs != NULL) {
            hobject abs_expr = b->eabs(bs->bind_type, &bs->var, expr);
            b->free_ref(expr);
            expr = abs_expr;

            PgfBind *tmp = bs;
            bs = bs->next;
            free(tmp);
		}

		return expr;

error:
		while (bs != NULL) {
            PgfBind *tmp = bs;
            bs = bs->next;
            free(tmp);
		}

		return 0;
	} else {
		expr = parse_term();
		if (expr == 0)
			return 0;

		while (token_tag != PGF_TOKEN_EOF &&
		       token_tag != PGF_TOKEN_RPAR &&
		       token_tag != PGF_TOKEN_RCURLY &&
		       token_tag != PGF_TOKEN_RTRIANGLE &&
		       token_tag != PGF_TOKEN_COLON &&
		       token_tag != PGF_TOKEN_COMMA &&
		       token_tag != PGF_TOKEN_SEMI &&
		       token_tag != PGF_TOKEN_UNKNOWN) {
			hobject arg = parse_arg();
			if (arg == 0)
				return expr;

            expr = b->eapp(expr, arg);
		}

		return expr;
	}
}

bool PgfExprParser::parse_hypos(size_t *n_hypos, PgfTypeHypo **hypos)
{
	PgfText *var;
	PgfBindType bind_type = PGF_BIND_TYPE_EXPLICIT;

	for (;;) {
		if (bind_type == PGF_BIND_TYPE_EXPLICIT &&
		    token_tag == PGF_TOKEN_LCURLY) {
			bind_type = PGF_BIND_TYPE_IMPLICIT;
			token();
		}

        PgfText *var;
		if (token_tag == PGF_TOKEN_IDENT) {
			var = token_value;
		} else if (token_tag == PGF_TOKEN_WILD) {
			var = &wildcard;
		} else {
			return false;
		}

        *hypos = (PgfTypeHypo*) realloc(*hypos, sizeof(PgfTypeHypo)*(*n_hypos+1));
        PgfTypeHypo *bt = *hypos+*n_hypos;
        bt->bind_type = bind_type;
        bt->cid  = textdup(var);
        bt->type = 0;
        (*n_hypos)++;

        token();

		if (bind_type == PGF_BIND_TYPE_IMPLICIT &&
			token_tag == PGF_TOKEN_RCURLY) {
			bind_type = PGF_BIND_TYPE_EXPLICIT;
			token();
		}

		if (token_tag != PGF_TOKEN_COMMA) {
			break;
		}

		token();
	}

	if (bind_type == PGF_BIND_TYPE_IMPLICIT)
		return false;

	return true;
}

hobject PgfExprParser::parse_type()
{
    hobject type = 0;

    size_t n_hypos = 0;
    PgfTypeHypo *hypos = NULL;

    PgfText *cat = NULL;

    size_t n_args = 0;
    hobject *args = NULL;

	for (;;) {
		if (token_tag == PGF_TOKEN_LPAR) {
			token();

            size_t n_start = n_hypos;

			if ((token_tag == PGF_TOKEN_IDENT &&
			     (lookahead(',') || 
				  lookahead(':'))) ||
				(token_tag == PGF_TOKEN_LCURLY) ||
				(token_tag == PGF_TOKEN_WILD)) {

				if (!parse_hypos(&n_hypos, &hypos))
					goto exit;

				if (token_tag != PGF_TOKEN_COLON)
					goto exit;

				token();
			} else {
                hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
                PgfTypeHypo *bt = &hypos[n_hypos];
                bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
                bt->cid  = textdup(&wildcard);
                bt->type = 0;
                n_hypos++;
			}

            size_t n_end = n_hypos;

			hobject type = parse_type();
			if (type == 0)
				goto exit;

			if (token_tag != PGF_TOKEN_RPAR)
				goto exit;

			token();

			if (token_tag != PGF_TOKEN_RARROW)
				goto exit;

			token();

			for (size_t i = n_start; i < n_end; i++) {
				hypos[i].type = type;
			}
		} else if (token_tag == PGF_TOKEN_IDENT) {
            cat = textdup(token_value);

            token();

            while (token_tag != PGF_TOKEN_EOF &&
                   token_tag != PGF_TOKEN_RPAR &&
                   token_tag != PGF_TOKEN_RTRIANGLE &&
                   token_tag != PGF_TOKEN_RARROW) {
                hobject arg = parse_arg();
                if (arg == 0)
                    break;

                args = (hobject*) realloc(args, sizeof(hobject)*(n_args+1));
                args[n_args++] = arg;
            }

			if (token_tag != PGF_TOKEN_RARROW)
				break;

            hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
            PgfTypeHypo *bt = &hypos[n_hypos];
            bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
            bt->cid  = textdup(&wildcard);
            bt->type = b->dtyp(0,NULL,cat,n_args,args);
            n_hypos++;

            while (n_args > 0) {
                b->free_ref(args[--n_args]);
            }

            free(args); args = NULL;
            free(cat);

			token();
		} else {
            goto exit;
        }
	}

	type = b->dtyp(n_hypos,hypos,cat,n_args,args);

exit:
    while (n_hypos > 0) {
        PgfTypeHypo *hypo = &hypos[--n_hypos];
        b->free_ref(hypo->type);
        free(hypo->cid);
    }
    free(hypos);

    free(cat);

    while (n_args > 0) {
        b->free_ref(args[--n_args]);
    }
    free(args);

    return type;
}

PgfExprPrinter::PgfExprPrinter(PgfBridge *bridge)
{
    b = bridge;
}

void PgfExprPrinter::put_lit(hobject lit)
{
    b->match_lit(this, lit);
}

void PgfExprPrinter::put_expr(hobject expr)
{
    b->match_expr(this, expr);
}

void PgfExprPrinter::put_type(hobject ty)
{
    b->match_type(this, ty);
}

void PgfExprPrinter::put_str(const char* s)
{
    printf("%s",s);
}

void PgfExprPrinter::put_text(PgfText *text)
{
    printf("%s",text->text);
}

hobject PgfExprPrinter::eabs(PgfBindType btype, PgfText *name, hobject body)
{
    put_str("\\");
    if (btype == PGF_BIND_TYPE_IMPLICIT)
        put_str("{");
    put_text(name);
    if (btype == PGF_BIND_TYPE_IMPLICIT)
        put_str("}");
    put_str(" -> ");
    put_expr(body);
    return 0;
}

hobject PgfExprPrinter::eapp(hobject fun, hobject arg)
{
    put_expr(fun);
    put_str(" ");
    put_expr(arg);
    return 0;
}

hobject PgfExprPrinter::elit(hobject lit)
{
    return 0;
}

hobject PgfExprPrinter::emeta(PgfMetaId meta)
{
    return 0;
}

hobject PgfExprPrinter::efun(PgfText *name)
{
    put_text(name);
    return 0;
}

hobject PgfExprPrinter::evar(int index)
{
    return 0;
}

hobject PgfExprPrinter::etyped(hobject expr, hobject typ)
{
    return 0;
}

hobject PgfExprPrinter::eimplarg(hobject expr)
{
    return 0;
}

hobject PgfExprPrinter::lint(int v)
{
    return 0;
}

hobject PgfExprPrinter::lflt(double v)
{
    return 0;
}

hobject PgfExprPrinter::lstr(PgfText *v)
{
    return 0;
}

hobject PgfExprPrinter::dtyp(int n_hypos, PgfTypeHypo *hypos,
                             PgfText *cat,
                             int n_exprs, hobject *exprs)
{
    return 0;
}

void PgfExprPrinter::match_lit(PgfBridge *bridge, hobject lit)
{
}

void PgfExprPrinter::match_expr(PgfBridge *bridge, hobject expr)
{
}

void PgfExprPrinter::match_type(PgfBridge *bridge, hobject type)
{
}

void PgfExprPrinter::free_ref(hobject x)
{
}

void PgfExprPrinter::free_me()
{
}

