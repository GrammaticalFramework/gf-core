#include "pgf/data.h"
#include <math.h>

PgfLiteral PgfDBMarshaller::match_lit(PgfUnmarshaller *u, PgfLiteral l)
{
    switch (ref<PgfLiteral>::get_tag(l)) {
    case PgfLiteralInt::tag: {
        auto lint = ref<PgfLiteralInt>::untagged(l);
        return u->lint(lint->size, lint->val);
    }
    case PgfLiteralFlt::tag: {
        return u->lflt(ref<PgfLiteralFlt>::untagged(l)->val);
    }
    case PgfLiteralStr::tag: {
        return u->lstr(&ref<PgfLiteralStr>::untagged(l)->val);
    }
	default:
		throw pgf_error("Unknown literal tag");
    }
}

PgfExpr PgfDBMarshaller::match_expr(PgfUnmarshaller *u, PgfExpr e)
{
    switch (ref<PgfExpr>::get_tag(e)) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(e);
        PgfExpr body = match_expr(u,eabs->body);
        PgfExpr res = u->eabs(eabs->bind_type,&eabs->name,body);
        u->free_ref(body);
        return res;
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(e);
        PgfExpr fun = match_expr(u,eapp->fun);
        PgfExpr arg = match_expr(u,eapp->arg);
        PgfExpr res = u->eapp(fun,arg);
        u->free_ref(arg);
        u->free_ref(fun);
        return res;
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(e);
        PgfLiteral lit = match_lit(u,elit->lit);
        PgfExpr res = u->elit(lit);
        u->free_ref(lit);
        return res;
    }
    case PgfExprMeta::tag: {
        return u->emeta(ref<PgfExprMeta>::untagged(e)->id);
    }
	case PgfExprFun::tag: {
        return u->efun(&ref<PgfExprFun>::untagged(e)->name);
    }
	case PgfExprVar::tag: {
        return u->evar(ref<PgfExprVar>::untagged(e)->var);
	}
	case PgfExprTyped::tag: {
        auto etyped = ref<PgfExprTyped>::untagged(e);
        PgfExpr expr = match_expr(u,etyped->expr);
        PgfType type = match_type(u,etyped->type.as_object());
        PgfExpr res = u->etyped(expr,type);
        u->free_ref(type);
        u->free_ref(expr);
        return res;
	}
	case PgfExprImplArg::tag: {
        auto eimpl = ref<PgfExprImplArg>::untagged(e);
        PgfExpr expr = match_expr(u,eimpl->expr);
        PgfExpr res = u->eimplarg(expr);
        u->free_ref(expr);
        return res;
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

PGF_INTERNAL
PgfType PgfDBMarshaller::match_type(PgfUnmarshaller *u, PgfType ty)
{
    ref<PgfDTyp> tp = ty;

    PgfTypeHypo *hypos = (PgfTypeHypo *)
        alloca(tp->hypos->len * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < tp->hypos->len; i++) {
        hypos[i].bind_type = tp->hypos->data[i].bind_type;
        hypos[i].cid = &(*tp->hypos->data[i].cid);
        hypos[i].type = match_type(u, tp->hypos->data[i].type.as_object());
    }

    PgfExpr *exprs = (PgfExpr *)
        alloca(tp->exprs->len * sizeof(PgfExpr));
    for (size_t i = 0; i < tp->exprs->len; i++) {
        exprs[i] = match_expr(u, tp->exprs->data[i]);
    }

    PgfType res = u->dtyp(tp->hypos->len, hypos,
                          &tp->name,
                          tp->exprs->len, exprs);

    for (size_t i = 0; i < tp->hypos->len; i++) {
        u->free_ref(hypos[i].type);
    }

    for (size_t i = 0; i < tp->exprs->len; i++) {
        u->free_ref(exprs[i]);
    }

    return res;
}

PgfExpr PgfDBUnmarshaller::eabs(PgfBindType bind_type, PgfText *name, PgfExpr body)
{
    ref<PgfExprAbs> eabs =
        PgfDB::malloc<PgfExprAbs>(name->size+1);
    eabs->bind_type = bind_type;
    eabs->body = m->match_expr(this, body);
    memcpy(&eabs->name, name, sizeof(PgfText)+name->size+1);
    return ref<PgfExprAbs>::tagged(eabs);
}

PgfExpr PgfDBUnmarshaller::eapp(PgfExpr fun, PgfExpr arg)
{
    ref<PgfExprApp> eapp = PgfDB::malloc<PgfExprApp>();
    eapp->fun = m->match_expr(this, fun);
	eapp->arg = m->match_expr(this, arg);
    return ref<PgfExprApp>::tagged(eapp);
}

PgfExpr PgfDBUnmarshaller::elit(PgfLiteral lit)
{
    ref<PgfExprLit> elit = PgfDB::malloc<PgfExprLit>();
    elit->lit = m->match_lit(this, lit);
    return ref<PgfExprLit>::tagged(elit);
}

PgfExpr PgfDBUnmarshaller::emeta(PgfMetaId meta_id)
{
    ref<PgfExprMeta> emeta = PgfDB::malloc<PgfExprMeta>();
	emeta->id = meta_id;
	return ref<PgfExprMeta>::tagged(emeta);
}

PgfExpr PgfDBUnmarshaller::efun(PgfText *name)
{
    ref<PgfExprFun> efun =
        PgfDB::malloc<PgfExprFun>(name->size+1);
    memcpy(&efun->name, name, sizeof(PgfText)+name->size+1);
    return ref<PgfExprFun>::tagged(efun);
}

PgfExpr PgfDBUnmarshaller::evar(int index)
{
    ref<PgfExprVar> evar = PgfDB::malloc<PgfExprVar>();
    evar->var = index;
    return ref<PgfExprVar>::tagged(evar);
}

PgfExpr PgfDBUnmarshaller::etyped(PgfExpr expr, PgfType ty)
{
    ref<PgfExprTyped> etyped = PgfDB::malloc<PgfExprTyped>();
    etyped->expr = m->match_expr(this, expr);
    etyped->type = m->match_type(this, ty);
    return ref<PgfExprTyped>::tagged(etyped);
}

PgfExpr PgfDBUnmarshaller::eimplarg(PgfExpr expr)
{
    ref<PgfExprImplArg> eimpl = current_db->malloc<PgfExprImplArg>();
	eimpl->expr = m->match_expr(this, expr);
    return ref<PgfExprImplArg>::tagged(eimpl);
}

PgfLiteral PgfDBUnmarshaller::lint(size_t size, uintmax_t *val)
{
    ref<PgfLiteralInt> lit_int =
        PgfDB::malloc<PgfLiteralInt>(size*sizeof(uintmax_t));
    lit_int->size = size;
    memcpy(&lit_int->val, val, size*sizeof(uintmax_t));
    return ref<PgfLiteralInt>::tagged(lit_int);
}

PgfLiteral PgfDBUnmarshaller::lflt(double val)
{
    ref<PgfLiteralFlt> lit_flt = PgfDB::malloc<PgfLiteralFlt>();
    lit_flt->val = val;
    return ref<PgfLiteralFlt>::tagged(lit_flt);
}

PgfLiteral PgfDBUnmarshaller::lstr(PgfText *val)
{
    ref<PgfLiteralStr> lit_str =
        PgfDB::malloc<PgfLiteralStr>(val->size+1);
    memcpy(&lit_str->val, val, sizeof(PgfText)+val->size+1);
    return ref<PgfLiteralStr>::tagged(lit_str);
}

PgfType PgfDBUnmarshaller::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                PgfText *cat,
                                size_t n_exprs, PgfExpr *exprs)
{
    ref<PgfDTyp> ty =
        PgfDB::malloc<PgfDTyp>(cat->size+1);
    memcpy(&ty->name, cat, sizeof(PgfText)+cat->size+1);
    ty->hypos = vector_new<PgfHypo>(n_hypos);
    for (size_t i = 0; i < n_hypos; i++) {
        ref<PgfHypo> hypo = vector_elem(ty->hypos,i);
        hypo->bind_type = hypos[i].bind_type;
        hypo->cid = textdup_db(hypos[i].cid);
        hypo->type = m->match_type(this, hypos[i].type);
    }
    ty->exprs = vector_new<PgfExpr>(n_exprs);
    for (size_t i = 0; i < n_exprs; i++) {
        *vector_elem(ty->exprs,i) = m->match_expr(this, exprs[i]);
    }

    return ty.as_object();
}

void PgfDBUnmarshaller::free_ref(object x)
{
    PgfDB::free(ref<void>::untagged(x));
}

PgfExprParser::PgfExprParser(PgfText *input, PgfUnmarshaller *unmarshaller)
{
    inp = input;
    pos = (const char*) &inp->text;
	ch  = ' ';
    u   = unmarshaller;
    token_pos = NULL;
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

PGF_INTERNAL bool
pgf_is_ident_first(uint32_t ucs)
{
	return (ucs == '_') ||
           (ucs >= 'a' && ucs <= 'z') ||
           (ucs >= 'A' && ucs <= 'Z') ||
           (ucs >= 192 && ucs <= 255 && ucs != 247 && ucs != 215);
}

PGF_INTERNAL bool
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
        case '\'':
            putc('\'');
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
    if (token_value != NULL)
        free(token_value);

	token_tag   = PGF_TOKEN_UNKNOWN;
    token_pos   = pos;
	token_value = NULL;

	while (isspace(ch)) {
        token_pos   = pos;
		ch = getc();
	}

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
		} else if (isdigit(ch)) {
            putc('-');
            goto digit;
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
digit:
			do {
				putc(ch);
				ch = getc();
			} while (isdigit(ch));

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

PgfExpr PgfExprParser::parse_term()
{
	switch (token_tag) {
	case PGF_TOKEN_LPAR: {
		token();
		PgfExpr expr = parse_expr();
		if (token_tag == PGF_TOKEN_RPAR) {
			token();
			return expr;
		} else {
            if (expr != 0)
                u->free_ref(expr);
			return 0;
		}
	}
	case PGF_TOKEN_LTRIANGLE: {
		token();
		PgfExpr expr = parse_expr();
		if (expr == 0)
			return 0;

		if (token_tag != PGF_TOKEN_COLON) {
            u->free_ref(expr);
			return 0;
		}
		token();

		PgfType type = parse_type();
		if (type == 0) {
            u->free_ref(expr);
			return 0;
        }

		if (token_tag != PGF_TOKEN_RTRIANGLE) {
            u->free_ref(expr);
            u->free_ref(type);
			return 0;
		}
		token();

		PgfExpr e = u->etyped(expr,type);
        u->free_ref(expr);
        u->free_ref(type);
        return e;
	}
	case PGF_TOKEN_QUESTION: {
		token();

		PgfMetaId id = 0;
		if (token_tag == PGF_TOKEN_INT) {
			id = atoi((const char*) &token_value->text);
			token();
		}
		return u->emeta(id);
	}
	case PGF_TOKEN_IDENT: {
        PgfExpr e = u->efun(token_value);
		token();
        return e;
	}
	case PGF_TOKEN_INT: {
        size_t size = (token_value->size + LINT_BASE_LOG - 1)/LINT_BASE_LOG;
        uintmax_t *value = (uintmax_t *) alloca(size*sizeof(uintmax_t));
        char *p = token_value->text + token_value->size;
        for (size_t i = size; i > 0; i--) {
            char tmp = *p; *p = 0;

            char *s = p - LINT_BASE_LOG;
            if (s < token_value->text)
                s = token_value->text;
            value[i-1] = (uintmax_t)
                (s == token_value->text) ? strtoll(s, NULL, 10)
                                         : strtoull(s, NULL, 10);

            *p = tmp;
            p  = s;
        }
        PgfLiteral lit = u->lint(size, value);
        PgfExpr e = u->elit(lit);
        u->free_ref(lit);
		token();
		return e;
	}
	case PGF_TOKEN_STR: {
        PgfLiteral lit = u->lstr(token_value);
        PgfExpr e = u->elit(lit);
        u->free_ref(lit);
		token();
		return e;
	}
	case PGF_TOKEN_FLT: {
		double d = atof((const char*) &token_value->text);
        PgfLiteral lit = u->lflt(d);
        PgfExpr e = u->elit(lit);
        u->free_ref(lit);
		token();
		return e;
	}
	default:
		return 0;
	}
}

PgfExpr PgfExprParser::parse_arg()
{
	PgfExpr arg;

	if (token_tag == PGF_TOKEN_LCURLY) {
		token();

		arg = parse_expr();
		if (arg == 0)
			return 0;

		if (token_tag != PGF_TOKEN_RCURLY) {
            u->free_ref(arg);
			return 0;
		}
		token();

		PgfExpr e = u->eimplarg(arg);
        u->free_ref(arg);
        arg = e;
	} else {
		arg = parse_term();
	}

	return arg;
}

PGF_INTERNAL PgfText wildcard = {size: 1, text: {'_',0}};

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
        memcpy(&bind->var, var, sizeof(PgfText)+var->size+1);
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

PgfExpr PgfExprParser::parse_expr()
{
    PgfExpr expr;

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
            PgfExpr abs_expr = u->eabs(bs->bind_type, &bs->var, expr);
            u->free_ref(expr);
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
			PgfExpr arg = parse_arg();
			if (arg == 0)
				return expr;

            expr = u->eapp(expr, arg);
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

PgfType PgfExprParser::parse_type()
{
    PgfType type = 0;

    size_t n_hypos = 0;
    PgfTypeHypo *hypos = NULL;

    PgfText *cat = NULL;

    size_t n_args = 0;
    PgfType *args = NULL;

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

			PgfType type = parse_type();
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
                PgfExpr arg = parse_arg();
                if (arg == 0)
                    break;

                args = (PgfExpr*) realloc(args, sizeof(PgfExpr)*(n_args+1));
                args[n_args++] = arg;
            }

			if (token_tag != PGF_TOKEN_RARROW)
				break;

            hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
            PgfTypeHypo *bt = &hypos[n_hypos];
            bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
            bt->cid  = textdup(&wildcard);
            bt->type = u->dtyp(0,NULL,cat,n_args,args);
            n_hypos++;

            while (n_args > 0) {
                u->free_ref(args[--n_args]);
            }

            free(args); args = NULL;
            free(cat);

			token();
		} else {
            goto exit;
        }
	}

	type = u->dtyp(n_hypos,hypos,cat,n_args,args);

exit:
    while (n_hypos > 0) {
        PgfTypeHypo *hypo = &hypos[--n_hypos];
        u->free_ref(hypo->type);
        free(hypo->cid);
    }
    free(hypos);

    free(cat);

    while (n_args > 0) {
        u->free_ref(args[--n_args]);
    }
    free(args);

    return type;
}

PgfExpr PgfExprProbEstimator::eabs(PgfBindType bind_type, PgfText *name, PgfExpr body)
{
    m->match_expr(this, body);
    return 0;
}

PgfExpr PgfExprProbEstimator::eapp(PgfExpr fun, PgfExpr arg)
{
    m->match_expr(this, fun);
    m->match_expr(this, arg);
    return 0;
}

PgfExpr PgfExprProbEstimator::elit(PgfLiteral lit)
{
    return 0;
}

PgfExpr PgfExprProbEstimator::emeta(PgfMetaId meta_id)
{
    return 0;
}

PgfExpr PgfExprProbEstimator::efun(PgfText *name)
{
    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->abstract.funs, name);
    if (absfun == 0)
        prob = INFINITY;
    else
        prob += absfun->ep.prob;

    return 0;
}

PgfExpr PgfExprProbEstimator::evar(int index)
{
    return 0;
}

PgfExpr PgfExprProbEstimator::etyped(PgfExpr expr, PgfType ty)
{
    m->match_expr(this, expr);
    return 0;
}

PgfExpr PgfExprProbEstimator::eimplarg(PgfExpr expr)
{
    m->match_expr(this, expr);
    return 0;
}

PgfLiteral PgfExprProbEstimator::lint(size_t size, uintmax_t *val)
{
    return 0;
}

PgfLiteral PgfExprProbEstimator::lflt(double val)
{
    return 0;
}

PgfLiteral PgfExprProbEstimator::lstr(PgfText *val)
{
    return 0;
}

PgfType PgfExprProbEstimator::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                   PgfText *cat,
                                   size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

void PgfExprProbEstimator::free_ref(object x)
{
}

PGF_INTERNAL
void pgf_literal_free(PgfLiteral literal)
{
    switch (ref<PgfLiteral>::get_tag(literal)) {
    case PgfLiteralInt::tag: {
        PgfDB::free(ref<PgfLiteralInt>::untagged(literal));
        break;
    }
    case PgfLiteralFlt::tag: {
        PgfDB::free(ref<PgfLiteralFlt>::untagged(literal));
        break;
    }
    case PgfLiteralStr::tag: {
        PgfDB::free(ref<PgfLiteralStr>::untagged(literal));
        break;
    }
	default:
		throw pgf_error("Unknown literal tag");
    }
}

PGF_INTERNAL
void pgf_expr_free(PgfExpr expr)
{
    switch (ref<PgfExpr>::get_tag(expr)) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(expr);
        pgf_expr_free(eabs->body);
        PgfDB::free(eabs);
        break;
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(expr);
        pgf_expr_free(eapp->fun);
        pgf_expr_free(eapp->arg);
        PgfDB::free(eapp);
        break;
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(expr);
        pgf_literal_free(elit->lit);
        PgfDB::free(elit);
        break;
    }
    case PgfExprMeta::tag: {
        PgfDB::free(ref<PgfExprMeta>::untagged(expr));
        break;
    }
	case PgfExprFun::tag: {
        PgfDB::free(ref<PgfExprFun>::untagged(expr));
        break;
    }
	case PgfExprVar::tag: {
        PgfDB::free(ref<PgfExprVar>::untagged(expr));
        break;
	}
	case PgfExprTyped::tag: {
        auto etyped = ref<PgfExprTyped>::untagged(expr);
        pgf_expr_free(etyped->expr);
        pgf_type_free(etyped->type);
        PgfDB::free(etyped);
        break;
	}
	case PgfExprImplArg::tag: {
        auto eimpl = ref<PgfExprImplArg>::untagged(expr);
        pgf_expr_free(eimpl->expr);
        PgfDB::free(eimpl);
        break;
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

PGF_INTERNAL
void pgf_context_free(ref<PgfVector<PgfHypo>> hypos)
{
    for (size_t i = 0; i < hypos->len; i++) {
        PgfDB::free(vector_elem(hypos, i)->cid);
        pgf_type_free(vector_elem(hypos, i)->type);
    }

    PgfDB::free(hypos);
}

PGF_INTERNAL
void pgf_type_free(ref<PgfDTyp> dtyp)
{
    pgf_context_free(dtyp->hypos);

    for (size_t i = 0; i < dtyp->exprs->len; i++) {
        pgf_expr_free(*vector_elem(dtyp->exprs, i));
    }
    PgfDB::free(dtyp->exprs);

    PgfDB::free(dtyp);
}

PGF_INTERNAL
void pgf_patt_free(PgfPatt patt)
{
    switch (ref<PgfPatt>::get_tag(patt)) {
    case PgfPattApp::tag: {
        auto papp = ref<PgfPattApp>::untagged(patt);
        PgfDB::free(papp->ctor);
        for (size_t i = 0; i < papp->args.len; i++) {
            PgfPatt patt = *vector_elem(ref<PgfVector<PgfPatt>>::from_ptr(&papp->args), i);
            pgf_patt_free(patt);
        }
        PgfDB::free(papp);
        break;
    }
	case PgfPattVar::tag: {
        PgfDB::free(ref<PgfPattVar>::untagged(patt));
        break;
	}
	case PgfPattAs::tag: {
        auto pas = ref<PgfPattAs>::untagged(patt);
        pgf_patt_free(pas->patt);
        PgfDB::free(pas);
        break;
	}
	case PgfPattWild::tag: {
        PgfDB::free(ref<PgfPattWild>::untagged(patt));
        break;
	}
    case PgfPattLit::tag: {
        auto plit = ref<PgfPattLit>::untagged(patt);
        pgf_literal_free(plit->lit);
        PgfDB::free(plit);
        break;
    }
	case PgfPattImplArg::tag: {
        auto pimpl = ref<PgfPattImplArg>::untagged(patt);
        pgf_patt_free(pimpl->patt);
        PgfDB::free(pimpl);
        break;
	}
	case PgfPattTilde::tag: {
        auto ptilde = ref<PgfPattTilde>::untagged(patt);
        pgf_patt_free(ptilde->expr);
        PgfDB::free(ptilde);
        break;
	}
	default:
		throw pgf_error("Unknown pattern tag");
    }
}
