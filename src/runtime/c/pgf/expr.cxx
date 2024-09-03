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
        alloca(tp->hypos.size() * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < tp->hypos.size(); i++) {
        hypos[i].bind_type = tp->hypos[i].bind_type;
        hypos[i].cid = &(*tp->hypos[i].cid);
        hypos[i].type = match_type(u, tp->hypos[i].type.as_object());
    }

    PgfExpr *exprs = (PgfExpr *)
        alloca(tp->exprs.size() * sizeof(PgfExpr));
    for (size_t i = 0; i < tp->exprs.size(); i++) {
        exprs[i] = match_expr(u, tp->exprs[i]);
    }

    PgfType res = u->dtyp(tp->hypos.size(), hypos,
                          &tp->name,
                          tp->exprs.size(), exprs);

    for (size_t i = 0; i < tp->hypos.size(); i++) {
        u->free_ref(hypos[i].type);
    }

    for (size_t i = 0; i < tp->exprs.size(); i++) {
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
    return eabs.tagged();
}

PgfExpr PgfDBUnmarshaller::eapp(PgfExpr fun, PgfExpr arg)
{
    ref<PgfExprApp> eapp = PgfDB::malloc<PgfExprApp>();
    eapp->fun = m->match_expr(this, fun);
	eapp->arg = m->match_expr(this, arg);
    return eapp.tagged();
}

PgfExpr PgfDBUnmarshaller::elit(PgfLiteral lit)
{
    ref<PgfExprLit> elit = PgfDB::malloc<PgfExprLit>();
    elit->lit = m->match_lit(this, lit);
    return elit.tagged();
}

PgfExpr PgfDBUnmarshaller::emeta(PgfMetaId meta_id)
{
    ref<PgfExprMeta> emeta = PgfDB::malloc<PgfExprMeta>();
	emeta->id = meta_id;
	return emeta.tagged();
}

PgfExpr PgfDBUnmarshaller::efun(PgfText *name)
{
    ref<PgfExprFun> efun =
        PgfDB::malloc<PgfExprFun>(name->size+1);
    memcpy(&efun->name, name, sizeof(PgfText)+name->size+1);
    return efun.tagged();
}

PgfExpr PgfDBUnmarshaller::evar(int index)
{
    ref<PgfExprVar> evar = PgfDB::malloc<PgfExprVar>();
    evar->var = index;
    return evar.tagged();
}

PgfExpr PgfDBUnmarshaller::etyped(PgfExpr expr, PgfType ty)
{
    ref<PgfExprTyped> etyped = PgfDB::malloc<PgfExprTyped>();
    etyped->expr = m->match_expr(this, expr);
    etyped->type = m->match_type(this, ty);
    return etyped.tagged();
}

PgfExpr PgfDBUnmarshaller::eimplarg(PgfExpr expr)
{
    ref<PgfExprImplArg> eimpl = current_db->malloc<PgfExprImplArg>();
	eimpl->expr = m->match_expr(this, expr);
    return eimpl.tagged();
}

PgfLiteral PgfDBUnmarshaller::lint(size_t size, uintmax_t *val)
{
    ref<PgfLiteralInt> lit_int =
        PgfDB::malloc<PgfLiteralInt>(size*sizeof(uintmax_t));
    lit_int->size = size;
    memcpy(&lit_int->val, val, size*sizeof(uintmax_t));
    return lit_int.tagged();
}

PgfLiteral PgfDBUnmarshaller::lflt(double val)
{
    ref<PgfLiteralFlt> lit_flt = PgfDB::malloc<PgfLiteralFlt>();
    lit_flt->val = val;
    return lit_flt.tagged();
}

PgfLiteral PgfDBUnmarshaller::lstr(PgfText *val)
{
    ref<PgfLiteralStr> lit_str =
        PgfDB::malloc<PgfLiteralStr>(val->size+1);
    memcpy(&lit_str->val, val, sizeof(PgfText)+val->size+1);
    return lit_str.tagged();
}

PgfType PgfDBUnmarshaller::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                PgfText *cat,
                                size_t n_exprs, PgfExpr *exprs)
{
    ref<PgfDTyp> ty =
        PgfDB::malloc<PgfDTyp>(cat->size+1);
    memcpy(&ty->name, cat, sizeof(PgfText)+cat->size+1);
    vector<PgfHypo> new_hypos = vector<PgfHypo>::alloc(n_hypos);
    ty->hypos = new_hypos;
    for (size_t i = 0; i < n_hypos; i++) {
        ref<PgfHypo> hypo = ty->hypos.elem(i);
        hypo->bind_type = hypos[i].bind_type;
        hypo->cid = textdup_db(hypos[i].cid);
        PgfType type = m->match_type(this, hypos[i].type);
        hypo->type = type;
    }
    vector<PgfExpr> new_exprs = vector<PgfExpr>::alloc(n_exprs);
    ty->exprs = new_exprs;
    for (size_t i = 0; i < n_exprs; i++) {
        PgfExpr expr = m->match_expr(this, exprs[i]);
        ty->exprs[i] = expr;
    }

    return ty.as_object();
}

void PgfDBUnmarshaller::free_ref(object expr)
{
}

PgfLiteral PgfInternalMarshaller::match_lit(PgfUnmarshaller *u, PgfLiteral l)
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

PgfExpr PgfInternalMarshaller::match_expr(PgfUnmarshaller *u, PgfExpr e)
{
    switch (ref<PgfExpr>::get_tag(e)) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(e);
        return u->eabs(eabs->bind_type,&eabs->name,eabs->body);
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(e);
        return u->eapp(eapp->fun,eapp->arg);
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(e);
        return u->elit(elit->lit);
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
        return u->etyped(etyped->expr,etyped->type.as_object());
	}
	case PgfExprImplArg::tag: {
        auto eimpl = ref<PgfExprImplArg>::untagged(e);
        return u->eimplarg(eimpl->expr);
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

PGF_INTERNAL
PgfType PgfInternalMarshaller::match_type(PgfUnmarshaller *u, PgfType ty)
{
    ref<PgfDTyp> tp = ty;

    PgfTypeHypo *hypos = (PgfTypeHypo *)
        alloca(tp->hypos.size() * sizeof(PgfTypeHypo));
    for (size_t i = 0; i < tp->hypos.size(); i++) {
        hypos[i].bind_type = tp->hypos[i].bind_type;
        hypos[i].cid = &(*tp->hypos[i].cid);
        hypos[i].type = tp->hypos[i].type.as_object();
    }

    return  u->dtyp(tp->hypos.size(), hypos,
                    &tp->name,
                    tp->exprs.size(), tp->exprs.get_data());
}

PgfExprParser::PgfExprParser(PgfText *input, PgfUnmarshaller *unmarshaller)
{
    inp = input;
    pos = (const char*) &inp->text;
	ch  = ' ';
    u   = unmarshaller;
    token_pos = NULL;
    token_value = NULL;
    bs = NULL;

	token();
}

PgfExprParser::~PgfExprParser()
{
    free(token_value);
}

bool PgfExprParser::getc()
{
    if (pos - ((const char*) &inp->text) >= inp->size) {
        ch = ' ';
        return false;
    }

    ch = pgf_utf8_decode((const uint8_t **) &pos);
    return true;
}

void PgfExprParser::putc(uint32_t ucs)
{
    size_t ucs_len =
             (ucs < 0x80 ? 1 :
	          ucs < 0x800 ? 2 :
	          ucs < 0x10000 ? 3 :
	          ucs < 0x200000 ? 4 :
	          ucs < 0x4000000 ? 5 :
                                6
	         );
    size_t len = token_value ? token_value->size : 0;

    token_value = (PgfText*)
        realloc(token_value, sizeof(PgfText)+len+ucs_len+1);
    uint8_t *p = (uint8_t*) (token_value->text+len);
    pgf_utf8_encode(ucs, &p);
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

bool PgfExprParser::str_char()
{
    if (ch == '\\') {
        if (!getc())
            return false;

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
        case '0':
            putc('\0');
        default:
            return false;
        }
    } else {
        putc(ch);
    }
    return getc();
}

void PgfExprParser::token()
{
    if (token_value != NULL)
        free(token_value);

	token_tag   = PGF_TOKEN_UNKNOWN;
    token_pos   = pos;
	token_value = NULL;

	while (pgf_utf8_is_space(ch)) {
        token_pos   = pos;
        if (!getc()) {
            token_tag = PGF_TOKEN_EOF;
            return;
        }
	}

	switch (ch) {
	case '(':
		getc();
		token_tag = PGF_TOKEN_LPAR;
		break;
	case ')':
		getc();
		token_tag = PGF_TOKEN_RPAR;
		break;
	case '{':
		getc();
		token_tag = PGF_TOKEN_LCURLY;
		break;
	case '}':
		getc();
		token_tag = PGF_TOKEN_RCURLY;
		break;
	case '<':
		getc();
		token_tag = PGF_TOKEN_LTRIANGLE;
		break;
	case '>':
		getc();
		token_tag = PGF_TOKEN_RTRIANGLE;
		break;
	case '?':
		getc();
		token_tag = PGF_TOKEN_QUESTION;
		break;
	case '\\':
		getc();
		token_tag = PGF_TOKEN_LAMBDA;
		break;
	case '-':
        if (getc()) {
            if (ch == '>') {
                getc();
                token_tag = PGF_TOKEN_RARROW;
            } else if (pgf_utf8_is_digit(ch)) {
                putc('-');
                goto digit;
            }
        }
		break;
	case ',':
		getc();
		token_tag = PGF_TOKEN_COMMA;
		break;
	case ':':
		getc();
		token_tag = PGF_TOKEN_COLON;
		break;
	case ';':
		getc();
		token_tag = PGF_TOKEN_SEMI;
		break;
	case '\'': {
        if (getc()) {
            while (ch != '\'') {
                if (!str_char())
                    break;
            }
            if (ch == '\'') {
                getc();
                token_tag = PGF_TOKEN_IDENT;
                if (token_value == NULL) {
                    token_value = (PgfText*)
                        malloc(sizeof(PgfText)+1);
                    token_value->size    = 0;
                    token_value->text[0] = 0;
                }
            }
        }
		break;
    }
    case '"': {
        if (getc()) {
            while (ch != '"') {
                if (!str_char())
                    break;
            }
            if (ch == '"') {
                getc();
                token_tag = PGF_TOKEN_STR;
                if (token_value == NULL) {
                    token_value = (PgfText*)
                        malloc(sizeof(PgfText)+1);
                    token_value->size    = 0;
                    token_value->text[0] = 0;
                }
            }
        }
        break;
    }
	default: {
		if (pgf_is_ident_first(ch)) {
			do {
				putc(ch);
				if (!getc())
                    break;
			} while (pgf_is_ident_rest(ch));
            if (token_value->size == 1 && strcmp(token_value->text,"_")==0)
                token_tag = PGF_TOKEN_WILD;
            else
                token_tag = PGF_TOKEN_IDENT;
		} else if (pgf_utf8_is_digit(ch)) {
digit:
			do {
				putc(ch);
				if (!getc())
                    break;
			} while (pgf_utf8_is_digit(ch));

            if (ch == '.') {
                putc(ch);
                if (getc()) {
                    while (pgf_utf8_is_digit(ch)) {
                        putc(ch);
                        if (!getc())
                            break;
                    }
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
	while (pgf_utf8_is_space(this->ch)) {
        if (!getc())
            break;
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
        PgfBind *last = bs;
        size_t index = 0;
        while (last != NULL) {
            if (textcmp(&last->var,token_value) == 0) {
                PgfExpr e = u->evar(index);
                token();
                return e;
            }
            last = last->next;
            index++;
        }

        PgfExpr e = u->efun(token_value);
		token();
        return e;
	}
	case PGF_TOKEN_INT: {
        size_t n_digits = token_value->size;
        char  *p_digits = token_value->text;
        if (*p_digits == '-') {
            n_digits--;
            p_digits++;
        }

        size_t size = (n_digits + LINT_BASE_LOG - 1)/LINT_BASE_LOG;
        uintmax_t *value = (uintmax_t *) alloca(size*sizeof(uintmax_t));
        char *p = token_value->text + token_value->size;
        for (size_t i = size; i > 0; i--) {
            char tmp = *p; *p = 0;

            char *s = p - LINT_BASE_LOG;
            if (s <= p_digits)
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

bool PgfExprParser::parse_bind()
{
    PgfBind *last = bs;
	PgfBindType bind_type = PGF_BIND_TYPE_EXPLICIT;

	if (token_tag == PGF_TOKEN_LCURLY) {
		bind_type = PGF_BIND_TYPE_IMPLICIT;
		token();
	}

	for (;;) {
        PgfText *var;
		if (token_tag == PGF_TOKEN_IDENT || token_tag == PGF_TOKEN_WILD) {
			var = token_value;
		} else {
			goto error;
		}

        PgfBind *bind = (PgfBind *) malloc(sizeof(PgfBind)+var->size+1);
        if (bind == NULL)
            goto error;
        bind->bind_type = bind_type;
        bind->next      = bs;
        memcpy(&bind->var, var, sizeof(PgfText)+var->size+1);
        bs = bind;

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

	return true;

error:
    pop_binds(last);
    return false;
}

bool PgfExprParser::parse_binds()
{
    PgfBind *last = bs;
	for (;;) {
        if (!parse_bind()) {
            goto error;
        }

		if (token_tag != PGF_TOKEN_COMMA)
			break;

		token();
	}

	return true;

error:
    pop_binds(last);
    return false;
}

void PgfExprParser::pop_binds(PgfBind *last)
{
    while (bs != last) {
        PgfBind *tmp = bs;
        bs = bs->next;
        free(tmp);
    }
}

PgfExpr PgfExprParser::parse_expr()
{
    PgfExpr expr;

	if (token_tag == PGF_TOKEN_LAMBDA) {
		token();

        PgfBind *last = bs;
		if (!parse_binds())
			return 0;

		if (token_tag != PGF_TOKEN_RARROW) {
			goto error;
		}
		token();

		expr = parse_expr();
		if (expr == 0)
			goto error;

		while (bs != last) {
            PgfExpr abs_expr = u->eabs(bs->bind_type, &bs->var, expr);
            u->free_ref(expr);
            expr = abs_expr;

            PgfBind *tmp = bs;
            bs = bs->next;
            free(tmp);
		}

		return expr;

error:
		pop_binds(last);
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
			PgfExpr fun = expr;
			PgfExpr arg = parse_arg();
			if (arg == 0)
				return expr;

            expr = u->eapp(fun, arg);

            u->free_ref(fun);
            u->free_ref(arg);
		}

		return expr;
	}
}

bool PgfExprParser::parse_hypos(size_t *n_hypos, PgfTypeHypo **hypos,
                                PgfBind **pbs)
{
	PgfText *var;
	PgfBindType bind_type = PGF_BIND_TYPE_EXPLICIT;

    *pbs = bs;
	for (;;) {
		if (bind_type == PGF_BIND_TYPE_EXPLICIT &&
		    token_tag == PGF_TOKEN_LCURLY) {
			bind_type = PGF_BIND_TYPE_IMPLICIT;
			token();
		}

        PgfText *var;
		if (token_tag == PGF_TOKEN_IDENT || token_tag == PGF_TOKEN_WILD) {
			var = token_value;
		} else {
			goto error;
		}

        *hypos = (PgfTypeHypo*) realloc(*hypos, sizeof(PgfTypeHypo)*(*n_hypos+1));
        PgfTypeHypo *bt = *hypos+*n_hypos;
        bt->bind_type = bind_type;
        bt->cid  = textdup(var);
        bt->type = 0;
        (*n_hypos)++;

        PgfBind *bind = (PgfBind *) malloc(sizeof(PgfBind)+var->size+1);
        if (bind == NULL)
            goto error;
        bind->bind_type = bind_type;
        bind->next      = *pbs;
        memcpy(&bind->var, var, sizeof(PgfText)+var->size+1);
        *pbs = bind;

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
		goto error;

	return true;

error:
    while (*pbs != bs) {
        PgfBind *tmp = *pbs;
        *pbs = (*pbs)->next;
        free(tmp);
    }
    return false;
}

static
PgfText *mk_wildcard()
{
    PgfText *t = (PgfText *) malloc(sizeof(PgfText)+2);
    if (t != NULL) {
        t->size = 1;
        t->text[0] = '_';
        t->text[1] = 0;
    }
    return t;
}

PgfType PgfExprParser::parse_type()
{
    PgfType type = 0;

    size_t n_hypos = 0;
    PgfTypeHypo *hypos = NULL;
    PgfBind *last = bs;

    PgfText *cat = NULL;

    size_t n_args = 0;
    PgfExpr *args = NULL;

	for (;;) {
		if (token_tag == PGF_TOKEN_LPAR) {
			token();

            size_t n_start = n_hypos;
            PgfBind *new_bs = bs;

			if ((token_tag == PGF_TOKEN_IDENT &&
			     (lookahead(',') ||
				  lookahead(':'))) ||
				(token_tag == PGF_TOKEN_LCURLY) ||
				(token_tag == PGF_TOKEN_WILD)) {

				if (!parse_hypos(&n_hypos, &hypos, &new_bs))
					goto exit;

				if (token_tag != PGF_TOKEN_COLON) {
                    bs = new_bs; // to be recycled
					goto exit;
                }

				token();
			} else {
                hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
                PgfTypeHypo *bt = &hypos[n_hypos];
                bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
                bt->cid  = mk_wildcard();
                bt->type = 0;
                n_hypos++;
			}

            size_t n_end = n_hypos;

			PgfType type = parse_type();
            bs = new_bs;
			if (type == 0) {
				goto exit;
            }

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
            bt->cid  = mk_wildcard();
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
    PgfType last_type = 0;
    while (n_hypos > 0) {
        PgfTypeHypo *hypo = &hypos[--n_hypos];
        free(hypo->cid);
        if (hypo->type != last_type) {
            u->free_ref(hypo->type);
            last_type = hypo->type;
        }
    }
    free(hypos);

    pop_binds(last);

    free(cat);

    while (n_args > 0) {
        u->free_ref(args[--n_args]);
    }
    free(args);

    return type;
}

PgfTypeHypo *PgfExprParser::parse_context(size_t *p_n_hypos)
{
    size_t n_hypos = 0;
    PgfTypeHypo *hypos = NULL;
    PgfBind *last = bs;

	for (;;) {
		if (token_tag == PGF_TOKEN_LPAR) {
			token();

            size_t n_start = n_hypos;
            PgfBind *new_bs = bs;

			if ((token_tag == PGF_TOKEN_IDENT &&
			     (lookahead(',') ||
				  lookahead(':'))) ||
				(token_tag == PGF_TOKEN_LCURLY) ||
				(token_tag == PGF_TOKEN_WILD)) {

				if (!parse_hypos(&n_hypos, &hypos, &new_bs))
					goto exit;

				if (token_tag != PGF_TOKEN_COLON) {
                    bs = new_bs; // to be recycled
					goto exit;
                }

				token();
			} else {
                hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
                PgfTypeHypo *bt = &hypos[n_hypos];
                bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
                bt->cid  = mk_wildcard();
                bt->type = 0;
                n_hypos++;
			}

            size_t n_end = n_hypos;

			PgfType type = parse_type();
            bs = new_bs;
			if (type == 0)
				goto exit;

			if (token_tag != PGF_TOKEN_RPAR)
				goto exit;

			token();

			for (size_t i = n_start; i < n_end; i++) {
				hypos[i].type = type;
			}
		} else if (token_tag == PGF_TOKEN_IDENT) {
            hypos = (PgfTypeHypo*) realloc(hypos, sizeof(PgfTypeHypo)*(n_hypos+1));
            PgfTypeHypo *bt = &hypos[n_hypos];
            bt->bind_type = PGF_BIND_TYPE_EXPLICIT;
            bt->cid  = mk_wildcard();
            bt->type = u->dtyp(0,NULL,token_value,0,NULL);
            n_hypos++;

            token();
		} else {
            goto exit;
        }
	}

exit:
    pop_binds(last);
    *p_n_hypos = n_hypos;
    return hypos;
}

PgfExpr PgfExprProbEstimator::eabs(PgfBindType bind_type, PgfText *name, PgfExpr body)
{
    m->match_expr(this, body);
    cat_prob = 0;
    return 0;
}

PgfExpr PgfExprProbEstimator::eapp(PgfExpr fun, PgfExpr arg)
{
    prob_t tmp = cat_prob_total;
    cat_prob_total = 0;
    m->match_expr(this, arg);
    cat_prob_total = tmp + cat_prob;
    m->match_expr(this, fun);
    return 0;
}

PgfExpr PgfExprProbEstimator::elit(PgfLiteral lit)
{
    cat_prob = 0;
    return 0;
}

PgfExpr PgfExprProbEstimator::emeta(PgfMetaId meta_id)
{
    prob += cat_prob_total;
    cat_prob = 0;
    return 0;
}

PgfExpr PgfExprProbEstimator::efun(PgfText *name)
{
    ref<PgfAbsFun> absfun =
        namespace_lookup(pgf->abstract.funs, name);
    if (absfun == 0) {
        prob = INFINITY;
        cat_prob = INFINITY;
    } else {
        prob += absfun->prob;

        ref<PgfAbsCat> abscat =
            namespace_lookup(pgf->abstract.cats, &absfun->type->name);
        if (abscat == 0)
            cat_prob = INFINITY;
        else
            cat_prob = abscat->prob;
    }
    return 0;
}

PgfExpr PgfExprProbEstimator::evar(int index)
{
    cat_prob = 0;
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
void pgf_literal_release(PgfLiteral literal)
{
    switch (ref<PgfLiteral>::get_tag(literal)) {
    case PgfLiteralInt::tag: {
        auto lint = ref<PgfLiteralInt>::untagged(literal);
        PgfDB::free(lint, lint->size*sizeof(uintmax_t));
        break;
    }
    case PgfLiteralFlt::tag: {
        PgfDB::free(ref<PgfLiteralFlt>::untagged(literal));
        break;
    }
    case PgfLiteralStr::tag: {
        auto lstr = ref<PgfLiteralStr>::untagged(literal);
        PgfDB::free(lstr, lstr->val.size+1);
        break;
    }
	default:
		throw pgf_error("Unknown literal tag");
    }
}

PGF_INTERNAL
void pgf_expr_release(PgfExpr expr)
{
    switch (ref<PgfExpr>::get_tag(expr)) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(expr);
        pgf_expr_release(eabs->body);
        PgfDB::free(eabs, eabs->name.size+1);
        break;
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(expr);
        pgf_expr_release(eapp->fun);
        pgf_expr_release(eapp->arg);
        PgfDB::free(eapp);
        break;
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(expr);
        pgf_literal_release(elit->lit);
        PgfDB::free(elit);
        break;
    }
    case PgfExprMeta::tag: {
        PgfDB::free(ref<PgfExprMeta>::untagged(expr));
        break;
    }
	case PgfExprFun::tag: {
        auto efun = ref<PgfExprFun>::untagged(expr);
        PgfDB::free(efun, efun->name.size+1);
        break;
    }
	case PgfExprVar::tag: {
        PgfDB::free(ref<PgfExprVar>::untagged(expr));
        break;
	}
	case PgfExprTyped::tag: {
        auto etyped = ref<PgfExprTyped>::untagged(expr);
        pgf_expr_release(etyped->expr);
        pgf_type_release(etyped->type);
        PgfDB::free(etyped);
        break;
	}
	case PgfExprImplArg::tag: {
        auto eimpl = ref<PgfExprImplArg>::untagged(expr);
        pgf_expr_release(eimpl->expr);
        PgfDB::free(eimpl);
        break;
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

PGF_INTERNAL
void pgf_context_release(vector<PgfHypo> hypos)
{
    for (size_t i = 0; i < hypos.size(); i++) {
        text_db_release(hypos[i].cid);
        pgf_type_release(hypos[i].type);
    }

    vector<PgfHypo>::release(hypos);
}

PGF_INTERNAL
void pgf_type_release(ref<PgfDTyp> dtyp)
{
    pgf_context_release(dtyp->hypos);

    for (PgfExpr expr : dtyp->exprs) {
        pgf_expr_release(expr);
    }
    vector<PgfExpr>::release(dtyp->exprs);

    PgfDB::free(dtyp, dtyp->name.size+1);
}
