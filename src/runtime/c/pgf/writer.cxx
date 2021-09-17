#include <math.h>
#include "data.h"
#include "writer.h"

PgfWriter::PgfWriter(FILE *out)
{
    this->out = out;
    this->abstract = 0;
}

void PgfWriter::write_uint8(uint8_t b)
{
    size_t n_items = fwrite(&b, sizeof(b), 1, out);
    if (ferror(out))
        throw pgf_error("an error occured while writing out the grammar");
    if (n_items != 1)
        throw pgf_error("couldn't write to the output file");
}

void PgfWriter::write_u16be(uint16_t u)
{
    uint8_t buf[2] = { (uint8_t) ((u>>8) & 0xFF)
                     , (uint8_t) (u & 0xFF)
                     };

    size_t n_items = fwrite(&buf, sizeof(buf), 1, out);
    if (ferror(out))
        throw pgf_error("an error occured while writing out the grammar");
    if (n_items != 1)
        throw pgf_error("couldn't write to the output file");
}

void PgfWriter::write_u64be(uint64_t u)
{
    uint8_t buf[8] = { (uint8_t) ((u>>56) & 0xFF)
                     , (uint8_t) ((u>>48) & 0xFF)
                     , (uint8_t) ((u>>40) & 0xFF)
                     , (uint8_t) ((u>>32) & 0xFF)
                     , (uint8_t) ((u>>24) & 0xFF)
                     , (uint8_t) ((u>>16) & 0xFF)
                     , (uint8_t) ((u>>8)  & 0xFF)
                     , (uint8_t) (u & 0xFF)
                     };

    size_t n_items = fwrite(&buf, sizeof(buf), 1, out);
    if (ferror(out))
        throw pgf_error("an error occured while writing out the grammar");
    if (n_items != 1)
        throw pgf_error("couldn't write to the output file");
}

void PgfWriter::write_double(double d)
{
   	int sign = signbit(d) > 0;
	unsigned rawexp;
	uint64_t mantissa;

	switch (fpclassify(d)) {
	case FP_NAN:
		rawexp   = 0x7ff;
		mantissa = 1;
		break;
	case FP_INFINITE:
		rawexp   = 0x7ff;
		mantissa = 0;
		break;
	default: {
		int exp;
		mantissa = (uint64_t) scalbn(frexp(d, &exp), 53);
		mantissa &= ~ (1ULL << 52);
		exp -= 53;

		rawexp = exp + 1075;
    }
    }

    uint64_t u = (((uint64_t) sign) << 63) |
                 (((uint64_t) rawexp & 0x7ff) << 52) |
                 mantissa;

	write_u64be(u);
}

void PgfWriter::write_uint(uint64_t u)
{
    for (;;) {
		uint8_t b = u & 0x7F;
		u = u >> 7;
		if (u == 0) {
            size_t n_items = fwrite(&b, sizeof(b), 1, out);
            if (ferror(out))
                throw pgf_error("an error occured while writing out the grammar");
            if (n_items != 1)
                throw pgf_error("couldn't write to the output file");

			break;
		} else {
            b = b | 0x80;

            size_t n_items = fwrite(&b, sizeof(b), 1, out);
            if (ferror(out))
                throw pgf_error("an error occured while writing out the grammar");
            if (n_items != 1)
                throw pgf_error("couldn't write to the output file");
		}
	}
}

void PgfWriter::write_name(PgfText *text)
{
	write_len(text->size);
    size_t n_items = fwrite(&text->text, text->size, 1, out);
    if (ferror(out))
        throw pgf_error("an error occured while writing out the grammar");
    if (n_items != 1)
        throw pgf_error("couldn't write to the output file");
}

void PgfWriter::write_text(PgfText *text)
{
    size_t len = 0;
	const uint8_t* p = (const uint8_t*) &text->text;
	const uint8_t* e = p + text->size;
	while (p < e && pgf_utf8_decode(&p) != 0)
		len++;

	write_len(len);
    size_t n_items = fwrite(&text->text, text->size, 1, out);
    if (ferror(out))
        throw pgf_error("an error occured while writing out the grammar");
    if (n_items != 1)
        throw pgf_error("couldn't write to the output file");
}

template<class V>
void PgfWriter::write_namespace(Namespace<V> nmsp, void (PgfWriter::*write_value)(ref<V>))
{
    write_len(nmsp->sz);
    write_namespace_helper(nmsp, write_value);
}

template<class V>
void PgfWriter::write_namespace_helper(Namespace<V> nmsp, void (PgfWriter::*write_value)(ref<V>))
{
    if (nmsp == 0)
        return;

    write_namespace_helper(nmsp->left, write_value);
    (this->*write_value)(nmsp->value);
    write_namespace_helper(nmsp->right, write_value);
}

template<class V>
void PgfWriter::write_vector(ref<PgfVector<V>> vec, void (PgfWriter::*write_value)(ref<V> val))
{
    write_len(vec->len);
    for (size_t i = 0; i < vec->len; i++) {
        (this->*write_value)(vector_elem<V>(vec,i));
    }
}

void PgfWriter::write_literal(PgfLiteral literal)
{
    auto tag = ref<PgfLiteral>::get_tag(literal);
    write_tag(tag);

    switch (tag) {
	case PgfLiteralInt::tag: {
        auto lint = ref<PgfLiteralInt>::untagged(literal);
        write_len(lint->size);
        for (size_t i = 0; i < lint->size; i++) {
            write_uint(lint->val[i]);
        }
		break;
	}
	case PgfLiteralStr::tag: {
		auto lstr = ref<PgfLiteralStr>::untagged(literal);
        write_text(&lstr->val);
		break;
	}
	case PgfLiteralFlt::tag: {
		auto lflt = ref<PgfLiteralFlt>::untagged(literal);
        write_double(lflt->val);
		break;
	}
	default:
		throw pgf_error("Unknown literal tag");
	}
}

void PgfWriter::write_expr(PgfExpr expr)
{
    auto tag = ref<PgfExpr>::get_tag(expr);
    write_tag(tag);

    switch (tag) {
    case PgfExprAbs::tag: {
        auto eabs = ref<PgfExprAbs>::untagged(expr);
        write_tag(eabs->bind_type);
        write_name(&eabs->name);
        write_expr(eabs->body);
        break;
    }
    case PgfExprApp::tag: {
        auto eapp = ref<PgfExprApp>::untagged(expr);
        write_expr(eapp->fun);
        write_expr(eapp->arg);
        break;
    }
    case PgfExprLit::tag: {
        auto elit = ref<PgfExprLit>::untagged(expr);
        write_literal(elit->lit);
        break;
    }
    case PgfExprMeta::tag: {
        write_int(ref<PgfExprMeta>::untagged(expr)->id);
        break;
    }
	case PgfExprFun::tag: {
        write_name(&ref<PgfExprFun>::untagged(expr)->name);
        break;
    }
	case PgfExprVar::tag: {
        write_int(ref<PgfExprVar>::untagged(expr)->var);
        break;
	}
	case PgfExprTyped::tag: {
        auto etyped = ref<PgfExprTyped>::untagged(expr);
        write_expr(etyped->expr);
        write_type(etyped->type);
        break;
	}
	case PgfExprImplArg::tag: {
        write_expr(ref<PgfExprImplArg>::untagged(expr)->expr);
        break;
	}
	default:
		throw pgf_error("Unknown expression tag");
    }
}

void PgfWriter::write_hypo(ref<PgfHypo> hypo)
{
    write_tag(hypo->bind_type);
	write_name(hypo->cid);
	write_type(hypo->type);
}

void PgfWriter::write_type(ref<PgfDTyp> ty)
{
    write_vector<PgfHypo>(ty->hypos, &PgfWriter::write_hypo);
    write_name(&ty->name);
    write_vector<PgfExpr>(ty->exprs, &PgfWriter::write_expr);
}

void PgfWriter::write_patt(PgfPatt patt)
{
    auto tag = ref<PgfPatt>::get_tag(patt);
    write_tag(tag);

	switch (tag) {
	case PgfPattApp::tag: {
		auto papp = ref<PgfPattApp>::untagged(patt);
        write_name(papp->ctor);
        write_vector(ref<PgfVector<PgfPatt>>::from_ptr(&papp->args), &PgfWriter::write_patt);
		break;
	}
	case PgfPattVar::tag: {
		auto pvar = ref<PgfPattVar>::untagged(patt);
        write_name(&pvar->name);
		break;
	}
	case PgfPattAs::tag: {
        auto pas = ref<PgfPattAs>::untagged(patt);
        write_name(&pas->name);
        write_patt(pas->patt);
		break;
	}
	case PgfPattWild::tag: {
		auto pwild = ref<PgfPattWild>::untagged(patt);
		break;
	}
	case PgfPattLit::tag: {
        auto plit = ref<PgfPattLit>::untagged(patt);
        write_literal(plit->lit);
		break;
	}
	case PgfPattImplArg::tag: {
        auto pimpl = ref<PgfPattImplArg>::untagged(patt);
		write_patt(pimpl->patt);
		break;
	}
	case PgfPattTilde::tag: {
        auto ptilde = ref<PgfPattTilde>::untagged(patt);
		write_expr(ptilde->expr);
		break;
	}
	default:
		throw pgf_error("Unknown pattern tag");
	}
}

void PgfWriter::write_defn(ref<ref<PgfEquation>> r)
{
    ref<PgfEquation> equ = *r;

    write_vector(ref<PgfVector<PgfPatt>>::from_ptr(&equ->patts), &PgfWriter::write_patt);
    write_expr(equ->body);
}

void PgfWriter::write_flag(ref<PgfFlag> flag)
{
    write_name(&flag->name);
    write_literal(flag->value);
}

void PgfWriter::write_absfun(ref<PgfAbsFun> absfun)
{
    write_name(&absfun->name);
    write_type(absfun->type);
    write_int(absfun->arity);
    if (absfun->defns == 0)
        write_tag(0);
    else {
        write_tag(1);
        write_vector<ref<PgfEquation>>(absfun->defns, &PgfWriter::write_defn);
    }
    write_double(exp(-absfun->ep.prob));
}

static
void count_funs_by_cat(Namespace<PgfAbsFun> funs, PgfText *cat, size_t *pcount)
{
    if (funs == 0)
        return;

    count_funs_by_cat(funs->left, cat, pcount);

    if (textcmp(&funs->value->name, cat) == 0) {
        *pcount++;
    }

    count_funs_by_cat(funs->right, cat, pcount);
}

static
void write_funs_by_cat(Namespace<PgfAbsFun> funs, PgfText *cat, PgfWriter *wtr)
{
    if (funs == 0)
        return;

    write_funs_by_cat(funs->left, cat, wtr);

    if (textcmp(&funs->value->name, cat) == 0) {
        wtr->write_double(exp(-funs->value->ep.prob));
        wtr->write_name(&funs->value->name);
    }

    write_funs_by_cat(funs->right, cat, wtr);
}

void PgfWriter::write_abscat(ref<PgfAbsCat> abscat)
{
    write_name(&abscat->name);
    write_vector(abscat->context, &PgfWriter::write_hypo);

	size_t n_count = 0;
    count_funs_by_cat(abstract->funs, &abscat->name, &n_count);

	write_len(n_count);
    write_funs_by_cat(abstract->funs, &abscat->name, this);
	
	write_double(exp(-abscat->prob));
}

void PgfWriter::write_abstract(ref<PgfAbstr> abstract)
{
    this->abstract = abstract;

    write_name(abstract->name);
    write_namespace<PgfFlag>(abstract->aflags, &PgfWriter::write_flag);
    write_namespace<PgfAbsFun>(abstract->funs, &PgfWriter::write_absfun);
    write_namespace<PgfAbsCat>(abstract->cats, &PgfWriter::write_abscat);

    this->abstract = 0;
}

void PgfWriter::write_pgf(ref<PgfPGF> pgf)
{
    write_u16be(pgf->major_version);
	write_u16be(pgf->minor_version);

    write_namespace<PgfFlag>(pgf->gflags, &PgfWriter::write_flag);

    write_abstract(ref<PgfAbstr>::from_ptr(&pgf->abstract));
}
