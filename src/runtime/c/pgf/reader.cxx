#include "data.h"
#include "reader.h"
#include <math.h>
#include <string.h>

PgfReader::PgfReader(FILE *in,PgfProbsCallback *probs_callback)
{
    this->in = in;
    this->probs_callback = probs_callback;
    this->abstract = 0;
    this->concrete = 0;
}

uint8_t PgfReader::read_uint8()
{
    uint8_t b;
    size_t n_bytes = fread((char*) &b, sizeof(b), 1, in);
    if (feof(in))
        throw pgf_error("reached end of file while reading the grammar");
    if (ferror(in))
        throw pgf_error("an error occured while reading the grammar");

	return b;
}

uint16_t PgfReader::read_u16be()
{
    uint8_t buf[2];
    size_t n_bytes = fread((char*) &buf, sizeof(buf), 1, in);
    if (feof(in))
        throw pgf_error("reached end of file while reading a grammar");
    if (ferror(in))
        throw pgf_error("an error occured while reading the grammar");

	return (((uint16_t) buf[0]) << 8 | buf[1]);
}

uint64_t PgfReader::read_u64be()
{
    uint8_t buf[8];
    size_t n_bytes = fread((char*) &buf, sizeof(buf), 1, in);
    if (feof(in))
        throw pgf_error("reached end of file while reading a grammar");
    if (ferror(in))
        throw pgf_error("an error occured while reading the grammar");

	return (((uint64_t) buf[0]) << 56 |
            ((uint64_t) buf[1]) << 48 |
            ((uint64_t) buf[2]) << 40 |
            ((uint64_t) buf[3]) << 32 |
            ((uint64_t) buf[4]) << 24 |
            ((uint64_t) buf[5]) << 16 |
            ((uint64_t) buf[6]) << 8  |
            ((uint64_t) buf[7]));
}

double PgfReader::read_double()
{
    uint64_t u = read_u64be();

    bool sign = u >> 63;
	unsigned rawexp = u >> 52 & 0x7ff;
	uint64_t mantissa = u & 0xfffffffffffff;
	double ret;

	if (rawexp == 0x7ff) {
		ret = (mantissa == 0) ? INFINITY : NAN;
	} else {
		uint64_t m = rawexp ? 1ULL << 52 | mantissa : mantissa << 1;
		ret = ldexp((double) m, rawexp - 1075);
	}
	return sign ? copysign(ret, -1.0) : ret;
}

prob_t PgfReader::read_prob(PgfText *name)
{
    double d = read_double();
    if (probs_callback != NULL) {
        d = probs_callback->fn(probs_callback, name);
    }
    return - log(d);
}

uint64_t PgfReader::read_uint()
{
	uint64_t u = 0;
	int shift = 0;
	uint8_t b = 0;
	do {
		b = read_uint8();
		u |= (b & ~0x80) << shift;
		shift += 7;
	} while (b & 0x80);
	return u;
}

object PgfReader::read_text_internal(size_t struct_size)
{
    size_t size = read_len();
	object offs = current_db->malloc_internal(struct_size+sizeof(PgfText)+size+1);
    PgfText* ptext = (PgfText*) (current_base+offs+struct_size);
    ptext->size = size;

    // If reading the extra bytes causes EOF, it is an encoding
    // error, not a legitimate end of character stream.
    fread(ptext->text, size, 1, in);
    if (feof(in))
        throw pgf_error("utf8 decoding error");
    if (ferror(in))
        throw pgf_error("an error occured while reading the grammar");

    ptext->text[size] = 0;

	return offs;
}

template<class V>
Namespace<V> PgfReader::read_namespace(ref<V> (PgfReader::*read_value)(), size_t len)
{
    if (len == 0)
        return 0;

    size_t half = len/2;
    Namespace<V> left  = read_namespace(read_value, half);
    ref<V> value = (this->*read_value)();
    Namespace<V> right = read_namespace(read_value, len-half-1);

    Namespace<V> node = Node<ref<V>>::new_node(value);
    node->sz   = 1+Node<ref<V>>::size(left)+Node<ref<V>>::size(right);
    node->left  = left;
    node->right = right;
    return node;
}

template<class V>
Namespace<V> PgfReader::read_namespace(ref<V> (PgfReader::*read_value)())
{
    size_t len = read_len();
    return read_namespace(read_value, len);
}

template<class V>
void PgfReader::merge_namespace(ref<V> (PgfReader::*read_value)())
{
    size_t len = read_len();
    for (size_t i = 0; i < len; i++) {
        ref<V> value = (this->*read_value)();
        V::release(value);
    }
}

template <class C, class V>
ref<C> PgfReader::read_vector(Vector<V> C::* field, void (PgfReader::*read_value)(ref<V> val))
{
    size_t len = read_len();
    ref<C> loc = vector_new<C,V>(field,len);
    for (size_t i = 0; i < len; i++) {
        (this->*read_value)(vector_elem(ref<Vector<V>>::from_ptr(&(loc->*field)),i));
    }
    return loc;
}

template <class V>
ref<Vector<V>> PgfReader::read_vector(void (PgfReader::*read_value)(ref<V> val))
{
    size_t len = read_len();
    ref<Vector<V>> vec = vector_new<V>(len);
    for (size_t i = 0; i < len; i++) {
        (this->*read_value)(vector_elem(vec,i));
    }
    return vec;
}

PgfLiteral PgfReader::read_literal()
{
    PgfLiteral lit = 0;

    uint8_t tag = read_tag();
    switch (tag) {
	case PgfLiteralStr::tag: {
		ref<PgfLiteralStr> lit_str =
            read_text<PgfLiteralStr>(&PgfLiteralStr::val);
        lit = lit_str.tagged();
		break;
	}
	case PgfLiteralInt::tag: {
        size_t size = read_len();
		ref<PgfLiteralInt> lit_int =
			PgfDB::malloc<PgfLiteralInt>(sizeof(uintmax_t)*size);
        lit_int->size   = size;
        for (size_t i = 0; i < size; i++) {
            lit_int->val[i] = (uintmax_t) read_uint();
        }
        lit = lit_int.tagged();
		break;
	}
	case PgfLiteralFlt::tag: {
		ref<PgfLiteralFlt> lit_flt =
			current_db->malloc<PgfLiteralFlt>();
		lit_flt->val = read_double();
        lit = lit_flt.tagged();
		break;
	}
	default:
		throw pgf_error("Unknown literal tag");
	}
	return lit;
}

ref<PgfFlag> PgfReader::read_flag()
{
    ref<PgfFlag> flag = read_name(&PgfFlag::name);
    flag->value = read_literal();
    return flag;
}

PgfExpr PgfReader::read_expr()
{
    PgfExpr expr = 0;
    uint8_t tag = read_tag();

    switch (tag) {
	case PgfExprAbs::tag:{
        PgfBindType bind_type = (PgfBindType) read_tag();
        ref<PgfExprAbs> eabs = read_name(&PgfExprAbs::name);
        eabs->bind_type = bind_type;
        PgfExpr body = read_expr();
        eabs->body = body;
        expr = eabs.tagged();
		break;
	}
	case PgfExprApp::tag: {
        PgfExpr fun = read_expr();
        PgfExpr arg = read_expr();
        ref<PgfExprApp> eapp = PgfDB::malloc<PgfExprApp>();
        eapp->fun = fun;
        eapp->arg = arg;
        expr = eapp.tagged();
		break;
	}
	case PgfExprLit::tag: {
        PgfExpr lit = read_literal();
        ref<PgfExprLit> elit = PgfDB::malloc<PgfExprLit>();
        elit->lit = lit;
        expr = elit.tagged();
		break;
	}
	case PgfExprMeta::tag: {
		ref<PgfExprMeta> emeta = PgfDB::malloc<PgfExprMeta>();
		emeta->id = read_int();
		expr = emeta.tagged();
		break;
	}
	case PgfExprFun::tag: {
		ref<PgfExprFun> efun = read_name(&PgfExprFun::name);
        expr = efun.tagged();
		break;
	}
	case PgfExprVar::tag: {
        ref<PgfExprVar> evar = PgfDB::malloc<PgfExprVar>();
		evar->var = read_int();
        expr = evar.tagged();
		break;
	}
	case PgfExprTyped::tag: {
        auto expr = read_expr();
        auto type = read_type();
        ref<PgfExprTyped> etyped = PgfDB::malloc<PgfExprTyped>();
        etyped->expr = expr;
        etyped->type = type;
        expr = etyped.tagged();
        break;
	}
	case PgfExprImplArg::tag: {
        auto expr = read_expr();
        ref<PgfExprImplArg> eimpl = current_db->malloc<PgfExprImplArg>();
        eimpl->expr = expr;
        expr = eimpl.tagged();
		break;
	}
	default:
		throw pgf_error("Unknown expression tag");
	}

    return expr;
}

void PgfReader::read_hypo(ref<PgfHypo> hypo)
{
    hypo->bind_type = (PgfBindType) read_tag();
    auto cid = read_name();
    hypo->cid = cid;
    auto type = read_type();
    hypo->type = type;
}

ref<PgfDTyp> PgfReader::read_type()
{
    auto hypos =
        read_vector<PgfHypo>(&PgfReader::read_hypo);
    ref<PgfDTyp> tp = read_name<PgfDTyp>(&PgfDTyp::name);
    tp->hypos = hypos;
    auto exprs =
        read_vector<PgfExpr>(&PgfReader::read_expr);
    tp->exprs = exprs;
    return tp;
}

ref<PgfAbsFun> PgfReader::read_absfun()
{
    ref<PgfAbsFun> absfun =
        read_name<PgfAbsFun>(&PgfAbsFun::name);
    ref<PgfExprFun> efun =
        ref<PgfExprFun>::from_ptr((PgfExprFun*) &absfun->name);
    absfun->type = read_type();
	absfun->arity = read_int();

    uint8_t tag = read_tag();
	switch (tag) {
	case 0:
        absfun->bytecode = 0;
        break;
    case 1: {
        read_len();
        absfun->bytecode = PgfDB::malloc<char>(0);
        break;
    }
    default:
        throw pgf_error("Unknown tag, 0 or 1 expected");
    }
    absfun->prob = read_prob(&absfun->name);
    return absfun;
}

ref<PgfAbsCat> PgfReader::read_abscat()
{
    ref<PgfAbsCat> abscat = read_name<PgfAbsCat>(&PgfAbsCat::name);
    abscat->context = read_vector<PgfHypo>(&PgfReader::read_hypo);
    abscat->prob  = read_prob(&abscat->name);
    return abscat;
}

struct PGF_INTERNAL_DECL PgfAbsCatCounts
{
    PgfText *name;
    size_t n_nan_probs;
    double probs_sum;
    prob_t prob;
};

struct PGF_INTERNAL_DECL PgfProbItor : PgfItor
{
    Vector<PgfAbsCatCounts> *cats;
};

static
PgfAbsCatCounts *find_counts(Vector<PgfAbsCatCounts> *cats, PgfText *name)
{
    size_t i = 0;
    size_t j = cats->len-1;
    while (i <= j) {
        size_t k = (i+j)/2;
        PgfAbsCatCounts *counts = &cats->data[k];
        int cmp = textcmp(name, counts->name);
        if (cmp < 0) {
            j = k-1;
        } else if (cmp > 0) {
            i = k+1;
        } else {
            return counts;
        }
    }

    return NULL;
}

static
void collect_counts(PgfItor *itor, PgfText *key, object value, PgfExn *err)
{
    PgfProbItor* prob_itor = (PgfProbItor*) itor;
    ref<PgfAbsFun> absfun = value;

    PgfAbsCatCounts *counts =
        find_counts(prob_itor->cats, &absfun->type->name);
    if (counts != NULL) {
        if (isnan(absfun->prob)) {
            counts->n_nan_probs++;
        } else {
            counts->probs_sum += exp(-absfun->prob);
        }
    }
}

static
void pad_probs(PgfItor *itor, PgfText *key, object value, PgfExn *err)
{
    PgfProbItor* prob_itor = (PgfProbItor*) itor;
    ref<PgfAbsFun> absfun = value;

    if (isnan(absfun->prob)) {
        PgfAbsCatCounts *counts =
            find_counts(prob_itor->cats, &absfun->type->name);
        if (counts != NULL) {
            absfun->prob = counts->prob;
        }
    }
}

void PgfReader::read_abstract(ref<PgfAbstr> abstract)
{
    this->abstract = abstract;

    abstract->name = read_name();
	abstract->aflags = read_namespace<PgfFlag>(&PgfReader::read_flag);
    abstract->funs = read_namespace<PgfAbsFun>(&PgfReader::read_absfun);
    abstract->cats = read_namespace<PgfAbsCat>(&PgfReader::read_abscat);

    if (probs_callback != NULL) {
        PgfExn err;
        err.type = PGF_EXN_NONE;

        PgfProbItor itor;
        itor.cats = namespace_to_sorted_names<PgfAbsCat,PgfAbsCatCounts>(abstract->cats);

        itor.fn = collect_counts;
        namespace_iter(abstract->funs, &itor, &err);

        for (size_t i = 0; i < itor.cats->len; i++) {
            PgfAbsCatCounts *counts = &itor.cats->data[i];
            counts->prob = - log((1-counts->probs_sum) / counts->n_nan_probs);
        }

        itor.fn = pad_probs;
        namespace_iter(abstract->funs, &itor, &err);

        free(itor.cats);
    }
}

void PgfReader::merge_abstract(ref<PgfAbstr> abstract)
{
    this->abstract = abstract;

    ref<PgfText> name = read_name();
    int cmp = textcmp(&(*abstract->name), &(*name));
    text_db_release(name);
    if (cmp != 0)
        throw pgf_error("The abstract syntax names doesn't match");

	merge_namespace<PgfFlag>(&PgfReader::read_flag);
    merge_namespace<PgfAbsFun>(&PgfReader::read_absfun);
    merge_namespace<PgfAbsCat>(&PgfReader::read_abscat);
}

ref<PgfLParam> PgfReader::read_lparam()
{
    size_t i0 = read_int();
    size_t n_terms = read_len();
    ref<PgfLParam> lparam =
        PgfDB::malloc<PgfLParam>(n_terms*sizeof(PgfLParam::terms[0]));
    lparam->i0 = i0;
    lparam->n_terms = n_terms;

    for (size_t i = 0; i < n_terms; i++) {
        lparam->terms[i].factor = read_int();
        lparam->terms[i].var    = read_int();
    }

    return lparam;
}

void PgfReader::read_variable_range(ref<PgfVariableRange> var_info)
{
    var_info->var   = read_int();
    var_info->range = read_int();
}

void PgfReader::read_parg(ref<PgfPArg> parg)
{
    parg->param  = read_lparam();
}

ref<PgfPResult> PgfReader::read_presult()
{
    ref<Vector<PgfVariableRange>> vars = 0;
    size_t n_vars = read_len();
    if (n_vars > 0) {
        vars = vector_new<PgfVariableRange>(n_vars);
        for (size_t i = 0; i < n_vars; i++) {
            read_variable_range(vector_elem(vars,i));
        }
    }

    size_t i0 = read_int();
    size_t n_terms = read_len();
    ref<PgfPResult> res =
        PgfDB::malloc<PgfPResult>(n_terms*sizeof(PgfLParam::terms[0]));
    res->vars = vars;
    res->param.i0 = i0;
    res->param.n_terms = n_terms;

    for (size_t i = 0; i < n_terms; i++) {
        res->param.terms[i].factor = read_int();
        res->param.terms[i].var    = read_int();
    }

    return res;
}

template<class I>
ref<I> PgfReader::read_symbol_idx()
{
    size_t d = read_int();
    size_t i0 = read_int();
    size_t n_terms = read_len();
    ref<I> sym_idx =
        PgfDB::malloc<I>(n_terms*sizeof(PgfLParam::terms[0]));
    sym_idx->d = d;
    sym_idx->r.i0 = i0;
    sym_idx->r.n_terms = n_terms;

    for (size_t i = 0; i < n_terms; i++) {
        sym_idx->r.terms[i].factor = read_int();
        sym_idx->r.terms[i].var    = read_int();
    }

    return sym_idx;
}

PgfSymbol PgfReader::read_symbol()
{
    PgfSymbol sym = 0;

    uint8_t tag = read_tag();
    switch (tag) {
	case PgfSymbolCat::tag: {
        ref<PgfSymbolCat> sym_cat = read_symbol_idx<PgfSymbolCat>();
        sym = sym_cat.tagged();
		break;
    }
	case PgfSymbolLit::tag: {
        ref<PgfSymbolLit> sym_lit = read_symbol_idx<PgfSymbolLit>();
        sym = sym_lit.tagged();
		break;
    }
	case PgfSymbolVar::tag: {
        ref<PgfSymbolVar> sym_var = PgfDB::malloc<PgfSymbolVar>();
        sym_var->d = read_int();
        sym_var->r = read_int();
        sym = sym_var.tagged();
		break;
    }
	case PgfSymbolKS::tag: {
        ref<PgfSymbolKS> sym_ks = read_text(&PgfSymbolKS::token);
        sym = sym_ks.tagged();
		break;
    }
	case PgfSymbolKP::tag: {
        size_t n_alts = read_len();
        ref<PgfSymbolKP> sym_kp = PgfDB::malloc<PgfSymbolKP>(n_alts*sizeof(PgfAlternative));
        sym_kp->alts.len = n_alts;

        for (size_t i = 0; i < n_alts; i++) {
            auto form     = read_seq();
            auto prefixes = read_vector(&PgfReader::read_text2);

            sym_kp->alts.data[i].form     = form;
            sym_kp->alts.data[i].prefixes = prefixes;
        }

        auto default_form = read_seq();
        sym_kp->default_form = default_form;

        sym = sym_kp.tagged();
		break;
    }
	case PgfSymbolBIND::tag: {
        sym = ref<PgfSymbolBIND>(0).tagged();
		break;
    }
	case PgfSymbolSOFTBIND::tag: {
        sym = ref<PgfSymbolSOFTBIND>(0).tagged();
		break;
    }
	case PgfSymbolNE::tag: {
        sym = ref<PgfSymbolNE>(0).tagged();
		break;
    }
	case PgfSymbolSOFTSPACE::tag: {
        sym = ref<PgfSymbolSOFTSPACE>(0).tagged();
		break;
    }
	case PgfSymbolCAPIT::tag: {
        sym = ref<PgfSymbolCAPIT>(0).tagged();
		break;
    }
	case PgfSymbolALLCAPIT::tag: {
        sym = ref<PgfSymbolALLCAPIT>(0).tagged();
		break;
    }
	default:
		throw pgf_error("Unknown symbol tag");
    }

    return sym;
}

ref<PgfSequence> PgfReader::read_seq()
{
	size_t n_syms = read_len();

	ref<PgfSequence> seq = PgfDB::malloc<PgfSequence>(n_syms*sizeof(PgfSymbol));
    seq->syms.len  = n_syms;

    for (size_t i = 0; i < n_syms; i++) {
        PgfSymbol sym = read_symbol();
        *vector_elem(&seq->syms,i) = sym;
    }

    return seq;
}

ref<Vector<ref<PgfSequence>>> PgfReader::read_seq_ids(object container)
{
    size_t len = read_len();
    ref<Vector<ref<PgfSequence>>> vec = vector_new<ref<PgfSequence>>(len);
    for (size_t i = 0; i < len; i++) {
        size_t seq_id = read_len();
        ref<PgfSequence> seq = phrasetable_relink(concrete->phrasetable,
                                                  container, i,
                                                  seq_id);
        if (seq == 0) {
            throw pgf_error("Invalid sequence id");
        }
        *vector_elem(vec,i) = seq;
    }
    return vec;
}

PgfPhrasetable PgfReader::read_phrasetable(size_t len)
{
    if (len == 0)
        return 0;

    PgfPhrasetableEntry value;

    size_t half = len/2;
    PgfPhrasetable left  = read_phrasetable(half);
    value.seq = read_seq();
    value.backrefs = 0;
    PgfPhrasetable right = read_phrasetable(len-half-1);

    PgfPhrasetable table = Node<PgfPhrasetableEntry>::new_node(value);
    table->sz    = 1+Node<PgfPhrasetableEntry>::size(left)+Node<PgfPhrasetableEntry>::size(right);
    table->left  = left;
    table->right = right;
    return table;
}

PgfPhrasetable PgfReader::read_phrasetable()
{
    size_t len = read_len();
    return read_phrasetable(len);
}

ref<PgfConcrLincat> PgfReader::read_lincat()
{
    ref<PgfConcrLincat> lincat = read_name(&PgfConcrLincat::name);
    lincat->abscat = namespace_lookup(abstract->cats, &lincat->name);
    lincat->fields = read_lincat_fields(lincat);
    lincat->n_lindefs = read_len();
    lincat->args = read_vector(&PgfReader::read_parg);
    lincat->res  = read_vector(&PgfReader::read_presult2);
    lincat->seqs = read_seq_ids(lincat.tagged());
    return lincat;
}

ref<Vector<PgfLincatField>> PgfReader::read_lincat_fields(ref<PgfConcrLincat> lincat)
{
    size_t len = read_len();
    ref<Vector<PgfLincatField>> fields = vector_new<PgfLincatField>(len);
    for (size_t i = 0; i < len; i++) {
        ref<PgfLincatField> field = vector_elem(fields,i);
        field->lincat = lincat;
        field->name = read_text();
        field->backrefs = 0;
        field->epsilons = 0;
    }
    return fields;
}

ref<PgfConcrLin> PgfReader::read_lin()
{
    ref<PgfConcrLin> lin = read_name(&PgfConcrLin::name);
    lin->absfun = namespace_lookup(abstract->funs, &lin->name);
    if (lin->absfun == 0)
        throw pgf_error("Found a lin without a fun");

    lin->args = read_vector(&PgfReader::read_parg);
    lin->res  = read_vector(&PgfReader::read_presult2);
    lin->seqs = read_seq_ids(lin.tagged());

    lin->lincat =
        namespace_lookup(concrete->lincats, &lin->absfun->type->name);
    if (lin->lincat == 0)
        throw pgf_error("Found a lin which uses a category without a lincat");

    ref<Vector<PgfHypo>> hypos = lin->absfun->type->hypos;
    ref<PgfConcrLincat> lincats[hypos->len];
    for (size_t d = 0; d < hypos->len; d++) {
        lincats[d] =
            namespace_lookup(concrete->lincats, 
                             &vector_elem(hypos,d)->type->name);
        if (lincats[d] == 0)
            throw pgf_error("Found a lin which uses a category without a lincat");
    }

    size_t n_fields = lin->lincat->fields->len;
    for (size_t seq_index = 0; seq_index < lin->seqs->len; seq_index++) {
        ref<PgfSequence> seq = *vector_elem(lin->seqs,seq_index);
        ref<PgfPResult> result = *vector_elem(lin->res, seq_index / n_fields);

        size_t dot = 0;
        if (dot >= seq->syms.len) {
            size_t index = seq_index % n_fields;
            ref<Vector<PgfLincatEpsilon>> epsilons =
                vector_elem(lin->lincat->fields,index)->epsilons;
            epsilons =
                vector_resize(epsilons, epsilons->len+1,
                              PgfDB::get_txn_id());
            vector_elem(lin->lincat->fields,index)->epsilons = epsilons;
            ref<PgfLincatEpsilon> epsilon =
                vector_elem(epsilons,epsilons->len-1);
            epsilon->lin = lin;
            epsilon->seq_index = seq_index;
        } else {
            PgfSymbol sym = *vector_elem(&seq->syms,dot);
            switch (ref<PgfSymbol>::get_tag(sym)) {
            case PgfSymbolCat::tag: {
                auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
                ref<PgfConcrLincat> lincat = lincats[sym_cat->d];

                size_t max_values = 1;
                size_t ranges[sym_cat->r.n_terms];
                for (size_t i = 0; i < sym_cat->r.n_terms; i++) {
                    size_t range = 1;
                    for (size_t j = 0; j < result->vars->len; j++) {
                        auto var_range = vector_elem(result->vars, j);
                        if (var_range->var == sym_cat->r.terms[i].var) {
                            range = var_range->range;
                            break;
                        }
                    }

                    ranges[i]   = range;
                    max_values *= range;
                }

                for (size_t values = 0; values < max_values; values++) {
                    size_t v = values;
                    size_t index = sym_cat->r.i0;
                    for (size_t i = 0; i < sym_cat->r.n_terms; i++) {
                        index += sym_cat->r.terms[i].factor * (v % ranges[i]);
                        v = v / ranges[i];
                    }

                    ref<Vector<PgfLincatBackref>> backrefs =
                        vector_elem(lincat->fields,index)->backrefs;
                    backrefs =
                        vector_resize(backrefs, backrefs->len+1,
                                      PgfDB::get_txn_id());
                    vector_elem(lincat->fields,index)->backrefs = backrefs;
                    ref<PgfLincatBackref> backref =
                        vector_elem(backrefs,backrefs->len-1);
                    backref->lin = lin;
                    backref->seq_index = seq_index;
                    backref->dot = dot;
                }
                break;
            }
            }
        }
    }
    return lin;
}

ref<PgfConcrPrintname> PgfReader::read_printname()
{
    ref<PgfConcrPrintname> printname = read_name(&PgfConcrPrintname::name);
    printname->printname = read_text();
    return printname;
}

ref<PgfConcr> PgfReader::read_concrete()
{
    concrete = read_name(&PgfConcr::name);
	concrete->cflags = read_namespace<PgfFlag>(&PgfReader::read_flag);
	concrete->phrasetable = read_phrasetable();
	concrete->lincats = read_namespace<PgfConcrLincat>(&PgfReader::read_lincat);
	concrete->lins = read_namespace<PgfConcrLin>(&PgfReader::read_lin);
	concrete->printnames = read_namespace<PgfConcrPrintname>(&PgfReader::read_printname);
    concrete->prev = 0;
    concrete->next = 0;
    return concrete;
}

ref<PgfPGF> PgfReader::read_pgf()
{
    ref<PgfPGF> pgf = PgfDB::malloc<PgfPGF>();

    pgf->major_version = read_u16be();
    pgf->minor_version = read_u16be();

    if (pgf->major_version != PGF_MAJOR_VERSION ||
        pgf->minor_version != PGF_MINOR_VERSION) {
        throw pgf_error("Unsupported format version");
    }

    pgf->gflags = read_namespace<PgfFlag>(&PgfReader::read_flag);

    read_abstract(ref<PgfAbstr>::from_ptr(&pgf->abstract));

    pgf->concretes = read_namespace<PgfConcr>(&PgfReader::read_concrete);

    return pgf;
}

void PgfReader::merge_pgf(ref<PgfPGF> pgf)
{
    uint16_t major_version = read_u16be();
    uint16_t minor_version = read_u16be();

    if (pgf->major_version != PGF_MAJOR_VERSION ||
        pgf->minor_version != PGF_MINOR_VERSION) {
        throw pgf_error("Unsupported format version");
    }

    merge_namespace<PgfFlag>(&PgfReader::read_flag); // ??

    merge_abstract(ref<PgfAbstr>::from_ptr(&pgf->abstract));

    size_t len = read_len();
    for (size_t i = 0; i < len; i++) {
        ref<PgfConcr> concr = PgfReader::read_concrete();
        pgf->concretes =
            namespace_insert(pgf->concretes, concr);
    }
}
