#include <pgf/data.h>
#include <pgf/expr.h>
#include <gu/enum.h>
#include <gu/seq.h>
#include <gu/assert.h>
#include <gu/choice.h>
#include <gu/file.h>
#include <gu/utf8.h>
#include <math.h>
#include <stdlib.h>

//#define PGF_PARSER_DEBUG
//#define PGF_COUNTS_DEBUG
//#define PGF_RESULT_DEBUG

typedef GuBuf PgfItemBuf;

typedef struct PgfParseState PgfParseState;

struct PgfItemConts {
	PgfCCat* ccat;
	size_t lin_idx;
	PgfParseState* state;
	prob_t outside_prob;
	PgfItemBuf* items;
	int ref_count;			// how many items point to this cont?
};

typedef GuSeq PgfItemContss;
typedef GuMap PgfContsMap;
typedef GuMap PgfGenCatMap;
typedef GuMap PgfChunksMap;

typedef GuBuf PgfCCatBuf;

typedef struct {
	PgfConcr* concr;
	GuPool* pool;      // this pool is used for structures internal to the parser
	GuPool* out_pool;  // this pool is used for the allocating the final abstract trees
	GuString sentence; // the sentence to be parsed
	bool case_sensitive;
	GuBuf* expr_queue; // during the extraction of abstract trees we push them in this queue
    int max_fid;
    PgfParseState *before;
    PgfParseState *after;
    PgfToken prefix;
    PgfTokenProb* tp;
    PgfExprEnum en;    // enumeration for the generated trees/tokens
#ifdef PGF_COUNTS_DEBUG
    int item_full_count;
    int item_real_count;
    int ccat_full_count;
    int prod_full_count;
#endif
    PgfItem* free_item;

    prob_t heuristic_factor;
    PgfCallbacksMap* callbacks;
    PgfOracleCallback* oracle;
} PgfParsing;

typedef enum { BIND_NONE, BIND_HARD, BIND_SOFT } BIND_TYPE;

struct PgfParseState {
	PgfParseState* next;

    PgfItemBuf* agenda;
	PgfContsMap* conts_map;
	PgfGenCatMap* generated_cats;
	PgfChunksMap* chunks_map;

	bool needs_bind;
    size_t start_offset;
    size_t end_offset;

	prob_t viterbi_prob;
};

typedef struct PgfAnswers {
	GuBuf* conts;
	GuBuf* exprs;
	PgfCCat* ccat;
	prob_t outside_prob;
} PgfAnswers;

#define PGF_EXPR_CHUNK_STATE ((size_t) -1)

typedef struct {
	PgfAnswers* answers;
	PgfExprProb ep;
	union {
		PgfPArgs* args;
		PgfParseState* state;
	};
	size_t arg_idx;  // if the value is PGF_EXPR_CHUNK_STATE, then
	                 // the relevant value above is state, not args.
} PgfExprState;

typedef struct PgfItemBase PgfItemBase;

struct PgfItem {
	union {
		PgfItemConts* conts;
		PgfItem *next;		// used to collect released items
	};

	PgfProduction prod;
	PgfPArgs* args;
	PgfSymbol curr_sym;
	uint16_t sym_idx;
	uint8_t alt_idx;     // position in the pre alternative
	uint8_t alt;         // the number of the alternative
	prob_t inside_prob;
};

static PgfSymbols*
pgf_collect_extern_tok(PgfParsing* ps, size_t start_offset, size_t end_offset)
{
	GuBuf* syms = gu_new_buf(PgfSymbol, ps->pool);

	const uint8_t* start = (uint8_t*) ps->sentence+start_offset;
	const uint8_t* end   = (uint8_t*) ps->sentence+end_offset;

	const uint8_t* p = start;
	GuUCS ucs = gu_utf8_decode(&p);
	while (start < end) {
		size_t len = 0;
		while (p <= end && !gu_ucs_is_space(ucs)) {
			len = (p - start);
			ucs = gu_utf8_decode(&p);
		}

		PgfSymbol sym;
		PgfSymbolKS* sks = (PgfSymbolKS*)
			gu_alloc_variant(PGF_SYMBOL_KS,
			                 sizeof(PgfSymbolKS)+len+1,
			                 gu_alignof(PgfSymbolKS),
			                 &sym, ps->pool);
		memcpy((char*) sks->token, start, len);
		((char*) sks->token)[len] = 0;
		gu_buf_push(syms, PgfSymbol, sym);

		start = p;
		while (gu_ucs_is_space(ucs)) {
			start = p;
			ucs = gu_utf8_decode(&p);
		}
	}

	return gu_buf_data_seq(syms);
}

#ifdef PGF_PARSER_DEBUG
PGF_INTERNAL void
pgf_print_fid(int fid, GuOut* out, GuExn* err);

PGF_INTERNAL_DECL void
pgf_print_symbol(PgfSymbol sym, GuOut *out, GuExn *err);

static void
pgf_item_symbols(PgfItem* item,
                 size_t* lin_idx, PgfSymbols** syms,
                 GuPool* pool) {
	*lin_idx = item->conts->lin_idx;

    GuVariantInfo i = gu_variant_open(item->prod);
    switch (i.tag) {
    case PGF_PRODUCTION_APPLY: {
        PgfProductionApply* papp = i.data;
        *syms = papp->fun->lins[item->conts->lin_idx]->syms;
        break;
    }
    case PGF_PRODUCTION_COERCE: {
        PgfSymbol sym =
			gu_new_variant_i(pool, PGF_SYMBOL_CAT,
						PgfSymbolCat,
						.d = 0, .r = item->conts->lin_idx);
		*syms = gu_new_seq(PgfSymbol, 1, pool);
		gu_seq_set(*syms, PgfSymbol, 0, sym);
        break;
    }
    case PGF_PRODUCTION_EXTERN: {
        PgfProductionExtern* pext = i.data;
		*syms = pext->lins[item->conts->lin_idx];
		break;
    }
    default:
        gu_impossible();
    }
}

PGF_INTERNAL void
pgf_print_production_args(PgfPArgs* args,
                          GuOut* out, GuExn* err);

PGF_INTERNAL void
pgf_print_production(int fid, PgfProduction prod,
                     GuOut *out, GuExn* err);

static void
pgf_print_item_seq(PgfItem *item,
                   GuOut *out, GuExn* err, GuPool* pool)
{
	size_t lin_idx;
	PgfSymbols* syms = NULL;
	pgf_item_symbols(item, &lin_idx, &syms, pool);

	gu_printf(out, err, "%d : ",lin_idx);

	size_t index;
	for (index = 0; index < gu_seq_length(syms); index++) {
		if (item->sym_idx == index)
			gu_printf(out, err, " . ");

		PgfSymbol sym = gu_seq_get(syms, PgfSymbol, index);
		pgf_print_symbol(sym, out, err);
	}

	if (item->sym_idx == index)
		gu_printf(out, err, " .");
}

static void
pgf_print_range(PgfParseState* start, PgfParseState* end, GuOut* out, GuExn* err)
{
	gu_printf(out, err, "%d-%d",
	          (start != NULL) ? start->end_offset : 0,
	          (start == end)  ? end->end_offset : end->start_offset);
}

static void
pgf_print_item(PgfItem* item, PgfParseState* state, GuOut* out, GuExn* err, GuPool* pool)
{
    gu_putc('[', out, err);
	pgf_print_range(item->conts->state, state, out, err);
	gu_puts("; ", out, err);
	pgf_print_fid(item->conts->ccat->fid, out, err);
	gu_puts(" -> ", out, err);

	GuVariantInfo i = gu_variant_open(item->prod);
	switch (i.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = i.data;
        PgfCncFun* fun = papp->fun;
        gu_printf(out, err, "F%d(", fun->funid);
        if (fun->ep != NULL) {
            pgf_print_expr(fun->ep->expr, NULL, 0, out, err);
        } else {
            PgfPArg* parg = gu_seq_index(item->args, PgfPArg, 0);
            gu_printf(out,err,"linref %s", parg->ccat->cnccat->abscat->name);
        }
        gu_printf(out, err, ")[");
        pgf_print_production_args(item->args, out, err);
        gu_printf(out, err, "]; ");
		break;
	}
	case PGF_PRODUCTION_COERCE: {
        gu_puts("_[", out, err);
        pgf_print_fid(gu_seq_index(item->args, PgfPArg, 0)->ccat->fid, out, err);
        gu_puts("]; ", out, err);
		break;
	}
	case PGF_PRODUCTION_EXTERN: {
		PgfProductionExtern* pext = i.data;
        gu_printf(out, err, "<extern>");
        if (pext->ep != NULL) {
			gu_printf(out, err, "(");
			pgf_print_expr(pext->ep->expr, NULL, 0, out, err);
			gu_printf(out, err, ")");
		}
		gu_printf(out, err, "[");
		pgf_print_production_args(item->args, out, err);
        gu_printf(out, err, "]; ");
		break;
	}
	default:
		gu_impossible();
	}
    
    pgf_print_item_seq(item, out, err, pool);
    gu_printf(out, err, "; %f+%f=%f]\n",
	            item->inside_prob,
	            item->conts->outside_prob,
	            item->inside_prob+item->conts->outside_prob);
}

#ifdef PGF_RESULT_DEBUG
static void
pgf_print_expr_state(PgfExprState* st,
                     GuOut* out, GuExn* err, GuBuf* stack)
{
	gu_buf_push(stack, int,
	            (st->arg_idx != PGF_EXPR_CHUNK_STATE) ?
	               (gu_seq_length(st->args) - st->arg_idx - 1) : 0);

	if (gu_buf_length(st->answers->conts) > 0) {
		PgfExprState* cont = gu_buf_get(st->answers->conts, PgfExprState*, 0);
		if (cont != NULL)
			pgf_print_expr_state(cont, out, err, stack);
	}

	gu_puts(" (", out, err);
	if (st->answers->ccat != NULL) {
		pgf_print_fid(st->answers->ccat->fid,out,err);
		gu_puts(":", out, err);
	}
	if (gu_variant_is_null(st->ep.expr))
	  gu_puts("_", out, err);
	else
	  pgf_print_expr(st->ep.expr, NULL, 0, out, err);
}

static void
pgf_print_expr_state0(PgfExprState* st,
                      GuOut* out, GuExn* err, GuPool* tmp_pool)
{	
	gu_printf(out, err, "[%f+%f=%f]",
		st->ep.prob,
		st->answers->outside_prob,
		st->answers->outside_prob+st->ep.prob);

	size_t n_args = (st->arg_idx == PGF_EXPR_CHUNK_STATE) ?
	                   0 : gu_seq_length(st->args);

	GuBuf* stack = gu_new_buf(int, tmp_pool);
	if (n_args > 0)
		gu_buf_push(stack, int, n_args - st->arg_idx);

	if (gu_buf_length(st->answers->conts) > 0) {
		PgfExprState* cont =
			gu_buf_get(st->answers->conts, PgfExprState*, 0);
		if (cont != NULL)
			pgf_print_expr_state(cont, out, err, stack);
	}

	if (n_args > 0)
		gu_puts(" (", out, err);
	else
		gu_puts(" ", out, err);

	if (gu_variant_is_null(st->ep.expr))
	  gu_puts("_", out, err);
	else
	  pgf_print_expr(st->ep.expr, NULL, 0, out, err);

	size_t n_counts = gu_buf_length(stack);
	for (size_t i = 0; i < n_counts; i++) {
		int count = gu_buf_get(stack, int, i);
		while (count-- > 0)
			gu_puts(" ?", out, err);

		gu_puts(")", out, err);
	}
	gu_puts("\n", out, err);
}
#endif
#endif

PGF_INTERNAL_DECL int
cmp_string(PgfCohortSpot* spot, GuString tok,
           bool case_sensitive);

PGF_INTERNAL_DECL bool
skip_space(GuString* psent, size_t* ppos);

static int
cmp_item_prob(GuOrder* self, const void* a, const void* b)
{
	PgfItem *item1 = *((PgfItem **) a);
	PgfItem *item2 = *((PgfItem **) b);

	prob_t prob1 = item1->inside_prob + item1->conts->outside_prob;
	prob_t prob2 = item2->inside_prob + item2->conts->outside_prob;
	
	return (prob1>prob2) - (prob1<prob2);
}

static GuOrder
pgf_item_prob_order[1] = { { cmp_item_prob } };

static int
cmp_item_production_idx_entry(GuOrder* self, const void* a, const void* b)
{
	PgfProductionIdxEntry *entry1 = (PgfProductionIdxEntry *) a;
	PgfProductionIdxEntry *entry2 = (PgfProductionIdxEntry *) b;

	if (entry1->ccat->fid < entry2->ccat->fid)
		return -1;
	else if (entry1->ccat->fid > entry2->ccat->fid)
		return 1;
	else if (entry1->lin_idx < entry2->lin_idx)
		return -1;
	else if (entry1->lin_idx > entry2->lin_idx)
		return 1;
	else
		return 0;
}

static GuOrder
pgf_production_idx_entry_order[1] = { { cmp_item_production_idx_entry } };

static inline PgfItemContss*
pgf_parsing_get_contss(PgfParseState* state, PgfCCat* cat, GuPool *pool)
{
	return gu_map_get(state->conts_map, cat, PgfItemContss*);
}

static PgfItemConts*
pgf_parsing_get_conts(PgfParseState* state,
                      PgfCCat* ccat, size_t lin_idx,
					  GuPool *pool)
{
	gu_require(lin_idx < ccat->cnccat->n_lins);

	PgfItemContss* contss = 
		pgf_parsing_get_contss(state, ccat, pool);
	if (contss == NULL) {
		size_t n_lins = ccat->cnccat->n_lins;
		contss = gu_new_seq(PgfItemConts*, n_lins, pool);
		for (size_t i = 0; i < n_lins; i++) {
			gu_seq_set(contss, PgfItemConts*, i, NULL);
		}
		gu_map_put(state->conts_map, ccat, PgfItemContss*, contss);
	}

	PgfItemConts* conts = gu_seq_get(contss, PgfItemConts*, lin_idx);
	if (!conts) {
		conts = gu_new(PgfItemConts, pool);
		conts->ccat      = ccat;
		conts->lin_idx   = lin_idx;
		conts->state     = state;
		conts->items     = gu_new_buf(PgfItem*, pool);
		conts->outside_prob = 0;
		conts->ref_count = 0;
		gu_seq_get(contss, PgfItemConts*, lin_idx) = conts;
	}
	return conts;
}

static void
gu_ccat_fini(GuFinalizer* fin)
{
	PgfCCat* cat = gu_container(fin, PgfCCat, fin);
	if (cat->prods != NULL)
		gu_seq_free(cat->prods);
}

static PgfCCat*
pgf_parsing_create_completed(PgfParsing* ps, PgfParseState* state,
                             PgfItemConts* conts,
                             prob_t viterbi_prob)
{
	PgfCCat* cat = gu_new_flex(ps->pool, PgfCCat, fin, 1);
	cat->cnccat = conts->ccat->cnccat;
	cat->lindefs = conts->ccat->lindefs;
	cat->linrefs = conts->ccat->linrefs;
	cat->viterbi_prob = viterbi_prob;
	cat->fid = ps->max_fid++;
	cat->chunk_count = (conts->ccat->fid == -5 ||
	                    conts->state->end_offset == state->end_offset);
	cat->conts = conts;
	cat->answers = NULL;
	cat->prods = NULL;
	cat->n_synprods = 0;

	gu_map_put(state->generated_cats, conts, PgfCCat*, cat);

	cat->fin[0].fn = gu_ccat_fini;
	gu_pool_finally(ps->pool, cat->fin);

#ifdef PGF_COUNTS_DEBUG
	ps->ccat_full_count++;
#endif

	return cat;
}

static PgfCCat*
pgf_parsing_get_completed(PgfParseState* state, PgfItemConts* conts)
{
	return gu_map_get(state->generated_cats, conts, PgfCCat*);
}

static void
pgf_item_set_curr_symbol(PgfItem* item, GuPool* pool)
{
	GuVariantInfo i = gu_variant_open(item->prod);
	switch (i.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = i.data;
		PgfCncFun* fun = papp->fun;
		gu_assert(item->conts->lin_idx < fun->n_lins);
		PgfSymbols* syms = fun->lins[item->conts->lin_idx]->syms;
		gu_assert(item->sym_idx <= gu_seq_length(syms));
		if (item->sym_idx == gu_seq_length(syms)) {
			item->curr_sym = gu_null_variant;
		} else {
			item->curr_sym = gu_seq_get(syms, PgfSymbol, item->sym_idx);
		}
		break;
	}
	case PGF_PRODUCTION_COERCE: {
		gu_assert(item->sym_idx <= 1);
		if (item->sym_idx == 1) {
			item->curr_sym = gu_null_variant;
		} else {
			item->curr_sym = gu_new_variant_i(pool, PGF_SYMBOL_CAT,
						PgfSymbolCat,
						.d = 0, .r = item->conts->lin_idx);
		}
		break;
	}
	case PGF_PRODUCTION_EXTERN: {
		PgfProductionExtern* pext = i.data;

		PgfSymbols* syms = pext->lins[item->conts->lin_idx];
		if (item->sym_idx == gu_seq_length(syms)) {
			item->curr_sym = gu_null_variant;
		} else {
			item->curr_sym = gu_seq_get(syms, PgfSymbol, item->sym_idx);
		}
		break;
	}
	default:
		gu_impossible();
	}
}

static PgfItem*
pgf_new_item(PgfParsing* ps, PgfItemConts* conts, PgfProduction prod)
{
	PgfItem* item;
	if (ps->free_item == NULL)
	  item = gu_new(PgfItem, ps->pool);
	else {
	  item = ps->free_item;
	  ps->free_item = ps->free_item->next;
	}

	GuVariantInfo pi = gu_variant_open(prod);
	switch (pi.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = pi.data;
		item->args = papp->args;
		item->inside_prob = papp->fun->ep->prob;
		
		int n_args = gu_seq_length(item->args);
		for (int i = 0; i < n_args; i++) {
			PgfPArg *arg = gu_seq_index(item->args, PgfPArg, i);
			item->inside_prob += arg->ccat->viterbi_prob;
		}
		break;
	}
	case PGF_PRODUCTION_COERCE: {
		PgfProductionCoerce* pcoerce = pi.data;
		item->args = gu_new_seq(PgfPArg, 1, ps->pool);
		PgfPArg* parg = gu_seq_index(item->args, PgfPArg, 0);
		parg->hypos = NULL;
		parg->ccat = pcoerce->coerce;
		item->inside_prob = pcoerce->coerce->viterbi_prob;
		break;
	}
	case PGF_PRODUCTION_EXTERN: {
		PgfProductionExtern* pext = pi.data;
		item->args = gu_empty_seq();
		item->inside_prob = pext->ep->prob;
		break;
	}
	default:
		gu_impossible();
	}
	item->conts = conts;
	item->prod  = prod;
	item->curr_sym = gu_null_variant;
	item->sym_idx = 0;
	item->alt_idx = 0;
	item->alt = 0;

	conts->ref_count++;

	pgf_item_set_curr_symbol(item, ps->pool);

#ifdef PGF_COUNTS_DEBUG
	ps->item_full_count++;
	ps->item_real_count++;
#endif

	return item;
}

static PgfItem*
pgf_item_copy(PgfItem* item, PgfParsing* ps)
{
	PgfItem* copy;
	if (ps->free_item == NULL)
	  copy = gu_new(PgfItem, ps->pool);
	else {
	  copy = ps->free_item;
	  ps->free_item = ps->free_item->next;
	}
	memcpy(copy, item, sizeof(PgfItem));

#ifdef PGF_COUNTS_DEBUG
	ps->item_full_count++;
	ps->item_real_count++;
#endif

	item->conts->ref_count++;

	return copy;
}

static PgfItem*
pgf_item_update_arg(PgfItem* item, size_t d, PgfCCat *new_ccat,
                    PgfParsing *ps)
{
	PgfCCat *old_ccat =
		gu_seq_index(item->args, PgfPArg, d)->ccat;

	PgfItem* new_item = pgf_item_copy(item, ps);
	size_t nargs = gu_seq_length(item->args);
	new_item->args = gu_new_seq(PgfPArg, nargs, ps->pool);
	memcpy(gu_seq_data(new_item->args), gu_seq_data(item->args),
	       nargs * sizeof(PgfPArg));
	gu_seq_set(new_item->args, PgfPArg, d,
		   ((PgfPArg) { .hypos = NULL, .ccat = new_ccat }));
	new_item->inside_prob += 
		new_ccat->viterbi_prob - old_ccat->viterbi_prob;

	return new_item;
}

static void
pgf_item_advance(PgfItem* item, GuPool* pool)
{
	if (GU_LIKELY(item->alt == 0)) {
		item->sym_idx++;
		pgf_item_set_curr_symbol(item, pool);
	}
	else
		item->alt_idx++;
}

static void
pgf_item_free(PgfParsing* ps, PgfItem* item)
{
	PgfItemConts* conts = item->conts;
	conts->ref_count--;
	do {
		if (conts->ref_count != 0)
			break;

		conts = conts->ccat->conts;
	} while (conts != NULL);

	if (conts == NULL) {
		size_t n_items = gu_buf_length(item->conts->items);
		for (size_t i = 0; i < n_items; i++) {
			PgfItem* cont = gu_buf_get(item->conts->items, PgfItem*, i);
			if (cont == NULL)
				continue;

			pgf_item_free(ps, cont);
		}
	}

#ifdef PGF_PARSER_DEBUG
	memset(item, 0, sizeof(*item));
#endif
	item->next = ps->free_item;
	ps->free_item = item;
#ifdef PGF_COUNTS_DEBUG
	ps->item_real_count--;
#endif
}

static void
pgf_result_predict(PgfParsing* ps,
                   PgfExprState* cont, PgfCCat* ccat,
                   prob_t outside_prob);

static void
pgf_result_production(PgfParsing* ps, 
                      PgfAnswers* answers, PgfProduction prod);

static void
pgf_parsing_complete(PgfParsing* ps, PgfItem* item, PgfExprProb *ep);

static void
pgf_parsing_push_item(PgfParseState* state, PgfItem* item)
{
	if (gu_buf_length(state->agenda) == 0) {
		state->viterbi_prob =
			item->inside_prob+item->conts->outside_prob;
	}
	gu_buf_heap_push(state->agenda, pgf_item_prob_order, &item);
}

static void
pgf_parsing_push_production(PgfParsing* ps, PgfParseState* state,
                            PgfItemConts* conts, PgfProduction prod)
{
	PgfItem* item =
        pgf_new_item(ps, conts, prod);
    gu_buf_heap_push(state->agenda, pgf_item_prob_order, &item);
}

static void
pgf_parsing_combine(PgfParsing* ps,
                    PgfParseState* before, PgfParseState* after,
                    PgfItem* cont, PgfCCat* cat, int lin_idx)
{
	PgfItem* item = NULL;
	switch (gu_variant_tag(cont->curr_sym)) {
	case PGF_SYMBOL_CAT: {
		PgfSymbolCat* scat = gu_variant_data(cont->curr_sym);
		item = pgf_item_update_arg(cont, scat->d, cat, ps);
		break;
	}
	case PGF_SYMBOL_LIT: {
		PgfSymbolLit* slit = gu_variant_data(cont->curr_sym);
		item = pgf_item_update_arg(cont, slit->d, cat, ps);
		break;
	}
	default:
		gu_impossible();
	}

	pgf_item_advance(item, ps->pool);
	pgf_parsing_push_item(before, item);
}

static PgfProduction
pgf_parsing_new_production(PgfItem* item, PgfExprProb *ep, GuPool *pool)
{
	GuVariantInfo i = gu_variant_open(item->prod);
	PgfProduction prod = gu_null_variant;
	switch (i.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = i.data;
		PgfProductionApply* new_papp = 
			gu_new_variant(PGF_PRODUCTION_APPLY,
				       PgfProductionApply,
				       &prod, pool);
		new_papp->fun = papp->fun;
		new_papp->args = item->args;
		break;
	}
	case PGF_PRODUCTION_COERCE: {
		PgfProductionCoerce* new_pcoerce =
			gu_new_variant(PGF_PRODUCTION_COERCE,
				       PgfProductionCoerce,
				       &prod, pool);
		PgfPArg* parg = gu_seq_index(item->args, PgfPArg, 0);
		new_pcoerce->coerce = parg->ccat;
		break;
	}
	case PGF_PRODUCTION_EXTERN: {
		prod = item->prod;
		break;
	}
	default:
		gu_impossible();
	}

#ifdef PGF_COUNTS_DEBUG
	ps->prod_full_count++;
#endif
	
	return prod;
}

static void
pgf_parsing_complete(PgfParsing* ps, PgfItem* item, PgfExprProb *ep)
{
	if (ps->oracle && ps->oracle->complete) {
		// ask the oracle whether to complete
		if (!ps->oracle->complete(ps->oracle,
		                          item->conts->ccat->cnccat->abscat->name,
		                          item->conts->ccat->cnccat->labels[item->conts->lin_idx],
		                          ps->before->end_offset))
			return;
	}

	PgfProduction prod =
		pgf_parsing_new_production(item, ep, ps->pool);

	PgfCCat* tmp_ccat = pgf_parsing_get_completed(ps->before, item->conts);
    PgfCCat* ccat = tmp_ccat;
    if (ccat == NULL) {
        ccat = pgf_parsing_create_completed(ps, ps->before, item->conts, item->inside_prob);
    }

	if (ccat->prods == NULL || ccat->n_synprods >= gu_seq_length(ccat->prods)) {
		ccat->prods = gu_realloc_seq(ccat->prods, PgfProduction, ccat->n_synprods+1);
	}
	gu_seq_set(ccat->prods, PgfProduction, ccat->n_synprods++, prod);

#ifdef PGF_PARSER_DEBUG
    GuPool* tmp_pool = gu_new_pool();
    GuOut* out = gu_file_out(stderr, tmp_pool);
    GuExn* err = gu_exn(tmp_pool);
    if (tmp_ccat == NULL) {
	    gu_printf(out, err, "[");
		pgf_print_range(item->conts->state, ps->before, out, err);
		gu_puts("; ", out, err);
        pgf_print_fid(item->conts->ccat->fid, out, err);
        gu_printf(out, err, "; %d; ",
                            item->conts->lin_idx);
        pgf_print_fid(ccat->fid, out, err);
		gu_puts("] ", out, err);
		pgf_print_fid(ccat->fid, out, err);
		gu_printf(out, err, ".chunk_count=%d\n", ccat->chunk_count);
	}
    pgf_print_production(ccat->fid, prod, out, err);
    gu_pool_free(tmp_pool);
#endif

	if (item->conts->ccat->fid == -5) {
		if (ps->before->end_offset == strlen(ps->sentence)) {
			PgfPArg* parg = gu_seq_index(item->args, PgfPArg, 0);
			pgf_result_predict(ps, NULL, parg->ccat, 0);
		}
		return;
	} else {
		size_t i = gu_seq_length(item->args);
		while (i > 0) {
			PgfPArg* parg = gu_seq_index(item->args, PgfPArg, i-1);
			
			if (pgf_parsing_get_completed(ps->before, parg->ccat->conts) != NULL) {
				parg->ccat->chunk_count++;

#ifdef PGF_PARSER_DEBUG
				GuPool* tmp_pool = gu_new_pool();
				GuOut* out = gu_file_out(stderr, tmp_pool);
				GuExn* err = gu_exn(tmp_pool);
				pgf_print_fid(parg->ccat->fid, out, err);
				gu_printf(out, err, ".chunk_count=%d\n", parg->ccat->chunk_count);
				gu_pool_free(tmp_pool);
#endif
			}

			i--;
		}
	}

	if (tmp_ccat != NULL) {
		PgfItemContss* contss =
			pgf_parsing_get_contss(ps->before, ccat, ps->pool);
		if (contss != NULL) {
			size_t n_contss = gu_seq_length(contss);
			for (size_t i = 0; i < n_contss; i++) {
				PgfItemConts* conts2 = gu_seq_get(contss, PgfItemConts*, i);
				/* If there are continuations for
				 * linearization index i, then (cat, i) has
				 * already been predicted. Add the new
				 * production immediately to the agenda,
				 * i.e. process it. */
				if (conts2) {
					pgf_parsing_push_production(ps, ps->before, conts2, prod);
				}
			}
		}

		// The category has already been created. If it has also been
		// predicted already, then process a new item for this production.
		PgfParseState* state = ps->after;
		while (state != NULL) {
			PgfItemContss* contss =
				pgf_parsing_get_contss(state, ccat, ps->pool);
			if (contss != NULL) {
				size_t n_contss = gu_seq_length(contss);
				for (size_t i = 0; i < n_contss; i++) {
					PgfItemConts* conts2 = gu_seq_get(contss, PgfItemConts*, i);
					/* If there are continuations for
					 * linearization index i, then (cat, i) has
					 * already been predicted. Add the new
					 * production immediately to the agenda,
					 * i.e. process it. */
					if (conts2) {
						pgf_parsing_push_production(ps, state, conts2, prod);
					}
				}
			}

			state = state->next;
		}
		
		if (ccat->answers != NULL) {
			pgf_result_production(ps, ccat->answers, prod);
		}
	} else {
		size_t n_conts = gu_buf_length(item->conts->items);
		for (size_t i = 0; i < n_conts; i++) {
			PgfItem* cont = gu_buf_get(item->conts->items, PgfItem*, i);
			pgf_parsing_combine(ps, ps->before, ps->after, cont, ccat, item->conts->lin_idx);
		}
    }
}

static PgfParseState*
pgf_new_parse_state(PgfParsing* ps, size_t start_offset,
                    BIND_TYPE bind_type)
{
	PgfParseState** pstate;
	if (ps->before == NULL && start_offset == 0)
		pstate = &ps->before;
	else {
		if (bind_type != BIND_NONE) {
			if (ps->before->start_offset == start_offset &&
			    ps->before->end_offset   == start_offset &&
			    !ps->before->needs_bind)
				return ps->before;
		} else {
			if (ps->before->start_offset == start_offset)
				return ps->before;
		}

		pstate = &ps->after;
		while (*pstate != NULL) {
			if (bind_type != BIND_NONE) {
				if ((*pstate)->start_offset == start_offset &&
				    (*pstate)->end_offset   == start_offset &&
				    !(*pstate)->needs_bind)
					return *pstate;
			} else {
				if ((*pstate)->start_offset == start_offset)
					return *pstate;
			}
			if ((*pstate)->start_offset > start_offset)
				break;
			pstate = &(*pstate)->next;
		}
	}

	size_t end_offset = start_offset;
	GuString current = ps->sentence + end_offset;
	size_t pos = 0;
	while (skip_space(&current, &pos)) {
		end_offset++;
	}

	if (bind_type == BIND_HARD && start_offset != end_offset)
		return NULL;

	PgfParseState* state = gu_new(PgfParseState, ps->pool);
	state->next = *pstate;
    state->agenda = gu_new_buf(PgfItem*, ps->pool);
	state->generated_cats = gu_new_addr_map(PgfItemConts*, PgfCCat*, &gu_null_struct, ps->pool);
	state->conts_map = gu_new_addr_map(PgfCCat*, PgfItemContss*, &gu_null_struct, ps->pool);
	state->chunks_map = NULL;
	state->needs_bind = (bind_type == BIND_NONE) &&
	                    (start_offset == end_offset);
	state->start_offset = start_offset;
	state->end_offset = end_offset;
	state->viterbi_prob = 0;

	if (ps->before == NULL && start_offset == 0)
		state->needs_bind = false;

	*pstate = state;

	return state;
}

PGF_INTERNAL_DECL int
pgf_symbols_cmp(PgfCohortSpot* spot,
                PgfSymbols* syms, size_t* sym_idx,
                bool case_sensitive);

static bool
pgf_parsing_scan_helper(PgfParsing *ps, PgfParseState* state,
                        int i, int j, ptrdiff_t min, ptrdiff_t max)
{
	// This is a variation of a binary search algorithm which
	// can retrieve all prefixes of a string with minimal
	// comparisons, i.e. there is no need to lookup every
	// prefix separately.

	bool found = false;
	while (i <= j) {
		int k  = (i+j) / 2;
		PgfSequence* seq = gu_seq_index(ps->concr->sequences, PgfSequence, k);

		PgfCohortSpot start   = {0, ps->sentence+state->end_offset}; 
		PgfCohortSpot current = start;

		size_t sym_idx = 0;
		int cmp = pgf_symbols_cmp(&current, seq->syms, &sym_idx, ps->case_sensitive);
		if (cmp < 0) {
			j = k-1;
		} else if (cmp > 0) {
			ptrdiff_t len = current.ptr - start.ptr;

			if (min <= len)
				if (pgf_parsing_scan_helper(ps, state, i, k-1, min, len))
					found = true;

			if (len+1 <= max)
				if (pgf_parsing_scan_helper(ps, state, k+1, j, len+1, max))
					found = true;

			break;
		} else {
			ptrdiff_t len = current.ptr - start.ptr;

			if (min <= len)
				if (pgf_parsing_scan_helper(ps, state, i, k-1, min, len))
					found = true;

			// Here we do bottom-up prediction for all lexical categories. 
			// The epsilon productions will be predicted in top-down
			// fashion while parsing.
			if (seq->idx != NULL && len > 0) {
				found = true;

				// A new state will mark the end of the current match
				PgfParseState* new_state =
					pgf_new_parse_state(ps, (size_t) (current.ptr - ps->sentence), BIND_NONE);

				// Bottom-up prediction for lexical rules
				size_t n_entries = gu_buf_length(seq->idx);
				for (size_t i = 0; i < n_entries; i++) {
					PgfProductionIdxEntry* entry =
						gu_buf_index(seq->idx, PgfProductionIdxEntry, i);

					PgfItemConts* conts =
						pgf_parsing_get_conts(state,
						                      entry->ccat, entry->lin_idx,
						                      ps->pool);

					// Create the new category if it doesn't exist yet
					PgfCCat* tmp_ccat = pgf_parsing_get_completed(new_state, conts);
					PgfCCat* ccat = tmp_ccat;
					if (ccat == NULL) {
						ccat = pgf_parsing_create_completed(ps, new_state, conts, INFINITY);
					}

					// Add the production
					if (ccat->prods == NULL || ccat->n_synprods >= gu_seq_length(ccat->prods)) {
						ccat->prods = gu_realloc_seq(ccat->prods, PgfProduction, ccat->n_synprods+1);
					}
					GuVariantInfo i;
					i.tag  = PGF_PRODUCTION_APPLY;
					i.data = entry->papp;
					PgfProduction prod = gu_variant_close(i);
					gu_seq_set(ccat->prods, PgfProduction, ccat->n_synprods++, prod);

					// Update the category's probability to be minimum
					if (ccat->viterbi_prob > entry->papp->fun->ep->prob)
						ccat->viterbi_prob = entry->papp->fun->ep->prob;

#ifdef PGF_PARSER_DEBUG
					GuPool* tmp_pool = gu_new_pool();
					GuOut* out = gu_file_out(stderr, tmp_pool);
					GuExn* err = gu_exn(tmp_pool);
					if (tmp_ccat == NULL) {
						gu_printf(out, err, "[");
						pgf_print_range(state, new_state, out, err);
						gu_puts("; ", out, err);
						pgf_print_fid(conts->ccat->fid, out, err);
						gu_printf(out, err, "; %d; ",
											conts->lin_idx);
						pgf_print_fid(ccat->fid, out, err);
						gu_puts("] ", out, err);
						pgf_print_fid(ccat->fid, out, err);
						gu_printf(out, err, ".chunk_count=%d\n", ccat->chunk_count);
					}
					pgf_print_production(ccat->fid, prod, out, err);
					gu_pool_free(tmp_pool);
#endif
				}
			}

			if (len <= max)
				if (pgf_parsing_scan_helper(ps, state, k+1, j, len, max))
					found = true;

			break;
		}
	}

	return found;
}

static void
pgf_parsing_scan(PgfParsing *ps)
{
	size_t len = strlen(ps->sentence);

	PgfParseState* state =
		pgf_new_parse_state(ps, 0, BIND_SOFT);

	while (state != NULL && state->end_offset < len) {
		if (state->needs_bind) {
			// We have encountered two tokens without space in between.
			// Those can be accepted only if there is a BIND token
			// in between. We encode this by having one more state
			// at the same offset. A transition between these two
			// states is possible only with the BIND token.
			state =
				pgf_new_parse_state(ps, state->end_offset, BIND_HARD);
		}

		if (!pgf_parsing_scan_helper
					   (ps, state,
						0, gu_seq_length(ps->concr->sequences)-1,
						1, len-state->end_offset)) {
			// skip one character and try again
			GuString s = ps->sentence+state->end_offset;
			gu_utf8_decode((const uint8_t**) &s);
			pgf_new_parse_state(ps, s-ps->sentence, BIND_NONE);
		}

		if (state == ps->before)
			state = ps->after;
		else
			state = state->next;
	}
}

static void
pgf_parsing_add_transition(PgfParsing* ps, PgfToken tok, PgfItem* item)
{
	PgfCohortSpot current = {0, ps->sentence + ps->before->end_offset};

	if (ps->prefix != NULL && *current.ptr == 0) {
		if (gu_string_is_prefix(ps->prefix, tok)) {
			PgfProductionApply* papp = gu_variant_data(item->prod);

			ps->tp = gu_new(PgfTokenProb, ps->out_pool);
			ps->tp->tok  = tok;
			ps->tp->cat  = item->conts->ccat->cnccat->abscat->name;
			ps->tp->fun  = papp->fun->absfun->name;
			ps->tp->prob = item->inside_prob + item->conts->outside_prob;
		}
	} else {
		if (!ps->before->needs_bind && cmp_string(&current, tok, ps->case_sensitive) == 0) {
			PgfParseState* state =
				pgf_new_parse_state(ps, (current.ptr - ps->sentence),
				                    BIND_NONE);
			pgf_parsing_push_item(state, item);
		} else {
			pgf_item_free(ps, item);
		}
	}
}

static void
pgf_parsing_td_predict(PgfParsing* ps,
                       PgfItem* item, PgfCCat* ccat, size_t lin_idx)
{
	PgfItemConts* conts = 
		pgf_parsing_get_conts(ps->before, ccat, lin_idx, ps->pool);
	gu_buf_push(conts->items, PgfItem*, item);

	if (gu_buf_length(conts->items) == 1) {
		/* First time we encounter this linearization
		 * of this category at the current position,
		 * so predict it. */

		if (ps->oracle != NULL && ps->oracle->predict) {
			// if there is an oracle ask him if this prediction is appropriate
			if (!ps->oracle->predict(ps->oracle,
			                         ccat->cnccat->abscat->name,
			                         ccat->cnccat->labels[lin_idx],
			                         ps->before->end_offset))
			return;
		}

		conts->outside_prob =
			item->inside_prob-conts->ccat->viterbi_prob+
			item->conts->outside_prob;

		if (ps->prefix != NULL) {
			// We do completion:
			//   - top-down prediction for both syntactic and lexical rules
			size_t n_prods;
			if (ccat->fid < ps->concr->total_cats) // in grammar
				n_prods = gu_seq_length(ccat->prods);
			else
				n_prods = ccat->n_synprods;
			for (size_t i = 0; i < n_prods; i++) {
				PgfProduction prod =
					gu_seq_get(ccat->prods, PgfProduction, i);
				pgf_parsing_push_production(ps, ps->before, conts, prod);
			}
		} else {
			// Top-down prediction for syntactic rules
			for (size_t i = 0; i < ccat->n_synprods; i++) {
				PgfProduction prod =
					gu_seq_get(ccat->prods, PgfProduction, i);
				pgf_parsing_push_production(ps, ps->before, conts, prod);
			}

			// Top-down prediction for epsilon lexical rules if any
			PgfSequence* seq = gu_seq_index(ps->concr->sequences, PgfSequence, 0);
			if (gu_seq_length(seq->syms) == 0 && seq->idx != NULL) {

				PgfProductionIdxEntry key;
				key.ccat    = ccat;
				key.lin_idx = lin_idx;
				key.papp    = NULL;
				PgfProductionIdxEntry* value =
					gu_seq_binsearch(gu_buf_data_seq(seq->idx),
									 pgf_production_idx_entry_order,
									 PgfProductionIdxEntry, &key);

				if (value != NULL) {
					GuVariantInfo i = { PGF_PRODUCTION_APPLY, value->papp };
					PgfProduction prod = gu_variant_close(i);
					pgf_parsing_push_production(ps, ps->before, conts, prod);

					PgfProductionIdxEntry* start =
						gu_buf_data(seq->idx);
					PgfProductionIdxEntry* end =
						start + gu_buf_length(seq->idx)-1;

					PgfProductionIdxEntry* left = value-1;
					while (left >= start &&
						   value->ccat->fid == left->ccat->fid &&
						   value->lin_idx   == left->lin_idx) {
						GuVariantInfo i = { PGF_PRODUCTION_APPLY, left->papp };
						PgfProduction prod = gu_variant_close(i);
						pgf_parsing_push_production(ps, ps->before, conts, prod);
						left--;
					}

					PgfProductionIdxEntry* right = value+1;
					while (right <= end &&
						   value->ccat->fid == right->ccat->fid &&
						   value->lin_idx   == right->lin_idx) {
						GuVariantInfo i = { PGF_PRODUCTION_APPLY, right->papp };
						PgfProduction prod = gu_variant_close(i);
						pgf_parsing_push_production(ps, ps->before, conts, prod);
						right++;
					}
				}
			}
		}
	}

	/* If the category has already been completed, combine. */
	PgfCCat* completed =
		pgf_parsing_get_completed(ps->before, conts);
	if (completed) {
		pgf_parsing_combine(ps, ps->before, ps->after, item, completed, lin_idx);
	}

	PgfParseState* state = ps->after;
	while (state != NULL) {
		PgfCCat* completed =
			pgf_parsing_get_completed(state, conts);
		if (completed) {
			pgf_parsing_combine(ps, state, state->next, item, completed, lin_idx);
		}

		state = state->next;
	}
}

static void
pgf_parsing_symbol(PgfParsing* ps, PgfItem* item, PgfSymbol sym);

static void
pgf_parsing_pre(PgfParsing* ps, PgfItem* item, PgfSymbols* syms)
{
	if (item->alt_idx < gu_seq_length(syms)) {
		PgfSymbol sym = gu_seq_get(syms, PgfSymbol, item->alt_idx);
		pgf_parsing_symbol(ps, item, sym);
	} else {
		item->alt = 0;
		pgf_item_advance(item, ps->pool);
		pgf_parsing_push_item(ps->before, item);
	}
}

static void
pgf_parsing_symbol(PgfParsing* ps, PgfItem* item, PgfSymbol sym)
{
	switch (gu_variant_tag(sym)) {
	case PGF_SYMBOL_CAT: {
		PgfSymbolCat* scat = gu_variant_data(sym);
		PgfPArg* parg = gu_seq_index(item->args, PgfPArg, scat->d);
		
		if (parg->ccat->prods == NULL) {
			// empty category
			pgf_item_free(ps, item);
			return;
		}

		pgf_parsing_td_predict(ps, item, parg->ccat, scat->r);
		break;
	}
	case PGF_SYMBOL_KS: {
		PgfSymbolKS* sks = gu_variant_data(sym);
		pgf_item_advance(item, ps->pool);
		pgf_parsing_add_transition(ps, sks->token, item);
		break;
	}
	case PGF_SYMBOL_KP: {
		PgfSymbolKP* skp = gu_variant_data(sym);

		if (item->alt == 0) {
			PgfItem* new_item;

			new_item = pgf_item_copy(item, ps);
			new_item->alt = 1;
			new_item->alt_idx = 0;
			pgf_parsing_pre(ps, new_item, skp->default_form);

			for (size_t i = 0; i < skp->n_forms; i++) {
				PgfSymbols* syms = skp->forms[i].form;
				PgfSymbols* syms2 = skp->default_form;
				bool skip = false; /*pgf_tokens_equal(toks, toks2);
				for (size_t j = 0; j < i; j++) {
					PgfTokens* toks2 = skp->forms[j].form;
					skip |= pgf_tokens_equal(toks, toks2);
				}*/
				if (!skip) {
					new_item = pgf_item_copy(item, ps);
					new_item->alt = i+2;
					new_item->alt_idx = 0;
					pgf_parsing_pre(ps, new_item, syms);
				}
			}
		} else {
			PgfSymbols* syms =
			   (item->alt == 1) ? skp->default_form : 
								  skp->forms[item->alt-2].form;
			pgf_parsing_pre(ps, item, syms);
		}
		break;
	}
	case PGF_SYMBOL_LIT: {
		PgfSymbolLit* slit = gu_variant_data(sym);
		PgfPArg* parg = gu_seq_index(item->args, PgfPArg, slit->d);

		if (parg->ccat->fid >= ps->concr->total_cats) {
			pgf_parsing_td_predict(ps, item, parg->ccat, slit->r);
		}
		else {
			PgfItemConts* conts = 
				pgf_parsing_get_conts(ps->before,
				                      parg->ccat, slit->r,
									  ps->pool);
			gu_buf_push(conts->items, PgfItem*, item);

			if (gu_buf_length(conts->items) == 1) {
				/* This is the first time when we encounter this 
				 * literal category so we must call the callback */

				bool match = false;
				if (!ps->before->needs_bind) {
					size_t start  = ps->before->end_offset;
					size_t offset = start;
					PgfExprProb *ep = NULL;

					if (ps->oracle != NULL && ps->oracle->literal) {
						ep = ps->oracle->literal(ps->oracle,
							                     parg->ccat->cnccat->abscat->name,
							                     parg->ccat->cnccat->labels[slit->r],
							                     &offset,
							                     ps->out_pool);
					} else {
						PgfLiteralCallback* callback =
							gu_map_get(ps->callbacks,
							           parg->ccat->cnccat,
							           PgfLiteralCallback*);

						if (callback != NULL) {
							ep = callback->match(callback, ps->concr,
							                     slit->r,
							                     ps->sentence, &offset,
							                     ps->out_pool);
						}
					}

					if (ep != NULL) {
						PgfSymbols* syms =
							pgf_collect_extern_tok(ps, start, offset);

						size_t n_lins = item->conts->ccat->cnccat->n_lins;

						PgfProduction prod;
						PgfProductionExtern* pext =
							gu_new_flex_variant(PGF_PRODUCTION_EXTERN,
										        PgfProductionExtern,
										        lins, n_lins,
										        &prod, ps->pool);
						pext->ep     = ep;
						pext->n_lins = n_lins;

						for (size_t i = 0; i < n_lins; i++) {
							pext->lins[i] = NULL;
						}
						pext->lins[item->conts->lin_idx] = syms;

						PgfItem* item =
							pgf_new_item(ps, conts, prod);
						item->curr_sym = gu_null_variant;
						item->sym_idx  = gu_seq_length(syms);
						PgfParseState* state =
							pgf_new_parse_state(ps, offset, BIND_NONE);
						pgf_parsing_push_item(state, item);
						match = true;
					}
				}

				if (!match) {
					pgf_item_free(ps, item);
				}
			} else {
				/* If it has already been completed, combine. */

				PgfCCat* completed =
					pgf_parsing_get_completed(ps->before, conts);
				if (completed) {
					pgf_parsing_combine(ps, ps->before, ps->after, item, completed, slit->r);
				}
						
				PgfParseState* state = ps->after;
				while (state != NULL) {
					PgfCCat* completed =
						pgf_parsing_get_completed(state, conts);
					if (completed) {
						pgf_parsing_combine(ps, state, state->next, item, completed, slit->r);
					}

					state = state->next;
				}
			}
		}
		break;
	}
	case PGF_SYMBOL_VAR:
		// XXX TODO proper support
		break;
	case PGF_SYMBOL_NE: {
		// Nothing can match with a non-existant token
		pgf_item_free(ps, item);
		break;
	}
	case PGF_SYMBOL_BIND: {
		if (ps->before->start_offset == ps->before->end_offset &&
		    ps->before->needs_bind) {
			PgfParseState* state =
				pgf_new_parse_state(ps, ps->before->end_offset, BIND_HARD);
			if (state != NULL) {
				pgf_item_advance(item, ps->pool);
				pgf_parsing_push_item(state, item);
			} else {
				pgf_item_free(ps, item);
			}
		} else {
			pgf_item_free(ps, item);
		}
		break;
	}
	case PGF_SYMBOL_SOFT_BIND:
	case PGF_SYMBOL_SOFT_SPACE: {
		if (ps->before->start_offset == ps->before->end_offset) {
			if (ps->before->needs_bind) {
				PgfParseState* state =
					pgf_new_parse_state(ps, ps->before->end_offset, BIND_HARD);
				if (state != NULL) {
					pgf_item_advance(item, ps->pool);
					pgf_parsing_push_item(state, item);
				} else {
					pgf_item_free(ps, item);
				}
			} else {
				pgf_item_free(ps, item);
			}
		} else {
			pgf_item_advance(item, ps->pool);
			pgf_parsing_push_item(ps->before, item);
		}
		break;
	}
	case PGF_SYMBOL_CAPIT:
	case PGF_SYMBOL_ALL_CAPIT: {
		pgf_item_advance(item, ps->pool);
		pgf_parsing_symbol(ps, item, item->curr_sym);
		break;
	}
	default:
		gu_impossible();
	}
}

static void
pgf_parsing_set_default_factors(PgfParsing* ps, PgfAbstr* abstr)
{
	PgfFlag* flag;

	flag =
		gu_seq_binsearch(abstr->aflags, pgf_flag_order, PgfFlag, "heuristic_search_factor");
	if (flag != NULL) {
		GuVariantInfo pi = gu_variant_open(flag->value);
		gu_assert (pi.tag == PGF_LITERAL_FLT);
		ps->heuristic_factor = ((PgfLiteralFlt*) pi.data)->val;
	}
}

PGF_INTERNAL_DECL bool
pgf_is_case_sensitive(PgfConcr* concr);

static PgfParsing*
pgf_new_parsing(PgfConcr* concr, GuString sentence,
                PgfCallbacksMap* callbacks, PgfOracleCallback* oracle,
                GuPool* pool, GuPool* out_pool)
{
	PgfParsing* ps = gu_new(PgfParsing, pool);
	ps->concr = concr;
	ps->pool = pool;
	ps->out_pool = out_pool;
	ps->sentence = sentence;
	ps->case_sensitive = pgf_is_case_sensitive(concr);
	ps->expr_queue = gu_new_buf(PgfExprState*, pool);
	ps->max_fid = concr->total_cats;
	ps->before = NULL;
	ps->after = NULL;
#ifdef PGF_COUNTS_DEBUG
	ps->item_full_count = 0;
	ps->item_real_count = 0;
	ps->ccat_full_count = 0;
	ps->prod_full_count = 0;
#endif
	ps->prefix = NULL;
	ps->tp = NULL;
	ps->free_item = NULL;
	ps->heuristic_factor = 0;
	ps->callbacks = callbacks;
	ps->oracle = oracle;

	pgf_parsing_set_default_factors(ps, concr->abstr);

	return ps;
}

#ifdef PGF_COUNTS_DEBUG
static void 
pgf_parsing_print_counts(PgfParsing* ps)
{
	printf("%d\t%d\t%d\t%d\n", 
		ps->item_full_count, 
		ps->item_real_count, 
		ps->ccat_full_count,
		ps->prod_full_count);
}
#endif

static int
cmp_expr_state(GuOrder* self, const void* a, const void* b)
{
	PgfExprState *s1 = *((PgfExprState **) a);
	PgfExprState *s2 = *((PgfExprState **) b);

	prob_t prob1 = s1->answers->outside_prob+s1->ep.prob;
	prob_t prob2 = s2->answers->outside_prob+s2->ep.prob;

	return (prob1>prob2) - (prob1<prob2);
}

static GuOrder
pgf_expr_state_order = { cmp_expr_state };

static void
pgf_result_production(PgfParsing* ps,
                      PgfAnswers* answers, PgfProduction prod)
{
	GuVariantInfo pi = gu_variant_open(prod);
	switch (pi.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = pi.data;

		PgfExprState *st = gu_new(PgfExprState, ps->pool);
		st->answers = answers;
		st->ep      = *papp->fun->ep;
		st->args    = papp->args;
		st->arg_idx = 0;

		size_t n_args = gu_seq_length(st->args);
		for (size_t k = 0; k < n_args; k++) {
			PgfPArg* parg = gu_seq_index(st->args, PgfPArg, k);
			st->ep.prob += parg->ccat->viterbi_prob;
		}

		gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st);
		break;
	}
	case PGF_PRODUCTION_COERCE: {
		PgfProductionCoerce* pcoerce = pi.data;

		PgfCCat* ccat = pcoerce->coerce;

		PgfExprState *st = gu_new(PgfExprState, ps->pool);
		st->answers = answers;
		st->ep.expr = gu_null_variant;
		st->ep.prob = ccat->viterbi_prob;
		st->args    = gu_empty_seq();
		st->arg_idx = 0;

		pgf_result_predict(ps, st, ccat, answers->outside_prob);
		break;
	}
	case PGF_PRODUCTION_EXTERN: {
		PgfProductionExtern* pext = pi.data;

		PgfExprState *st = gu_new(PgfExprState, ps->pool);
		st->answers = answers;
		st->ep      = *pext->ep;
		st->args    = gu_empty_seq();
		st->arg_idx = 0;

		gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st);
		break;
	}
	default:
		gu_impossible();
	}
}

static void
pgf_result_predict(PgfParsing* ps, 
                   PgfExprState* cont, PgfCCat* ccat,
                   prob_t outside_prob)
{
	PgfAnswers* answers = ccat->answers;
	if (answers == NULL) {
		answers = gu_new(PgfAnswers, ps->pool);
		answers->conts = gu_new_buf(PgfExprState*, ps->pool);
		answers->exprs = gu_new_buf(PgfExprProb*,  ps->pool);
		answers->outside_prob = outside_prob;
		answers->ccat = ccat;

		ccat->answers = answers;
	}

	gu_buf_push(answers->conts, PgfExprState*, cont);

	if (gu_buf_length(answers->conts) == 1) {
		if (ccat->prods == NULL)
			return;

		// Generation
		for (size_t i = 0; i < ccat->n_synprods; i++) {
			PgfProduction prod =
				gu_seq_get(ccat->prods, PgfProduction, i);
			pgf_result_production(ps, answers, prod);
		}
	} else {
		size_t n_exprs = gu_buf_length(answers->exprs);
		for (size_t i = 0; i < n_exprs; i++) {
			PgfExprProb* ep = gu_buf_get(answers->exprs, PgfExprProb*, i);

			PgfExprState* st = gu_new(PgfExprState, ps->pool);
			st->answers = cont->answers;
			st->ep.expr =
			    gu_variant_is_null(cont->ep.expr) ?
			      ep->expr :
				  gu_new_variant_i(ps->out_pool, 
				                   PGF_EXPR_APP, PgfExprApp,
				                   .fun = cont->ep.expr,
				                   .arg = ep->expr);
			st->ep.prob = cont->ep.prob+ep->prob;

			if (cont->arg_idx == PGF_EXPR_CHUNK_STATE) {
				st->state   = gu_map_get(cont->state->chunks_map, ccat, PgfParseState*);
				st->arg_idx = PGF_EXPR_CHUNK_STATE;
			} else {
				st->args    = cont->args;
				st->arg_idx = cont->arg_idx+1;
			}

			gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st);
		}
	}
}

static bool
pgf_parse_result_is_new(PgfExprState* st)
{
	// we have found a complete abstract tree but we must check
	// whether this is not a duplication. Since the trees are
	// generated in probability order it is enough to check only
	// trees with the same probability.

	size_t i = gu_buf_length(st->answers->exprs);
	while (i-- > 0) {
		PgfExprProb* ep = 
			gu_buf_get(st->answers->exprs, PgfExprProb*, i);

		if (ep->prob < st->ep.prob)
			break;

		if (pgf_expr_eq(ep->expr, st->ep.expr))
			return false;
	}

	return true;
}

static PgfParsing*
pgf_parsing_init(PgfConcr* concr, PgfCId cat, 
                 GuString sentence,
                 double heuristic_factor,
                 PgfCallbacksMap* callbacks, PgfOracleCallback* oracle,
                 GuExn* err, GuPool* pool, GuPool* out_pool)
{
	PgfCncCat* cnccat =
		gu_map_get(concr->cnccats, cat, PgfCncCat*);
	if (!cnccat) {
		GuExnData* exn = gu_raise(err, PgfExn);
		exn->data = "Unknown start category";
		return NULL;
	}

	PgfParsing* ps =
		pgf_new_parsing(concr, sentence, callbacks, oracle, pool, out_pool);

	if (heuristic_factor >= 0) {
		ps->heuristic_factor = heuristic_factor;
	}

	pgf_parsing_scan(ps);

	int fidString = -1;
	PgfCCat* start_ccat = gu_new(PgfCCat, ps->pool);
	start_ccat->cnccat = gu_map_get(concr->ccats, &fidString, PgfCCat*)->cnccat;
	start_ccat->lindefs = NULL;
	start_ccat->linrefs = NULL;
	start_ccat->viterbi_prob = 0;
	start_ccat->fid = -5;
	start_ccat->chunk_count = 1;
	start_ccat->conts = NULL;
	start_ccat->answers = NULL;
	start_ccat->prods = NULL;
	start_ccat->n_synprods = 0;

#ifdef PGF_COUNTS_DEBUG
	ps->ccat_full_count++;
#endif

	PgfItemConts* conts =
		pgf_parsing_get_conts(ps->before, start_ccat, 0, ps->pool);
    gu_buf_push(conts->items, PgfItem*, NULL);

	size_t n_ccats = gu_seq_length(cnccat->cats);
	for (size_t i = 0; i < n_ccats; i++) {
		PgfCCat* ccat = gu_seq_get(cnccat->cats, PgfCCat*, i);
		if (ccat != NULL) {
			PgfPArgs* args = gu_new_seq(PgfPArg, 1, ps->pool);
			gu_seq_set(args, PgfPArg, 0, ((PgfPArg) { .hypos = NULL, .ccat = ccat }));

			size_t n_funs = gu_seq_length(ccat->linrefs);
			for (size_t j = 0; j < n_funs; j++) {
				PgfProduction prod = gu_null_variant;
				PgfProductionApply* new_papp =
					gu_new_variant(PGF_PRODUCTION_APPLY,
						PgfProductionApply,
						&prod, pool);
				new_papp->fun  = gu_seq_get(ccat->linrefs, PgfCncFun*, j);
				new_papp->args = args;

                PgfItem* item = gu_new(PgfItem, ps->pool);
				item->args = args;
				item->inside_prob = 0;
				item->conts = conts;
				item->prod  = prod;
				item->curr_sym = gu_null_variant;
				item->sym_idx = 0;
				item->alt_idx = 0;
				item->alt = 0;

				conts->ref_count++;

				pgf_item_set_curr_symbol(item, ps->pool);

#ifdef PGF_COUNTS_DEBUG
				ps->item_full_count++;
				ps->item_real_count++;
#endif

                gu_buf_heap_push(ps->before->agenda, pgf_item_prob_order, &item);
			}
		}
	}

	return ps;
}

static bool
pgf_parsing_proceed(PgfParsing* ps)
{
	bool has_progress = false;

	prob_t best_prob = INFINITY;
	if (gu_buf_length(ps->expr_queue) > 0) {
		best_prob = gu_buf_get(ps->expr_queue, PgfExprState*, 0)->ep.prob;
	}

	PgfParseState* st   = ps->before;
	PgfParseState* last = NULL;
	prob_t delta_prob   = 0;
	while (st != NULL) {
		if (gu_buf_length(st->agenda) > 0) {
			if (last != NULL) {
				delta_prob +=
					(last->viterbi_prob-st->viterbi_prob) *
					ps->heuristic_factor;
			}
			last = st;

			PgfItem* item = gu_buf_get(st->agenda, PgfItem*, 0);
			prob_t item_prob =
				item->inside_prob+item->conts->outside_prob+delta_prob;
			if (item_prob < best_prob) {
				best_prob = item_prob;

				while (st != ps->before) {
					PgfParseState* tmp = ps->before->next;
					ps->before->next = ps->after;
					ps->after = ps->before;
					ps->before = tmp;
				}

				has_progress = true;
			}
		}

		st = st->next;
	}

	if (has_progress) {
		PgfItem* item;
		gu_buf_heap_pop(ps->before->agenda, pgf_item_prob_order, &item);
		
#ifdef PGF_PARSER_DEBUG
		GuPool* tmp_pool = gu_new_pool();
		GuOut* out = gu_file_out(stderr, tmp_pool);
		GuExn* err = gu_exn(tmp_pool);
		pgf_print_item(item, ps->before, out, err, tmp_pool);
		gu_pool_free(tmp_pool);
#endif

		if (gu_variant_is_null(item->curr_sym)) {
			pgf_parsing_complete(ps, item, NULL);
			pgf_item_free(ps, item);
		} else {
			pgf_parsing_symbol(ps, item, item->curr_sym);
		}
	}

	while (ps->after != NULL) {
		PgfParseState* tmp = ps->after->next;
		ps->after->next = ps->before;
		ps->before = ps->after;
		ps->after  = tmp;
	}

	return has_progress;
}

typedef struct {
	GuMapItor fn;
	PgfParsing* ps;
	PgfExprState* st;
} PgfChunkCatItor;

static void
pgf_iter_chunk_cat(GuMapItor* fn,
                   const void* key, void* value,
		           GuExn *err)
{
	PgfChunkCatItor* clo = (PgfChunkCatItor*) fn;
	PgfCCat* ccat = (PgfCCat*) key;

	prob_t outside_prob =
		clo->st->answers->outside_prob+
		clo->st->ep.prob+
		ccat->cnccat->abscat->prob;

	pgf_result_predict(clo->ps, clo->st, ccat, outside_prob);
}

static PgfExprProb*
pgf_parse_result_next(PgfParsing* ps)
{
	for (;;) {
		while (pgf_parsing_proceed(ps));

		if (gu_buf_length(ps->expr_queue) == 0)
			break;

		PgfExprState* st;
		gu_buf_heap_pop(ps->expr_queue, &pgf_expr_state_order, &st);

#ifdef PGF_PARSER_DEBUG
#ifdef PGF_RESULT_DEBUG
		GuPool* tmp_pool = gu_new_pool();
		GuOut* out = gu_file_out(stderr, tmp_pool);
		GuExn* err = gu_exn(tmp_pool);
		pgf_print_expr_state0(st, out, err, tmp_pool);
		gu_pool_free(tmp_pool);
#endif
#endif

		if (st->arg_idx == PGF_EXPR_CHUNK_STATE) {
			// here we look for chunks

			if (st->state == ps->before) {
				if (pgf_parse_result_is_new(st)) {
					gu_buf_push(st->answers->exprs, PgfExprProb*, &st->ep);
					return &st->ep;
				}
			} else {
				PgfChunkCatItor clo = { { pgf_iter_chunk_cat }, ps, st };
				if (st->state->chunks_map != NULL)				
					gu_map_iter(st->state->chunks_map, &clo.fn, NULL);
			}
		} else if (st->arg_idx < gu_seq_length(st->args)) {
			// here we handle normal unfinished expression states

			PgfCCat* ccat =
				gu_seq_index(st->args, PgfPArg, st->arg_idx)->ccat;

			if (ccat->fid < ps->concr->total_cats) {
				// when argument was not used by the parser,
				// we create a metavariable
				PgfExpr meta = gu_new_variant_i(ps->out_pool,
				                                PGF_EXPR_META, PgfExprMeta,
				                                .id = 0);

				st->ep.expr =
				    gu_variant_is_null(st->ep.expr) ?
				      meta :
					  gu_new_variant_i(ps->out_pool, 
				                       PGF_EXPR_APP, PgfExprApp,
				                       .fun = st->ep.expr,
				                       .arg = meta);
				st->arg_idx++;
				gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st);
			} else {
				prob_t outside_prob =
					st->answers->outside_prob+
					st->ep.prob-ccat->viterbi_prob;
				pgf_result_predict(ps, st, ccat, outside_prob);
			}
		} else if (pgf_parse_result_is_new(st)) {
			gu_buf_push(st->answers->exprs, PgfExprProb*, &st->ep);

			size_t n_conts = gu_buf_length(st->answers->conts);
			for (size_t i = 0; i < n_conts; i++) {
				PgfExprState* st2 = gu_buf_get(st->answers->conts, PgfExprState*, i);

				if (st2 == NULL) {
					return &st->ep;
				}

				PgfExprState* st3 = gu_new(PgfExprState, ps->pool);
				st3->answers = st2->answers;
				st3->ep.expr =
				    gu_variant_is_null(st2->ep.expr) ?
				      st->ep.expr :
					  gu_new_variant_i(ps->out_pool,
					                   PGF_EXPR_APP, PgfExprApp,
					                   .fun = st2->ep.expr,
					                   .arg = st->ep.expr);
				if (st2->arg_idx == PGF_EXPR_CHUNK_STATE) {
					st3->ep.prob = st2->ep.prob+st->answers->ccat->cnccat->abscat->prob +
					               st->ep.prob;
					st3->state   = gu_map_get(st2->state->chunks_map, st->answers->ccat, PgfParseState*);
					st3->arg_idx = PGF_EXPR_CHUNK_STATE;
				} else {
					st3->ep.prob = st2->ep.prob-st->answers->ccat->viterbi_prob +
					               st->ep.prob;
					st3->args    = st2->args;
					st3->arg_idx = st2->arg_idx+1;
				}

				gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st3);
			}
		}
	}

	return NULL;
}

static void
pgf_parse_result_enum_next(GuEnum* self, void* to, GuPool* pool)
{
	PgfParsing* ps = gu_container(self, PgfParsing, en);
	*(PgfExprProb**)to = pgf_parse_result_next(ps);
}

static PgfParseError*
pgf_parsing_new_exception(PgfParsing* ps, GuPool* pool)
{
	const uint8_t* p   = (uint8_t*) ps->sentence;
	const uint8_t* end = p + (ps->before ? ps->before->end_offset : 0);

	PgfParseError* err = gu_new(PgfParseError, pool);
	err->incomplete= (*end == 0);
	err->offset    = 0;
	err->token_ptr = (char*) p;

	while (p < end) {
		if (gu_ucs_is_space(gu_utf8_decode(&p))) {
			err->token_ptr = (char*) p;
		}
		err->offset++;
	}

	if (err->incomplete) {
		err->token_ptr = NULL;
		err->token_len = 0;
		return err;
	}

	while (*p && !gu_ucs_is_space(gu_utf8_decode(&p))) {
		end = p;
	}

	err->token_len = ((char*)end)-err->token_ptr;

	return err;
}

PGF_API GuEnum*
pgf_parse(PgfConcr* concr, PgfType* typ, GuString sentence,
          GuExn* err, 
          GuPool* pool, GuPool* out_pool)
{
	PgfCallbacksMap* callbacks = pgf_new_callbacks_map(concr, out_pool); 
    return pgf_parse_with_heuristics(concr, typ, sentence, -1.0, callbacks, err, pool, out_pool);
}

static void
pgf_iter_generated_cats(PgfParsing* ps, PgfParseState* next_state);

static void
pgf_process_generated_cat(PgfParsing* ps,
                          PgfParseState* state, PgfParseState* next_state,
                          PgfCCat* ccat)
{
	bool just_coercions = true;

	PgfCCat* children[ccat->n_synprods];
	for (size_t i = 0; i < ccat->n_synprods; i++) {
		PgfProduction prod =
			gu_seq_get(ccat->prods, PgfProduction, i);

		children[i] = NULL;

		GuVariantInfo inf = gu_variant_open(prod);
		switch (inf.tag) {
		case PGF_PRODUCTION_APPLY: {
			PgfProductionApply* papp = inf.data;	

			size_t j = gu_seq_length(papp->args);
			while (j > 0) {
				PgfPArg* parg = gu_seq_index(papp->args, PgfPArg, j-1);

				if (pgf_parsing_get_completed(state, parg->ccat->conts) != NULL &&
					ccat->conts->state->end_offset == parg->ccat->conts->state->end_offset) {
					children[i] = parg->ccat;
					break;
				}

				j--;
			}

			if (children[i] == NULL) {
				just_coercions = false;
				break;
			}
			break;
		}
		case PGF_PRODUCTION_COERCE: {
			PgfProductionCoerce* pcoerce = inf.data;
			children[i] = pcoerce->coerce;
			break;
		}
		case PGF_PRODUCTION_EXTERN:
			just_coercions = false;
		}
	}

	if (just_coercions) {
		ccat->chunk_count++;

		for (size_t i = 0; i < ccat->n_synprods; i++) {
			children[i]->chunk_count--;

#ifdef PGF_PARSER_DEBUG
			GuPool* tmp_pool = gu_new_pool();
			GuOut* out = gu_file_out(stderr, tmp_pool);
			GuExn* err = gu_exn(tmp_pool);
			pgf_print_fid(children[i]->fid, out, err);
			gu_printf(out, err, ".chunk_count=%d\n", children[i]->chunk_count);
			gu_pool_free(tmp_pool);
#endif

			if (children[i]->chunk_count == 0) {
				pgf_process_generated_cat(ps, state, next_state, children[i]);
			}
		}
	} else {
		PgfParseState* prev_state = ccat->conts->state;
		if (prev_state->chunks_map == NULL) {
			pgf_iter_generated_cats(ps, prev_state);

			if (prev_state->chunks_map == NULL) {
				prev_state->chunks_map =
					gu_new_addr_map(PgfCCat*, PgfParseState*,
					                &gu_null_struct, ps->pool);
			}
		}

#ifdef PGF_PARSER_DEBUG
		GuPool* tmp_pool = gu_new_pool();
		GuOut* out = gu_file_out(stderr, tmp_pool);
		GuExn* err = gu_exn(tmp_pool);
		gu_printf(out, err, "[%d - ", prev_state->end_offset);
		pgf_print_fid(ccat->fid, out, err);
		gu_printf(out, err, " - %d]\n", next_state->start_offset);
		gu_pool_free(tmp_pool);
#endif

		gu_map_put(prev_state->chunks_map, ccat, PgfParseState*, next_state);
	}
}

static void
pgf_iter_generated_cats(PgfParsing* ps, PgfParseState* next_state)
{
	size_t count = 0;
	PgfParseState* state = next_state;

	for (;;) {
		size_t i = 0;
		PgfCCat* ccat;
		PgfItemConts* conts;
		while (gu_map_next(state->generated_cats, &i, (void**)&conts, &ccat)) {
			if (ccat->chunk_count > 0)
				continue;

			count++;

			pgf_process_generated_cat(ps, state, next_state, ccat);
		}

		if (count > 0 || state->next == NULL)
			break;

		state = state->next;
	}
}

PGF_API GuEnum*
pgf_parse_with_heuristics(PgfConcr* concr, PgfType* typ, GuString sentence,
                          double heuristics,
                          PgfCallbacksMap* callbacks,
                          GuExn* err,
                          GuPool* pool, GuPool* out_pool)
{
	if (concr->sequences == NULL ||
	    concr->cnccats == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	// Begin parsing a sentence with the specified category
	PgfParsing* ps =
		pgf_parsing_init(concr, typ->cid, sentence, heuristics, callbacks, NULL, err, pool, out_pool);
	if (ps == NULL) {
		return NULL;
	}

#ifdef PGF_COUNTS_DEBUG
	pgf_parsing_print_counts(ps);
#endif

	while (gu_buf_length(ps->expr_queue) == 0) {
		if (!pgf_parsing_proceed(ps)) {
			GuExnData* exn = gu_raise(err, PgfParseError);
			exn->data = (void*) pgf_parsing_new_exception(ps, exn->pool);

			PgfExprState* st = gu_new(PgfExprState, ps->pool);
			st->answers = gu_new(PgfAnswers, ps->pool);
			st->answers->conts = gu_new_buf(PgfExprState*, ps->pool);
			st->answers->exprs = gu_new_buf(PgfExprProb*,  ps->pool);
			st->answers->ccat  = NULL;
			st->answers->outside_prob = 0;
			st->ep.expr =
				gu_new_variant_i(ps->out_pool,
					             PGF_EXPR_META, PgfExprMeta,
					             .id = 0);
			st->ep.prob = 0;
			st->state   = NULL;
			st->arg_idx = PGF_EXPR_CHUNK_STATE;

			pgf_iter_generated_cats(ps, ps->before);

			PgfParseState* state = ps->before;
			while (state != NULL) {
				if (state->chunks_map != NULL)
					st->state = state;
				state = state->next;
			}

			if (st->state != NULL) {
				gu_buf_heap_push(ps->expr_queue, &pgf_expr_state_order, &st);
			}
			break;
		}

#ifdef PGF_COUNTS_DEBUG
		pgf_parsing_print_counts(ps);
#endif
	}

	// Now begin enumerating the resulting syntax trees
	ps->en.next = pgf_parse_result_enum_next;
	return &ps->en;
}

PGF_API PgfParsing*
pgf_parse_to_chart(PgfConcr* concr, PgfType* typ, GuString sentence,
                   double heuristics,
                   PgfCallbacksMap* callbacks,
                   size_t n_roots,
                   GuExn* err,
                   GuPool* pool, GuPool* out_pool)
{
	if (concr->sequences == NULL ||
	    concr->cnccats == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	// Begin parsing a sentence with the specified category
	PgfParsing* ps =
		pgf_parsing_init(concr, typ->cid, sentence, heuristics, callbacks, NULL, err, pool, out_pool);
	if (ps == NULL) {
		return NULL;
	}

#ifdef PGF_COUNTS_DEBUG
	pgf_parsing_print_counts(ps);
#endif

	while (gu_buf_length(ps->expr_queue) < n_roots) {
		if (!pgf_parsing_proceed(ps)) {
			break;
		}

#ifdef PGF_COUNTS_DEBUG
		pgf_parsing_print_counts(ps);
#endif
	}

	return ps;
}

PGF_API PgfCCats*
pgf_get_parse_roots(PgfParsing* ps, GuPool* pool)
{
	size_t n_cats   = 0;
	size_t n_states = gu_buf_length(ps->expr_queue);
	GuSeq* roots = gu_new_seq(PgfCCat*, n_states, pool);
	for (size_t i = 0; i < n_states; i++) {
		PgfCCat* ccat = gu_buf_get(ps->expr_queue, PgfExprState*, i)->answers->ccat;

		bool found = false;
		for (size_t j = 0; j < n_cats; j++) {
			if (gu_seq_get(roots, PgfCCat*, j) == ccat) {
				found = true;
				break;
			}
		}

		if (!found) {
			gu_seq_set(roots, PgfCCat*, n_cats, ccat);
			n_cats++;
		}
	}
	roots->len = n_cats;
	return roots;
}
	
PGF_API GuSeq*
pgf_ccat_to_range(PgfParsing* ps, PgfCCat* ccat, GuPool* pool)
{
	PgfItemConts*  conts = ccat->conts;
	PgfParseState* state = ps->before;
	GuBuf* buf = gu_new_buf(PgfParseRange, pool);

	while (conts != NULL) {
		PgfParseRange* range = gu_buf_extend(buf);
		range->start = conts->state->end_offset;
		range->end   = conts->state->end_offset;
		range->field = conts->ccat->cnccat->labels[conts->lin_idx];
		
		while (state != NULL) {
			if (pgf_parsing_get_completed(state, conts) == ccat) {
				if (state->start_offset >= range->start)
					range->end = state->start_offset;
				break;
			}
			state = state->next;
		}

		conts = conts->ccat->conts;
	}

	return gu_buf_data_seq(buf);
}

PGF_API PgfExprEnum*
pgf_parse_with_oracle(PgfConcr* concr, PgfType* typ,
                      GuString sentence,
                      PgfOracleCallback* oracle,
                      GuExn* err,
                      GuPool* pool, GuPool* out_pool)
{
	if (concr->sequences == NULL ||
	    concr->cnccats == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	// Begin parsing a sentence with the specified category
	PgfCallbacksMap* callbacks = pgf_new_callbacks_map(concr, out_pool); 
	PgfParsing* ps =
		pgf_parsing_init(concr, typ->cid, sentence, -1, callbacks, oracle, err, pool, out_pool);
	if (ps == NULL) {
		return NULL;
	}

#ifdef PGF_COUNTS_DEBUG
	pgf_parsing_print_counts(ps);
#endif

	while (gu_buf_length(ps->expr_queue) == 0) {
		if (!pgf_parsing_proceed(ps)) {
			GuExnData* exn = gu_raise(err, PgfParseError);
			exn->data = (void*) pgf_parsing_new_exception(ps, exn->pool);
			return NULL;
		}

#ifdef PGF_COUNTS_DEBUG
		pgf_parsing_print_counts(ps);
#endif
	}

	// Now begin enumerating the resulting syntax trees
	ps->en.next = pgf_parse_result_enum_next;
	return &ps->en;
}

static void
pgf_parser_completions_next(GuEnum* self, void* to, GuPool* pool)
{
	PgfParsing* ps =
		gu_container(self, PgfParsing, en);

	ps->tp = NULL;
	while (ps->tp == NULL) {
		if (!pgf_parsing_proceed(ps))
			break;
			
#ifdef PGF_COUNTS_DEBUG
		pgf_parsing_print_counts(ps);
#endif
	}

	*((PgfTokenProb**)to) = ps->tp;
}

PGF_API GuEnum*
pgf_complete(PgfConcr* concr, PgfType* type, GuString sentence, 
             GuString prefix, GuExn *err, GuPool* pool)
{
	if (concr->sequences == NULL ||
	    concr->cnccats == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	// Begin parsing a sentence with the specified category
	PgfCallbacksMap* callbacks =
		pgf_new_callbacks_map(concr, pool);
	PgfParsing* ps =
		pgf_parsing_init(concr, type->cid, sentence, -1.0, callbacks, NULL, err, pool, pool);
	if (ps == NULL) {
		return NULL;
	}

#ifdef PGF_COUNTS_DEBUG
	pgf_parsing_print_counts(ps);
#endif

	size_t len = strlen(ps->sentence);
	while (ps->before->end_offset < len) {
		if (!pgf_parsing_proceed(ps)) {
			GuExnData* exn = gu_raise(err, PgfParseError);
			exn->data = (void*) pgf_parsing_new_exception(ps, exn->pool);
			return NULL;
		}

#ifdef PGF_COUNTS_DEBUG
		pgf_parsing_print_counts(ps);
#endif
	}

	// Now begin enumerating the completions
	ps->en.next = pgf_parser_completions_next;
	ps->prefix  = prefix;
	ps->tp      = NULL;
	return &ps->en;
}

PGF_API void
pgf_parser_index(PgfConcr* concr, 
                 PgfCCat* ccat, PgfProduction prod,
                 bool is_lexical,
                 GuPool *pool)
{
	GuVariantInfo i = gu_variant_open(prod);
	switch (i.tag) {
	case PGF_PRODUCTION_APPLY: {
		PgfProductionApply* papp = i.data;

		if (!is_lexical)
			break;

		for (size_t lin_idx = 0; lin_idx < papp->fun->n_lins; lin_idx++) {
			PgfSequence* seq = papp->fun->lins[lin_idx];

			size_t i = gu_buf_length(seq->idx);
			while (i > 0) {
				PgfProductionIdxEntry* entry =
					gu_buf_index(seq->idx, PgfProductionIdxEntry, i-1);

				if (entry->ccat->fid < ccat->fid)
					break;
				if (entry->lin_idx <= lin_idx)
					break;

				i--;
			}

			PgfProductionIdxEntry* entry = gu_buf_insert(seq->idx, i);
			entry->ccat    = ccat;
			entry->lin_idx = lin_idx;
			entry->papp    = papp;
		}
		break;
	}
	case PGF_PRODUCTION_COERCE:
		// Nothing to be done here
		break;
	default:
		gu_impossible();
	}
}

PGF_INTERNAL prob_t
pgf_ccat_set_viterbi_prob(PgfCCat* ccat) {
	if (ccat->fid < 0)
		return 0;
	
	if (ccat->viterbi_prob == 0) {       // uninitialized
		ccat->viterbi_prob = INFINITY;   // set to infinity to avoid loops

		if (ccat->prods == NULL)
			return INFINITY;

		prob_t viterbi_prob = INFINITY;
		
		size_t n_prods = gu_seq_length(ccat->prods);
		for (size_t i = 0; i < n_prods; i++) {
			PgfProduction prod =
				gu_seq_get(ccat->prods, PgfProduction, i);		
			
			prob_t prob = 0;

			GuVariantInfo inf = gu_variant_open(prod);
			switch (inf.tag) {
			case PGF_PRODUCTION_APPLY: {
				PgfProductionApply* papp = inf.data;
				prob = papp->fun->ep->prob;
				
				size_t n_args = gu_seq_length(papp->args);
				for (size_t j = 0; j < n_args; j++) {
					PgfPArg* arg = gu_seq_index(papp->args, PgfPArg, j);
					prob += pgf_ccat_set_viterbi_prob(arg->ccat);
				}
				break;
			}
			case PGF_PRODUCTION_COERCE: {
				PgfProductionCoerce* pcoerce = inf.data;
				prob = pgf_ccat_set_viterbi_prob(pcoerce->coerce);
				break;
			}
			default:
				gu_impossible();
				return 0;
			}
			
			if (viterbi_prob > prob)
				viterbi_prob = prob;
		}
		
		ccat->viterbi_prob = viterbi_prob;
	}

	return ccat->viterbi_prob;
}
