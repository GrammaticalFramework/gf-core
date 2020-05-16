#include <pgf/data.h>
#include <pgf/expr.h>
#include <pgf/linearizer.h>
#include <gu/utf8.h>

PGF_INTERNAL int
cmp_string(PgfCohortSpot* spot, GuString tok,
           bool case_sensitive)
{
	for (;;) {
		GuUCS c2 = gu_utf8_decode((const uint8_t**) &tok);
		if (c2 == 0)
			return 0;

		const uint8_t* p = (uint8_t*) spot->ptr;
		GuUCS c1 = gu_utf8_decode(&p);
		if (c1 == 0)
			return -1;

		if (!case_sensitive) {
			c1 = gu_ucs_to_lower(c1);
			c2 = gu_ucs_to_lower(c2);
		}

		if (c1 != c2)
			return (c1-c2);

		spot->ptr = (GuString) p;
		spot->pos++;
	}
}

PGF_INTERNAL bool
skip_space(GuString* psent, size_t* ppos)
{
	const uint8_t* p = (uint8_t*) *psent;
	if (!gu_ucs_is_space(gu_utf8_decode(&p)))
		return false;

	*psent = (GuString) p;
	(*ppos)++;
	return true;
}

PGF_INTERNAL int
pgf_symbols_cmp(PgfCohortSpot* spot,
                PgfSymbols* syms, size_t* sym_idx,
                bool case_sensitive)
{
	size_t n_syms = gu_seq_length(syms);
	while (*sym_idx < n_syms) {
		PgfSymbol sym = gu_seq_get(syms, PgfSymbol, *sym_idx);

		if (*sym_idx > 0) {
			if (!skip_space(&spot->ptr,&spot->pos)) {
				if (*spot->ptr == 0)
					return -1;
				return 1;
			}

			while (*spot->ptr != 0) {
				if (!skip_space(&spot->ptr,&spot->pos))
					break;
			}
		}

		GuVariantInfo inf = gu_variant_open(sym);
		switch (inf.tag) {
		case PGF_SYMBOL_CAT:
		case PGF_SYMBOL_LIT:
		case PGF_SYMBOL_VAR: {
			if (*spot->ptr == 0)
				return -1;
			return 1;
		}
		case PGF_SYMBOL_KS: {
			PgfSymbolKS* pks = inf.data;
			if (*spot->ptr == 0)
				return -1;

			int cmp = cmp_string(spot,pks->token, case_sensitive);
			if (cmp != 0)
				return cmp;
			break;
		}
		case PGF_SYMBOL_KP:
		case PGF_SYMBOL_BIND:
		case PGF_SYMBOL_NE:
		case PGF_SYMBOL_SOFT_BIND:
		case PGF_SYMBOL_SOFT_SPACE:
		case PGF_SYMBOL_CAPIT:
		case PGF_SYMBOL_ALL_CAPIT: {
			return -1;
		}
		default:
			gu_impossible();
		}

		(*sym_idx)++;
	}

	return 0;
}

static void
pgf_morpho_iter(PgfProductionIdx* idx,
                PgfMorphoCallback* callback,
                GuExn* err)
{
	size_t n_entries = gu_buf_length(idx);
	for (size_t i = 0; i < n_entries; i++) {
		PgfProductionIdxEntry* entry =
			gu_buf_index(idx, PgfProductionIdxEntry, i);

		PgfCId lemma = entry->papp->fun->absfun->name;
		GuString analysis = entry->ccat->cnccat->labels[entry->lin_idx];

		prob_t prob = entry->papp->fun->absfun->ep.prob;
		callback->callback(callback,
						   lemma, analysis, prob, err);
		if (!gu_ok(err))
			return;
	}
}

typedef struct {
	GuOrder order;
	bool case_sensitive;
} PgfSequenceOrder;

PGF_INTERNAL bool
pgf_is_case_sensitive(PgfConcr* concr)
{
	PgfFlag* flag =
		gu_seq_binsearch(concr->cflags, pgf_flag_order, PgfFlag, "case_sensitive");
	if (flag != NULL) {
		GuVariantInfo inf = gu_variant_open(flag->value);
		if (inf.tag == PGF_LITERAL_STR) {
			PgfLiteralStr* lstr = inf.data;
			if (strcmp(lstr->val, "off") == 0)
				return false;
		}
	}
	return true;
}

static int
pgf_sequence_cmp_fn(GuOrder* order, const void* p1, const void* p2)
{
	PgfSequenceOrder* self = gu_container(order, PgfSequenceOrder, order);

	PgfCohortSpot spot = {0, (GuString) p1};

	const PgfSequence* sp2 = p2;

	size_t sym_idx = 0;
	int res = pgf_symbols_cmp(&spot, sp2->syms, &sym_idx, self->case_sensitive);
	if (res == 0 && (*spot.ptr != 0 || sym_idx != gu_seq_length(sp2->syms))) {
		res = 1;
	}

	return res;
}

PGF_API void
pgf_lookup_morpho(PgfConcr *concr, GuString sentence,
                  PgfMorphoCallback* callback, GuExn* err)
{
	if (concr->sequences == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return;
		}
	}

	size_t index = 0;
	PgfSequenceOrder order = { { pgf_sequence_cmp_fn },
		                       pgf_is_case_sensitive(concr) };
	if (gu_seq_binsearch_index(concr->sequences, &order.order,
	                           PgfSequence, (void*) sentence, 
	                           &index)) {
		PgfSequence* seq = NULL;

		/* If the match is case-insensitive then there might be more
		 * matches around the current index. We must check the neighbour
		 * sequences for matching as well.
		 */

		if (!order.case_sensitive) {
			size_t i = index;
			while (i > 0) {
				seq = gu_seq_index(concr->sequences, PgfSequence, i-1);

				size_t sym_idx = 0;
				PgfCohortSpot spot = {0, sentence};
				if (pgf_symbols_cmp(&spot, seq->syms, &sym_idx, order.case_sensitive) != 0) {
					break;
				}

				if (seq->idx != NULL)
					pgf_morpho_iter(seq->idx, callback, err);

				i--;
			}
		}

		seq = gu_seq_index(concr->sequences, PgfSequence, index);
		if (seq->idx != NULL)
			pgf_morpho_iter(seq->idx, callback, err);

		if (!order.case_sensitive) {
			size_t i = index+1;
			while (i < gu_seq_length(concr->sequences)) {
				seq = gu_seq_index(concr->sequences, PgfSequence, i);

				size_t sym_idx = 0;
				PgfCohortSpot spot = {0, sentence};
				if (pgf_symbols_cmp(&spot, seq->syms, &sym_idx, order.case_sensitive) != 0) {
					break;
				}

				if (seq->idx != NULL)
					pgf_morpho_iter(seq->idx, callback, err);

				i++;
			}
		}
	}
}

typedef struct {
	GuEnum en;
	PgfConcr* concr;
	GuString sentence;
	size_t len;
	PgfMorphoCallback* callback;
    GuExn* err;
	bool case_sensitive;
	GuBuf* spots;
	GuBuf* skip_spots;
	GuBuf* empty_buf;
	GuBuf* found;
} PgfCohortsState;

static int
cmp_cohort_spot(GuOrder* self, const void* a, const void* b)
{
	PgfCohortSpot *s1 = (PgfCohortSpot *) a;
	PgfCohortSpot *s2 = (PgfCohortSpot *) b;

	return (s1->ptr-s2->ptr);
}

static GuOrder
pgf_cohort_spot_order[1] = {{ cmp_cohort_spot }};

static void
pgf_lookup_cohorts_report_skip(PgfCohortsState *state,
                               PgfCohortSpot* spot)
{
	size_t n_spots = gu_buf_length(state->skip_spots);
	for (size_t i = 0; i < n_spots; i++) {
		PgfCohortSpot* skip_spot =
			gu_buf_index(state->skip_spots, PgfCohortSpot, i);

		PgfCohortRange* range = gu_buf_insert(state->found, 0);
		range->start = *skip_spot;
		range->end   = *spot;
		range->buf   = state->empty_buf;
	}
	gu_buf_flush(state->skip_spots);
}

static void
pgf_lookup_cohorts_helper(PgfCohortsState *state, PgfCohortSpot* spot,
                          int i, int j, ptrdiff_t min, ptrdiff_t max)
{
	// This is a variation of a binary search algorithm which
	// can retrieve all prefixes of a string with minimal
	// comparisons, i.e. there is no need to lookup every
	// prefix separately.

	while (i <= j) {
		int k  = (i+j) / 2;
		PgfSequence* seq = gu_seq_index(state->concr->sequences, PgfSequence, k);

		PgfCohortSpot current = *spot;

		size_t sym_idx = 0;
		int cmp = pgf_symbols_cmp(&current, seq->syms, &sym_idx, state->case_sensitive);
		if (cmp < 0) {
			j = k-1;
		} else if (cmp > 0) {
			ptrdiff_t len = current.ptr - spot->ptr;

			if (min <= len)
				pgf_lookup_cohorts_helper(state, spot, i, k-1, min, len);

			if (len+1 <= max)
				pgf_lookup_cohorts_helper(state, spot, k+1, j, len+1, max);

			break;
		} else {
			ptrdiff_t len = current.ptr - spot->ptr;

			if (min <= len)
				pgf_lookup_cohorts_helper(state, spot, i, k-1, min, len);

			if (seq->idx != NULL && gu_buf_length(seq->idx) > 0) {
				// Report unknown words
				pgf_lookup_cohorts_report_skip(state, spot);

				// Report the actual hit
				PgfCohortRange* range = gu_buf_insert(state->found, 0);
				range->start = *spot;
				range->end   = current;
				range->buf   = seq->idx;

				// Schedule the next search spot
				while (*current.ptr != 0) {
					if (!skip_space(&current.ptr, &current.pos))
						break;
				}

				gu_buf_heap_push(state->spots, pgf_cohort_spot_order, &current);
			}

			if (len <= max)
				pgf_lookup_cohorts_helper(state, spot, k+1, j, len, max);

			break;
		}
	}
}

static void
pgf_lookup_cohorts_enum_next(GuEnum* self, void* to, GuPool* pool)
{
	PgfCohortsState* state = gu_container(self, PgfCohortsState, en);

	while (gu_buf_length(state->found) == 0 &&
	       gu_buf_length(state->spots) > 0) {
		PgfCohortSpot spot;
		gu_buf_heap_pop(state->spots, pgf_cohort_spot_order, &spot);

		GuString next_ptr = state->sentence+state->len;
		while (gu_buf_length(state->spots) > 0) {
			GuString ptr =
				gu_buf_index(state->spots, PgfCohortSpot, 0)->ptr;
			if (ptr > spot.ptr) {
				next_ptr = ptr;
				break;
			}
			gu_buf_heap_pop(state->spots, pgf_cohort_spot_order, &spot);
		}

		bool needs_report = true;
		while (next_ptr > spot.ptr) {
			pgf_lookup_cohorts_helper
			               (state, &spot,
			                0, gu_seq_length(state->concr->sequences)-1,
			                1, (state->sentence+state->len)-spot.ptr);

			// got a hit -> exit
			if (gu_buf_length(state->found) > 0)
				break;

			if (needs_report) {
				// no hit, but the word must be reported as unknown.
				gu_buf_push(state->skip_spots, PgfCohortSpot, spot);
				needs_report = false;
			}

			// skip one character
			const uint8_t* ptr = (const uint8_t*) spot.ptr;
			GuUCS c = gu_utf8_decode(&ptr);
			if (gu_ucs_is_space(c)) {
				// We have encounter a space and we must report
				// a new unknown word.
				pgf_lookup_cohorts_report_skip(state, &spot);

				spot.ptr = (GuString) ptr;
				spot.pos++;

				// Schedule the next search spot
				while (*spot.ptr != 0) {
					if (!skip_space(&spot.ptr, &spot.pos))
						break;
				}

				gu_buf_heap_push(state->spots, pgf_cohort_spot_order, &spot);
				break;
			} else {
				spot.ptr = (GuString) ptr;
				spot.pos++;
			}
		}
	}

	PgfCohortSpot end_spot = {state->len, state->sentence+state->len};
	pgf_lookup_cohorts_report_skip(state, &end_spot);

	PgfCohortRange* pRes = (PgfCohortRange*)to;

	if (gu_buf_length(state->found) == 0) {
		pRes->start.pos = 0;
		pRes->start.ptr = NULL;
		pRes->end.pos   = 0;
		pRes->end.ptr   = NULL;
		pRes->buf       = NULL;
	} else for (;;) {
		*pRes = gu_buf_pop(state->found, PgfCohortRange);
		pgf_morpho_iter(pRes->buf, state->callback, state->err);

		if (gu_buf_length(state->found) <= 0)
			break;

		PgfCohortRange* last =
	         gu_buf_index_last(state->found, PgfCohortRange);
		if (last->start.ptr != pRes->start.ptr ||
		    last->end.ptr   != pRes->end.ptr)
		    break;
	}
}

PGF_API GuEnum*
pgf_lookup_cohorts(PgfConcr *concr, GuString sentence,
                   PgfMorphoCallback* callback,
                   GuPool* pool, GuExn* err)
{
	if (concr->sequences == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	PgfCohortsState* state = gu_new(PgfCohortsState, pool);
	state->en.next       = pgf_lookup_cohorts_enum_next;
	state->concr         = concr;
	state->sentence      = sentence;
	state->len           = strlen(sentence);
	state->callback      = callback;
	state->err           = err;
	state->case_sensitive= pgf_is_case_sensitive(concr);
	state->spots         = gu_new_buf(PgfCohortSpot, pool);
	state->skip_spots    = gu_new_buf(PgfCohortSpot, pool);
	state->empty_buf     = gu_new_buf(PgfProductionIdxEntry, pool);
	state->found         = gu_new_buf(PgfCohortRange, pool);

	PgfCohortSpot spot = {0,sentence};
	while (*spot.ptr != 0) {
		if (!skip_space(&spot.ptr, &spot.pos))
			break;
	}

	gu_buf_heap_push(state->spots, pgf_cohort_spot_order, &spot);

	return &state->en;
}

typedef struct {
	GuEnum en;
	PgfSequences* sequences;
	GuString prefix;
	size_t seq_idx;
	bool case_sensitive;
} PgfFullFormState;

struct PgfFullFormEntry {
	GuString tokens;
	PgfProductionIdx* idx;
};

static void
gu_fullform_enum_next(GuEnum* self, void* to, GuPool* pool)
{
	PgfFullFormState* st = gu_container(self, PgfFullFormState, en);
	PgfFullFormEntry* entry = NULL;

	if (st->sequences != NULL) {
		size_t n_seqs = gu_seq_length(st->sequences);
		while (st->seq_idx < n_seqs) {
			PgfSequence* seq = gu_seq_index(st->sequences, PgfSequence, st->seq_idx);
			GuString tokens = pgf_get_tokens(seq->syms, 0, pool);

			PgfCohortSpot spot = {0, st->prefix};
			if (cmp_string(&spot, tokens, st->case_sensitive) > 0 || *spot.ptr != 0) {
				st->seq_idx = n_seqs;
				break;
			}

			if (*tokens != 0 && seq->idx != NULL) {
				entry = gu_new(PgfFullFormEntry, pool);
				entry->tokens = tokens;
				entry->idx    = seq->idx;

				st->seq_idx++;
				break;
			}

			st->seq_idx++;
		}
	}

	*((PgfFullFormEntry**) to) = entry;
}

PGF_API GuEnum*
pgf_fullform_lexicon(PgfConcr *concr, GuPool* pool)
{
	PgfFullFormState* st = gu_new(PgfFullFormState, pool);
	st->en.next   = gu_fullform_enum_next;
	st->sequences = concr->sequences;
	st->prefix    = "";
	st->seq_idx   = 0;
	st->case_sensitive = true;
	return &st->en;
}

PGF_API GuString
pgf_fullform_get_string(PgfFullFormEntry* entry)
{
	return entry->tokens;
}

PGF_API void
pgf_fullform_get_analyses(PgfFullFormEntry* entry,
                          PgfMorphoCallback* callback, GuExn* err)
{
	pgf_morpho_iter(entry->idx, callback, err);
}

PGF_API GuEnum*
pgf_lookup_word_prefix(PgfConcr *concr, GuString prefix,
                       GuPool* pool, GuExn* err)
{
	if (concr->sequences == NULL) {
		GuExnData* err_data = gu_raise(err, PgfExn);
		if (err_data) {
			err_data->data = "The concrete syntax is not loaded";
			return NULL;
		}
	}

	PgfFullFormState* state = gu_new(PgfFullFormState, pool);
	state->en.next   = gu_fullform_enum_next;
	state->sequences = concr->sequences;
	state->prefix    = prefix;
	state->seq_idx   = 0;
	state->case_sensitive = pgf_is_case_sensitive(concr);

	PgfSequenceOrder order = { { pgf_sequence_cmp_fn }, 
		                       state->case_sensitive };
	if (!gu_seq_binsearch_index(concr->sequences, &order.order,
	                            PgfSequence, (void*) prefix, 
	                            &state->seq_idx)) {
		state->seq_idx++;
	} else if (!state->case_sensitive) {
		/* If the match is case-insensitive then there might be more
		 * matches around the current index. Since we scroll down 
		 * anyway, it is enough to search upwards now.
		 */

		while (state->seq_idx > 0) {
			PgfSequence* seq =
				gu_seq_index(concr->sequences, PgfSequence, state->seq_idx-1);

			size_t sym_idx = 0;
			PgfCohortSpot spot = {0, state->prefix};
			if (pgf_symbols_cmp(&spot, seq->syms, &sym_idx, state->case_sensitive) > 0 || *spot.ptr != 0) {
				break;
			}

			state->seq_idx--;
		}
	}

	return &state->en;
}
