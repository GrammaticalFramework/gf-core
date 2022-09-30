#include "data.h"
#include <queue>

PgfPhrasetableIds::PgfPhrasetableIds()
{
	next_id = 0;
	n_pairs = 0;
	pairs   = NULL;
	chains  = NULL;
}

void PgfPhrasetableIds::start(ref<PgfConcr> concr)
{
	next_id = 0;
	n_pairs = phrasetable_size(concr->phrasetable);
	size_t mem_size = sizeof(SeqIdPair)*n_pairs;
	pairs = (SeqIdPair*) malloc(mem_size);
	if (pairs == NULL)
		throw pgf_systemerror(ENOMEM);
	memset(pairs, 0, mem_size);
}

size_t PgfPhrasetableIds::add(ref<PgfSequence> seq)
{
	size_t index = (seq.as_object() >> 4) % n_pairs;
	if (pairs[index].seq == 0) {
		pairs[index].seq = seq;
		pairs[index].seq_id = next_id++;
		return pairs[index].seq_id;
	} else {
		SeqIdChain *chain =
			(SeqIdChain*) malloc(sizeof(SeqIdChain));
		if (chain == NULL)
			throw pgf_systemerror(ENOMEM);
		chain->next   = chains;
		chain->chain  = pairs[index].chain;
		chain->seq    = seq;
		chain->seq_id = next_id++;
		pairs[index].chain = chain;
		chains        = chain;
		return chain->seq_id;
	}
}

size_t PgfPhrasetableIds::get(ref<PgfSequence> seq)
{
	size_t index = (seq.as_object() >> 4) % n_pairs;
	if (pairs[index].seq == seq) {
		return pairs[index].seq_id;
	} else {
		SeqIdChain *chain = pairs[index].chain;
		while (chain != NULL) {
			if (chain->seq == seq)
				return chain->seq_id;
			chain = chain->chain;
		}
		throw pgf_error("Can't find sequence id");
	}
}

void PgfPhrasetableIds::end()
{
	next_id = 0;
	n_pairs = 0;
	
	while (chains != NULL) {
		SeqIdChain *next = chains->next;
		free(chains);
		chains = next;
	}

	free(pairs);
	pairs = NULL;
}

static
int lparam_cmp(PgfLParam *p1, PgfLParam *p2)
{
	if (p1->i0 < p2->i0)
		return -1;
	else if (p1->i0 > p2->i0)
		return 1;

	for (size_t i = 0; ; i++) {
        if (i >= p1->n_terms)
            return -(i < p2->n_terms);
        if (i >= p2->n_terms)
            return 1;

        if (p1->terms[i].factor > p2->terms[i].factor)
            return 1;
        else if (p1->terms[i].factor < p2->terms[i].factor)
            return -1;
        else if (p1->terms[i].var > p2->terms[i].var)
            return 1;
        else if (p1->terms[i].var < p2->terms[i].var)
            return -1;
    }

	return 0;
}

static
int sequence_cmp(ref<PgfSequence> seq1, ref<PgfSequence> seq2);

static
void symbol_cmp(PgfSymbol sym1, PgfSymbol sym2, int res[2])
{
    uint8_t t1 = ref<PgfSymbol>::get_tag(sym1);
    uint8_t t2 = ref<PgfSymbol>::get_tag(sym2);

    if (t1 != t2) {
		res[0] = (res[1] = ((int) t1) - ((int) t2));
		return;
	}

	switch (t1) {
	case PgfSymbolCat::tag: {
        auto sym_cat1 = ref<PgfSymbolCat>::untagged(sym1);
        auto sym_cat2 = ref<PgfSymbolCat>::untagged(sym2);
        if (sym_cat1->d < sym_cat2->d)
			res[0] = (res[1] = -1);
        else if (sym_cat1->d > sym_cat2->d)
			res[0] = (res[1] = 1);
		else
			res[0] = (res[1] = lparam_cmp(&sym_cat1->r, &sym_cat2->r));
		break;
    }
	case PgfSymbolLit::tag: {
        auto sym_lit1 = ref<PgfSymbolLit>::untagged(sym1);
        auto sym_lit2 = ref<PgfSymbolLit>::untagged(sym2);
        if (sym_lit1->d < sym_lit2->d)
			res[0] = (res[1] = -1);
        else if (sym_lit1->d > sym_lit2->d)
			res[0] = (res[1] = 1);
		else
			res[0] = (res[1] = lparam_cmp(&sym_lit1->r, &sym_lit2->r));
		break;
    }
	case PgfSymbolVar::tag: {
        auto sym_var1 = ref<PgfSymbolVar>::untagged(sym1);
        auto sym_var2 = ref<PgfSymbolVar>::untagged(sym2);
        if (sym_var1->d < sym_var2->d)
			res[0] = (res[1] = -1);
        else if (sym_var1->d > sym_var2->d)
			res[0] = (res[1] = 1);
		else if (sym_var1->r < sym_var2->r)
			res[0] = (res[1] = -1);
        else if (sym_var1->r > sym_var2->r)
			res[0] = (res[1] = 1);
		break;
    }
	case PgfSymbolKS::tag: {
        auto sym_ks1 = ref<PgfSymbolKS>::untagged(sym1);
        auto sym_ks2 = ref<PgfSymbolKS>::untagged(sym2);
        texticmp(&sym_ks1->token,&sym_ks2->token,res);
		break;
    }
	case PgfSymbolKP::tag: {
        auto sym_kp1 = ref<PgfSymbolKP>::untagged(sym1);
        auto sym_kp2 = ref<PgfSymbolKP>::untagged(sym2);
        res[0] = (res[1] = sequence_cmp(sym_kp1->default_form, sym_kp2->default_form));
        if (res[0] != 0)
			return;

		for (size_t i = 0; ; i++) {
			if (i >= sym_kp1->alts.len) {
				res[0] = (res[1] = -(i < sym_kp2->alts.len));
				return;
			}
			if (i >= sym_kp2->alts.len) {
				res[0] = (res[1] = 1);
				return;
			}

			res[0] = (res[1] = sequence_cmp(sym_kp1->alts.data[i].form, sym_kp2->alts.data[i].form));
			if (res[0] != 0)
				return;
				
			ref<Vector<ref<PgfText>>> prefixes1 = sym_kp1->alts.data[i].prefixes;
			ref<Vector<ref<PgfText>>> prefixes2 = sym_kp2->alts.data[i].prefixes;
			for (size_t j = 0; ; j++) {
				if (j >= prefixes1->len) {
					res[0] = (res[1] = -(j < prefixes2->len));
					return;
				}
				if (j >= prefixes2->len) {
					res[0] = (res[1] = 1);
					return;
				}

				res[0] = (res[1] = textcmp(&(**vector_elem(prefixes1, j)), &(**vector_elem(prefixes2, j))));
				if (res[0] != 0)
					return;
			}
		}
    }
	case PgfSymbolBIND::tag:
	case PgfSymbolSOFTBIND::tag:
	case PgfSymbolNE::tag:
	case PgfSymbolSOFTSPACE::tag:
	case PgfSymbolCAPIT::tag:
	case PgfSymbolALLCAPIT::tag:
        break;
	default:
		throw pgf_error("Unknown symbol tag");
    }
}

static
int sequence_cmp(ref<PgfSequence> seq1, ref<PgfSequence> seq2)
{
	int res[2] = {0,0};
	for (size_t i = 0; ; i++) {
        if (i >= seq1->syms.len) {
			if (i < seq2->syms.len)
				return -1;
            return res[1];
		}
        if (i >= seq2->syms.len)
            return 1;

		symbol_cmp(seq1->syms.data[i], seq2->syms.data[i], res);
		if (res[0] != 0)
			return res[0];
    }

	return 0;
}

static
int text_sequence_cmp(PgfTextSpot *spot, const uint8_t *end,
                      ref<PgfSequence> seq,
                      bool case_sensitive, bool full_match)
{
	int res1 = 0;

    size_t i = 0;
    const uint8_t *s2 = NULL;
    const uint8_t *e2 = NULL;

    size_t count = 0;

    for (;;) {
        if (spot->ptr >= end) {
            if (s2 < e2 || i < seq->syms.len)
                return -1;
            return case_sensitive ? res1 : 0;
        }

        if (s2 >= e2 && i >= seq->syms.len)
            return full_match ? 1 : 0;

        uint32_t ucs1  = pgf_utf8_decode(&spot->ptr); spot->pos++;
        uint32_t ucs1i = pgf_utf8_to_upper(ucs1);

        if (s2 >= e2) {
            if (s2 != NULL) {
                if (pgf_utf8_is_space(ucs1)) {
                    count++;
                    continue;
                }

                if (count == 0) {
                    return (((int) ucs1) - ' ');
                } else {
                    count = 0;
                }
            }

            uint8_t t = ref<PgfSymbol>::get_tag(seq->syms.data[i]);
            if (t != PgfSymbolKS::tag) {
                return ((int) PgfSymbolKS::tag) - ((int) t);
            }

            auto sym_ks = ref<PgfSymbolKS>::untagged(seq->syms.data[i]);
            s2 = (uint8_t *) &sym_ks->token.text;
            e2 = s2+sym_ks->token.size;

            i++;
        }

        uint32_t ucs2  = pgf_utf8_decode(&s2);
        uint32_t ucs2i = pgf_utf8_to_upper(ucs2);

        if (ucs1i > ucs2i) {
            return 1;
        }
        else if (ucs1i < ucs2i) {
            return -1;
        }
        else if (res1 == 0) {
            if (ucs1 > ucs2) {
                res1 =  1;
            } else if (ucs1 < ucs2) {
                res1 = -1;
            }
        }
    }
}

PGF_INTERNAL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table,
                                       ref<PgfSequence> seq,
                                       object container,
                                       size_t seq_index,
                                       ref<PgfPhrasetableEntry> *pentry)
{
    if (table == 0) {
        PgfPhrasetableEntry entry;
        entry.seq = seq;
        entry.backrefs = vector_new<PgfSequenceBackref>(1);
        entry.backrefs->data[0].container = container;
        entry.backrefs->data[0].seq_index = seq_index;
        PgfPhrasetable new_table = Node<PgfPhrasetableEntry>::new_node(entry);
        *pentry = ref<PgfPhrasetableEntry>::from_ptr(&new_table->value);
        return new_table;
	}

    int cmp = sequence_cmp(seq,table->value.seq);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_internalize(table->left,
                                                      seq,
                                                      container,
                                                      seq_index,
                                                      pentry);
        table = Node<PgfPhrasetableEntry>::upd_node(table,left,table->right);
        return Node<PgfPhrasetableEntry>::balanceL(table);
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_internalize(table->right,
                                                       seq,
                                                       container,
                                                       seq_index,
                                                       pentry);
        table = Node<PgfPhrasetableEntry>::upd_node(table, table->left, right);
        return Node<PgfPhrasetableEntry>::balanceR(table);
    } else {
        PgfSequence::release(seq);

        size_t len = (table->value.backrefs)
                         ? table->value.backrefs->len
                         : 0;

        ref<Vector<PgfSequenceBackref>> backrefs =
            vector_resize<PgfSequenceBackref>(table->value.backrefs, len+1, table->txn_id);
        backrefs->data[len].container = container;
        backrefs->data[len].seq_index = seq_index;

        PgfPhrasetable new_table =
            Node<PgfPhrasetableEntry>::upd_node(table, table->left, table->right);
        new_table->value.backrefs = backrefs;
        *pentry = ref<PgfPhrasetableEntry>::from_ptr(&new_table->value);
        return new_table;
     }
}

PGF_INTERNAL
ref<PgfSequence> phrasetable_relink(PgfPhrasetable table,
                                    object container,
                                    size_t seq_index,
                                    size_t seq_id)
{
	while (table != 0) {
		size_t left_sz = (table->left==0) ? 0 : table->left->sz;
		if (seq_id < left_sz)
			table = table->left;
		else if (seq_id == left_sz) {
            size_t len = (table->value.backrefs == 0)
                             ? 0
                             : table->value.backrefs->len;

            ref<Vector<PgfSequenceBackref>> backrefs =
                vector_resize<PgfSequenceBackref>(table->value.backrefs, len+1, table->txn_id);
            backrefs->data[len].container = container;
            backrefs->data[len].seq_index = seq_index;
            table->value.backrefs = backrefs;

			return table->value.seq;
        } else {
			table = table->right;
			seq_id -= left_sz+1;
		}
	}
	return 0;
}

PgfPhrasetable phrasetable_delete(PgfPhrasetable table,
                                  object container,
                                  size_t seq_index,
                                  ref<PgfSequence> seq)
{
    if (table == 0)
        return 0;

    int cmp = sequence_cmp(seq,table->value.seq);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_delete(table->left,
                                                 container, seq_index,
                                                 seq);
        if (left == table->left)
            return table;
        table = Node<PgfPhrasetableEntry>::upd_node(table,left,table->right);
        return Node<PgfPhrasetableEntry>::balanceR(table);
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_delete(table->right, 
                                                  container, seq_index,
                                                  seq);
        if (right == table->right)
            return table;
        table = Node<PgfPhrasetableEntry>::upd_node(table,table->left,right);
        return Node<PgfPhrasetableEntry>::balanceL(table);
    } else {
        size_t len = table->value.backrefs->len;
        if (len > 1) {
            ref<Vector<PgfSequenceBackref>> backrefs =
                vector_resize<PgfSequenceBackref>(table->value.backrefs, len-1, table->txn_id);
            size_t i = 0;
            while (i < len-1) {
                ref<PgfSequenceBackref> backref =
                    vector_elem(backrefs, i);
                if (backref->container == container &&
                    backref->seq_index == seq_index) {
                    break;
                }
                i++;
            }
            i++;
            while (i < len) {
                *vector_elem(backrefs, i-1) = *vector_elem(table->value.backrefs, i);
                i++;
            }

            PgfPhrasetable new_table =
                Node<PgfPhrasetableEntry>::upd_node(table, table->left, table->right);
            new_table->value.backrefs = backrefs;
            return new_table;
        } else {
            PgfSequence::release(table->value.seq);
            Vector<PgfSequenceBackref>::release(table->value.backrefs);
            if (table->left == 0) {
                Node<PgfPhrasetableEntry>::release(table);
                return table->right;
            } else if (table->right == 0) {
                Node<PgfPhrasetableEntry>::release(table);
                return table->left;
            } else if (table->left->sz > table->right->sz) {
                PgfPhrasetable node;
                PgfPhrasetable left = Node<PgfPhrasetableEntry>::pop_last(table->left, &node);
                node = Node<PgfPhrasetableEntry>::upd_node(node, left, table->right);
                Node<PgfPhrasetableEntry>::release(table);
                return Node<PgfPhrasetableEntry>::balanceR(node);
            } else {
                PgfPhrasetable node;
                PgfPhrasetable right = Node<PgfPhrasetableEntry>::pop_first(table->right, &node);
                node = Node<PgfPhrasetableEntry>::upd_node(node, table->left, right);
                Node<PgfPhrasetableEntry>::release(table);
                return Node<PgfPhrasetableEntry>::balanceL(node);
            }
        }
    }
}

PGF_INTERNAL
size_t phrasetable_size(PgfPhrasetable table)
{
    return Node<PgfPhrasetableEntry>::size(table);
}

PGF_INTERNAL
void phrasetable_lookup(PgfPhrasetable table,
                        PgfText *sentence,
                        bool case_sensitive,
                        PgfPhraseScanner *scanner, PgfExn* err)
{
    if (table == 0)
        return;

    PgfTextSpot current;
    current.pos = 0;
    current.ptr = (uint8_t *) sentence->text;
    const uint8_t *end = current.ptr+sentence->size;
    int cmp = text_sequence_cmp(&current,end,table->value.seq,case_sensitive,true);
    if (cmp < 0) {
        phrasetable_lookup(table->left,sentence,case_sensitive,scanner,err);
    } else if (cmp > 0) {
        phrasetable_lookup(table->right,sentence,case_sensitive,scanner,err);
    } else {
        auto backrefs = table->value.backrefs;
        if (backrefs != 0) {
            for (size_t i = 0; i < backrefs->len; i++) {
                PgfSequenceBackref backref = *vector_elem<PgfSequenceBackref>(backrefs,i);
                switch (ref<PgfConcrLin>::get_tag(backref.container)) {
                case PgfConcrLin::tag: {
                    ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(backref.container);
                    if (lin->absfun->type->hypos->len == 0) {
                        scanner->match(lin, backref.seq_index, err);
                        if (err->type != PGF_EXN_NONE)
                            return;
                    }
                    break;
                }
                case PgfConcrLincat::tag: {
                    //ignore
                    break;
                }
                }
            }
        }

        if (!case_sensitive) {
            phrasetable_lookup(table->left,sentence,false,scanner,err);
            if (err->type != PGF_EXN_NONE)
                return;
            phrasetable_lookup(table->right,sentence,false,scanner,err);
            if (err->type != PGF_EXN_NONE)
                return;
        }
     }
}

struct PGF_INTERNAL_DECL PgfCohortsState {
    class PgfTextSpotComparator : std::less<PgfTextSpot> {
    public:
        bool operator()(PgfTextSpot &lhs, PgfTextSpot &rhs) const 
        {
            return lhs.pos > rhs.pos;
        }
    };

    PgfTextSpot spot;
    std::priority_queue<PgfTextSpot, std::vector<PgfTextSpot>, PgfTextSpotComparator> queue;

    PgfTextSpot last;
    bool skipping;
    const uint8_t *end;    // pointer into the end of the sentence

    bool case_sensitive;
    PgfPhraseScanner *scanner;
    PgfExn* err;
};

static
void finish_skipping(PgfCohortsState *state) {
    if (state->skipping) {
        while (!state->queue.empty()) {
            PgfTextSpot spot = state->queue.top();
            if (spot.pos >= state->spot.pos)
                break;

            if (spot.pos != state->last.pos) {
                if (state->last.pos > 0) {
                    state->scanner->space(&spot, &spot,
                                          state->err);
                    if (state->err->type != PGF_EXN_NONE)
                        return;
                }

                state->scanner->start_matches(&state->spot,
                                              state->err);
                if (state->err->type != PGF_EXN_NONE)
                    return;

                state->scanner->end_matches(&state->spot,
                                            state->err);
                if (state->err->type != PGF_EXN_NONE)
                    return;

                state->last = spot;
            }

            state->queue.pop();
        }

        state->scanner->space(&state->spot, &state->spot,
                              state->err);

        state->last.pos = 0;
        state->last.ptr = NULL;
        state->skipping = false;
    }
}

static
void phrasetable_lookup_prefixes(PgfCohortsState *state,
                                 PgfPhrasetable table,
                                 ptrdiff_t min, ptrdiff_t max)
{
    if (table == 0)
        return;

    PgfTextSpot current = state->spot;
    int cmp = text_sequence_cmp(&current,state->end,table->value.seq,state->case_sensitive,false);
    if (cmp < 0) {
        phrasetable_lookup_prefixes(state,table->left,min,max);
    } else if (cmp > 0) {
        ptrdiff_t len = current.ptr - state->spot.ptr;

        if (min <= len-1)
            phrasetable_lookup_prefixes(state,table->left,min,len-1);

        if (len <= max)
            phrasetable_lookup_prefixes(state,table->right,len,max);
    } else {
        ptrdiff_t len = current.ptr - state->spot.ptr;

        finish_skipping(state);
        if (state->err->type != PGF_EXN_NONE)
            return;

        if (min <= len)
            phrasetable_lookup_prefixes(state,table->left,min,len);

        auto backrefs = table->value.backrefs;
        if (len > 0 && backrefs != 0) {
            if (state->last.pos != current.pos) {
                if (state->last.pos > 0) {
                    state->scanner->end_matches(&state->last,
                                                state->err);
                    if (state->err->type != PGF_EXN_NONE)
                        return;
                }

                state->scanner->start_matches(&current,
                                              state->err);
                if (state->err->type != PGF_EXN_NONE)
                    return;

                state->last = current;
            }
            state->queue.push(current);

            for (size_t i = 0; i < backrefs->len; i++) {
                PgfSequenceBackref backref = *vector_elem<PgfSequenceBackref>(backrefs,i);
                switch (ref<PgfConcrLin>::get_tag(backref.container)) {
                case PgfConcrLin::tag: {
                    ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(backref.container);
                    if (lin->absfun->type->hypos->len == 0) {
                        state->scanner->match(lin,
                                              backref.seq_index,
                                              state->err);
                        if (state->err->type != PGF_EXN_NONE)
                            return;
                    }
                    break;
                }
                case PgfConcrLincat::tag: {
                    //ignore
                    break;
                }
                }
            }
        }

        if (len <= max)
            phrasetable_lookup_prefixes(state,table->right,len,max);
     }
}

PGF_INTERNAL
void phrasetable_lookup_cohorts(PgfPhrasetable table,
                                PgfText *sentence,
                                bool case_sensitive,
                                PgfPhraseScanner *scanner, PgfExn* err)
{
    PgfTextSpot spot;
    spot.pos = 0;
    spot.ptr = (uint8_t *) sentence->text;

    PgfCohortsState state;
    state.spot.pos = -1;
    state.spot.ptr = NULL;
    state.queue.push(spot);
    state.last = spot;
    state.skipping = false;
    state.end = (uint8_t *) &sentence->text[sentence->size];
    state.case_sensitive = case_sensitive;
    state.scanner = scanner;
    state.err = err;

    while (!state.queue.empty()) {
        PgfTextSpot spot = state.queue.top();
        state.queue.pop();

        if (spot.pos != state.spot.pos) {
            state.spot = spot;

            // skip leading spaces
            while (state.spot.ptr < state.end) {
                const uint8_t *ptr = state.spot.ptr;
                uint32_t ucs = pgf_utf8_decode(&ptr);
                if (!pgf_utf8_is_space(ucs))
                    break;
                state.spot.pos++;
                state.spot.ptr = ptr;
            }

            state.scanner->space(&spot,&state.spot,state.err);
            if (state.err->type != PGF_EXN_NONE)
                return;

            while (state.spot.ptr < state.end) {
                phrasetable_lookup_prefixes(&state, table, 1, sentence->size);
                if (state.err->type != PGF_EXN_NONE)
                    return;

                if (state.last.pos > 0) {
                    // We found at least one match.
                    // The last range is yet to be reported.
                    state.scanner->end_matches(&state.last,
                                               state.err);
                    if (state.err->type != PGF_EXN_NONE)
                        return;
                    state.last.pos = 0;
                    state.last.ptr = (uint8_t*) sentence->text;
                    break;
                } else {
                    // No matches were found, try the next position
                    if (!state.skipping) {
                        while (!state.queue.empty() &&
                               state.queue.top().pos < state.spot.pos) {
                            state.queue.pop();
                        }
                        state.queue.push(state.spot);
                        state.skipping = true;
                    }

                    const uint8_t *ptr = state.spot.ptr;
                    uint32_t ucs = pgf_utf8_decode(&ptr);
                    if (pgf_utf8_is_space(ucs)) {
                        state.queue.push(state.spot);
                        break;
                    }
                    state.spot.pos++;
                    state.spot.ptr = ptr;
                }
            }

            finish_skipping(&state);
            if (state.err->type != PGF_EXN_NONE)
                return;

            state.spot = spot;
        }
    }
}

PGF_INTERNAL
void phrasetable_iter(PgfConcr *concr,
                      PgfPhrasetable table,
                      PgfSequenceItor* itor,
                      PgfMorphoCallback *callback,
                      PgfPhrasetableIds *seq_ids, PgfExn *err)
{
    if (table == 0)
        return;

    phrasetable_iter(concr, table->left, itor, callback, seq_ids, err);
    if (err->type != PGF_EXN_NONE)
        return;

	size_t seq_id = seq_ids->add(table->value.seq);
    int res = itor->fn(itor, seq_id, table->value.seq.as_object(), err);
    if (err->type != PGF_EXN_NONE)
        return;

    if (table->value.backrefs != 0 && res == 0 && callback != 0) {
        for (size_t i = 0; i < table->value.backrefs->len; i++) {
            PgfSequenceBackref backref = *vector_elem<PgfSequenceBackref>(table->value.backrefs,i);
            switch (ref<PgfConcrLin>::get_tag(backref.container)) {
            case PgfConcrLin::tag: {
                ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(backref.container);
                ref<PgfConcrLincat> lincat =
                    namespace_lookup(concr->lincats, &lin->absfun->type->name);
                if (lincat != 0) {
                    ref<PgfLincatField> field =
                        vector_elem(lincat->fields, backref.seq_index % lincat->fields->len);

                    callback->fn(callback, &lin->absfun->name, &(*field->name), lincat->abscat->prob+lin->absfun->prob, err);
                    if (err->type != PGF_EXN_NONE)
                        return;
                }
                break;
            }
            case PgfConcrLincat::tag: {
                //ignore
                break;
            }
            }
        }
    }

    phrasetable_iter(concr, table->right, itor, callback, seq_ids, err);
    if (err->type != PGF_EXN_NONE)
        return;
}

PGF_INTERNAL
void phrasetable_release(PgfPhrasetable table)
{
    if (table == 0)
        return;
    phrasetable_release(table->left);
    phrasetable_release(table->right);
    Node<PgfPhrasetableEntry>::release(table);
}
