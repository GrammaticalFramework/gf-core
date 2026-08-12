#include "data.h"
#include "printer.h"
#include <queue>

// #define DEBUG_PARSE_INDEX

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

PGF_INTERNAL
int text_symbol_cmp(PgfTextSpot *spot, const uint8_t *end,
                    ref<PgfSymbolKS> sym_ks, bool case_sensitive)
{
    int res1 = 0;

    const uint8_t *s2 = (uint8_t *) &sym_ks->token.text;
    const uint8_t *e2 = s2+sym_ks->token.size;

    for (;;) {
        if (spot->ptr >= end) {
            if (s2 < e2)
                return -1;
            return case_sensitive ? res1 : 0;
        }

        if (s2 >= e2) {
            return case_sensitive ? res1 : 0;
        }

        uint32_t ucs1  = pgf_utf8_decode(&spot->ptr); spot->pos++;
        uint32_t ucs1i = pgf_utf8_to_upper(ucs1);

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

static
bool text_symbols_match(PgfTextSpot *spot, const uint8_t *end,
                        vector<PgfSymbol> syms, size_t dot, bool *bind,
                        bool case_sensitive)
{
    while (dot < syms.size()) {
        PgfSymbol sym = syms[dot];
        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolKS::tag: {
            const uint8_t *start = spot->ptr;
            for (;;) {
                const uint8_t *ptr = spot->ptr;
                uint32_t ucs = pgf_utf8_decode(&ptr);
                if (!pgf_utf8_is_space(ucs))
                    break;
                spot->ptr = ptr;
                spot->pos++;
            }

            if (*bind != (start == spot->ptr))
                return false;

            if (text_symbol_cmp(spot,end,sym,case_sensitive) != 0)
                return false;

            break;
        }
        case PgfSymbolKP::tag: {
            auto symkp = ref<PgfSymbolKP>::untagged(syms[dot]);

            PgfTextSpot current = *spot;
            if (text_symbols_match(&current, end, symkp->default_form, 0, bind, case_sensitive)) {
                goto matched;
            }

            for (size_t i = 0; i < symkp->alts.size(); i++) {
                current = *spot;
                if (text_symbols_match(&current, end, symkp->alts[i].form, 0, bind, case_sensitive)) {
                    goto matched;
                }
            }

            return false;

        matched:
            *spot = current;
            break;
        }
        case PgfSymbolBIND::tag: {
            *bind = true;
            break;
        }
        case PgfSymbolSOFTBIND::tag:
        case PgfSymbolSOFTSPACE::tag: {
            *bind = true;
            break;
        }
        case PgfSymbolCAPIT::tag:
        case PgfSymbolALLCAPIT::tag:
            // skip
            break;
        default:
            return false;
        }

        dot++;
    }

    return true;
}

static
bool text_item_match(PgfTextSpot *spot, const uint8_t *end,
                     ref<PgfItem> item,
                     bool case_sensitive)
{
    bool bind = false;
    size_t dot = item->dot+1;
    vector<PgfSymbol> syms = item->rule->syms.as_vector();
    if (item->pre_alt > 0) {
        auto symkp = ref<PgfSymbolKP>::untagged(syms[item->pre_dot]);
        if (item->pre_alt == 1) {
            if (!text_symbols_match(spot, end, symkp->default_form, item->dot, &bind, case_sensitive))
                return false;
        } else {
            if (!text_symbols_match(spot, end, symkp->alts[item->pre_alt-2].form, item->dot, &bind, case_sensitive))
                return false;
        }
        dot = item->pre_dot+1;
    }
    return text_symbols_match(spot, end, syms, dot, &bind, case_sensitive);
}

PGF_INTERNAL_DECL
size_t get_next_padovan(size_t min);

template<class K>
PGF_INTERNAL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable<K> phrasetable,
                                        ref<K> key,
                                        size_t *n_items)
{
    while (phrasetable != 0) {
        int cmp = compare_key(key, phrasetable->value.key);
        if (cmp < 0)
            phrasetable = phrasetable->left;
        else if (cmp > 0)
            phrasetable = phrasetable->right;
        else {
            *n_items = phrasetable->value.n_items;
            return phrasetable->value.items;
        }
    }

    *n_items = 0;
    return 0;
}

PGF_INTERNAL
void phrasetable_lookup(PgfPhrasetable<PgfSymbolKS> table,
                        PgfText *sentence,
                        bool case_sensitive,
                        PgfPhraseScanner *scanner, PgfExn* err)
{
    if (table == 0)
        return;

    PgfTextSpot spot;
    spot.pos = 0;
    spot.ptr = (uint8_t *) sentence->text;
    const uint8_t *end = spot.ptr+sentence->size;
    int cmp = text_symbol_cmp(&spot,end,table->value.key,case_sensitive);
    if (cmp < 0) {
        phrasetable_lookup(table->left,sentence,case_sensitive,scanner,err);
    } else if (cmp > 0) {
        phrasetable_lookup(table->right,sentence,case_sensitive,scanner,err);
    } else {
        if (!case_sensitive) {
            phrasetable_lookup(table->left,sentence,case_sensitive,scanner,err);
            if (err->type != PGF_EXN_NONE)
                return;
        }

        for (size_t i = 0; i < table->value.n_items; i++) {
            ref<PgfItem> item = table->value.items[i];
            switch (ref<PgfConcrLin>::get_tag(item->rule->container)) {
            case PgfConcrLin::tag: {
                ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(item->rule->container);
                if (lin->absfun->type->hypos.size() == 0) {
                    PgfTextSpot current = spot;
                    if (text_item_match(&current, end, item, case_sensitive) && current.ptr == end) {
                        scanner->match(lin, item->rule->lin_idx->i0, err);
                        if (err->type != PGF_EXN_NONE)
                            return;
                    }
                }
                break;
            }
            case PgfConcrLincat::tag: {
                //ignore
                break;
            }
            }
        }

        if (!case_sensitive) {
            phrasetable_lookup(table->right,sentence,case_sensitive,scanner,err);
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
/*
        state->scanner->space(&state->spot, &state->spot,
                              state->err);
*/
        state->last.pos = 0;
        state->last.ptr = NULL;
        state->skipping = false;
    }
}

static
void phrasetable_lookup_prefixes(PgfCohortsState *state,
                                 PgfPhrasetable<PgfSymbolKS> table,
                                 ptrdiff_t min, ptrdiff_t max)
{
    if (table == 0)
        return;

    PgfTextSpot current = state->spot;
    int cmp = text_symbol_cmp(&current,state->end,table->value.key,state->case_sensitive);
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

        if (len > 0) {
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

            for (size_t i = 0; i < table->value.n_items; i++) {
                auto rule = table->value.items[i]->rule;
                switch (ref<PgfConcrLin>::get_tag(rule->container)) {
                case PgfConcrLin::tag: {
                    ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(rule->container);
                    if (lin->absfun->type->hypos.size() == 0) {
                        state->scanner->match(lin,
                                              rule->lin_idx->i0,
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
void phrasetable_lookup_cohorts(PgfPhrasetable<PgfSymbolKS> table,
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

template<class K>
PGF_INTERNAL
PgfPhrasetable<K> phrasetable_insert(PgfPhrasetable<K> table,
                                     ref<K> key, ref<PgfItem> item)
{
    if (table == 0) {
        auto items = vector<ref<PgfItem>>::alloc(1);
        items[0] = item;
        return Node<PgfPhrasetableValue<K>>::new_node({.key=key,.n_items=1,.items=items});
	}

    int cmp = compare_key(key, table->value.key);
    if (cmp < 0) {
        PgfPhrasetable<K> left = phrasetable_insert(table->left, key, item);
        table = Node<PgfPhrasetableValue<K>>::upd_node(table,left,table->right);
        return Node<PgfPhrasetableValue<K>>::balanceL(table);
    } else if (cmp > 0) {
        PgfPhrasetable<K> right = phrasetable_insert(table->right, key, item);
        table = Node<PgfPhrasetableValue<K>>::upd_node(table, table->left, right);
        return Node<PgfPhrasetableValue<K>>::balanceR(table);
    } else {
        PgfPhrasetable<K> new_table =
            Node<PgfPhrasetableValue<K>>::upd_node(table, table->left, table->right);

        auto items = new_table->value.items;
        if (new_table->value.n_items >= items.size()) {
            size_t new_len = get_next_padovan(new_table->value.n_items+1);
            items = items.realloc(new_len, new_table->txn_id);
        }
        items[new_table->value.n_items] = item;
        new_table->value.n_items++;
        new_table->value.items = items;
        return new_table;
    }
}

static
int compare_key(ref<PgfSymbolKS> symks1, ref<PgfSymbolKS> symks2) {
    int res[2] = {0,0};
    texticmp(&symks1->token, &symks2->token, res);
    if (res[0] != 0)
        return res[0];
    return res[1];
}

template
PgfPhrasetable<PgfSymbolKS> phrasetable_insert<PgfSymbolKS>(PgfPhrasetable<PgfSymbolKS> table,
                                                            ref<PgfSymbolKS> key,
                                                            ref<PgfItem> item);

static
int compare_key(ref<PgfConcrLincat> lincat1, ref<PgfConcrLincat> lincat2) {
    return textcmp(&lincat1->name, &lincat2->name);
}

template
PGF_INTERNAL
PgfPhrasetable<PgfConcrLincat> phrasetable_insert<PgfConcrLincat>(PgfPhrasetable<PgfConcrLincat> table,
                                                                  ref<PgfConcrLincat> key,
                                                                  ref<PgfItem> item);

template
PGF_INTERNAL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable<PgfConcrLincat> phrasetable,
                                        ref<PgfConcrLincat> key,
                                        size_t *n_items);

static
int compare_key(ref<PgfCCat> ccat1, ref<PgfCCat> ccat2) {
    return ((int) ccat1->fid) - ((int) ccat2->fid);
}

template
PgfPhrasetable<PgfCCat> phrasetable_insert<PgfCCat>(PgfPhrasetable<PgfCCat> table,
                                                    ref<PgfCCat> key,
                                                    ref<PgfItem> item);

template
PGF_INTERNAL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable<PgfCCat> phrasetable,
                                        ref<PgfCCat> key,
                                        size_t *n_items);

static
int compare_key(ref<PgfSymbolBIND> symbind1, ref<PgfSymbolBIND> symbind2) {
    return 0;
}

template
PgfPhrasetable<PgfSymbolBIND> phrasetable_insert<PgfSymbolBIND>(PgfPhrasetable<PgfSymbolBIND> table,
                                                                ref<PgfSymbolBIND> key,
                                                                ref<PgfItem> item);

template
PGF_INTERNAL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable<PgfSymbolBIND> phrasetable,
                                        ref<PgfSymbolBIND> key,
                                        size_t *n_items);

PGF_INTERNAL
PgfEpsilontable epsilontable_insert(PgfEpsilontable table,
                                    ref<PgfConcrLincat> lincat,
                                    interval_t value, interval_t lin_idx,
                                    PgfMetaId fid, prob_t viterbi_prob,
                                    ref<PgfItem> item,
                                    ref<PgfCCat> *pepsilon)
{
    if (table == 0) {
        auto items = vector<ref<PgfItem>>::alloc(1);
        items[0] = item;
        PgfEpsilontable new_table =
            Node<PgfCCat>::new_node({.lincat=lincat,
                                     .fid=fid,
                                     .value=value,
                                     .lin_idx=lin_idx,
                                     .viterbi_prob=viterbi_prob,
                                     .n_items=1,
                                     .items=items});
        *pepsilon = ref<PgfCCat>::from_ptr(&new_table->value);
        return new_table;
	}

    int cmp = textcmp(&lincat->name, &table->value.lincat->name);
    if (cmp == 0) {
        cmp = ((int)fid) - ((int)table->value.fid);
    }

    if (cmp < 0) {
        PgfEpsilontable left = epsilontable_insert(table->left,
                                                   lincat, value, lin_idx, fid, viterbi_prob, item, pepsilon);
        table = Node<PgfCCat>::upd_node(table,left,table->right);
        return Node<PgfCCat>::balanceL(table);
    } else if (cmp > 0) {
        PgfEpsilontable right = epsilontable_insert(table->right,
                                                    lincat, value, lin_idx, fid, viterbi_prob, item, pepsilon);
        table = Node<PgfCCat>::upd_node(table, table->left, right);
        return Node<PgfCCat>::balanceR(table);
    } else {
        PgfEpsilontable new_table =
            Node<PgfCCat>::upd_node(table, table->left, table->right);

        auto items = table->value.items;
        if (table->value.n_items >= items.size()) {
            size_t new_len = get_next_padovan(table->value.n_items+1);
            items = items.realloc(new_len, table->txn_id);
        }
        items[table->value.n_items] = item;
        new_table->value.n_items++;
        new_table->value.items = items;
        *pepsilon = ref<PgfCCat>::from_ptr(&new_table->value);
        return new_table;
    }
}

PGF_INTERNAL
void epsilontable_add(ref<PgfCCat> epsilon,ref<PgfItem> item)
{
    auto items = epsilon->items;
    if (epsilon->n_items >= items.size()) {
        size_t new_len = get_next_padovan(epsilon->n_items+1);
        items = items.realloc(new_len, PgfDB::get_txn_id());
    }
    items[epsilon->n_items] = item;
    epsilon->n_items++;
    epsilon->items = items;
}

PGF_INTERNAL
ref<PgfCCat> epsilontable_get(PgfEpsilontable table,
                              PgfText *name, PgfMetaId fid)
{
    if (table == 0) {
        return 0;
	}

    int cmp = textcmp(name, &table->value.lincat->name);
    if (cmp == 0) {
        cmp = ((int)fid) - ((int)table->value.fid);
    }

    if (cmp < 0) {
        return epsilontable_get(table->left,name,fid);
    } else if (cmp > 0) {
        return epsilontable_get(table->right,name,fid);
    } else {
        return ref<PgfCCat>::from_ptr(&table->value);
    }
}

PGF_INTERNAL
void epsilontable_iter(PgfEpsilontable table, ref<PgfConcrLincat> lincat, std::function<void(ref<PgfCCat> arg)> &f)
{
    if (table == 0)
        return;

    int cmp = textcmp(&lincat->name, &table->value.lincat->name);
    if (cmp < 0)
        epsilontable_iter(table->left,  lincat, f);
    else if (cmp > 0)
        epsilontable_iter(table->right, lincat, f);
    else {
        epsilontable_iter(table->left,  lincat, f);
        f(ref<PgfCCat>::from_ptr(&table->value));
        epsilontable_iter(table->right, lincat, f);
    }
}

PGF_INTERNAL
void epsilontable_release(PgfEpsilontable table)
{
    if (table == 0)
        return;
    epsilontable_release(table->left);
    epsilontable_release(table->right);
    for (size_t i = 0; i < table->value.n_items; i++) {
        PgfItem::release(table->value.items[i]);
    }
    vector<ref<PgfItem>>::release(table->value.items);
    Node<PgfCCat>::release(table);
}
