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

static
int sequence_cmp(vector<PgfSymbol> seq1, vector<PgfSymbol> seq2);

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
			if (i >= sym_kp1->alts.size()) {
				res[0] = (res[1] = -(i < sym_kp2->alts.size()));
				return;
			}
			if (i >= sym_kp2->alts.size()) {
				res[0] = (res[1] = 1);
				return;
			}

			res[0] = (res[1] = sequence_cmp(sym_kp1->alts[i].form, sym_kp2->alts[i].form));
			if (res[0] != 0)
				return;
				
			vector<ref<PgfText>> prefixes1 = sym_kp1->alts[i].prefixes;
			vector<ref<PgfText>> prefixes2 = sym_kp2->alts[i].prefixes;
			for (size_t j = 0; ; j++) {
				if (j >= prefixes1.size()) {
					res[0] = (res[1] = -(j < prefixes2.size()));
					return;
				}
				if (j >= prefixes2.size()) {
					res[0] = (res[1] = 1);
					return;
				}

				res[0] = (res[1] = textcmp(&*prefixes1[j], &*prefixes2[j]));
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
    case PgfSymbolACat::tag: {
        auto sym_acat1 = ref<PgfSymbolACat>::untagged(sym1);
        auto sym_acat2 = ref<PgfSymbolACat>::untagged(sym2);
        res[0] = (res[1] = textcmp(&sym_acat1->name,&sym_acat2->name));
        return;
    }
    case PgfSymbolCCat::tag: {
        auto sym_ccat1 = ref<PgfSymbolCCat>::untagged(sym1);
        auto sym_ccat2 = ref<PgfSymbolCCat>::untagged(sym2);
        res[0] = (res[1] = textcmp(&sym_ccat1->lincat->name,&sym_ccat2->lincat->name));
        if (res[0] != 0)
            return;
        if (sym_ccat1->value < sym_ccat2->value)
			res[0] = (res[1] = -1);
        else if (sym_ccat1->value > sym_ccat2->value)
			res[0] = (res[1] = 1);
        if (sym_ccat1->lin_idx < sym_ccat2->lin_idx)
			res[0] = (res[1] = -1);
        else if (sym_ccat1->lin_idx > sym_ccat2->lin_idx)
			res[0] = (res[1] = 1);
        else
            res[0] = (res[1] = 0);
        return;
    }
	default:
		throw pgf_error("Unknown symbol tag");
    }
}

static
int sequence_cmp(vector<PgfSymbol> seq1, vector<PgfSymbol> seq2)
{
	int res[2] = {0,0};
	for (size_t i = 0; ; i++) {
        if (i >= seq1.size()) {
			if (i < seq2.size())
				return -1;
            return res[1];
		}
        if (i >= seq2.size())
            return 1;

		symbol_cmp(seq1[i], seq2[i], res);
		if (res[0] != 0)
			return res[0];
    }

	return 0;
}

PGF_INTERNAL
int text_symbol_cmp(PgfTextSpot *spot, const uint8_t *end,
                    PgfSymbol sym, bool case_sensitive)
{
    uint8_t tag = ref<PgfSymbol>::get_tag(sym);
    if (PgfSymbolKS::tag != tag)
        return ((int) PgfSymbolKS::tag) - ((int) tag);

    int res1 = 0;

    auto sym_ks = ref<PgfSymbolKS>::untagged(sym);
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
    size_t dot = item->dot;
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

static
int symbol_cmp(ref<PgfConcrLincat> lincat, interval_t value, interval_t lin_idx, PgfSymbol sym)
{
    uint8_t tag = ref<PgfSymbol>::get_tag(sym);
    if (PgfSymbolCCat::tag != tag)
        return ((int) PgfSymbolCCat::tag) - ((int) tag);

    auto symcf = ref<PgfSymbolCCat>::untagged(sym);
    int res = textcmp(&lincat->name, &symcf->lincat->name);
    if (res != 0)
        return res;
    if (value < symcf->value)
        return -1;
    else if (value > symcf->value)
        return 1;
    else if (lin_idx < symcf->lin_idx)
        return -1;
    else if (lin_idx > symcf->lin_idx)
        return 1;
    else
        return 0;
}

static
int symbol_cmp(PgfSymbol sym1, PgfSymbol sym2)
{
    uint8_t tag1 = ref<PgfSymbol>::get_tag(sym1);
    uint8_t tag2 = ref<PgfSymbol>::get_tag(sym2);
    if (tag1 != tag2)
        return ((int) tag1) - ((int) tag2);

    switch (tag1) {
    case PgfSymbolKS::tag: {
        auto symks1 = ref<PgfSymbolKS>::untagged(sym1);
        auto symks2 = ref<PgfSymbolKS>::untagged(sym2);
        int res[2] = {0,0};
        texticmp(&symks1->token, &symks2->token, res);
        if (res[0] != 0)
            return res[0];
        return res[1];
    }
    case PgfSymbolACat::tag: {
        auto symcf1 = ref<PgfSymbolACat>::untagged(sym1);
        auto symcf2 = ref<PgfSymbolACat>::untagged(sym2);
        return textcmp(&symcf1->name, &symcf2->name);
    }
    case PgfSymbolCCat::tag: {
        auto symcf1 = ref<PgfSymbolCCat>::untagged(sym1);
        auto symcf2 = ref<PgfSymbolCCat>::untagged(sym2);
        int res = textcmp(&symcf1->lincat->name, &symcf2->lincat->name);
        if (res != 0)
            return res;
        if (symcf1->value < symcf2->value)
            return -1;
        else if (symcf1->value > symcf2->value)
            return 1;
        else if (symcf1->lin_idx < symcf2->lin_idx)
            return -1;
        else if (symcf1->lin_idx > symcf2->lin_idx)
            return 1;
        else
            return 0;
    }
    default:
        return 0;
    }
}

ref<PgfPhrasetableNode> PgfPhrasetableNode::new_node(PgfSymbol sym, size_t n_items)
{
    auto items = vector<ref<PgfItem>>::alloc(n_items);

    auto node = PgfDB::malloc<PgfPhrasetableNode>();
    node->sym     = sym;
    node->n_items = 0;
    node->items   = items;
    node->txn_id  = PgfDB::get_txn_id();
    node->sz      = 1;
    node->left    = 0;
    node->right   = 0;
    
    return node;
}

PgfPhrasetable PgfPhrasetableNode::upd_node(PgfPhrasetable node, PgfPhrasetable left, PgfPhrasetable right)
{
    if (node->txn_id != PgfDB::get_txn_id()) {
        PgfPhrasetable new_node = PgfDB::malloc<PgfPhrasetableNode>();
        new_node->sym      = node->sym;
        new_node->n_items  = node->n_items;
        new_node->items    = node->items;
        new_node->txn_id   = PgfDB::get_txn_id();
        release(node);
        node = new_node;
    }

    node->sz        = 1+PgfPhrasetableNode::size(left)+PgfPhrasetableNode::size(right);
    node->left      = left;
    node->right     = right;

    return node;
}

PgfPhrasetable PgfPhrasetableNode::balanceL(PgfPhrasetable node)
{
    if (node->right == 0) {
        if (node->left == 0) {
            return node;
        } else {
            if (node->left->left == 0) {
                if (node->left->right == 0) {
                    return node;
                } else {
                    PgfPhrasetable left_right = node->left->right;
                    PgfPhrasetable left  = upd_node(node->left,0,0);
                    PgfPhrasetable right = upd_node(node,0,0);
                    return upd_node(left_right,
                                    left,
                                    right);
                }
            } else {
                if (node->left->right == 0) {
                    PgfPhrasetable left  = node->left;
                    PgfPhrasetable right = upd_node(node,0,0);
                    return upd_node(left,
                                    left->left,
                                    right);
                } else {
                    if (node->left->right->sz < RATIO * node->left->left->sz) {
                        PgfPhrasetable left  = node->left;
                        PgfPhrasetable right =
                            upd_node(node,
                                     left->right,
                                     0);
                        return upd_node(left,
                                        left->left,
                                        right);
                    } else {
                        PgfPhrasetable left_right = node->left->right;
                        PgfPhrasetable left  =
                            upd_node(node->left,
                                     node->left->left,
                                     left_right->left);
                        PgfPhrasetable right =
                            upd_node(node,
                                     left_right->right,
                                     0);
                        return upd_node(left_right,
                                        left,
                                        right);
                    }
                }
            }
        }
    } else {
        if (node->left == 0) {
            return node;
        } else {
            if (node->left->sz > DELTA*node->right->sz) {
                if (node->left->right->sz < RATIO*node->left->left->sz) {
                    PgfPhrasetable left  = node->left;
                    PgfPhrasetable right =
                        upd_node(node,
                                 left->right,
                                 node->right);
                    return upd_node(left,
                                    left->left,
                                    right);
                } else {
                    PgfPhrasetable left_right = node->left->right;
                    PgfPhrasetable left  =
                        upd_node(node->left,
                                 node->left->left,
                                 left_right->left);
                    PgfPhrasetable right =
                        upd_node(node,
                                 left_right->right,
                                 node->right);
                    return upd_node(left_right,
                                    left,
                                    right);
                }
            } else {
                return node;
            }
        }
    }
}

PgfPhrasetable PgfPhrasetableNode::balanceR(PgfPhrasetable node)
{
    if (node->left == 0) {
        if (node->right == 0) {
            return node;
        } else {
            if (node->right->left == 0) {
                if (node->right->right == 0) {
                    return node;
                } else {
                    PgfPhrasetable right = node->right;
                    PgfPhrasetable left  =
                        upd_node(node,
                                 0,
                                 0);
                    return upd_node(right,
                                    left,
                                    right->right);
                }
            } else {
                if (node->right->right == 0) {
                    PgfPhrasetable right_left = node->right->left;
                    PgfPhrasetable right =
                        upd_node(node->right,0,0);
                    PgfPhrasetable left =
                        upd_node(node,0,0);
                    return upd_node(right_left,
                                    left,
                                    right);
                } else {
                    if (node->right->left->sz < RATIO * node->right->right->sz) {
                        PgfPhrasetable right = node->right;
                        PgfPhrasetable left  =
                            upd_node(node,
                                     0,
                                     right->left);
                        return upd_node(right,
                                        left,
                                        right->right);
                    } else {
                        PgfPhrasetable right_left = node->right->left;
                        PgfPhrasetable right =
                            upd_node(node->right,
                                     right_left->right,
                                     node->right->right);
                        PgfPhrasetable left =
                            upd_node(node,
                                     0,
                                     right_left->left);
                        return upd_node(right_left,
                                        left,
                                        right);
                    }
                }
            }
        }
    } else {
        if (node->right == 0) {
            return node;
        } else {
            if (node->right->sz > DELTA*node->left->sz) {
                if (node->right->left->sz < RATIO*node->right->right->sz) {
                    PgfPhrasetable right = node->right;
                    PgfPhrasetable left =
                        upd_node(node,
                                 node->left,
                                 right->left);
                    return upd_node(right,
                                    left,
                                    right->right);
                } else {
                    PgfPhrasetable right_left = node->right->left;
                    PgfPhrasetable right =
                        upd_node(node->right,
                                 right_left->right,
                                 node->right->right);
                    PgfPhrasetable left =
                        upd_node(node,
                                 node->left,
                                 right_left->left);
                    return upd_node(right_left,
                                    left,
                                    right);
                }
            } else {
                return node;
            }
        }
    }
}

void PgfPhrasetableNode::release(ref<PgfPhrasetableNode> node)
{
    PgfDB::free(node);
}

void phrasetable_iter(PgfPhrasetable table, ref<PgfConcrLincat> lincat, std::function<void(ref<PgfSymbolCCat> arg,size_t,vector<ref<PgfItem>>)> &f)
{
    if (table == 0)
        return;

    int cmp = 0;
    ref<PgfSymbolCCat> symcf = 0;
    uint8_t tag = ref<PgfSymbol>::get_tag(table->sym);
    if (PgfSymbolCCat::tag != tag) {
        cmp = ((int) PgfSymbolCCat::tag) - ((int) tag);
    } else {
        symcf = ref<PgfSymbolCCat>::untagged(table->sym);
        cmp = textcmp(&lincat->name, &symcf->lincat->name);
    }

    if (cmp < 0)
        phrasetable_iter(table->left,  lincat, f);
    else if (cmp > 0)
        phrasetable_iter(table->right, lincat, f);
    else {
        phrasetable_iter(table->left,  lincat, f);
        f(symcf,table->n_items,table->items);
        phrasetable_iter(table->right, lincat, f);
    }
}

vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable table, PgfSymbol sym, size_t *n_items)
{
    while (table != 0) {
        int cmp = symbol_cmp(sym,table->sym);
        if (cmp < 0)
            table = table->left;
        else if (cmp > 0)
            table = table->right;
        else {
            *n_items = table->n_items;
            return table->items;
        }
    }

    *n_items = 0;
    return 0;
}

vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable phrasetable,
                                        ref<PgfConcrLincat> lincat,
                                        size_t *n_items)
{
    while (phrasetable != 0) {
        int cmp;
        uint8_t tag = ref<PgfSymbol>::get_tag(phrasetable->sym);
        if (PgfSymbolACat::tag != tag) {
            cmp = ((int) PgfSymbolACat::tag) - ((int) tag);
        } else {
            auto symcf = ref<PgfSymbolACat>::untagged(phrasetable->sym);
            cmp = textcmp(&lincat->name, &symcf->name);
        }
        if (cmp < 0)
            phrasetable = phrasetable->left;
        else if (cmp > 0)
            phrasetable = phrasetable->right;
        else {
            *n_items = phrasetable->n_items;
            return phrasetable->items;
        }
    }

    *n_items = 0;
    return 0;
}

PGF_INTERNAL
void phrasetable_lookup(PgfPhrasetable table,
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
    int cmp = text_symbol_cmp(&spot,end,table->sym,case_sensitive);
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

        for (size_t i = 0; i < table->n_items; i++) {
            ref<PgfItem> item = table->items[i];
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
                                 PgfPhrasetable table,
                                 ptrdiff_t min, ptrdiff_t max)
{
    if (table == 0)
        return;

    PgfTextSpot current = state->spot;
    int cmp = text_symbol_cmp(&current,state->end,table->sym,state->case_sensitive);
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

            for (size_t i = 0; i < table->n_items; i++) {
                auto rule = table->items[i]->rule;
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

PgfPhrasetable phrasetable_insert(PgfPhrasetable table,
                                  PgfSymbol sym,
                                  ref<PgfItem> item)
{
    if (table == 0) {
        PgfPhrasetable new_table = PgfPhrasetableNode::new_node(sym,1);
        new_table->n_items = 1;
        new_table->items[0] = item;
        return new_table;
	}

    int cmp = symbol_cmp(sym,table->sym);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_insert(table->left, sym, item);
        table = PgfPhrasetableNode::upd_node(table,left,table->right);
        return PgfPhrasetableNode::balanceL(table);
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_insert(table->right, sym, item);
        table = PgfPhrasetableNode::upd_node(table, table->left, right);
        return PgfPhrasetableNode::balanceR(table);
    } else {
        PgfPhrasetable new_table =
            PgfPhrasetableNode::upd_node(table, table->left, table->right);

        auto items = new_table->items;
        if (new_table->n_items >= items.size()) {
            size_t new_len = get_next_padovan(new_table->n_items+1);
            items = items.realloc(new_len, new_table->txn_id);
        }
        items[new_table->n_items] = item;
        new_table->n_items++;
        new_table->items = items;
        return new_table;
    }
}

PgfPhrasetable phrasetable_insert(PgfPhrasetable table,
                                  ref<PgfConcrLincat> lincat,
                                  interval_t value, interval_t lin_idx,
                                  PgfMetaId fid,
                                  ref<PgfItem> item)
{
    if (table == 0) {
        ref<PgfSymbolCCat> symcf = PgfDB::malloc<PgfSymbolCCat>();
        symcf->lincat = lincat;
        symcf->value   = value;
        symcf->lin_idx = lin_idx;
        symcf->fid = fid;
        PgfPhrasetable new_table = PgfPhrasetableNode::new_node(symcf.tagged(),1);
        new_table->n_items = 1;
        new_table->items[0] = item;
        return new_table;
	}

    int cmp = symbol_cmp(lincat,value,lin_idx,table->sym);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_insert(table->left,
                                                lincat, value, lin_idx, fid, item);
        table = PgfPhrasetableNode::upd_node(table,left,table->right);
        return PgfPhrasetableNode::balanceL(table);
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_insert(table->right,
                                                  lincat, value, lin_idx, fid, item);
        table = PgfPhrasetableNode::upd_node(table, table->left, right);
        return PgfPhrasetableNode::balanceR(table);
    } else {
        PgfPhrasetable new_table =
            PgfPhrasetableNode::upd_node(table, table->left, table->right);

        auto items = new_table->items;
        if (new_table->n_items >= items.size()) {
            size_t new_len = get_next_padovan(new_table->n_items+1);
            items = items.realloc(new_len, new_table->txn_id);
        }
        items[new_table->n_items] = item;
        new_table->n_items++;
        new_table->items = items;
        return new_table;
    }
}
