#include "data.h"

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

PGF_INTERNAL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table, ref<PgfSequence> *pseq)
{
    if (table == 0) {
		PgfPhrasetable table = Node<PgfSequence>::new_node(*pseq);
		Node<PgfSequence>::add_value_ref(table->value);
        return table;
	}

    int cmp = sequence_cmp(*pseq,table->value);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_internalize(table->left, pseq);
        if (left == table->left)
			return table;
		else {
			PgfPhrasetable node = Node<PgfSequence>::balanceL(table->value,left,table->right);
			namespace_release(left);
			return node;
		}
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_internalize(table->right, pseq);
        if (right == table->right)
			return table;
		else {
			PgfPhrasetable node  = Node<PgfSequence>::balanceR(table->value, table->left, right);
			phrasetable_release(right);
			return node;
		}
    } else {
		if (!(--(*pseq)->ref_count)) {
            PgfSequence::release(*pseq);
        }

		Node<PgfSequence>::add_value_ref(table->value);

		*pseq = table->value;
        return table;
    }
}

PGF_INTERNAL_DECL
ref<PgfSequence> phrasetable_get(PgfPhrasetable table, size_t seq_id)
{
	while (table != 0) {
		size_t left_sz = table->left->sz;
		if (seq_id < left_sz)
			table = table->left;
		else if (seq_id == left_sz)
			return table->value;
		else {
			table = table->right;
			seq_id -= left_sz+1;
		}
	}
	return 0;
}

PGF_INTERNAL
void phrasetable_iter(PgfPhrasetable table, PgfSequenceItor* itor, size_t *p_next_id, PgfExn *err)
{
    if (table == 0)
        return;

    phrasetable_iter(table->left, itor, p_next_id, err);
    if (err->type != PGF_EXN_NONE)
        return;

	table->value->seq_id = (*p_next_id)++;
    itor->fn(itor, table->value.as_object(), err);
    if (err->type != PGF_EXN_NONE)
        return;

    phrasetable_iter(table->right, itor, p_next_id, err);
    if (err->type != PGF_EXN_NONE)
        return;
}

PGF_INTERNAL
void phrasetable_release(PgfPhrasetable table)
{
	namespace_release(table);
}
