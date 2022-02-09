#include "data.h"

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

void PgfPhrasetableEntry::add_ref()
{
    seq->ref_count++;
    if (backrefs != 0)
        backrefs->ref_count++;
}

void PgfPhrasetableEntry::release_ref()
{
    if (!(--seq->ref_count)) {
        PgfSequence::release(seq);
    }

    if (backrefs != 0 && !(--backrefs->ref_count)) {
        PgfSequenceBackrefs::release(backrefs);
    }
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

PGF_INTERNAL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table,
                                       object container,
                                       size_t seq_index,
                                       ref<PgfSequence> *pseq)
{
    if (table == 0) {
        PgfPhrasetableEntry entry;
        entry.seq = *pseq;
        entry.backrefs = PgfDB::malloc<PgfSequenceBackrefs>(sizeof(PgfSequenceBackref));
        entry.backrefs->ref_count = 1;
        entry.backrefs->list.len = 1;
        entry.backrefs->list.data[0].container = container;
        entry.backrefs->list.data[0].seq_index = seq_index;
        entry.seq->ref_count++;
        return Node<PgfPhrasetableEntry>::new_node(entry);
	}

    int cmp = sequence_cmp(*pseq,table->value.seq);
    if (cmp < 0) {
        PgfPhrasetable left = phrasetable_internalize(table->left,
                                                      container,
                                                      seq_index,
                                                      pseq);
        PgfPhrasetable node = Node<PgfPhrasetableEntry>::balanceL(table->value,left,table->right);
        phrasetable_release(left);
        return node;
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_internalize(table->right,
                                                       container,
                                                       seq_index,
                                                       pseq);
        PgfPhrasetable node  = Node<PgfPhrasetableEntry>::balanceR(table->value, table->left, right);
        phrasetable_release(right);
        return node;
    } else {
		if (!(--(*pseq)->ref_count)) {
            PgfSequence::release(*pseq);
        }

		table->value.seq->ref_count++;
		*pseq = table->value.seq;

        if (table->left != 0)
            table->left->ref_count++;
        if (table->right != 0)
            table->right->ref_count++;

        size_t len = (table->value.backrefs)
                         ? table->value.backrefs->list.len
                         : 0;
        PgfPhrasetableEntry entry;
        entry.seq = table->value.seq;
        entry.backrefs = PgfDB::malloc<PgfSequenceBackrefs>((len+1)*sizeof(PgfSequenceBackref));
        entry.backrefs->ref_count = 1;
        entry.backrefs->list.len = len+1;
        memcpy(entry.backrefs->list.data, table->value.backrefs->list.data, len*sizeof(PgfSequenceBackref));
        entry.backrefs->list.data[len].container = container;
        entry.backrefs->list.data[len].seq_index = seq_index;
        entry.seq->ref_count++;
        return Node<PgfPhrasetableEntry>::new_node(entry,table->left,table->right);
     }
}

PGF_INTERNAL_DECL
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
                             : table->value.backrefs->list.len;

            ref<PgfSequenceBackrefs> backrefs =
                vector_resize<PgfSequenceBackrefs,PgfSequenceBackref>(table->value.backrefs, &PgfSequenceBackrefs::list, len+1);
            backrefs->ref_count = 1;
            backrefs->list.data[len].container = container;
            backrefs->list.data[len].seq_index = seq_index;
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
        PgfPhrasetable node = Node<PgfPhrasetableEntry>::balanceR(table->value,left,table->right);
        phrasetable_release(left);
        return node;
    } else if (cmp > 0) {
        PgfPhrasetable right = phrasetable_delete(table->right, 
                                                  container, seq_index,
                                                  seq);
        if (right == table->right)
            return table;
        PgfPhrasetable node  = Node<PgfPhrasetableEntry>::balanceL(table->value,table->left,right);
        phrasetable_release(right);
        return node;
    } else {
        PgfPhrasetableEntry entry;

        size_t len = table->value.backrefs->list.len;
        if (len > 1) {
            entry.backrefs =
                PgfDB::malloc<PgfSequenceBackrefs>((len-1)*sizeof(PgfSequenceBackref));
            entry.backrefs->ref_count = 1;
            entry.backrefs->list.len  = len-1;
            size_t i = 0;
            while (i < len) {
                PgfSequenceBackref *backref = 
                    vector_elem(&table->value.backrefs->list, i);
                if (backref->container == container &&
                    backref->seq_index == seq_index) {
                    break;
                }
                *vector_elem(&entry.backrefs->list, i) = *backref;
                i++;
            }
            i++;
            while (i < len) {
                PgfSequenceBackref *backref = 
                    vector_elem(&table->value.backrefs->list, i);
                *vector_elem(&entry.backrefs->list, i-1) = *backref;
                i++;
            }

            entry.seq = table->value.seq;
            table->value.seq->ref_count++;
            return Node<PgfPhrasetableEntry>::new_node(entry, table->left, table->right);
        } else {
            if (table->left == 0) {
                if (table->right != 0)
                    table->right->ref_count++;
                return table->right;
            } else if (table->right == 0) {
                if (table->left != 0)
                    table->left->ref_count++;
                return table->left;
            } else if (table->left->sz > table->right->sz) {
                PgfPhrasetable new_left = Node<PgfPhrasetableEntry>::pop_last(table->left, &entry);
                PgfPhrasetable node = Node<PgfPhrasetableEntry>::balanceR(entry, new_left, table->right);
                phrasetable_release(new_left);
                return node;
            } else {
                PgfPhrasetable new_right = Node<PgfPhrasetableEntry>::pop_first(table->right, &entry);
                PgfPhrasetable node = Node<PgfPhrasetableEntry>::balanceL(entry, table->left, new_right);
                phrasetable_release(new_right);
                return node;
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
        for (size_t i = 0; i < table->value.backrefs->list.len; i++) {
            PgfSequenceBackref backref = *vector_elem(&table->value.backrefs->list,i);
            switch (ref<PgfConcrLin>::get_tag(backref.container)) {
            case PgfConcrLin::tag: {
                ref<PgfConcrLin> lin = ref<PgfConcrLin>::untagged(backref.container);
                ref<PgfConcrLincat> lincat =
                    namespace_lookup(concr->lincats, &lin->absfun->type->name);
                if (lincat != 0) {
                    ref<PgfText> field =
                        *vector_elem(lincat->fields, backref.seq_index % lincat->fields->len);

                    callback->fn(callback, &lin->absfun->name, &(*field), lincat->abscat->prob+lin->absfun->prob, err);
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
    Node<PgfPhrasetableEntry>::release(table);
}
