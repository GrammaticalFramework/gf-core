#ifndef PHRASETABLE_H
#define PHRASETABLE_H

struct PgfConcrLin;
struct PgfConcrLincat;

struct PGF_INTERNAL_DECL PgfTextSpot {
	size_t pos;          // position in Unicode characters
	const uint8_t *ptr;  // pointer into the spot location
};

struct PGF_INTERNAL_DECL PgfItem {
    PgfMetaId res;

    struct {
        size_t &operator[](int i) {
            PgfItem *item = containerof(PgfItem,vars,this);
            return ((size_t*) (((PgfMetaId*) (item+1))+item->rule->args.size()))[i];
        }
        size_t size() {
            PgfItem *item = containerof(PgfItem,vars,this);
            return (item->rule->ranges != 0) ? item->rule->ranges.size() : 0;
        }
    } vars;

    struct {
        PgfMetaId &operator[](int i) {
            PgfItem *item = containerof(PgfItem,args,this);
            return ((PgfMetaId*) (item+1))[i];
        }
        size_t size() {
            PgfItem *item = containerof(PgfItem,args,this);
            return item->rule->args.size();
        }
    } args;

    static
    void release(ref<PgfItem> item) {
        size_t ex_size =
            sizeof(PgfMetaId) * item->args.size() +
            sizeof(size_t)    * item->vars.size();
        PgfDB::free(item, ex_size);
    }

    uint16_t pre_alt;
    uint16_t pre_dot;
    uint16_t dot;
    ref<PgfConcrRule> rule;
};

struct PGF_INTERNAL_DECL PgfCCat {
    ref<PgfConcrLincat> lincat;
    PgfMetaId fid;
    interval_t value, lin_idx;
    prob_t viterbi_prob;

    // Here n_items tells us how many actual items there are in
    // the vector items. On the other hand, items.size() tells us
    // how big buffer we have allocated.
    size_t n_items;
    vector<ref<PgfItem>> items;
};

template<class K>
struct PGF_INTERNAL_DECL PgfPhrasetableValue {
    ref<K> key;

    // Here n_items tells us how many actual items there are in
    // the vector items. On the other hand, items.size() tells us
    // how big buffer we have allocated.
    size_t n_items;
    vector<ref<PgfItem>> items;
};

template <class K>
using PgfPhrasetable = ref<Node<PgfPhrasetableValue<K>>>;

template<class K>
PGF_INTERNAL_DECL
PgfPhrasetable<K> phrasetable_insert(PgfPhrasetable<K> table,
                                     ref<K> key, ref<PgfItem> item);

template<class K>
PGF_INTERNAL_DECL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable<K> phrasetable,
                                        ref<K> key,
                                        size_t *n_items);

class PGF_INTERNAL_DECL PgfPhraseScanner {
public:
    virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)=0;
    virtual void start_matches(PgfTextSpot *spot, PgfExn* err)=0;
    virtual void match(ref<PgfConcrLin> lin, size_t lin_idx, PgfExn* err)=0;
    virtual void end_matches(PgfTextSpot *spot, PgfExn* err)=0;
};

PGF_INTERNAL_DECL
void phrasetable_lookup(PgfPhrasetable<PgfSymbolKS> phrasetable,
                        PgfText *sentence,
                        bool case_sensitive,
                        PgfPhraseScanner *scanner, PgfExn* err);

PGF_INTERNAL_DECL
void phrasetable_lookup_cohorts(PgfPhrasetable<PgfSymbolKS> phrasetable,
                                PgfText *sentence,
                                bool case_sensitive,
                                PgfPhraseScanner *scanner, PgfExn* err);

template <class V>
void phrasetable_release(PgfPhrasetable<V> table)
{
    if (table == 0)
        return;
    phrasetable_release(table->left);
    phrasetable_release(table->right);
    for (size_t i = 0; i < table->value.n_items; i++) {
        PgfItem::release(table->value.items[i]);
    }
    vector<ref<PgfItem>>::release(table->value.items);
    Node<PgfPhrasetableValue<V>>::release(table);
}


typedef ref<Node<PgfCCat>> PgfEpsilontable;

// Creates a new epsilon category with its first item.
// The new category is mutable within the current transaction
PGF_INTERNAL_DECL
PgfEpsilontable epsilontable_insert(PgfEpsilontable table,
                                    ref<PgfConcrLincat> lincat,
                                    interval_t value, interval_t lin_idx,
                                    PgfMetaId fid, prob_t viterbi_prob,
                                    ref<PgfItem> item,
                                    ref<PgfCCat> *pepsilon);

// Adds a new item to an existing epsilon category. The category
// must have been created by epsilontable_insert in the current transaction.
PGF_INTERNAL_DECL
void epsilontable_add(ref<PgfCCat> epsilon, ref<PgfItem> item);

PGF_INTERNAL_DECL
ref<PgfCCat> epsilontable_get(PgfEpsilontable table,
                              PgfText *name, PgfMetaId fid);

PGF_INTERNAL
void epsilontable_iter(PgfEpsilontable table, ref<PgfConcrLincat> lincat, std::function<void(ref<PgfCCat> arg)> &f);

PGF_INTERNAL_DECL
void epsilontable_release(PgfEpsilontable table);

#endif
