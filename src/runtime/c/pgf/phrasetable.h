#ifndef PHRASETABLE_H
#define PHRASETABLE_H

struct PgfConcrLin;
struct PgfConcrLincat;

struct PGF_INTERNAL_DECL PgfTextSpot {
	size_t pos;          // position in Unicode characters
	const uint8_t *ptr;  // pointer into the spot location
};

struct PGF_INTERNAL_DECL PgfItem {
    struct {
        size_t &operator[](int i) {
            PgfItem *item = containerof(PgfItem,vars,this);
            return ((size_t*) (((ref<PgfSymbolCCat>*) (item+1))+item->rule->args.size()))[i];
        }
        size_t size() {
            PgfItem *item = containerof(PgfItem,vars,this);
            return (item->rule->ranges != 0) ? item->rule->ranges.size() : 0;
        }
    } vars;

    struct {
        ref<PgfSymbolCCat> &operator[](int i) {
            PgfItem *item = containerof(PgfItem,args,this);
            return ((ref<PgfSymbolCCat>*) (item+1))[i];
        }
        size_t size() {
            PgfItem *item = containerof(PgfItem,args,this);
            return item->rule->args.size();
        }
    } args;

    uint16_t pre_alt;
    uint16_t pre_dot;
    uint16_t dot;
    ref<PgfConcrRule> rule;
};

struct PgfPhrasetableNode;
typedef ref<PgfPhrasetableNode> PgfPhrasetable;

struct PGF_INTERNAL_DECL PgfPhrasetableNode {
    const static size_t DELTA = 3;
    const static size_t RATIO = 2;

public:
    PgfSymbol sym;

    // Here n_items tells us how many actual items there are in
    // the vector items. On the other hand, items.size() tells us
    // how big buffer we have allocated.
    size_t n_items;
    vector<ref<PgfItem>> items;

    txn_t txn_id;

    size_t sz;
    ref<PgfPhrasetableNode> left;
    ref<PgfPhrasetableNode> right;

    static
    ref<PgfPhrasetableNode> new_node(PgfSymbol sym, size_t n_items);

    static
    ref<PgfPhrasetableNode> upd_node(ref<PgfPhrasetableNode> node, ref<PgfPhrasetableNode> left, ref<PgfPhrasetableNode> right);

    static
    ref<PgfPhrasetableNode> balanceL(ref<PgfPhrasetableNode> node);

    static
    ref<PgfPhrasetableNode> balanceR(ref<PgfPhrasetableNode> node);

    static
    size_t size(ref<PgfPhrasetableNode> node)
    {
        if (node == 0)
            return 0;
        return node->sz;
    }

    static
    void release(ref<PgfPhrasetableNode> node);
};

PgfPhrasetable phrasetable_insert(PgfPhrasetable table,
                                  PgfSymbol sym,
                                  ref<PgfItem> item);

PgfPhrasetable phrasetable_insert(PgfPhrasetable table,
                                  ref<PgfConcrLincat> lincat,
                                  interval_t value, interval_t lin_idx,
                                  PgfMetaId fid, prob_t viterbi_prob,
                                  ref<PgfItem> item);

PGF_INTERNAL_DECL
void phrasetable_iter(PgfPhrasetable phrasetable,ref<PgfConcrLincat> lincat,std::function<void(ref<PgfSymbolCCat> symcf,size_t,vector<ref<PgfItem>>)> &f);

PGF_INTERNAL_DECL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable phrasetable, PgfSymbol sym, size_t *n_items);

PGF_INTERNAL_DECL
vector<ref<PgfItem>> phrasetable_lookup(PgfPhrasetable phrasetable,
                                        ref<PgfConcrLincat> lincat,
                                        size_t *n_items);

class PGF_INTERNAL_DECL PgfPhraseScanner {
public:
    virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)=0;
    virtual void start_matches(PgfTextSpot *spot, PgfExn* err)=0;
    virtual void match(ref<PgfConcrLin> lin, size_t lin_idx, PgfExn* err)=0;
    virtual void end_matches(PgfTextSpot *spot, PgfExn* err)=0;
};

PGF_INTERNAL_DECL
void phrasetable_lookup(PgfPhrasetable phrasetable,
                        PgfText *sentence,
                        bool case_sensitive,
                        PgfPhraseScanner *scanner, PgfExn* err);

PGF_INTERNAL_DECL
void phrasetable_lookup_cohorts(PgfPhrasetable phrasetable,
                                PgfText *sentence,
                                bool case_sensitive,
                                PgfPhraseScanner *scanner, PgfExn* err);

#endif
