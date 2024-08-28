#ifndef PHRASETABLE_H
#define PHRASETABLE_H

struct PgfSequence;
struct PgfSequenceBackref;

struct PGF_INTERNAL_DECL PgfPhrasetableEntry {
    ref<PgfSequence> seq;

    // Here n_backrefs tells us how many actual backrefs there are in
    // the vector backrefs. On the other hand, backrefs->len tells us
    // how big buffer we have allocated.
    size_t n_backrefs;
    vector<PgfSequenceBackref> backrefs;
};

struct PgfSequenceItor;
typedef ref<Node<PgfPhrasetableEntry>> PgfPhrasetable;


#if __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wattributes"
#endif

struct PgfPhrasetableIds {
public:
	PGF_INTERNAL_DECL PgfPhrasetableIds();
	PGF_INTERNAL_DECL ~PgfPhrasetableIds() { end(); }

	PGF_INTERNAL_DECL void start(ref<PgfConcr> concr);
	PGF_INTERNAL_DECL size_t add(ref<PgfSequence> seq);
	PGF_INTERNAL_DECL size_t get(ref<PgfSequence> seq);
	PGF_INTERNAL_DECL void end();

private:
	size_t next_id;

	struct PGF_INTERNAL_DECL SeqIdChain;

	struct PGF_INTERNAL_DECL SeqIdPair {
		SeqIdChain *chain;
		ref<PgfSequence> seq;
		size_t seq_id;
	};

	struct PGF_INTERNAL_DECL SeqIdChain : public SeqIdPair {
		SeqIdChain *next;
	};

	size_t n_pairs;
	SeqIdPair  *pairs;
	SeqIdChain *chains;
};

#if __GNUC__
#pragma GCC diagnostic pop
#endif

struct PgfConcrLincat;

PGF_INTERNAL_DECL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table,
                                       ref<PgfSequence> seq,
                                       ref<PgfConcrLincat> lincat,
                                       object container,
                                       size_t seq_index,
                                       ref<PgfPhrasetableEntry> *pentry);

PGF_INTERNAL_DECL
ref<PgfSequence> phrasetable_relink(PgfPhrasetable table,
                                    object container,
                                    size_t seq_index,
                                    size_t seq_id);

PGF_INTERNAL_DECL
PgfPhrasetable phrasetable_delete(PgfPhrasetable table,
                                  object container,
                                  size_t seq_index,
                                  ref<PgfSequence> seq);

PGF_INTERNAL_DECL
size_t phrasetable_size(PgfPhrasetable table);

struct PgfConcrLin;

struct PGF_INTERNAL_DECL PgfTextSpot {
	size_t pos;          // position in Unicode characters
	const uint8_t *ptr;  // pointer into the spot location
};

class PGF_INTERNAL_DECL PgfPhraseScanner {
public:
    virtual void space(PgfTextSpot *start, PgfTextSpot *end, PgfExn* err)=0;
    virtual void start_matches(PgfTextSpot *spot, PgfExn* err)=0;
    virtual void match(ref<PgfConcrLin> lin, size_t seq_index, PgfExn* err)=0;
    virtual void end_matches(PgfTextSpot *spot, PgfExn* err)=0;
};

PGF_INTERNAL_DECL
void phrasetable_lookup(PgfPhrasetable table,
                        PgfText *sentence,
                        bool case_sensitive,
                        PgfPhraseScanner *scanner, PgfExn* err);

PGF_INTERNAL_DECL
void phrasetable_lookup_cohorts(PgfPhrasetable table,
                                PgfText *sentence,
                                bool case_sensitive,
                                PgfPhraseScanner *scanner, PgfExn* err);

PGF_INTERNAL_DECL
void phrasetable_iter(PgfConcr *concr,
                      PgfPhrasetable table,
                      PgfSequenceItor* itor,
                      PgfMorphoCallback *callback,
                      PgfPhrasetableIds *seq_ids, PgfExn *err);

PGF_INTERNAL_DECL
void phrasetable_release(PgfPhrasetable table);

// The following are used internally in the parser

enum SeqMatch { SM_FULL_MATCH, SM_PREFIX, SM_PARTIAL };

PGF_INTERNAL_DECL
int text_sequence_cmp(PgfTextSpot *spot, const uint8_t *end,
                      ref<PgfSequence> seq, size_t *p_i,
                      bool case_sensitive, SeqMatch sm);

// The following is used internally in the grammar builder

PGF_INTERNAL_DECL
void phrasetable_add_backref(ref<PgfPhrasetableEntry> entry, txn_t txn_id,
                             object container,
                             size_t seq_index);

#endif
