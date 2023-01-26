#ifndef PHRASETABLE_H
#define PHRASETABLE_H

struct PgfSequence;
struct PgfSequenceBackref;

struct PGF_INTERNAL_DECL PgfPhrasetableEntry {
    ref<PgfSequence> seq;
    ref<Vector<PgfSequenceBackref>> backrefs;
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

PGF_INTERNAL_DECL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table,
                                       ref<PgfSequence> seq,
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

class PgfConcrLin;

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

#endif
