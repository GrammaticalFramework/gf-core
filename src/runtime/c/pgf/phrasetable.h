#ifndef PHRASETABLE_H
#define PHRASETABLE_H

class PgfSequence;
class PgfSequenceItor;
typedef ref<Node<PgfSequence>> PgfPhrasetable;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wattributes"

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

#pragma GCC diagnostic pop

PGF_INTERNAL_DECL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table, ref<PgfSequence> *seq);

PGF_INTERNAL_DECL
ref<PgfSequence> phrasetable_get(PgfPhrasetable table, size_t seq_id);

PGF_INTERNAL_DECL
void phrasetable_iter(PgfPhrasetable table, PgfSequenceItor* itor,
                      PgfPhrasetableIds *seq_ids, PgfExn *err);

PGF_INTERNAL_DECL
void phrasetable_release(PgfPhrasetable table);

#endif
