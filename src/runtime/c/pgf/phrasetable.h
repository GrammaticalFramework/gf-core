#ifndef PHRASETABLE_H
#define PHRASETABLE_H

class PgfSequence;
class PgfSequenceItor;
typedef ref<Node<PgfSequence>> PgfPhrasetable;

PGF_INTERNAL_DECL
PgfPhrasetable phrasetable_internalize(PgfPhrasetable table, ref<PgfSequence> *seq);

PGF_INTERNAL_DECL
ref<PgfSequence> phrasetable_get(PgfPhrasetable table, size_t seq_id);

PGF_INTERNAL_DECL
void phrasetable_iter(PgfPhrasetable table, PgfSequenceItor* itor, size_t *p_next_id, PgfExn *err);

PGF_INTERNAL_DECL
void phrasetable_release(PgfPhrasetable table);

#endif
