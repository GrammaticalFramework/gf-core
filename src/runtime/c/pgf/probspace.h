#ifndef PROBSPACE_H
#define PROBSPACE_H

/*
 * The probspace is an index where abstract functions are ordered by
 * the tripple of (category,probability,name). Here the category is
 * any of the categories used in the type of the function.
 *
 * When a new function is created, it is inserted first in
 * the namespace of functions and after that in the probspace.
 * The insertion in the probspace is done once for every category in
 * the type of the function. The insertion with a result category is
 * always done after the insertion with the argument categories.
 * This means that we can easily identify functions which return a given
 * category by just checking that:
 *
 *    entry->cat == ref<PgfText>::from_ptr(&fun->type->name);
 *
 * The later is important when the index is used in tree generation
 * and parsing.
 *
 * The index has the following uses:
 *
 *   - When a category is removed we must also remove all functions that
 * use it in their types, otherwise the data will be inconsistant.
 * We use the index to find which functions to remove.
 *
 *   - When a lincat is removed we must remove all lins from the same
 * language which depend on the lincat. We identify them through
 * the abstract types and whence through this index.
 *
 *   - When we do exhaustive or random generation, we need to know
 * which functions return a given category. We simply go through
 * the index but also check the above condition to distinguish the case
 * where the category is consumed from the case where the category is
 * returned. In exhaustive generation, to ensure that all results
 * are returned in decreasing probability order, we must consume
 * functions in the same order. This is the reason why we keep the
 * index sorted by probability.
 *
 *   - When a function is removed, we must remove it both from
 * the namespace as well as from the probspace. Since the index is
 * sorted by function name, as well, we use that to find which entry
 * to remove.
 *
 */
struct PGF_INTERNAL_DECL PgfProbspaceEntry {
    ref<PgfText> cat;
    ref<PgfAbsFun> fun;
    
    bool is_result();
};

typedef ref<Node<PgfProbspaceEntry>> PgfProbspace;

PGF_INTERNAL_DECL
PgfProbspace probspace_insert(PgfProbspace space,
                              ref<PgfAbsFun> fun);

PGF_INTERNAL_DECL
PgfProbspace probspace_delete(PgfProbspace space,
                              ref<PgfAbsFun> fun);

PGF_INTERNAL_DECL
PgfProbspace probspace_delete_by_cat(PgfProbspace space, PgfText *cat,
                                     PgfItor* itor, PgfExn *err);

PGF_INTERNAL_DECL
void probspace_iter(PgfProbspace space, PgfText *cat,
                    PgfItor* itor, bool all, PgfExn *err);

/* Given a random number from 0 to 1, select a random function from
 * the given category */
PGF_INTERNAL_DECL
ref<PgfAbsFun> probspace_random(PgfProbspace space,
                                PgfText *cat, prob_t rand);

PGF_INTERNAL_DECL
void probspace_release(PgfProbspace space);

#endif
