#include "data.h"
#include "math.h"

bool PgfProbspaceEntry::is_result() {
    return cat == ref<PgfText>::from_ptr(&fun->type->name);
}

static
int entry_cmp(PgfProbspaceEntry *entry1, PgfProbspaceEntry *entry2)
{
    int cmp = textcmp(&(*entry1->cat), &(*entry2->cat));
    if (cmp != 0)
        return cmp;
        
    if (entry1->fun->prob < entry2->fun->prob)
        return -1;
    else if (entry1->fun->prob > entry2->fun->prob)
        return 1;
    
    return textcmp(&entry1->fun->name, &entry2->fun->name);
}

static
PgfProbspace probspace_insert(PgfProbspace space,
                              PgfProbspaceEntry *entry)
{
    if (space == 0) {
        return Node<PgfProbspaceEntry>::new_node(*entry);
	}

    int cmp = entry_cmp(entry,&space->value);
    if (cmp < 0) {
        PgfProbspace left = probspace_insert(space->left,entry);
        space = Node<PgfProbspaceEntry>::upd_node(space,left,space->right);
        return Node<PgfProbspaceEntry>::balanceL(space);
    } else if (cmp > 0) {
        PgfProbspace right = probspace_insert(space->right,entry);
        space = Node<PgfProbspaceEntry>::upd_node(space, space->left, right);
        return Node<PgfProbspaceEntry>::balanceR(space);
    } else {
        space = Node<PgfProbspaceEntry>::upd_node(space,space->left,space->right);
        space->value = *entry;
        return space;
    }
}

static
PgfProbspace probspace_insert(PgfProbspace space,
                              ref<PgfAbsFun> fun,
                              ref<PgfDTyp> type)
{
    for (size_t i = 0; i < type->hypos->len; i++) {
        ref<PgfHypo> hypo = vector_elem(type->hypos,i);
        space = probspace_insert(space,fun,hypo->type);
    }

    PgfProbspaceEntry entry;
    entry.cat = ref<PgfText>::from_ptr(&type->name);
    entry.fun = fun;
    space = probspace_insert(space,&entry);

    return space;
}

PGF_INTERNAL
PgfProbspace probspace_insert(PgfProbspace space,
                              ref<PgfAbsFun> fun)
{
    return probspace_insert(space,fun,fun->type);
}

static
PgfProbspace probspace_delete(PgfProbspace space, PgfProbspaceEntry *entry)
{
    if (space == 0) {
        return 0;
    }

    int cmp = entry_cmp(entry,&space->value);
    if (cmp < 0) {
        PgfProbspace left = probspace_delete(space->left, entry);
        space = Node<PgfProbspaceEntry>::upd_node(space,left,space->right);
        return Node<PgfProbspaceEntry>::balanceR(space);
    } else if (cmp > 0) {
        PgfProbspace right = probspace_delete(space->right, entry);
        space = Node<PgfProbspaceEntry>::upd_node(space,space->left,right);
        return Node<PgfProbspaceEntry>::balanceL(space);
    } else {
        if (space->left == 0) {
            Node<PgfProbspaceEntry>::release(space);
            return space->right;
        } else if (space->right == 0) {
            Node<PgfProbspaceEntry>::release(space);
            return space->left;
        } else if (space->left->sz > space->right->sz) {
            PgfProbspace node;
            PgfProbspace left = Node<PgfProbspaceEntry>::pop_last(space->left, &node);
            node = Node<PgfProbspaceEntry>::upd_node(node, left, space->right);
            Node<PgfProbspaceEntry>::release(space);
            return Node<PgfProbspaceEntry>::balanceR(node);
        } else {
            PgfProbspace node;
            PgfProbspace right = Node<PgfProbspaceEntry>::pop_first(space->right, &node);
            node = Node<PgfProbspaceEntry>::upd_node(node, space->left, right);
            Node<PgfProbspaceEntry>::release(space);
            return Node<PgfProbspaceEntry>::balanceL(node);
        }
    }
}

static
PgfProbspace probspace_delete(PgfProbspace space,
                              ref<PgfAbsFun> fun,
                              ref<PgfDTyp> type)
{
    for (size_t i = 0; i < type->hypos->len; i++) {
        ref<PgfHypo> hypo = vector_elem(type->hypos,i);
        space = probspace_delete(space,fun,hypo->type);
    }

    PgfProbspaceEntry entry;
    entry.cat = ref<PgfText>::from_ptr(&type->name);
    entry.fun = fun;
    space = probspace_delete(space,&entry);

    return space;
}

PGF_INTERNAL
PgfProbspace probspace_delete(PgfProbspace space, ref<PgfAbsFun> fun)
{
    return probspace_delete(space,fun,fun->type);
}

PGF_INTERNAL_DECL
PgfProbspace probspace_delete_by_cat(PgfProbspace space, PgfText *cat,
                                     PgfItor* itor, PgfExn *err)
{
    if (space == 0) {
        return 0;
    }

    int cmp = textcmp(cat,&(*space->value.cat));
    if (cmp < 0) {
        PgfProbspace left = probspace_delete_by_cat(space->left, cat, itor, err);
        if (err->type != PGF_EXN_NONE)
            return 0;

        return Node<PgfProbspaceEntry>::link(space,left,space->right);
    } else if (cmp > 0) {
        PgfProbspace right = probspace_delete_by_cat(space->right, cat, itor, err);
        if (err->type != PGF_EXN_NONE)
            return 0;

        return Node<PgfProbspaceEntry>::link(space,space->left,right);
    } else {
        itor->fn(itor, &space->value.fun->name, space->value.fun.as_object(), err);
        if (err->type != PGF_EXN_NONE)
            return 0;

        PgfProbspace left = probspace_delete_by_cat(space->left, cat, itor, err);
        if (err->type != PGF_EXN_NONE)
            return 0;

        PgfProbspace right = probspace_delete_by_cat(space->right, cat, itor, err);
        if (err->type != PGF_EXN_NONE)
            return 0;

        Node<PgfProbspaceEntry>::release(space);

        return Node<PgfProbspaceEntry>::link(left,right);
    }
}

bool probspace_iter(PgfProbspace space, PgfText *cat,
                    std::function<bool(ref<PgfAbsFun>)> &f, bool all)
{
    if (space == 0)
        return true;

    int cmp = textcmp(cat,&(*space->value.cat));
    if (cmp < 0) {
        return probspace_iter(space->left, cat, f, all);
    } else if (cmp > 0) {
        return probspace_iter(space->right, cat, f, all);
    } else {
        if (!probspace_iter(space->left, cat, f, all))
            return false;

        if (all || space->value.is_result()) {
            if (!f(space->value.fun))
                return false;
        }

        return probspace_iter(space->right, cat, f, all);
    }
}

struct PGF_INTERNAL RSState {
    const std::set<ref<PgfAbsFun>> &excluded;
    prob_t rand;
    ref<PgfAbsFun> result;
};
                      
static
bool probspace_random(PgfProbspace space, PgfText *cat, 
                      RSState *st)
{
    if (space == 0)
        return false;

    int cmp = textcmp(cat,&(*space->value.cat));
    if (cmp < 0) {
        return probspace_random(space->left, cat, st);
    } else if (cmp > 0) {
        return probspace_random(space->right, cat, st);
    } else {
        if (probspace_random(space->left, cat, st))
            return true;

        bool is_res = space->value.is_result();
        if (is_res && !st->excluded.count(space->value.fun)) {
            st->rand  -= expf(-space->value.fun->prob);
            st->result = space->value.fun;
            if (st->rand <= 0)
                return true;
        }

        if (probspace_random(space->right, cat, st))
            return true;
    }

    return false;
}

PGF_INTERNAL
ref<PgfAbsFun> probspace_random(PgfProbspace space,
                                PgfText *cat, prob_t rand,
                                const std::set<ref<PgfAbsFun>> &excluded)
{
    RSState st = {excluded, rand, 0};
    probspace_random(space,cat,&st);
    return st.result;
}

PGF_INTERNAL
void probspace_release(PgfProbspace space)
{
    if (space == 0)
        return;
    probspace_release(space->left);
    probspace_release(space->right);
    Node<PgfProbspaceEntry>::release(space);
}
