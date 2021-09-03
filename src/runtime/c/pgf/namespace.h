#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "db.h"

template <class V>
class Node;

template <class V>
using Namespace = ref<Node<V>>;

template <class V>
class PGF_INTERNAL_DECL Node
{
public:
    size_t ref_count;

    size_t sz;
    ref<V> value;
    ref<Node> left;
    ref<Node> right;

    static
    ref<Node> new_node(ref<V> value)
    {
        ref<Node> node = current_db->malloc<Node>();
        node->ref_count = 1;
        node->sz        = 1;
        node->value     = value;
        node->left      = 0;
        node->right     = 0;
        return node;
    }

    static
    ref<Node> new_node(ref<V> value, ref<Node> left, ref<Node> right)
    {
        ref<Node> node = current_db->malloc<Node>();
        node->ref_count = 1;
        node->sz        = 1+namespace_size(left)+namespace_size(right);
        node->value     = value;
        node->left      = left;
        node->right     = right;
        return node;
    }

    static
    ref<Node> balanceL(ref<V> value, ref<Node> left, ref<Node> right)
    {
        if (right == 0) {
            if (left == 0) {
                return new_node(value);
            } else {
                if (left->left == 0) {
                    if (left->right == 0) {
                        left->ref_count++;
                        return new_node(value,left,0);
                    } else {
                        Namespace<V> new_left  = new_node(left->value);
                        Namespace<V> new_right = new_node(value);
                        return new_node(left->right->value,
                                        new_left,
                                        new_right);
                    }
                } else {
                    if (left->right == 0) {
                        Namespace<V> new_right = new_node(value);
                        left->left->ref_count++;
                        return new_node(left->value,
                                        left->left,
                                        new_right);
                    } else {
                        if (left->right->sz < 2 * left->left->sz) {
                            left->left->ref_count++;
                            left->right->ref_count++;
                            Namespace<V> new_right =
                                new_node(value,
                                         left->right,
                                         0);
                            return new_node(left->value,
                                            left->left,
                                            new_right);
                        } else {
                            left->left->ref_count++;
                            if (left->right->left != 0)
                                left->right->left->ref_count++;
                            if (left->right->right != 0)
                                left->right->right->ref_count++;
                            Namespace<V> new_left  =
                                new_node(left->value,
                                         left->left,
                                         left->right->left);
                            Namespace<V> new_right =
                                new_node(value,
                                         left->right->right,
                                         0);
                            return new_node(left->right->value,
                                            new_left,
                                            new_right);
                        }
                    }
                }
            }
        } else {
            if (left == 0) {
                right->ref_count++;
                return new_node(value,0,right);
            } else {
                if (left->sz > 3*right->sz) {
                    if (left->right->sz < 2*left->left->sz) {
                        left->left->ref_count++;
                        left->right->ref_count++;
                        right->ref_count++;
                        Namespace<V> new_right =
                            new_node(value,
                                     left->right,
                                     right);
                        return new_node(left->value,
                                        left->left,
                                        new_right);
                    } else {
                        left->left->ref_count++;
                        if (left->right->left != 0)
                            left->right->left->ref_count++;
                        if (left->right->right != 0)
                            left->right->right->ref_count++;
                        right->ref_count++;
                        Namespace<V> new_left =
                            new_node(left->value,
                                     left->left,
                                     left->right->left);
                        Namespace<V> new_right =
                            new_node(value,
                                     left->right->right,
                                     right);
                        return new_node(left->right->value,
                                        new_left,
                                        new_right);
                    }
                } else {
                    left->ref_count++;
                    right->ref_count++;
                    return new_node(value,left,right);
                }
            }
        }
    }

    static
    ref<Node> balanceR(ref<V> value, ref<Node> left, ref<Node> right)
    {
        if (left == 0) {
            if (right == 0) {
                return new_node(value);
            } else {
                if (right->left == 0) {
                    if (right->right == 0) {
                        right->ref_count++;
                        return new_node(value,0,right);
                    } else {
                        right->right->ref_count++;
                        Namespace<V> new_left =
                            new_node(value);
                        return new_node(right->value,
                                        new_left,
                                        right->right);
                    }
                } else {
                    if (right->right == 0) {
                        Namespace<V> new_left =
                            new_node(value);
                        Namespace<V> new_right =
                            new_node(right->value);
                        return new_node(right->left->value,
                                        new_left,
                                        new_right);
                    } else {
                        if (right->left->sz < 2 * right->right->sz) {
                            right->left->ref_count++;
                            right->right->ref_count++;
                            Namespace<V> new_left =
                                new_node(value,
                                         0,
                                         right->left);
                            return new_node(right->value,
                                            new_left,
                                            right->right);
                        } else {
                            if (right->left->left != 0)
                                right->left->left->ref_count++;
                            if (right->left->right != 0)
                                right->left->right->ref_count++;
                            right->right->ref_count++;
                            Namespace<V> new_left =
                                new_node(value,
                                         0,
                                         right->left->left);
                            Namespace<V> new_right =
                                new_node(right->value,
                                         right->left->right,
                                         right->right);
                            return new_node(right->left->value,
                                            new_left,
                                            new_right);
                        }
                    }
                }
            }
        } else {
            if (right == 0) {
                left->ref_count++;
                return new_node(value,left,0);
            } else {
                if (right->sz > 3*left->sz) {
                    if (right->left->sz < 2*right->right->sz) {
                        left->ref_count++;
                        right->left->ref_count++;
                        right->right->ref_count++;
                        Namespace<V> new_left =
                            new_node(value,
                                     left,
                                     right->left);
                        return new_node(right->value,
                                        new_left,
                                        right->right);
                    } else {
                        left->ref_count++;
                        if (right->left->left != 0)
                            right->left->left->ref_count++;
                        if (right->left->right != 0)
                            right->left->right->ref_count++;
                        right->right->ref_count++;
                        Namespace<V> new_left =
                            new_node(value,
                                     left,
                                     right->left->left);
                        Namespace<V> new_right =
                            new_node(right->value,
                                     right->left->right,
                                     right->right);
                        return new_node(right->left->value,
                                        new_left,
                                        new_right);
                    }
                } else {
                    left->ref_count++;
                    right->ref_count++;
                    return new_node(value,left,right);
                }
            }
        }
    }
};

template <class V>
Namespace<V> namespace_empty()
{
    return 0;
}

template <class V>
Namespace<V> namespace_singleton(ref<V> value)
{
    return Node<V>::new_node(value);
}

template <class V>
Namespace<V> namespace_insert(Namespace<V> map, ref<V> value)
{
    if (map == 0)
        return Node<V>::new_node(value);

    int cmp = textcmp(&value->name,&map->value->name);
    if (cmp < 0) {
        Namespace<V> left = namespace_insert(map->left, value);
        Namespace<V> node = Node<V>::balanceL(map->value,left,map->right);
        namespace_release(left);
        return node;
    } else if (cmp > 0) {
        Namespace<V> right = namespace_insert(map->right, value);
        Namespace<V> node  = Node<V>::balanceR(map->value, map->left, right);
        namespace_release(right);
        return node;
    } else {
        map->left->ref_count++;
        map->right->ref_count++;
        return Node<V>::new_node(value,map->left,map->right);
    }
}

template <class V>
ref<V> namespace_lookup(Namespace<V> map, PgfText *name)
{
    while (map != 0) {
        int cmp = textcmp(name,&map->value->name);
        if (cmp < 0)
            map = map->left;
        else if (cmp > 0)
            map = map->right;
        else
            return map->value;
    }
    return 0;
}

template <class V>
size_t namespace_size(Namespace<V> map)
{
    if (map == 0)
        return 0;
    return map->sz;
}

template <class V>
void namespace_iter(Namespace<V> map, PgfItor* itor, PgfExn *err)
{
    if (map == 0)
        return;

    namespace_iter(map->left, itor, err);
    if (err->type != PGF_EXN_NONE)
        return;

    itor->fn(itor, &map->value->name, &(*map->value), err);
    if (err->type != PGF_EXN_NONE)
        return;

    namespace_iter(map->right, itor, err);
    if (err->type != PGF_EXN_NONE)
        return;
}

template <class V>
void namespace_release(Namespace<V> node)
{
    if (node == 0)
        return;

    if (!(--node->ref_count)) {
        namespace_release(node->left);
        namespace_release(node->right);
        DB::free(node);
    }
}

#endif
