#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "db.h"

// #define DEBUG_NAMESPACE

/* Namespace<V> expects that the class V contains,
 * a member 'PgfText name' which contains the name of the object.
 */

template <class V>
class Node;

template <class V>
using Namespace = ref<Node<ref<V>>>;

template <class V>
class PGF_INTERNAL_DECL Node
{
public:
    txn_t txn_id;

    size_t sz;
    V value;
    ref<Node> left;
    ref<Node> right;

    static
    ref<Node> new_node(V value)
    {
        ref<Node> node = PgfDB::malloc<Node>();
        node->txn_id   = PgfDB::get_txn_id();
        node->sz       = 1;
        node->value    = value;
        node->left     = 0;
        node->right    = 0;

#ifdef DEBUG_NAMESPACE
        printf("new     node %6ld %s\n", node.as_object(), node->value->name.text);
#endif
        return node;
    }

    static
    ref<Node> upd_node(ref<Node> node, ref<Node> left, ref<Node> right)
    {
        if (node->txn_id != PgfDB::get_txn_id()) {
            ref<Node> new_node = PgfDB::malloc<Node>();
            new_node->txn_id   = PgfDB::get_txn_id();
            new_node->value    = node->value;
            release(node);
            node = new_node;

#ifdef DEBUG_NAMESPACE
            printf("new     node %6ld %s(%ld,%ld)\n", node.as_object(), node->value->name.text, left.as_object(), right.as_object());
#endif
        }

        node->sz        = 1+Node::size(left)+Node::size(right);
        node->left      = left;
        node->right     = right;
        return node;
    }

    static
    ref<Node> balanceL(ref<Node> node)
    {
        if (node->right == 0) {
            if (node->left == 0) {
                return node;
            } else {
                if (node->left->left == 0) {
                    if (node->left->right == 0) {
                        return node;
                    } else {
                        ref<Node<V>> left_right = node->left->right;
                        ref<Node<V>> left  = upd_node(node->left,0,0);
                        ref<Node<V>> right = upd_node(node,0,0);
                        return upd_node(left_right,
                                        left,
                                        right);
                    }
                } else {
                    if (node->left->right == 0) {
                        ref<Node<V>> left  = node->left;
                        ref<Node<V>> right = upd_node(node,0,0);
                        return upd_node(left,
                                        left->left,
                                        right);
                    } else {
                        if (node->left->right->sz < 2 * node->left->left->sz) {
                            ref<Node<V>> left  = node->left;
                            ref<Node<V>> right =
                                upd_node(node,
                                         left->right,
                                         0);
                            return upd_node(left,
                                            left->left,
                                            right);
                        } else {
                            ref<Node<V>> left_right = node->left->right;
                            ref<Node<V>> left  =
                                upd_node(node->left,
                                         node->left->left,
                                         left_right->left);
                            ref<Node<V>> right =
                                upd_node(node,
                                         left_right->right,
                                         0);
                            return upd_node(left_right,
                                            left,
                                            right);
                        }
                    }
                }
            }
        } else {
            if (node->left == 0) {
                return node;
            } else {
                if (node->left->sz > 3*node->right->sz) {
                    if (node->left->right->sz < 2*node->left->left->sz) {
                        ref<Node<V>> left  = node->left;
                        ref<Node<V>> right =
                            upd_node(node,
                                     left->right,
                                     node->right);
                        return upd_node(left,
                                        left->left,
                                        right);
                    } else {
                        ref<Node<V>> left_right = node->left->right;
                        ref<Node<V>> left  =
                            upd_node(node->left,
                                     node->left->left,
                                     left_right->left);
                        ref<Node<V>> right =
                            upd_node(node,
                                     left_right->right,
                                     node->right);
                        return upd_node(left_right,
                                        left,
                                        right);
                    }
                } else {
                    return node;
                }
            }
        }
    }

    static
    ref<Node> balanceR(ref<Node> node)
    {
        if (node->left == 0) {
            if (node->right == 0) {
                return node;
            } else {
                if (node->right->left == 0) {
                    if (node->right->right == 0) {
                        return node;
                    } else {
                        ref<Node<V>> right = node->right;
                        ref<Node<V>> left  =
                            upd_node(node,
                                     0,
                                     0);
                        return upd_node(right,
                                        left,
                                        right->right);
                    }
                } else {
                    if (node->right->right == 0) {
                        ref<Node<V>> right_left = node->right->left;
                        ref<Node<V>> right =
                            upd_node(node->right,0,0);
                        ref<Node<V>> left =
                            upd_node(node,0,0);
                        return upd_node(right_left,
                                        left,
                                        right);
                    } else {
                        if (node->right->left->sz < 2 * node->right->right->sz) {
                            ref<Node<V>> right = node->right;
                            ref<Node<V>> left  =
                                upd_node(node,
                                         0,
                                         right->left);
                            return upd_node(right,
                                            left,
                                            right->right);
                        } else {
                            ref<Node<V>> right_left = node->right->left;
                            ref<Node<V>> right =
                                upd_node(node->right,
                                         right_left->right,
                                         node->right->right);
                            ref<Node<V>> left =
                                upd_node(node,
                                         0,
                                         right_left->left);
                            return upd_node(right_left,
                                            left,
                                            right);
                        }
                    }
                }
            }
        } else {
            if (node->right == 0) {
                return node;
            } else {
                if (node->right->sz > 3*node->left->sz) {
                    if (node->right->left->sz < 2*node->right->right->sz) {
                        ref<Node<V>> right = node->right;
                        ref<Node<V>> left =
                            upd_node(node,
                                     node->left,
                                     right->left);
                        return upd_node(right,
                                        left,
                                        right->right);
                    } else {
                        ref<Node<V>> right_left = node->right->left;
                        ref<Node<V>> right =
                            upd_node(node->right,
                                     right_left->right,
                                     node->right->right);
                        ref<Node<V>> left =
                            upd_node(node,
                                     node->left,
                                     right_left->left);
                        return upd_node(right_left,
                                        left,
                                        right);
                    }
                } else {
                    return node;
                }
            }
        }
    }

    static
    size_t size(ref<Node> node)
    {
        if (node == 0)
            return 0;
        return node->sz;
    }

    static
    ref<Node> pop_first(ref<Node> node, ref<Node> *res)
    {
        if (node == 0) {
            return 0;
        } else if (node->left == 0) {
            *res = node;
            return node->right;
        } else {
            ref<Node> left = pop_first(node->left, res);
            node = upd_node(node, left, node->right);
            return balanceR(node);
        }
    }

    static
    ref<Node> pop_last(ref<Node> node, ref<Node> *res)
    {
        if (node == 0) {
            return 0;
        } else if (node->right == 0) {
            *res = node;
            return node->left;
        } else {
            ref<Node> right = pop_last(node->right, res);
            node = upd_node(node, node->left, right);
            return balanceL(node);
        }
    }

    static
    void release(ref<Node> node)
    {
#ifdef DEBUG_NAMESPACE
        printf("release node %6ld %s (ref_count=%ld)\n", node.as_object(), node->value->name.text, node->ref_count-1);
#endif
        PgfDB::free(node);
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
        return Node<ref<V>>::new_node(value);

    int cmp = textcmp(&value->name,&map->value->name);
    if (cmp < 0) {
        Namespace<V> left = namespace_insert(map->left, value);
        map = Node<ref<V>>::upd_node(map,left,map->right);
        return Node<ref<V>>::balanceL(map);
    } else if (cmp > 0) {
        Namespace<V> right = namespace_insert(map->right, value);
        map = Node<ref<V>>::upd_node(map,map->left,right);
        return Node<ref<V>>::balanceR(map);
    } else {
        map = Node<ref<V>>::upd_node(map,map->left,map->right);
        map->value = value;
        return map;
    }
}

template <class V>
Namespace<V> namespace_delete(Namespace<V> map, PgfText* name,
                              ref<V> *pvalue)
{
    if (map == 0) {
        if (pvalue != NULL) *pvalue = 0;
        return 0;
    }

    int cmp = textcmp(name,&map->value->name);
    if (cmp < 0) {
        Namespace<V> left = namespace_delete(map->left, name, pvalue);
        map = Node<ref<V>>::upd_node(map,left,map->right);
        return Node<ref<V>>::balanceR(map);
    } else if (cmp > 0) {
        Namespace<V> right = namespace_delete(map->right, name, pvalue);
        map = Node<ref<V>>::upd_node(map,map->left,right);
        return Node<ref<V>>::balanceL(map);
    } else {
        if (pvalue != NULL) *pvalue = map->value;

        if (map->left == 0) {
            Node<ref<V>>::release(map);
            return map->right;
        } else if (map->right == 0) {
            Node<ref<V>>::release(map);
            return map->left;
        } else if (map->left->sz > map->right->sz) {
            Namespace<V> node;
            Namespace<V> left = Node<ref<V>>::pop_last(map->left, &node);
            node = Node<ref<V>>::upd_node(node, left, map->right);
            Node<ref<V>>::release(map);
            return Node<ref<V>>::balanceR(node);
        } else {
            Namespace<V> node;
            Namespace<V> right = Node<ref<V>>::pop_first(map->right, &node);
            node = Node<ref<V>>::upd_node(node, map->left, right);
            Node<ref<V>>::release(map);
            return Node<ref<V>>::balanceL(node);
        }
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
    return Node<ref<V>>::size(map);
}

template <class V>
void namespace_iter(Namespace<V> map, PgfItor* itor, PgfExn *err)
{
    if (map == 0)
        return;

    namespace_iter(map->left, itor, err);
    if (err->type != PGF_EXN_NONE)
        return;

    itor->fn(itor, &map->value->name, map->value.as_object(), err);
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
    namespace_release(node->left);
    namespace_release(node->right);
    Node<ref<V>>::release(node);
}

#endif
