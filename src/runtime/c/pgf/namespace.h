#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "db.h"

template <class V>
class Node;

template <class V>
using Namespace = ref<Node<V>>;

template <class V>
class PGF_INTERNAL_DECL Node {
public:
    size_t sz;
    ref<V> value;
    ref<Node> left;
    ref<Node> right;

    static
    ref<Node> new_node(ref<V> value) {
        ref<Node> node = current_db->malloc<Node>();
        node->sz    = 1;
        node->value = value;
        node->left  = 0;
        node->right = 0;
        return node;
    }

    static
    ref<Node> new_node(ref<V> value, ref<Node> left, ref<Node> right) {
        ref<Node> node = current_db->malloc<Node>();
        node->sz    = 1+namespace_size(left)+namespace_size(right);
        node->value = value;
        node->left  = left;
        node->right = right;
        return node;
    }

    static
    ref<Node> balanceL(ref<V> value, ref<Node> left, ref<Node> right) {
        if (right == 0) {
            if (left == 0) {
                return new_node(value);
            } else {
                if (left->left == 0) {
                    if (left->right == 0) {
                        return new_node(value,left,0);
                    } else {
                        return new_node(left->right->value,
                                        new_node(left->value),
                                        new_node(value));
                    }
                } else {
                    if (left->right == 0) {
                        return new_node(left->value,
                                        left->left,
                                        new_node(value));
                    } else {
                        if (left->right->sz < 2 * left->left->sz) {
                            return new_node(left->value,
                                            left->left,
                                            new_node(value,
                                                     left->right,
                                                     0));
                        } else {
                            return new_node(left->right->value,
                                            new_node(left->value,
                                                     left->left,
                                                     left->right->left),
                                            new_node(value,
                                                     left->right->right,
                                                     0));
                        }
                    }
                }
            }
        } else {
            if (left == 0) {
                return new_node(value,0,right);
            } else {
                if (left->sz > 3*right->sz) {
                    if (left->right->sz < 2*left->left->sz)
                        return new_node(left->value,
                                        left->left,
                                        new_node(value,
                                                 left->right,
                                                 right));
                    else
                        return new_node(left->right->value,
                                        new_node(left->value,
                                                 left->left,
                                                 left->right->left),
                                        new_node(value,
                                                 left->right->right,
                                                 right));
                } else {
                    return new_node(value,left,right);
                }
            }
        }
    }

    static
    ref<Node> balanceR(ref<V> value, ref<Node> left, ref<Node> right) {
        if (left == 0) {
            if (right == 0) {
                return new_node(value);
            } else {
                if (right->left == 0) {
                    if (right->right == 0) {
                        return new_node(value,0,right);
                    } else {
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
                            Namespace<V> new_left =
                                new_node(value,
                                         0,
                                         right->left);
                            return new_node(right->value,
                                            new_left,
                                            right->right);
                        } else {
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
                return new_node(value,left,0);
            } else {
                if (right->sz > 3*left->sz) {
                    if (right->left->sz < 2*right->right->sz) {
                        Namespace<V> new_left =
                            new_node(value,
                                     left,
                                     right->left);
                        return new_node(right->value,
                                        new_left,
                                        right->right);
                    } else {
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
                                        new_right
                                        );
                    }
                } else {
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
        return Node<V>::balanceL(map->value,left,map->right);
    } else if (cmp > 0) {
        Namespace<V> right = namespace_insert(map->right, value);
        return Node<V>::balanceR(map->value, map->left, right);
    } else
        return Node<V>::new_node(value,map->left,map->right);
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
void namespace_iter(Namespace<V> map, PgfItor* itor)
{
    if (map == 0)
        return;

    namespace_iter(map->left, itor);
    itor->fn(itor, &map->value->name, &(*map->value));
    namespace_iter(map->right, itor);
}
#endif
