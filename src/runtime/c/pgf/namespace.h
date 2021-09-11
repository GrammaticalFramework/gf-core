#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "db.h"

// #define DEBUG_NAMESPACE

/* Namespace<V> expects that the class V contains:
 *
 * - A member 'size_t ref_count' which keeps track of the number of
 * references to the particular object.
 * - A member 'PgfText name' which contains the name of the object.
 * - A method:
 *      static void release(ref<V> object)
 * which is executed when ref_count becomes zero. After that the memory
 * for the object is released as well.
 */

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

#ifdef DEBUG_NAMESPACE
        printf("new     node %6ld %s\n", node.as_object(), node->value->name.text);
#endif
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

#ifdef DEBUG_NAMESPACE
        printf("new     node %6ld %s(%ld,%ld)\n", node.as_object(), node->value->name.text, left.as_object(), right.as_object());
#endif
        return node;
    }

    static
    void add_node_ref(ref<Node> node)
    {
        node->ref_count++;
#ifdef DEBUG_NAMESPACE
        printf("add_ref node %6ld %s (ref_count=%ld)\n", node.as_object(), node->value->name.text, node->ref_count);
#endif
    }

    static
    void add_value_ref(ref<V> value)
    {
        value->ref_count++;
#ifdef DEBUG_NAMESPACE
        printf("add_ref value %5ld %s (ref_count=%ld)\n", value.as_object(), value->name.text, value->ref_count);
#endif
    }

    static
    ref<Node> balanceL(ref<V> value, ref<Node> left, ref<Node> right)
    {
        if (right == 0) {
            if (left == 0) {
                value->ref_count++;
                return new_node(value);
            } else {
                if (left->left == 0) {
                    if (left->right == 0) {
                        add_value_ref(value);
                        add_node_ref(left);
                        return new_node(value,left,0);
                    } else {
                        add_value_ref(value);
                        add_value_ref(left->value);
                        add_value_ref(left->right->value);
                        Namespace<V> new_left  = new_node(left->value);
                        Namespace<V> new_right = new_node(value);
                        return new_node(left->right->value,
                                        new_left,
                                        new_right);
                    }
                } else {
                    if (left->right == 0) {
                        add_value_ref(value);
                        Namespace<V> new_right = new_node(value);
                        add_value_ref(left->value);
                        add_node_ref(left->left);
                        return new_node(left->value,
                                        left->left,
                                        new_right);
                    } else {
                        if (left->right->sz < 2 * left->left->sz) {
                            add_value_ref(value);
                            add_value_ref(left->value);
                            add_node_ref(left->left);
                            add_node_ref(left->right);
                            Namespace<V> new_right =
                                new_node(value,
                                         left->right,
                                         0);
                            return new_node(left->value,
                                            left->left,
                                            new_right);
                        } else {
                            add_value_ref(value);
                            add_value_ref(left->value);
                            add_value_ref(left->right->value);
                            add_node_ref(left->left);
                            if (left->right->left != 0)
                                add_node_ref(left->right->left);
                            if (left->right->right != 0)
                                add_node_ref(left->right->right);
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
                add_value_ref(value);
                add_node_ref(right);
                return new_node(value,0,right);
            } else {
                if (left->sz > 3*right->sz) {
                    if (left->right->sz < 2*left->left->sz) {
                        add_value_ref(value);
                        add_value_ref(left->value);
                        add_node_ref(left->left);
                        add_node_ref(left->right);
                        add_node_ref(right);
                        Namespace<V> new_right =
                            new_node(value,
                                     left->right,
                                     right);
                        return new_node(left->value,
                                        left->left,
                                        new_right);
                    } else {
                        add_value_ref(value);
                        add_value_ref(left->value);
                        add_value_ref(left->right->value);
                        add_node_ref(left->left);
                        if (left->right->left != 0)
                            add_node_ref(left->right->left);
                        if (left->right->right != 0)
                            add_node_ref(left->right->right);
                        add_node_ref(right);
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
                    add_value_ref(value);
                    add_node_ref(left);
                    add_node_ref(right);
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
                add_value_ref(value);
                return new_node(value);
            } else {
                if (right->left == 0) {
                    if (right->right == 0) {
                        add_value_ref(value);
                        add_node_ref(right);
                        return new_node(value,0,right);
                    } else {
                        add_value_ref(value);
                        add_value_ref(right->value);
                        add_node_ref(right->right);
                        Namespace<V> new_left =
                            new_node(value);
                        return new_node(right->value,
                                        new_left,
                                        right->right);
                    }
                } else {
                    if (right->right == 0) {
                        add_value_ref(value);
                        add_value_ref(right->value);
                        add_value_ref(right->left->value);
                        Namespace<V> new_left =
                            new_node(value);
                        Namespace<V> new_right =
                            new_node(right->value);
                        return new_node(right->left->value,
                                        new_left,
                                        new_right);
                    } else {
                        if (right->left->sz < 2 * right->right->sz) {
                            add_value_ref(value);
                            add_value_ref(right->value);
                            add_node_ref(right->left);
                            add_node_ref(right->right);
                            Namespace<V> new_left =
                                new_node(value,
                                         0,
                                         right->left);
                            return new_node(right->value,
                                            new_left,
                                            right->right);
                        } else {
                            add_value_ref(value);
                            add_value_ref(right->value);
                            add_value_ref(right->left->value);
                            if (right->left->left != 0)
                                add_node_ref(right->left->left);
                            if (right->left->right != 0)
                                add_node_ref(right->left->right);
                            add_node_ref(right->right);
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
                add_value_ref(value);
                add_node_ref(left);
                return new_node(value,left,0);
            } else {
                if (right->sz > 3*left->sz) {
                    if (right->left->sz < 2*right->right->sz) {
                        add_value_ref(value);
                        add_value_ref(right->value);
                        add_node_ref(left);
                        add_node_ref(right->left);
                        add_node_ref(right->right);
                        Namespace<V> new_left =
                            new_node(value,
                                     left,
                                     right->left);
                        return new_node(right->value,
                                        new_left,
                                        right->right);
                    } else {
                        add_value_ref(value);
                        add_value_ref(right->value);
                        add_value_ref(right->left->value);
                        add_node_ref(left);
                        if (right->left->left != 0)
                            add_node_ref(right->left->left);
                        if (right->left->right != 0)
                            add_node_ref(right->left->right);
                        add_node_ref(right->right);
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
                    add_value_ref(value);
                    add_node_ref(left);
                    add_node_ref(right);
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
Namespace<V> namespace_delete(Namespace<V> map, PgfText* name)
{
    if (map == 0)
        return 0;

    int cmp = textcmp(name,&map->value->name);
    if (cmp < 0) {
        Namespace<V> left = namespace_delete(map->left, name);
        if (left == map->left)
            return map;
        Namespace<V> node = Node<V>::balanceR(map->value,left,map->right);
        namespace_release(left);
        return node;
    } else if (cmp > 0) {
        Namespace<V> right = namespace_delete(map->right, name);
        if (right == map->right)
            return map;
        Namespace<V> node  = Node<V>::balanceL(map->value,map->left,right);
        namespace_release(right);
        return node;
    } else {
        if (map->left == 0) {
            if (map->right != 0)
                map->right->ref_count++;
            return map->right;
        } else if (map->right == 0) {
            if (map->left != 0)
                map->left->ref_count++;
            return map->left;
        } else if (map->left->sz > map->right->sz) {
            ref<V> value;
            Namespace<V> new_left = namespace_pop_last(map->left, &value);
            Namespace<V> node = Node<V>::balanceR(value, new_left, map->right);
            namespace_release(new_left);
            return node;
        } else {
            ref<V> value;
            Namespace<V> new_right = namespace_pop_first(map->right, &value);
            Namespace<V> node = Node<V>::balanceL(value, map->left, new_right);
            namespace_release(new_right);
            return node;
        }
    }
}

template <class V>
Namespace<V> namespace_pop_first(Namespace<V> map, ref<V> *res)
{
    if (map == 0) {
        return 0;
    } else if (map->left == 0) {
        *res = map->value;
        if (map->right != 0)
            map->right->ref_count++;
        return map->right;
    } else {
        Namespace<V> new_left = namespace_pop_first(map->left, res);
        Namespace<V> node = Node<V>::balanceR(map->value, new_left, map->right);
        namespace_release(new_left);
        return node;
    }
}

template <class V>
Namespace<V> namespace_pop_last(Namespace<V> map, ref<V> *res)
{
    if (map == 0) {
        return 0;
    } else if (map->right == 0) {
        *res = map->value;
        if (map->left != 0)
            map->left->ref_count++;
        return map->left;
    } else {
        Namespace<V> new_right = namespace_pop_last(map->right, res);
        return Node<V>::balanceR(map->value, map->left, new_right);
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

#ifdef DEBUG_NAMESPACE
    printf("release node %6ld %s (ref_count=%ld)\n", node.as_object(), node->value->name.text, node->ref_count-1);
#endif

    if (!(--node->ref_count)) {
        namespace_release(node->left);
        namespace_release(node->right);

#ifdef DEBUG_NAMESPACE
        printf("release value %5ld %s (ref_count=%ld)\n", node->value.as_object(), node->value->name.text, node->value->ref_count-1);
#endif

        if (!(--node->value->ref_count)) {
            V::release(node->value);
            PgfDB::free(node->value);
        }

        PgfDB::free(node);
    }
}

#endif
