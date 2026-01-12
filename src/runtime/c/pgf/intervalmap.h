#ifndef INTERVAL_MAP_H
#define INTERVAL_MAP_H

typedef std::pair<size_t,size_t> interval_t;

template<class V>
class PGF_INTERNAL_DECL interval_map {
    const static size_t DELTA = 3;
    const static size_t RATIO = 2;

    struct Node {
        size_t sz;
        size_t start, end, max;

        V value;

        Node *left;
        Node *right;

        Node(size_t start, size_t end)
        {
            this->sz = 1;
            this->start = start;
            this->end = end;
            this->max = end;
            this->left = NULL;
            this->right = NULL;
            memset(&value, 0, sizeof(value));
        }
    };

    Node *root;

    static
    Node *insert(Node *node, size_t start, size_t end, Node **target)
    {
        if (node == NULL) {
            node = new Node(start, end);
            *target = node;
            return node;
        }

        int cmp;
        if (node->start < start)
            cmp = -1;
        else if (node->start > start)
            cmp = 1;
        else if (node->end < end)
            cmp = -1;
        else if (node->end > end)
            cmp = 1;
        else
            cmp = 0;

        if (cmp < 0) {
            Node *left = insert(node->left, start, end, target);
            node = upd_node(node,left,node->right);
            return balanceL(node);
        } else if (cmp > 0) {
            Node *right = insert(node->right, start, end, target);
            node = upd_node(node,node->left,right);
            return balanceR(node);
        } else {
            *target = node;
            return node;
        }
    }

    static
    V *lookup(Node *node, size_t start, size_t end)
    {
        if (node == NULL) {
            return NULL;
        }

        int cmp;
        if (node->start < start)
            cmp = -1;
        else if (node->start > start)
            cmp = 1;
        else if (node->end < end)
            cmp = -1;
        else if (node->end > end)
            cmp = 1;
        else
            cmp = 0;

        if (cmp < 0) {
            return lookup(node->left, start, end);
        } else if (cmp > 0) {
            return lookup(node->right, start, end);
        } else {
            return &node->value;
        }
    }

    static size_t size(Node *node)
    {
        if (node == 0)
            return 0;
        return node->sz;
    }

    static
    Node *upd_node(Node *node, Node *left, Node *right)
    {
        node->sz        = 1+size(left)+size(right);
        node->max       = std::max((left  == NULL) ? node->end : left->max,
                                   (right == NULL) ? node->end : right->max);
        node->left      = left;
        node->right     = right;
        return node;
    }

    static
    Node *balanceL(Node *node)
    {
        if (node->right == NULL) {
            if (node->left == NULL) {
                return node;
            } else {
                if (node->left->left == NULL) {
                    if (node->left->right == NULL) {
                        return node;
                    } else {
                        Node *left_right = node->left->right;
                        Node *left  = upd_node(node->left,NULL,NULL);
                        Node *right = upd_node(node,NULL,NULL);
                        return upd_node(left_right,
                                        left,
                                        right);
                    }
                } else {
                    if (node->left->right == 0) {
                        Node *left  = node->left;
                        Node *right = upd_node(node,NULL,NULL);
                        return upd_node(left,
                                        left->left,
                                        right);
                    } else {
                        if (node->left->right->sz < RATIO * node->left->left->sz) {
                            Node *left  = node->left;
                            Node *right =
                                upd_node(node,
                                         left->right,
                                         NULL);
                            return upd_node(left,
                                            left->left,
                                            right);
                        } else {
                            Node *left_right = node->left->right;
                            Node *left =
                                upd_node(node->left,
                                         node->left->left,
                                         left_right->left);
                            Node *right =
                                upd_node(node,
                                         left_right->right,
                                         NULL);
                            return upd_node(left_right,
                                            left,
                                            right);
                        }
                    }
                }
            }
        } else {
            if (node->left == NULL) {
                return node;
            } else {
                if (node->left->sz > DELTA*node->right->sz) {
                    if (node->left->right->sz < RATIO*node->left->left->sz) {
                        Node *left  = node->left;
                        Node *right =
                            upd_node(node,
                                     left->right,
                                     node->right);
                        return upd_node(left,
                                        left->left,
                                        right);
                    } else {
                        Node *left_right = node->left->right;
                        Node *left  =
                            upd_node(node->left,
                                     node->left->left,
                                     left_right->left);
                        Node *right =
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
    Node *balanceR(Node *node)
    {
        if (node->left == NULL) {
            if (node->right == NULL) {
                return node;
            } else {
                if (node->right->left == NULL) {
                    if (node->right->right == NULL) {
                        return node;
                    } else {
                        Node *right = node->right;
                        Node *left  =
                            upd_node(node,
                                     NULL,
                                     NULL);
                        return upd_node(right,
                                        left,
                                        right->right);
                    }
                } else {
                    if (node->right->right == NULL) {
                        Node *right_left = node->right->left;
                        Node *right =
                            upd_node(node->right,NULL,NULL);
                        Node *left =
                            upd_node(node,NULL,NULL);
                        return upd_node(right_left,
                                        left,
                                        right);
                    } else {
                        if (node->right->left->sz < RATIO * node->right->right->sz) {
                            Node *right = node->right;
                            Node *left  =
                                upd_node(node,
                                         NULL,
                                         right->left);
                            return upd_node(right,
                                            left,
                                            right->right);
                        } else {
                            Node *right_left = node->right->left;
                            Node *right =
                                upd_node(node->right,
                                         right_left->right,
                                         node->right->right);
                            Node *left =
                                upd_node(node,
                                         NULL,
                                         right_left->left);
                            return upd_node(right_left,
                                            left,
                                            right);
                        }
                    }
                }
            }
        } else {
            if (node->right == NULL) {
                return node;
            } else {
                if (node->right->sz > DELTA*node->left->sz) {
                    if (node->right->left->sz < RATIO*node->right->right->sz) {
                        Node *right = node->right;
                        Node *left =
                            upd_node(node,
                                     node->left,
                                     right->left);
                        return upd_node(right,
                                        left,
                                        right->right);
                    } else {
                        Node *right_left = node->right->left;
                        Node *right =
                            upd_node(node->right,
                                     right_left->right,
                                     node->right->right);
                        Node *left =
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

public:
    interval_map() {
        root = NULL;
    }

    V &operator[](interval_t interval)
    {
        Node *node;
        this->root = insert(this->root, interval.first, interval.second, &node);
        return node->value;
    }

    V *lookup(interval_t interval)
    {
        return lookup(this->root, interval.first, interval.second);
    }

    size_t size()
    {
        return size(root);
    }

    class iterator {
        struct Parent {
            Node *node;
            Parent *next;
        };

        Parent *spine;

    public:
        iterator() {
            spine = NULL;
        }

        iterator(Node *node) {
            spine = NULL;
            while (node != NULL) {
                Parent *parent = new Parent;
                parent->node = node;
                parent->next = spine;
                spine = parent;
                node = node->left;
            }
        }

        bool operator ==(const iterator other) const {
            return this->spine == other.spine;
        }

        bool operator !=(const iterator other) const {
            return this->spine != other.spine;
        }

        std::pair<interval_t,V&> operator *() const {
            return std::pair<interval_t,V&>
                              (interval_t(spine->node->start,spine->node->end)
                              ,spine->node->value
                              );
        }

        void operator ++() {
            Parent *parent = spine->next;
            Node *node = spine->node->right;
            delete spine;
            spine = parent;

            while (node != NULL) {
                parent = new Parent;
                parent->node = node;
                parent->next = spine;
                spine = parent;
                node = node->left;
            }
        }

        ~iterator() {
            while (spine != NULL) {
                Parent *parent = spine->next;
                delete spine;
                spine = parent;
            }
        }
    };

    iterator begin() const {
        return iterator(root);
    }

    iterator end() const {
        return iterator();
    }
};

#endif
