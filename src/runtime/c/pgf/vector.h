#ifndef VECTOR_H
#define VECTOR_H

template <class A>
struct PGF_INTERNAL Vector {
    size_t len;
    A data[];
    
public:
    static void release(ref<Vector> vec) {
        PgfDB::free(vec, vec->len*sizeof(A));
    }
};

template <class A> inline PGF_INTERNAL
ref<Vector<A>> vector_new(size_t len)
{
    ref<Vector<A>> res = PgfDB::malloc<Vector<A>>(len*sizeof(A));
    res->len = len;
    return res;
}

template <class C, class A> inline PGF_INTERNAL
ref<C> vector_new(Vector<A> C::* field, size_t len)
{
    ref<C> res = PgfDB::malloc<C>(len*sizeof(A)).as_object();
    (res->*field).len = len;
    return res;
}

PGF_INTERNAL_DECL size_t
get_next_padovan(size_t min);

/* Resize a vector by changing its length. If there is no enough space
 * the implementation will create a copy, but whenever possible it will
 * return the reference to the original vector. A copy is created also
 * if txn_id is different from the current transaction. In this way
 * it is safe to change the length. */
template <class A> inline PGF_INTERNAL
ref<Vector<A>> vector_resize(ref<Vector<A>> vec, size_t len, txn_t txn_id)
{
    size_t new_len = get_next_padovan(len);
    size_t old_len = get_next_padovan(vec->len);

    vec = PgfDB::realloc<Vector<A>>(vec,old_len*sizeof(A),new_len*sizeof(A),txn_id);
    vec->len = len;
    return vec;
}

template <class A> inline PGF_INTERNAL
ref<A> vector_elem(ref<Vector<A>> v, size_t index)
{
    return ref<A>::from_ptr(&v->data[index]);
}

template <class A> inline PGF_INTERNAL
A *vector_elem(Vector<A> *v, size_t index)
{
    return &v->data[index];
}

#endif // VECTOR_H
