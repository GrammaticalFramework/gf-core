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

/* Resize a vector by creating a new one and copying the old content.
 * The new vector is now also safe to update */
template <class A> inline PGF_INTERNAL
ref<Vector<A>> vector_copy(ref<Vector<A>> vec, size_t len)
{
    size_t size = len*sizeof(A);
    ref<Vector<A>> res = PgfDB::malloc<Vector<A>>(size);
    res->len = len;
    memcpy(res->data, vec->data, size);
    return res;
}

/* Resize a vector by changing its length. If there is no enough space
 * the implementation will create a copy, but whenever possible it will
 * return the reference of the original vector. In the later case, it
 * changes the length in-place which means that the function is safe
 * only if the vector was created during the current transaction. */
template <class A> inline PGF_INTERNAL
ref<Vector<A>> vector_unsafe_resize(ref<Vector<A>> vec, size_t len)
{
    size_t old_len = get_next_padovan(vec->len);
    size_t new_len = get_next_padovan(len);

    if (old_len == new_len)
        return vec;

    ref<Vector<A>> res = PgfDB::realloc<Vector<A>>(vec,old_len*sizeof(A),new_len*sizeof(A)).as_object();
    res->len = len;
    return res;
}

/* Resize a vector embedded in another structure, by changing its length. 
 * If there is no enough space the implementation will copy the structure,
 * but whenever possible it will return a reference to 
 * the original structure. In the later case, it changes 
 * the vector's length in-place which means that the function is safe
 * only if the structure was created during the current transaction. */
template <class C, class A> inline PGF_INTERNAL
ref<C> vector_unsafe_resize(ref<C> r, Vector<A> C::* field, size_t len)
{
    size_t old_len = get_next_padovan((r->*field).len);
    size_t new_len = get_next_padovan(len);

    if (old_len == new_len)
        return r;

    ref<C> res = PgfDB::realloc<C>(r,old_len*sizeof(A),new_len*sizeof(A)).as_object();
    (res->*field).len = len;
    return res;
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
