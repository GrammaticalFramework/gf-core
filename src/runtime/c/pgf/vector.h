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
