#ifndef VECTOR_H
#define VECTOR_H

template <class A>
struct PGF_INTERNAL Vector {
    size_t len;
    A data[];
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
    ptrdiff_t offset = (ptrdiff_t) &(((C*) NULL)->*field);
    ref<C> res = PgfDB::malloc<C>(len*sizeof(A)).as_object();
    (res->*field).len = len;
    return res;
}

PGF_INTERNAL_DECL size_t
get_next_padovan(size_t min);

template <class A> inline PGF_INTERNAL
ref<Vector<A>> vector_resize(ref<Vector<A>> r, size_t len)
{
    ref<Vector<A>> res = PgfDB::realloc<Vector<A>>(r,get_next_padovan(len)*sizeof(A)).as_object();
    res->len = len;
    return res;
}

template <class C, class A> inline PGF_INTERNAL
ref<C> vector_resize(ref<C> r, Vector<A> C::* field, size_t len)
{
    ptrdiff_t offset = (ptrdiff_t) &(((C*) NULL)->*field);
    ref<C> res = PgfDB::realloc<C>(r,get_next_padovan(len)*sizeof(A)).as_object();
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
