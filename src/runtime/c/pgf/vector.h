#ifndef VECTOR_H
#define VECTOR_H

template <class A>
struct PgfVector {
    size_t len;
    A data[];
};

template <class A> inline
ref<PgfVector<A>> vector_new(size_t len)
{
    ref<PgfVector<A>> res = DB::malloc<PgfVector<A>>(sizeof(PgfVector<A>)+len*sizeof(A));
    res->len = len;
    return res;
}

template <class C, class A> inline
ref<C> vector_new(PgfVector<A> C::* field, size_t len)
{
    ref<C> res = DB::malloc<C>(((size_t) &(((C*) NULL)->*field))+sizeof(PgfVector<A>)+len*sizeof(A));
    (res->*field).len = len;
    return res;
}

template <class A> inline
ref<A> vector_elem(ref<PgfVector<A>> v, size_t index)
{
    return ref<A>::from_ptr(&v->data[index]);
}

#endif // VECTOR_H
