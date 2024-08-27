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

template<class A> class PGF_INTERNAL_DECL vector {
private:
    object offset;

    struct V {
        size_t size;
        A data[];
    };

    V* v() const { return (V*) (current_base+offset); }

    vector(object o) {
        this->offset = o;
    }

public:
    class iterator {
    public:
        object offset;

        iterator(ref<A> r) {
            this->offset = r.as_object();
        }

        bool operator!=(iterator other) {
            return offset != other.offset;
        }

        void operator++() {
            offset += sizeof(A);
        }

        A &operator*() {
            return *((A*) (current_base+offset));
        }
    };

    size_t size() { return v()->size; };
    A &operator[] (size_t i) { return v()->data[i]; };
    iterator begin() { return iterator(ref<A>::from_ptr(&v()->data[0])); }
    iterator end()   { return iterator(ref<A>::from_ptr(&v()->data[v()->size])); }

    static vector<A> alloc(size_t size)
    {
        auto res = PgfDB::malloc<vector<A>::V>(size*sizeof(A));
        res->size = size;
        return vector<A>(res.as_object());
    }
};


#endif // VECTOR_H
