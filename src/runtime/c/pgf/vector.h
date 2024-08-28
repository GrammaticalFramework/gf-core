#ifndef VECTOR_H
#define VECTOR_H

template<class A> class vector;

template<class A> class PGF_INTERNAL_DECL inline_vector {
private:
    size_t len;
    A data[];

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

    size_t size() { return len; };
    A &operator[] (size_t i) { return data[i]; };
    ref<A> elem(size_t i) { return ref<A>::from_ptr(&data[i]); };
    iterator begin() { return iterator(ref<A>::from_ptr(&data[0])); }
    iterator end()   { return iterator(ref<A>::from_ptr(&data[len])); }

    template <class C>
    static ref<C> alloc(inline_vector<A> C::* field, size_t size)
    {
        ref<C> res = PgfDB::malloc<C>(size*sizeof(A)).as_object();
        (res->*field).len = size;
        return res;
    }

    template <class C>
    static void release(inline_vector<A> C::* field, ref<C> o) {
        PgfDB::free(o,(o->*field).len*sizeof(A));
    }

    vector<A> as_vector() { return vector<A>(((unsigned char*) this)-current_base); }

    friend class vector<A>;
};

template<class A> class PGF_INTERNAL_DECL vector {
private:
    object offset;

    inline_vector<A>* v() const { return (inline_vector<A>*) (current_base+offset); }

public:
    vector() { }
    vector(object o) {
        this->offset = o;
    }

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

    size_t size() { return v()->len; };
    A &operator[] (size_t i) { return v()->data[i]; };
    ref<A> elem(size_t i) { return ref<A>::from_ptr(&v()->data[i]); };
    iterator begin() { return iterator(ref<A>::from_ptr(&v()->data[0])); }
    iterator end()   { return iterator(ref<A>::from_ptr(&v()->data[v()->len])); }

    bool operator ==(vector<A>& other) const { return offset==other.as_object(); }
    bool operator !=(vector<A>& other) const { return offset!=other.as_object(); }
    bool operator ==(object other_offset) const { return offset==other_offset; }
    bool operator !=(object other_offset) const { return offset!=other_offset; }

    A *get_data() {
        return v()->data;
    }

    static vector<A> alloc(size_t size)
    {
        auto res = PgfDB::malloc<inline_vector<A>>(size*sizeof(A));
        res->len = size;
        return vector<A>(res.as_object());
    }

    vector<A> realloc(size_t new_size, txn_t txn_id)
    {
        ref<inline_vector<A>> r = offset;
        auto res = PgfDB::realloc<inline_vector<A>>(r,v()->len*sizeof(A),new_size*sizeof(A),txn_id);
        res->len = new_size;
        return res->as_vector();
    }

    static void release(vector<A> vec) {
        PgfDB::free(ref<inline_vector<A>>(vec.offset), vec.v()->len*sizeof(A));
    }
};

#endif // VECTOR_H
