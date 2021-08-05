#ifndef DB_H
#define DB_H

class DB;

extern PGF_INTERNAL_DECL unsigned char* current_base;
extern PGF_INTERNAL_DECL DB* current_db;

typedef size_t moffset;

typedef moffset variant;

struct malloc_state;

template<class A> class ref {
private:
    moffset offset;

    friend class DB;

public:
    ref<A>() { }
    ref<A>(moffset o) { offset = o; }

    A* operator->() const { return (A*) (current_base+offset); }
    operator A*()   const { return (A*) (current_base+offset); }
    bool operator ==(ref<A>& other) const { return offset==other->offset; }
    bool operator ==(moffset other_offset) const { return offset==other_offset; }

    ref<A>& operator= (const ref<A>& r) {
        offset = r.offset;
        return *this;
    }

    static
    ref<A> from_ptr(A *ptr) { return (((uint8_t*) ptr) - current_base); }

    static
    variant tagged(ref<A> ref) {
        assert(A::tag < 2*sizeof(size_t));
        return (ref.offset | A::tag);
    }

    static
    ref<A> untagged(variant v) {
        return (v & ~(2*sizeof(size_t) - 1));
    }

    static
    uint8_t get_tag(variant v) {
        return (v & (2*sizeof(size_t) - 1));
    }

    static
    ref<A> null() { return 0; }
};

class PGF_INTERNAL_DECL DB {
private:
    int fd;
    malloc_state* ms;

    friend class PgfReader;

public:
    DB(const char* pathname);
    ~DB();

    template<class A>
    static ref<A> malloc() {
        return current_db->malloc_internal(sizeof(A));
    }

    template<class A>
    static ref<A> malloc(size_t bytes) {
        return current_db->malloc_internal(bytes);
    }

    template<class A>
    static ref<A> get_root() {
        return current_db->get_root_internal();
    }

    template<class A>
    static void set_root(ref<A> root) {
        current_db->set_root_internal(root.offset);
    }

private:
    void init_state(size_t size);

    moffset malloc_internal(size_t bytes);
    void free_internal(moffset o);

    moffset get_root_internal();
    void set_root_internal(moffset root_offset);

    unsigned char* relocate(unsigned char* ptr);
};

#endif
