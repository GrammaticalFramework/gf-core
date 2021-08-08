#ifndef DB_H
#define DB_H

class DB;

extern PGF_INTERNAL_DECL __thread unsigned char* current_base __attribute__((tls_model("initial-exec")));
extern PGF_INTERNAL_DECL __thread DB*            current_db   __attribute__((tls_model("initial-exec")));

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

    pthread_rwlock_t rwlock;

    friend class PgfReader;

public:
    DB(const char* pathname, int flags, int mode);
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

    static void sync();

private:
    void init_state(size_t size);

    moffset malloc_internal(size_t bytes);
    void free_internal(moffset o);

    moffset get_root_internal();
    void set_root_internal(moffset root_offset);

    unsigned char* relocate(unsigned char* ptr);

    friend class DB_scope;
};

enum DB_scope_mode {READER_SCOPE, WRITER_SCOPE};

class PGF_INTERNAL_DECL DB_scope {
public:
    DB_scope(DB *db, DB_scope_mode type);
    ~DB_scope();

private:
    DB* save_db;
    DB_scope* next_scope;
};

extern PGF_INTERNAL_DECL thread_local DB_scope *last_db_scope;

#endif
