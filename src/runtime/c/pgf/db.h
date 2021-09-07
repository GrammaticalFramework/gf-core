#ifndef DB_H
#define DB_H

#define MALLOC_ALIGN_MASK (2*sizeof(size_t) - 1)

class PgfDB;

extern PGF_INTERNAL_DECL __thread unsigned char* current_base __attribute__((tls_model("initial-exec")));
extern PGF_INTERNAL_DECL __thread PgfDB*         current_db   __attribute__((tls_model("initial-exec")));

struct malloc_state;

template<class A> class PGF_INTERNAL_DECL ref {
private:
    object offset;

    friend class PgfDB;

public:
    ref<A>() { }
    ref<A>(object o) { offset = o; }

    A* operator->() const { return (A*) (current_base+offset); }
    operator A*()   const { return (A*) (current_base+offset); }
    bool operator ==(ref<A>& other) const { return offset==other->offset; }
    bool operator !=(ref<A>& other) const { return offset!=other->offset; }
    bool operator ==(object other_offset) const { return offset==other_offset; }
    bool operator !=(object other_offset) const { return offset!=other_offset; }

    ref<A>& operator= (const ref<A>& r) {
        offset = r.offset;
        return *this;
    }

    static
    ref<A> from_ptr(A *ptr) { return (((uint8_t*) ptr) - current_base); }

    object as_object() { return offset; }

    static
    object tagged(ref<A> ref) {
        assert(A::tag < MALLOC_ALIGN_MASK + 1);
        return (ref.offset | A::tag);
    }

    static
    ref<A> untagged(object v) {
        return (v & ~MALLOC_ALIGN_MASK);
    }

    static
    uint8_t get_tag(object v) {
        return (v & MALLOC_ALIGN_MASK);
    }
};

class PgfDB {
private:
    int fd;
    malloc_state* ms;

    pthread_rwlock_t rwlock;

    friend class PgfReader;

public:
    PGF_INTERNAL_DECL PgfDB(const char* pathname, int flags, int mode);
    PGF_INTERNAL_DECL ~PgfDB();

    template<class A>
    static ref<A> malloc() {
        return current_db->malloc_internal(sizeof(A));
    }

    template<class A>
    static ref<A> malloc(size_t bytes) {
        return current_db->malloc_internal(bytes);
    }

    template<class A>
    static void free(ref<A> o) {
        return current_db->free_internal(o.as_object());
    }

    template<class A>
    static ref<A> get_root() {
        return current_db->get_root_internal();
    }

    template<class A>
    static void set_root(ref<A> root) {
        current_db->set_root_internal(root.offset);
    }

    PGF_INTERNAL_DECL static void sync();

private:
    PGF_INTERNAL_DECL void init_state(size_t size);

    PGF_INTERNAL_DECL object malloc_internal(size_t bytes);
    PGF_INTERNAL_DECL void free_internal(object o);

    PGF_INTERNAL_DECL object get_root_internal();
    PGF_INTERNAL_DECL void set_root_internal(object root_offset);

    PGF_INTERNAL_DECL unsigned char* relocate(unsigned char* ptr);

    friend class DB_scope;
};

enum DB_scope_mode {READER_SCOPE, WRITER_SCOPE};

class PGF_INTERNAL_DECL DB_scope {
public:
    DB_scope(PgfDB *db, DB_scope_mode type);
    ~DB_scope();

private:
    PgfDB* save_db;
    DB_scope* next_scope;
};

extern PGF_INTERNAL_DECL thread_local DB_scope *last_db_scope;

#endif
