#ifndef DB_H
#define DB_H

#define MALLOC_ALIGN_MASK (2*sizeof(size_t) - 1)

class PgfDB;

extern PGF_INTERNAL_DECL __thread unsigned char* current_base __attribute__((tls_model("initial-exec")));
extern PGF_INTERNAL_DECL __thread PgfDB*         current_db   __attribute__((tls_model("initial-exec")));

struct block_descr;
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
    bool operator ==(ref<A>& other) const { return offset==other.as_object(); }
    bool operator !=(ref<A>& other) const { return offset!=other.as_object(); }
    bool operator ==(object other_offset) const { return offset==other_offset; }
    bool operator !=(object other_offset) const { return offset!=other_offset; }

    ref<A>& operator= (const ref<A>& r) {
        offset = r.offset;
        return *this;
    }

    static
    ref<A> from_ptr(A *ptr) { return (((uint8_t*) ptr) - current_base); }

    object as_object() const { return offset; }

    object tagged() {
        assert(A::tag < MALLOC_ALIGN_MASK + 1);
        return (offset | A::tag);
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

enum DB_scope_mode {READER_SCOPE, WRITER_SCOPE};

typedef size_t txn_t;

class PgfDB {
private:
    int fd;
    const char *filepath;
    malloc_state* ms;
    unsigned char* base;

    // The following four fields are normally equal to
    // the corresponding fields in the malloc_state.
    // The exception is when a transaction is active.
    object top;         
    object free_blocks;
    object free_descriptors[3];

    size_t mmap_size;
    size_t page_size;
#ifndef _WIN32
    pid_t pid;
#else
    DWORD pid;
    HANDLE hMap;
    HANDLE hMutex;
    HANDLE hRWEvent;
#endif

    friend class PgfReader;

public:
    // Here we count to how many revisions the client has access.
    // When the count is zero we release the database.
    size_t ref_count;

    PGF_INTERNAL_DECL PgfDB(const char* filepath, int flags, int mode);
    PGF_INTERNAL_DECL ~PgfDB();

    PGF_INTERNAL_DECL static txn_t get_txn_id();

    template<class A>
    static ref<A> malloc(size_t extra_bytes=0) {
        return current_db->malloc_internal(sizeof(A)+extra_bytes);
    }

    template<class A>
    static ref<A> realloc(ref<A> r, size_t old_extra_bytes, size_t new_extra_bytes) {
        return current_db->realloc_internal(r.as_object(), sizeof(A)+old_extra_bytes, sizeof(A)+new_extra_bytes);
    }

    template<class A>
    static void free(ref<A> o, size_t extra_bytes=0) {
        current_db->free_internal(o.as_object(), sizeof(A)+extra_bytes);
    }

	PGF_INTERNAL_DECL void cleanup_revisions();

    PGF_INTERNAL_DECL object get_active_revision();
    PGF_INTERNAL_DECL void register_revision(object o);
    PGF_INTERNAL_DECL void unregister_revision(object o);

    PGF_INTERNAL_DECL ref<PgfPGF> revision2pgf(PgfRevision revision);
    PGF_INTERNAL_DECL ref<PgfConcr> revision2concr(PgfConcrRevision revision);

    PGF_INTERNAL_DECL void start_transaction();
    PGF_INTERNAL_DECL void commit(object o);
    PGF_INTERNAL_DECL void rollback();

private:
    PGF_INTERNAL_DECL int init_state();

    PGF_INTERNAL_DECL size_t block_descr_size(object map);
    PGF_INTERNAL_DECL object new_block_descr(object o, size_t size, txn_t txn_id);
    PGF_INTERNAL_DECL object upd_block_descr(object map, object left, object right);
    PGF_INTERNAL_DECL object balanceL_block_descriptor(object map);
    PGF_INTERNAL_DECL object balanceR_block_descriptor(object map);
    PGF_INTERNAL_DECL object pop_first_block_descriptor(object map, object *res);
    PGF_INTERNAL_DECL object pop_last_block_descriptor(object map, object *res);
    PGF_INTERNAL_DECL object insert_block_descriptor(object map, object o, size_t size);
    PGF_INTERNAL_DECL object delete_block_descriptor(object map, size_t *psize, object *po);
    
#ifdef DEBUG_MEMORY_ALLOCATOR
    PGF_INTERNAL_DECL void dump_free_blocks(object map);
#endif

    PGF_INTERNAL_DECL object malloc_internal(size_t bytes);

    PGF_INTERNAL_DECL object realloc_internal(object oldo, size_t old_bytes, size_t new_bytes);

    PGF_INTERNAL_DECL void free_internal(object o, size_t bytes);

    PGF_INTERNAL_DECL void lock(DB_scope_mode m);
    PGF_INTERNAL_DECL void unlock();

    PGF_INTERNAL_DECL void resize_map(size_t new_size);

    friend class DB_scope;
};

class PGF_INTERNAL_DECL DB_scope {
public:
    DB_scope(PgfDB *db, DB_scope_mode m);
    ~DB_scope();

private:
    PgfDB* save_db;
    DB_scope* next_scope;
};

extern PGF_INTERNAL_DECL __thread DB_scope *last_db_scope __attribute__((tls_model("initial-exec")));

#endif
