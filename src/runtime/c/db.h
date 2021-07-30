#ifndef DB_H
#define DB_H

class DB;

extern thread_local PGF_INTERNAL_DECL unsigned char* current_base;
extern thread_local PGF_INTERNAL_DECL DB* current_db;

typedef size_t moffset;

struct malloc_state;

template<class A> class ref {
    size_t offset;

public:
    ref<A>() { }
    ref<A>(size_t o) { offset = o; }
    inline A* operator->() const { return (A*) (current_base+offset); }
    inline operator A*()   const { return (A*) (current_base+offset); }
    inline bool operator ==(ref<A>& other) const { return offset==other->offset; }
    inline operator size_t() { return offset; }
};

class PGF_INTERNAL_DECL DB {
    int fd;
    malloc_state* ms;

public:
    DB(const char* pathname);
    ~DB();

    template<class A> ref<A> malloc() {
        return malloc(sizeof(A));
    }

    moffset malloc(size_t bytes);

    template<class A> ref<A> get_root() {
        return get_root_offset();
    }
    template<class A> void set_root(ref<A> root) {
        set_root_offset(root);
    }

private:
    void init_state(size_t size);

    void free(moffset o);

    moffset get_root_offset();
    void set_root_offset(moffset root);
};

#endif
