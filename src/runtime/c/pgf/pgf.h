#ifndef PGF_H_
#define PGF_H_

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#if defined(_MSC_VER)

#if defined(COMPILING_PGF)
#define PGF_API_DECL __declspec(dllexport) EXTERN_C
#define PGF_API      __declspec(dllexport) EXTERN_C
#else
#define PGF_API_DECL __declspec(dllimport)
#define PGF_API      ERROR_NOT_COMPILING_LIBPGF
#endif
#define PGF_INTERNAL_DECL
#define PGF_INTERNAL

#elif defined(__MINGW32__)

#define PGF_API_DECL EXTERN_C
#define PGF_API      EXTERN_C

#define PGF_INTERNAL_DECL
#define PGF_INTERNAL

#else

#define PGF_API_DECL EXTERN_C
#define PGF_API      EXTERN_C

#define PGF_INTERNAL_DECL  __attribute__ ((visibility ("hidden")))
#define PGF_INTERNAL       __attribute__ ((visibility ("hidden")))

#endif

#include<stdint.h>
#include <sys/types.h>

/* A generic structure to store text. The last field is variable length */
typedef struct {
    size_t size;
    char text[];
} PgfText;

/* A generic structure to pass a callback for iteration over a collection */
typedef struct PgfItor PgfItor;

struct PgfItor {
	void (*fn)(PgfItor* self, PgfText* key, void *value);
};

typedef enum {
	PGF_BIND_TYPE_EXPLICIT,
	PGF_BIND_TYPE_IMPLICIT
} PgfBindType;

typedef int PgfMetaId;

typedef struct {
    PgfBindType bind_type;
	PgfText *cid;
	uintptr_t type;
} PgfTypeHypo;

/* This structure tells the runtime how to create abstract syntax
 * expressions in the heap of the host language. For instance,
 * when used from Haskell the runtime will create values of
 * an algebraic data type which can be garbage collected
 * when not needed. Similarly in Python the expressions are
 * normal Python objects. From the point of view of the runtime,
 * each node is a value of type uintptr_t. For Haskell that would
 * actually be a stable pointer, while for Python that would be
 * a PyObject pointer.
 */
#ifdef __cplusplus
struct PgfUnmarshaller {
    virtual uintptr_t eabs(PgfBindType btype, PgfText *name, uintptr_t body)=0;
    virtual uintptr_t eapp(uintptr_t fun, uintptr_t arg)=0;
    virtual uintptr_t elit(uintptr_t lit)=0;
    virtual uintptr_t emeta(PgfMetaId meta)=0;
    virtual uintptr_t efun(PgfText *name)=0;
    virtual uintptr_t evar(int index)=0;
    virtual uintptr_t etyped(uintptr_t expr, uintptr_t typ)=0;
    virtual uintptr_t eimplarg(uintptr_t expr)=0;
    virtual uintptr_t lint(int v)=0;
    virtual uintptr_t lflt(double v)=0;
    virtual uintptr_t lstr(PgfText *v)=0;
    virtual uintptr_t dtyp(int n_hypos, PgfTypeHypo *hypos,
                           PgfText *cat,
                           int n_exprs, uintptr_t *exprs)=0;
    virtual void free_ref(uintptr_t x)=0;
    virtual void free_me()=0;
};

struct PgfMarshaller {
    virtual uintptr_t match_lit(PgfUnmarshaller *u, uintptr_t lit)=0;
    virtual uintptr_t match_expr(PgfUnmarshaller *u, uintptr_t expr)=0;
    virtual uintptr_t match_type(PgfUnmarshaller *u, uintptr_t ty)=0;
    virtual void free_ref(uintptr_t x)=0;
    virtual void free_me()=0;
};
#else
typedef struct PgfUnmarshaller PgfUnmarshaller;
typedef struct PgfUnmarshallerVtbl PgfUnmarshallerVtbl;
struct PgfUnmarshallerVtbl {
    uintptr_t (*eabs)(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, uintptr_t body);
    uintptr_t (*eapp)(PgfUnmarshaller *this, uintptr_t fun, uintptr_t arg);
    uintptr_t (*elit)(PgfUnmarshaller *this, uintptr_t lit);
    uintptr_t (*emeta)(PgfUnmarshaller *this, PgfMetaId meta);
    uintptr_t (*efun)(PgfUnmarshaller *this, PgfText *name);
    uintptr_t (*evar)(PgfUnmarshaller *this, int index);
    uintptr_t (*etyped)(PgfUnmarshaller *this, uintptr_t expr, uintptr_t typ);
    uintptr_t (*eimplarg)(PgfUnmarshaller *this, uintptr_t expr);
    uintptr_t (*lint)(PgfUnmarshaller *this, int v);
    uintptr_t (*lflt)(PgfUnmarshaller *this, double v);
    uintptr_t (*lstr)(PgfUnmarshaller *this, PgfText *v);
    uintptr_t (*dtyp)(PgfUnmarshaller *this,
                      int n_hypos, PgfTypeHypo *hypos,
                      PgfText *cat,
                      int n_exprs, uintptr_t *exprs);
    void (*free_ref)(PgfUnmarshaller *this, uintptr_t x);
    void (*free_me)(PgfUnmarshaller *this);
};
struct PgfUnmarshaller {
    PgfUnmarshallerVtbl *vtbl;
};

typedef struct PgfMarshaller PgfMarshaller;
typedef struct PgfMarshallerVtbl PgfMarshallerVtbl;
struct PgfMarshallerVtbl {
    uintptr_t (*match_lit)(PgfUnmarshaller *u, uintptr_t lit);
    uintptr_t (*match_expr)(PgfUnmarshaller *u, uintptr_t expr);
    uintptr_t (*match_type)(PgfUnmarshaller *u, uintptr_t ty);
    void (*free_me)(PgfUnmarshaller *this);
};
struct PgfMarshaller {
    PgfMarshallerVtbl *vtbl;
};
#endif

typedef float prob_t;

typedef struct PgfPGF PgfPGF;

/* All functions that may fail take a reference to a PgfExn structure.
 * It is used as follows:
 * 
 * - If everything went fine, the field type will be equal to
 * PGF_EXN_NONE and all other fields will be zeroed.
 *
 * - If the exception was caused by external factors such as an error
 * from a system call, then type will be PGF_EXN_SYSTEM_ERROR and
 * the field code will contain the value of errno from the C runtime.
 *
 * - If the exception was caused by factors related to the GF runtime
 * itself, then the error type is PGF_EXN_PGF_ERROR, and the field
 * msg will contain a newly allocated string which must be freed from
 * the caller.
 */

typedef enum {
    PGF_EXN_NONE,
    PGF_EXN_SYSTEM_ERROR,
    PGF_EXN_PGF_ERROR
}  PgfExnType;

typedef struct {
    PgfExnType type;
    int code;
    const char *msg;
} PgfExn;

/* Reads a PGF file and keeps it in memory. */
PGF_API_DECL
PgfPGF *pgf_read_pgf(const char* fpath,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err);

/* Reads a PGF file and stores the unpacked data in an NGF file
 * ready to be shared with other process, or used for quick startup.
 * The NGF file is platform dependent and should not be copied
 * between machines. */
PGF_API_DECL
PgfPGF *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err);

/* Tries to read the grammar from an already booted NGF file.
 * If the file does not exist then a new one is created, and the
 * grammar is set to be empty. It can later be populated with
 * rules dynamically. */
PGF_API_DECL
PgfPGF *pgf_read_ngf(const char* fpath,
                     PgfUnmarshaller *unmarshaller,
                     PgfExn* err);

/* Release the grammar when it is no longer needed. */
PGF_API_DECL
void pgf_free(PgfPGF *pgf);

PGF_API_DECL
PgfText *pgf_abstract_name(PgfPGF *pgf);

PGF_API_DECL
void pgf_iter_categories(PgfPGF *pgf, PgfItor *itor);

PGF_API_DECL
uintptr_t pgf_start_cat(PgfPGF *pgf);

PGF_API_DECL PgfTypeHypo*
pgf_category_context(PgfPGF *pgf, PgfText *catname, size_t *n_hypos);

PGF_API_DECL prob_t
pgf_category_prob(PgfPGF* pgf, PgfText *catname);

PGF_API_DECL
void pgf_iter_functions(PgfPGF *pgf, PgfItor *itor);

PGF_API_DECL
void pgf_iter_functions_by_cat(PgfPGF *pgf, PgfText *cat, PgfItor *itor);

PGF_API_DECL
uintptr_t pgf_function_type(PgfPGF *pgf, PgfText *funname);

PGF_API_DECL
int pgf_function_is_constructor(PgfPGF *pgf, PgfText *funname);

PGF_API_DECL
prob_t pgf_function_prob(PgfPGF *pgf, PgfText *funname);

typedef struct PgfPrintContext PgfPrintContext;

struct PgfPrintContext {
	PgfPrintContext* next;
	PgfText name;
};

PGF_API_DECL
PgfText *pgf_print_expr(uintptr_t e,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m);

PGF_API_DECL
uintptr_t pgf_read_expr(PgfText *input, PgfUnmarshaller *u);

PGF_API_DECL
PgfText *pgf_print_type(uintptr_t ty,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m);

PGF_API_DECL
uintptr_t pgf_read_type(PgfText *input, PgfUnmarshaller *u);

#endif // PGF_H_
