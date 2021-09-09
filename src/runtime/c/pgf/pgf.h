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

/* All functions that may fail take a reference to a PgfExn structure.
 * It is used as follows:
 * 
 * - If everything went fine, the field type will be equal to
 * PGF_EXN_NONE and all other fields will be zeroed.
 *
 * - If the exception was caused by external factors such as an error
 * from a system call, then type will be PGF_EXN_SYSTEM_ERROR and
 * the field code will contain the value of errno from the C runtime.
 * The field msg will be NULL or it may contain a file name.
 * The file name will be the same string that was passed when the API
 * function was called. This means that the string does not have to
 * be freed from the error handling code.
 *
 * - If the exception was caused by factors related to the GF runtime
 * itself, then the error type is PGF_EXN_PGF_ERROR, and the field
 * msg will contain a newly allocated string which must be freed from
 * the caller.
 *
 * - If another unidentified error occurred then type will be
 * PGF_EXN_OTHER_ERROR.
 */

typedef enum {
    PGF_EXN_NONE,
    PGF_EXN_SYSTEM_ERROR,
    PGF_EXN_PGF_ERROR,
    PGF_EXN_OTHER_ERROR
}  PgfExnType;

typedef struct {
    PgfExnType type;
    int code;
    const char *msg;
} PgfExn;

/* A generic structure to pass a callback for iteration over a collection */
typedef struct PgfItor PgfItor;

struct PgfItor {
	void (*fn)(PgfItor* self, PgfText* key, void *value,
		       PgfExn *err);
};

typedef uintptr_t object;

/// An abstract syntax tree
typedef object PgfExpr;

/// A literal for an abstract syntax tree
typedef object PgfLiteral;
typedef object PgfType;

typedef enum {
	PGF_BIND_TYPE_EXPLICIT,
	PGF_BIND_TYPE_IMPLICIT
} PgfBindType;

typedef int PgfMetaId;

typedef struct {
    PgfBindType bind_type;
	PgfText *cid;
	PgfType type;
} PgfTypeHypo;

/* Arbitrary size integers are represented as an array of uintmax_t
 * values. Each value in the array is at most LINT_BASE-1 big.
 * LINT_BASE itself is always 10 ^ LINT_BASE_LOG. */
#if __WORDSIZE == 8
#define LINT_BASE 100
#define LINT_BASE_LOG 2
#elif __WORDSIZE == 16
#define LINT_BASE 10000
#define LINT_BASE_LOG 4
#elif __WORDSIZE == 32
#define LINT_BASE 1000000000
#define LINT_BASE_LOG 9
#elif __WORDSIZE == 64
#define LINT_BASE 10000000000000000000
#define LINT_BASE_LOG 19
#else
#error "Platforms with the current __WORDSIZE are not supported yet"
#endif

/* The PgfUnmarshaller structure tells the runtime how to create
 * abstract syntax expressions and types in the heap of
 * the host language. For instance, when used from Haskell the runtime
 * will create values of an algebraic data type which can be
 * garbage collected when not needed. Similarly in Python
 * the expressions are normal objects. From the point of view of
 * the runtime, each node is a value of type object. For Haskell that
 * would actually be a stable pointer, while for Python that would be
 * a PyObject pointer.
 *
 * The unmarshaller also has the method free_ref which lets the
 * runtime to release the object when it is not needed anymore.
 *
 * The runtime also needs a way to pattern match on expressions
 * and types. The PgfMarshaller structure allows just that.
 * The methods match_lit, match_expr, match_type do pattern matching
 * and depending on the kind of literal, expression or type they call
 * a different method from the unmarshaller that is passed as
 * an argument.
 */
#ifdef __cplusplus
struct PgfUnmarshaller {
    virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body)=0;
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg)=0;
    virtual PgfExpr elit(PgfLiteral lit)=0;
    virtual PgfExpr emeta(PgfMetaId meta)=0;
    virtual PgfExpr efun(PgfText *name)=0;
    virtual PgfExpr evar(int index)=0;
    virtual PgfExpr etyped(PgfExpr expr, PgfType typ)=0;
    virtual PgfExpr eimplarg(PgfExpr expr)=0;
    virtual PgfLiteral lint(size_t size, uintmax_t *v)=0;
    virtual PgfLiteral lflt(double v)=0;
    virtual PgfLiteral lstr(PgfText *v)=0;
    virtual PgfType dtyp(int n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         int n_exprs, PgfExpr *exprs)=0;
    virtual void free_ref(object x)=0;
};

struct PgfMarshaller {
    virtual object match_lit(PgfUnmarshaller *u, PgfLiteral lit)=0;
    virtual object match_expr(PgfUnmarshaller *u, PgfExpr expr)=0;
    virtual object match_type(PgfUnmarshaller *u, PgfType ty)=0;
};
#else
typedef struct PgfUnmarshaller PgfUnmarshaller;
typedef struct PgfUnmarshallerVtbl PgfUnmarshallerVtbl;
struct PgfUnmarshallerVtbl {
    PgfExpr (*eabs)(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body);
    PgfExpr (*eapp)(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg);
    PgfExpr (*elit)(PgfUnmarshaller *this, PgfLiteral lit);
    PgfExpr (*emeta)(PgfUnmarshaller *this, PgfMetaId meta);
    PgfExpr (*efun)(PgfUnmarshaller *this, PgfText *name);
    PgfExpr (*evar)(PgfUnmarshaller *this, int index);
    PgfExpr (*etyped)(PgfUnmarshaller *this, PgfExpr expr, PgfType typ);
    PgfExpr (*eimplarg)(PgfUnmarshaller *this, PgfExpr expr);
    PgfLiteral (*lint)(PgfUnmarshaller *this, size_t size, uintmax_t *v);
    PgfLiteral (*lflt)(PgfUnmarshaller *this, double v);
    PgfLiteral (*lstr)(PgfUnmarshaller *this, PgfText *v);
    PgfType (*dtyp)(PgfUnmarshaller *this,
                    int n_hypos, PgfTypeHypo *hypos,
                    PgfText *cat,
                    int n_exprs, PgfExpr *exprs);
    void (*free_ref)(PgfUnmarshaller *this, object x);
};
struct PgfUnmarshaller {
    PgfUnmarshallerVtbl *vtbl;
};

typedef struct PgfMarshaller PgfMarshaller;
typedef struct PgfMarshallerVtbl PgfMarshallerVtbl;
struct PgfMarshallerVtbl {
    object (*match_lit)(PgfUnmarshaller *u, PgfLiteral lit);
    object (*match_expr)(PgfUnmarshaller *u, PgfExpr expr);
    object (*match_type)(PgfUnmarshaller *u, PgfType ty);
};
struct PgfMarshaller {
    PgfMarshallerVtbl *vtbl;
};
#endif

typedef float prob_t;

typedef struct PgfDB PgfDB;
typedef object PgfRevision;

/* Reads a PGF file and builds the database in memory.
 * If successful, *revision will contain the initial revision of
 * the grammar. */
PGF_API_DECL
PgfDB *pgf_read_pgf(const char* fpath, PgfRevision *revision,
                    PgfExn* err);

/* Reads a PGF file and stores the unpacked data in an NGF file
 * ready to be shared with other process, or used for quick re-start.
 * The NGF file is platform dependent and should not be copied
 * between machines. */
PGF_API_DECL
PgfDB *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                    PgfRevision *revision,
                    PgfExn* err);

/* Tries to read the grammar from an already booted NGF file.
 * If the file does not exist then a new one is created, and the
 * grammar is set to be empty. It can later be populated with
 * rules dynamically. The default grammar revision is stored
 * in *revision. */
PGF_API_DECL
PgfDB *pgf_read_ngf(const char* fpath,
                    PgfRevision *revision,
                    PgfExn* err);

/* Release the database when it is no longer needed. */
PGF_API_DECL
void pgf_free(PgfDB *pgf);

PGF_API_DECL
void pgf_free_revision(PgfDB *pgf, PgfRevision revision);

/* Returns a newly allocated text which contains the abstract name of
 * the grammar. The text must be released with a call to free.
 */
PGF_API_DECL
PgfText *pgf_abstract_name(PgfDB *db, PgfRevision revision,
                           PgfExn* err);

PGF_API_DECL
void pgf_iter_categories(PgfDB *db, PgfRevision revision,
                         PgfItor *itor, PgfExn *err);

PGF_API_DECL
PgfType pgf_start_cat(PgfDB *db, PgfRevision revision,
                      PgfUnmarshaller *u,
                      PgfExn* err);

PGF_API_DECL
PgfTypeHypo *pgf_category_context(PgfDB *db, PgfRevision revision,
                                  PgfText *catname, size_t *n_hypos, PgfUnmarshaller *u,
                                  PgfExn* err);

PGF_API_DECL
prob_t pgf_category_prob(PgfDB *db, PgfRevision revision,
                         PgfText *catname,
                         PgfExn* err);

PGF_API_DECL
void pgf_iter_functions(PgfDB *db, PgfRevision revision,
                        PgfItor *itor, PgfExn *err);

PGF_API_DECL
void pgf_iter_functions_by_cat(PgfDB *db, PgfRevision revision,
                               PgfText *cat, PgfItor *itor, PgfExn *err);

PGF_API_DECL
PgfType pgf_function_type(PgfDB *db, PgfRevision revision,
                          PgfText *funname, PgfUnmarshaller *u,
                          PgfExn* err);

PGF_API_DECL
int pgf_function_is_constructor(PgfDB *db, PgfRevision revision,
                                PgfText *funname,
                                PgfExn* err);

PGF_API_DECL
prob_t pgf_function_prob(PgfDB *db, PgfRevision revision,
                         PgfText *funname,
                         PgfExn* err);

typedef struct PgfPrintContext PgfPrintContext;

struct PgfPrintContext {
	PgfPrintContext* next;
	PgfText name;
};

PGF_API_DECL
PgfText *pgf_print_expr(PgfExpr e,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m);

PGF_API_DECL
PgfExpr pgf_read_expr(PgfText *input, PgfUnmarshaller *u);

PGF_API_DECL
PgfText *pgf_print_type(PgfType ty,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m);

PGF_API_DECL
PgfType pgf_read_type(PgfText *input, PgfUnmarshaller *u);

PGF_API_DECL
PgfRevision pgf_clone_revision(PgfDB *db, PgfRevision revision,
                               PgfText *name,
                               PgfExn *err);

PGF_API_DECL
void pgf_commit_revision(PgfDB *db, PgfRevision revision,
                         PgfExn *err);

PGF_API_DECL
PgfRevision pgf_checkout_revision(PgfDB *db, PgfText *name,
                                  PgfExn *err);

PGF_API_DECL
void pgf_create_function(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfType ty, prob_t prob,
                         PgfMarshaller *m,
                         PgfExn *err);

PGF_API_DECL
void pgf_drop_function(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err);

#endif // PGF_H_
