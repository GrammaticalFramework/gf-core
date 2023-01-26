#ifndef PGF_H_
#define PGF_H_

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)

#if defined(COMPILING_PGF)
#define PGF_API_DECL EXTERN_C __declspec(dllexport)
#define PGF_API      EXTERN_C __declspec(dllexport)
#elif defined(COMPILING_STATIC_PGF)
#define PGF_API_DECL EXTERN_C __declspec(dllexport)
#define PGF_API      EXTERN_C __declspec(dllexport)
#else
#define PGF_API_DECL __declspec(dllimport)
#define PGF_API      ERROR_NOT_COMPILING_LIBPGF
#endif
#define PGF_INTERNAL_DECL
#define PGF_INTERNAL

#else

#define PGF_API_DECL EXTERN_C
#define PGF_API      EXTERN_C

#define PGF_INTERNAL_DECL  __attribute__ ((visibility ("hidden")))
#define PGF_INTERNAL       __attribute__ ((visibility ("hidden")))

#endif

#if defined(_MSC_VER)
#pragma warning(disable : 4200)
#endif

#include <stdint.h>
#include <sys/types.h>

#define PGF_MAJOR_VERSION 2
#define PGF_MINOR_VERSION 1

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
    PGF_EXN_TYPE_ERROR,
    PGF_EXN_OTHER_ERROR
}  PgfExnType;

typedef struct {
    PgfExnType type;
    int code;
    const char *msg;
} PgfExn;

typedef uintptr_t object;

/* A generic structure to pass a callback for iteration over a collection */
typedef struct PgfItor PgfItor;

struct PgfItor {
	void (*fn)(PgfItor* self, PgfText* key, object value,
		       PgfExn *err);
};

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
#if defined(__WORDSIZE)
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
#define LINT_BASE 10000000000000000000UL
#define LINT_BASE_LOG 19
#else
#error "Platforms with the current __WORDSIZE are not supported yet"
#endif
#elif defined(_WIN64)
#define LINT_BASE 10000000000000000000UL
#define LINT_BASE_LOG 19
#elif defined(_WIN32)
#define LINT_BASE 1000000000
#define LINT_BASE_LOG 9
#elif defined(EMSCRIPTEN)
#define LINT_BASE 10000000000000000000UL
#define LINT_BASE_LOG 19
#else
#error "Unsupported platform"
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
    virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         size_t n_exprs, PgfExpr *exprs)=0;
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
    object (*match_lit)(PgfMarshaller *this, PgfUnmarshaller *u, PgfLiteral lit);
    object (*match_expr)(PgfMarshaller *this, PgfUnmarshaller *u, PgfExpr expr);
    object (*match_type)(PgfMarshaller *this, PgfUnmarshaller *u, PgfType ty);
};
struct PgfMarshaller {
    PgfMarshallerVtbl *vtbl;
};
#endif

typedef float prob_t;

#ifdef __cplusplus
class PgfDB;
#else
typedef struct PgfDB PgfDB;
#endif
typedef object PgfRevision;
typedef object PgfConcrRevision;

typedef struct PgfProbsCallback PgfProbsCallback;
struct PgfProbsCallback {
	double (*fn)(PgfProbsCallback* self, PgfText *name);
};

/* Reads a PGF file and builds the database in memory.
 * If successful, *revision will contain the initial revision of
 * the grammar. */
PGF_API_DECL
PgfDB *pgf_read_pgf(const char* fpath, PgfRevision *revision,
                    PgfProbsCallback *probs_callback,
                    PgfExn* err);

/* Reads a PGF file and stores the unpacked data in an NGF file
 * ready to be shared with other process, or used for quick re-start.
 * The NGF file is platform dependent and should not be copied
 * between machines. */
PGF_API_DECL
PgfDB *pgf_boot_ngf(const char* pgf_path, const char* ngf_path,
                    PgfRevision *revision,
                    PgfProbsCallback *probs_callback,
                    PgfExn* err);

/* Tries to read the grammar from an already booted NGF file.
 * The function fails if the file does not exist. The default grammar
 * revision is stored in *revision. */
PGF_API_DECL
PgfDB *pgf_read_ngf(const char* fpath,
                    PgfRevision *revision,
                    PgfExn* err);

/* Creates a new NGF file with a grammar with the given abstract_name.
 * Aside from the name, the grammar is otherwise empty but can be later
 * populated with new functions and categories. If fpath is NULL then
 * the file is not stored on the disk but only in memory. */
PGF_API_DECL
PgfDB *pgf_new_ngf(PgfText *abstract_name,
                   const char *fpath,
                   PgfRevision *revision,
                   PgfExn* err);

PGF_API_DECL
void pgf_merge_pgf(PgfDB *db, PgfRevision revision,
                   const char* fpath,
                   PgfExn* err);

PGF_API_DECL
void pgf_write_pgf(const char* fpath,
                   PgfDB *db, PgfRevision revision,
                   PgfExn* err);

PGF_API_DECL
const char *pgf_file_path(PgfDB *db);

/* Release a revision. If this is the last revision for the given
 * database, then the database is released as well. */
PGF_API_DECL
void pgf_free_revision(PgfDB *pgf, PgfRevision revision);

PGF_API_DECL
void pgf_free_concr_revision(PgfDB *db, PgfConcrRevision revision);

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
void pgf_iter_concretes(PgfDB *db, PgfRevision revision,
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

PGF_API_DECL
PgfText *pgf_concrete_name(PgfDB *db, PgfConcrRevision revision,
                           PgfExn* err);

PGF_API_DECL
PgfText *pgf_concrete_language_code(PgfDB *db, PgfConcrRevision revision,
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
PgfText *pgf_print_ident(PgfText *name);

PGF_API_DECL
PgfExpr pgf_read_expr(PgfText *input, PgfUnmarshaller *u);

PGF_API_DECL
PgfExpr pgf_read_expr_ex(PgfText *input, const char **end_pos, PgfUnmarshaller *u);

PGF_API_DECL
prob_t pgf_expr_prob(PgfDB *db, PgfRevision revision,
                     PgfExpr e,
                     PgfMarshaller *m,
                     PgfExn *err);

PGF_API_DECL
PgfText *pgf_print_type(PgfType ty,
                        PgfPrintContext *ctxt, int prio,
                        PgfMarshaller *m);

PGF_API_DECL
PgfText *pgf_print_context(size_t n_hypos, PgfTypeHypo *hypos,
                           PgfPrintContext *ctxt, int prio,
                           PgfMarshaller *m);

PGF_API_DECL
PgfType pgf_read_type(PgfText *input, PgfUnmarshaller *u);

PGF_API_DECL
PgfTypeHypo *pgf_read_context(PgfText *input, PgfUnmarshaller *u, size_t *n_hypos);

PGF_API_DECL
PgfText *pgf_print_start_cat_internal(PgfDB *db, PgfRevision revision, PgfExn *err);

PGF_API_DECL
PgfText *pgf_print_category_internal(object o);

PGF_API_DECL
PgfText *pgf_print_function_internal(object o);

PGF_API_DECL
void pgf_iter_lincats(PgfDB *db, PgfConcrRevision cnc_revision,
                      PgfItor *itor, PgfExn *err);

PGF_API_DECL
void pgf_iter_lins(PgfDB *db, PgfConcrRevision cnc_revision,
                   PgfItor *itor, PgfExn *err);

typedef struct PgfPhrasetableIds PgfPhrasetableIds;

typedef struct PgfSequenceItor PgfSequenceItor;
struct PgfSequenceItor {
	int (*fn)(PgfSequenceItor* self, size_t seq_id, object value,
              PgfExn *err);
};

typedef struct PgfMorphoCallback PgfMorphoCallback;
struct PgfMorphoCallback {
	void (*fn)(PgfMorphoCallback* self, PgfText *lemma, PgfText *analysis, prob_t prob,
	           PgfExn* err);
};

PGF_API_DECL
void pgf_lookup_morpho(PgfDB *db, PgfConcrRevision cnc_revision,
                       PgfText *sentence,
                       PgfMorphoCallback* callback, PgfExn* err);

typedef struct PgfCohortsCallback PgfCohortsCallback;
struct PgfCohortsCallback {
    PgfMorphoCallback morpho;
	void (*fn)(PgfCohortsCallback* self, size_t start, size_t end,
	           PgfExn* err);
};

PGF_API_DECL
void pgf_lookup_cohorts(PgfDB *db, PgfConcrRevision cnc_revision,
                        PgfText *sentence,
                        PgfCohortsCallback* callback, PgfExn* err);

PGF_API_DECL
PgfPhrasetableIds *pgf_iter_sequences(PgfDB *db, PgfConcrRevision cnc_revision,
                                      PgfSequenceItor *itor,
                                      PgfMorphoCallback *callback,
                                      PgfExn *err);

PGF_API_DECL
void pgf_get_lincat_counts_internal(object o, size_t *counts);

PGF_API_DECL
PgfText *pgf_get_lincat_field_internal(object o, size_t i);

PGF_API_DECL
size_t pgf_get_lin_get_prod_count(object o);

PGF_API_DECL
PgfText *pgf_print_lindef_internal(PgfPhrasetableIds *seq_ids, object o, size_t i);

PGF_API_DECL
PgfText *pgf_print_linref_internal(PgfPhrasetableIds *seq_ids, object o, size_t i);

PGF_API_DECL
PgfText *pgf_print_lin_internal(PgfPhrasetableIds *seq_ids, object o, size_t i);

PGF_API_DECL
PgfText *pgf_print_sequence_internal(size_t seq_id, object o);

PGF_API_DECL
PgfText *pgf_sequence_get_text_internal(object o);

PGF_API_DECL
void pgf_release_phrasetable_ids(PgfPhrasetableIds *seq_ids);

PGF_API_DECL
void pgf_check_expr(PgfDB *db, PgfRevision revision,
                    PgfExpr* pe, PgfType ty,
                    PgfMarshaller *m, PgfUnmarshaller *u,
                    PgfExn *err);

PGF_API_DECL
PgfType pgf_infer_expr(PgfDB *db, PgfRevision revision,
                       PgfExpr* pe,
                       PgfMarshaller *m, PgfUnmarshaller *u,
                       PgfExn *err);

PGF_API_DECL
void pgf_check_type(PgfDB *db, PgfRevision revision,
                    PgfType* pty,
                    PgfMarshaller *m, PgfUnmarshaller *u,
                    PgfExn *err);

PGF_API_DECL
PgfRevision pgf_start_transaction(PgfDB *db, PgfExn *err);

PGF_API_DECL
void pgf_commit_transaction(PgfDB *db, PgfRevision revision,
                            PgfExn *err);

PGF_API_DECL
PgfRevision pgf_checkout_revision(PgfDB *db, PgfExn *err);

PGF_API_DECL
PgfText *pgf_create_function(PgfDB *db, PgfRevision revision,
                             PgfText *name_pattern,
                             PgfType ty, size_t arity, char *bytecode,
                             prob_t prob,
                             PgfMarshaller *m,
                             PgfExn *err);

PGF_API_DECL
void pgf_drop_function(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err);

PGF_API_DECL
void pgf_create_category(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         size_t n_hypos, PgfTypeHypo *context, prob_t prob,
                         PgfMarshaller *m,
                         PgfExn *err);

PGF_API_DECL
void pgf_drop_category(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err);

PGF_API_DECL
PgfConcrRevision pgf_create_concrete(PgfDB *db, PgfRevision revision,
                                     PgfText *name,
                                     PgfExn *err);

PGF_API_DECL
PgfConcrRevision pgf_clone_concrete(PgfDB *db, PgfRevision revision,
                                    PgfText *name,
                                    PgfExn *err);

PGF_API_DECL
void pgf_drop_concrete(PgfDB *db, PgfRevision revision,
                       PgfText *name,
                       PgfExn *err);

#ifdef __cplusplus
struct PgfLinBuilderIface {
    virtual void start_production(PgfExn *err)=0;
    virtual void add_argument(size_t n_hypos, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)=0;
    virtual void set_result(size_t n_vars, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)=0;
    virtual void add_variable(size_t var, size_t range, PgfExn *err)=0;
    virtual void start_sequence(size_t n_syms, PgfExn *err)=0;
    virtual void add_symcat(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)=0;
    virtual void add_symlit(size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err)=0;
    virtual void add_symvar(size_t d, size_t r, PgfExn *err)=0;
    virtual void add_symks(PgfText *token, PgfExn *err)=0;
    virtual void start_symkp(size_t n_syms, size_t n_alts, PgfExn *err)=0;
    virtual void start_symkp_alt(size_t n_syms, size_t n_prefs, PgfText **prefs, PgfExn *err)=0;
    virtual void end_symkp_alt(PgfExn *err)=0;
    virtual void end_symkp(PgfExn *err)=0;
    virtual void add_symbind(PgfExn *err)=0;
    virtual void add_symsoftbind(PgfExn *err)=0;
    virtual void add_symne(PgfExn *err)=0;
    virtual void add_symsoftspace(PgfExn *err)=0;
    virtual void add_symcapit(PgfExn *err)=0;
    virtual void add_symallcapit(PgfExn *err)=0;
    virtual object end_sequence(PgfExn *err)=0;
    virtual void add_sequence_id(object seq_id, PgfExn *err)=0;
    virtual void end_production(PgfExn *err)=0;
};

struct PgfBuildLinIface {
    virtual void build(PgfLinBuilderIface *builder, PgfExn *err)=0;
};
#else
typedef struct PgfLinBuilderIface PgfLinBuilderIface;

typedef struct {
    void (*start_production)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_argument)(PgfLinBuilderIface *this, size_t n_hypos, size_t i0, size_t n_terms, size_t *terms, PgfExn *err);
    void (*set_result)(PgfLinBuilderIface *this, size_t n_vars, size_t i0, size_t n_terms, size_t *terms, PgfExn *err);
    void (*add_variable)(PgfLinBuilderIface *this, size_t var, size_t range, PgfExn *err);
    void (*start_sequence)(PgfLinBuilderIface *this, size_t n_syms, PgfExn *err);
    void (*add_symcat)(PgfLinBuilderIface *this, size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err);
    void (*add_symlit)(PgfLinBuilderIface *this, size_t d, size_t i0, size_t n_terms, size_t *terms, PgfExn *err);
    void (*add_symvar)(PgfLinBuilderIface *this, size_t d, size_t r, PgfExn *err);
    void (*add_symks)(PgfLinBuilderIface *this, PgfText *token, PgfExn *err);
    void (*start_symkp)(PgfLinBuilderIface *this, size_t n_syms, size_t n_alts, PgfExn *err);
    void (*start_symkp_alt)(PgfLinBuilderIface *this, size_t n_syms, size_t n_prefs, PgfText **prefs, PgfExn *err);
    void (*end_symkp_alt)(PgfLinBuilderIface *this, PgfExn *err);
    void (*end_symkp)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symbind)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symsoftbind)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symne)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symsoftspace)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symcapit)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_symallcapit)(PgfLinBuilderIface *this, PgfExn *err);
    object (*end_sequence)(PgfLinBuilderIface *this, PgfExn *err);
    void (*add_sequence_id)(PgfLinBuilderIface *this, object seq_id, PgfExn *err);
    void (*end_production)(PgfLinBuilderIface *this, PgfExn *err);
} PgfLinBuilderIfaceVtbl;

struct PgfLinBuilderIface {
    PgfLinBuilderIfaceVtbl *vtbl;
};

typedef struct PgfBuildLinIface PgfBuildLinIface;

typedef struct {
    void (*build)(PgfBuildLinIface *this, PgfLinBuilderIface *builder, PgfExn *err);
} PgfBuildLinIfaceVtbl;

struct PgfBuildLinIface {
    PgfBuildLinIfaceVtbl *vtbl;
};
#endif

PGF_API_DECL
void pgf_create_lincat(PgfDB *db,
                       PgfRevision revision, PgfConcrRevision cnc_revision,
                       PgfText *name, size_t n_fields, PgfText **fields,
                       size_t n_lindefs, size_t n_linrefs, PgfBuildLinIface *build,
                       PgfExn *err);

PGF_API_DECL
void pgf_drop_lincat(PgfDB *db, PgfConcrRevision revision,
                     PgfText *name, PgfExn *err);

PGF_API_DECL
void pgf_create_lin(PgfDB *db,
                    PgfRevision revision, PgfConcrRevision cnc_revision,
                    PgfText *name, size_t n_prods,
                    PgfBuildLinIface *build,
                    PgfExn *err);

PGF_API_DECL
void pgf_drop_lin(PgfDB *db, PgfConcrRevision revision,
                  PgfText *name, PgfExn *err);

PGF_API_DECL
int pgf_has_linearization(PgfDB *db, PgfConcrRevision revision,
                          PgfText *name, PgfExn *err);

PGF_API_DECL
PgfText **pgf_category_fields(PgfDB *db, PgfConcrRevision revision,
                              PgfText *name, size_t *p_n_fields,
                              PgfExn *err);

#ifdef __cplusplus
struct PgfLinearizationOutputIface
{
    /// Output tokens
    virtual void symbol_token(PgfText *tok)=0;

    /// Begin phrase
    virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)=0;

    /// End phrase
    virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)=0;

    /// handling nonExist
    virtual void symbol_ne()=0;

    /// token binding
    virtual void symbol_bind()=0;

    /// called when the current linearization is finished
    virtual void flush()=0;
};
#else
typedef struct PgfLinearizationOutputIface     PgfLinearizationOutputIface;
typedef struct PgfLinearizationOutputIfaceVtbl PgfLinearizationOutputIfaceVtbl;
struct PgfLinearizationOutputIfaceVtbl
{
    /// Output tokens
    void (*symbol_token)(PgfLinearizationOutputIface *this, PgfText *tok);

    /// Begin phrase
    void (*begin_phrase)(PgfLinearizationOutputIface *this, PgfText *cat, int fid, PgfText *ann, PgfText *fun);

    /// End phrase
    void (*end_phrase)(PgfLinearizationOutputIface *this, PgfText *cat, int fid, PgfText *ann, PgfText *fun);

    /// handling nonExist
    void (*symbol_ne)(PgfLinearizationOutputIface *this);

    /// token binding
    void (*symbol_bind)(PgfLinearizationOutputIface *this);

    /// called when the current linearization is finished
    void (*flush)(PgfLinearizationOutputIface *this);
};
struct PgfLinearizationOutputIface
{
    PgfLinearizationOutputIfaceVtbl *vtbl;
};
#endif

PGF_API_DECL
PgfText *pgf_linearize(PgfDB *db, PgfConcrRevision revision,
                       PgfExpr expr, PgfPrintContext *ctxt,
                       PgfMarshaller *m,
                       PgfExn* err);

PGF_API_DECL
PgfText **pgf_tabular_linearize(PgfDB *db, PgfConcrRevision revision,
                                PgfExpr expr, PgfPrintContext *ctxt,
                                PgfMarshaller *m, PgfExn* err);

PGF_API_DECL
PgfText **pgf_tabular_linearize_all(PgfDB *db, PgfConcrRevision revision,
                                    PgfExpr expr, PgfPrintContext *ctxt,
                                    PgfMarshaller *m, PgfExn* err);

PGF_API_DECL
void pgf_bracketed_linearize(PgfDB *db, PgfConcrRevision revision,
                             PgfExpr expr, PgfPrintContext *ctxt,
                             PgfMarshaller *m,
                             PgfLinearizationOutputIface *out,
                             PgfExn* err);

PGF_API_DECL
void pgf_bracketed_linearize_all(PgfDB *db, PgfConcrRevision revision,
                                 PgfExpr expr, PgfPrintContext *ctxt,
                                 PgfMarshaller *m,
                                 PgfLinearizationOutputIface *out,
                                 PgfExn* err);

#ifdef __cplusplus
struct PgfExprEnum {
    virtual PgfExpr fetch(PgfDB *db, PgfUnmarshaller *u, prob_t *prob)=0;
    virtual ~PgfExprEnum() {};
};
#else
typedef struct PgfExprEnum PgfExprEnum;
typedef struct PgfExprEnumVtbl PgfExprEnumVtbl;
struct PgfExprEnumVtbl {
    PgfExpr (*fetch)(PgfExprEnum *this, PgfDB *db, PgfUnmarshaller *u, prob_t *prob);
};
struct PgfExprEnum {
    PgfExprEnumVtbl *vtbl;
};
#endif

PGF_API_DECL
PgfExprEnum *pgf_parse(PgfDB *db, PgfConcrRevision revision,
                       PgfType ty, PgfMarshaller *m,
                       PgfText *sentence,
                       PgfExn * err);

PGF_API_DECL
void pgf_free_expr_enum(PgfExprEnum *en);

PGF_API_DECL
PgfText *pgf_get_printname(PgfDB *db, PgfConcrRevision revision,
                           PgfText *fun, PgfExn* err);

PGF_API_DECL
void pgf_set_printname(PgfDB *db, PgfConcrRevision revision,
                       PgfText *fun, PgfText *name, PgfExn* err);

PGF_API_DECL
PgfLiteral pgf_get_global_flag(PgfDB *db, PgfRevision revision,
                               PgfText *name,
                               PgfUnmarshaller *u,
                               PgfExn *err);
PGF_API_DECL
void pgf_set_global_flag(PgfDB *db, PgfRevision revision,
                         PgfText *name,
                         PgfLiteral value,
                         PgfMarshaller *m,
                         PgfExn *err);
PGF_API_DECL
PgfLiteral pgf_get_abstract_flag(PgfDB *db, PgfRevision revision,
                                 PgfText *name,
                                 PgfUnmarshaller *u,
                                 PgfExn *err);
PGF_API_DECL
void pgf_set_abstract_flag(PgfDB *db, PgfRevision revision,
                           PgfText *name,
                           PgfLiteral value,
                           PgfMarshaller *m,
                           PgfExn *err);
PGF_API_DECL
PgfLiteral pgf_get_concrete_flag(PgfDB *db, PgfConcrRevision revision,
                                 PgfText *name,
                                 PgfUnmarshaller *u,
                                 PgfExn *err);
PGF_API_DECL
void pgf_set_concrete_flag(PgfDB *db, PgfConcrRevision revision,
                           PgfText *name,
                           PgfLiteral value,
                           PgfMarshaller *m,
                           PgfExn *err);

typedef struct {
    int noLeaves;
    int noFun;
    int noCat;
    int noDep;
    const char *nodeFont;
    const char *leafFont;
    const char *nodeColor;
    const char *leafColor;
    const char *nodeEdgeStyle;
    const char *leafEdgeStyle;
} PgfGraphvizOptions;

PGF_API_DECL PgfText *
pgf_graphviz_parse_tree(PgfDB *db, PgfConcrRevision revision,
                        PgfExpr expr, PgfPrintContext *ctxt,
                        PgfMarshaller *m,
                        PgfGraphvizOptions* opts,
                        PgfExn *err);

#endif // PGF_H_
