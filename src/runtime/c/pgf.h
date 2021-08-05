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

/* A generic structure to store text. The last field is variable length */
typedef struct {
    size_t size;
    char text[];
} PgfText;

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

PGF_API_DECL
PgfPGF *pgf_read(const char* fpath, PgfExn* err);

PGF_API_DECL
void pgf_free(PgfPGF *pgf);

#endif // PGF_H_
