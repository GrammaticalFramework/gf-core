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

typedef struct PgfPGF PgfPGF;

typedef struct {
    const char *type;
    const char *msg;
} PgfExn;

PGF_API_DECL
PgfPGF *pgf_read(const char* fpath, PgfExn* err);

#endif // PGF_H_
