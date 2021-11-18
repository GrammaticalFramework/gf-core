#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <string.h>
#include <assert.h>
#include <exception>
#include <stdexcept>

#include "pgf.h"

#ifdef _WIN32
#include <windows.h>
#endif

class PGF_INTERNAL_DECL pgf_error : public std::runtime_error {
public:
    pgf_error(const char *msg) : std::runtime_error(msg)
    {
        this->msg = msg;
    }

    virtual const char *what() const throw ()
    {
    	return msg;
    }

private:
    const char *msg;
};

class PGF_INTERNAL_DECL pgf_systemerror : public std::runtime_error {
public:
    pgf_systemerror(int code) : std::runtime_error("pgf_systemerror")
    {
        this->m_code     = code;
        this->m_filepath = NULL;
    }

    pgf_systemerror(int code, const char *filepath) : std::runtime_error("pgf_systemerror")
    {
        this->m_code     = code;
        this->m_filepath = filepath;
    }

    virtual int code() const
    {
        return m_code;
    }

    const char *filepath() const
    {
        return m_filepath;
    }

private:
    int m_code;
    const char *m_filepath;
};

class PgfPGF;
class PgfConcr;

#include "db.h"
#include "text.h"
#include "vector.h"
#include "namespace.h"
#include "expr.h"

struct PGF_INTERNAL_DECL PgfFlag {
    size_t ref_count;
    PgfLiteral value;
    PgfText name;

    static void release(ref<PgfFlag> pgf);
};

typedef struct {
	PgfExpr body;
	Vector<PgfPatt> patts;
} PgfEquation;

struct PGF_INTERNAL_DECL PgfAbsFun {
    size_t ref_count;

    ref<PgfDTyp> type;
	int arity;
    ref<char> bytecode;
    PgfExprProb ep;
    PgfText name;

    static void release(ref<PgfAbsFun> cat);
};

struct PGF_INTERNAL_DECL PgfAbsCat {
    size_t ref_count;

	ref<Vector<PgfHypo>> context;
	prob_t prob;
    PgfText name;

    static void release(ref<PgfAbsCat> cat);
};

typedef struct {
	ref<PgfText> name;
    Namespace<PgfFlag> aflags;
    Namespace<PgfAbsFun> funs;
    Namespace<PgfAbsCat> cats;
} PgfAbstr;

struct PGF_INTERNAL_DECL PgfConcrLincat {
    size_t ref_count;

    ref<PgfAbsCat> abscat;

    ref<Vector<ref<PgfText>>> fields;
    PgfText name;

    static void release(ref<PgfConcrLincat> lincat);
};

struct PGF_INTERNAL_DECL PgfLParam {
    size_t i0;
    size_t n_terms;
    struct {
        size_t factor;
        size_t var;
    } terms[];
};

struct PGF_INTERNAL_DECL PgfPArg {
    ref<PgfLParam> param;
};

typedef object PgfSymbol;

struct PGF_INTERNAL_DECL PgfSymbolCat {
    static const uint8_t tag = 0;
    size_t d;
    PgfLParam r;
};

struct PGF_INTERNAL_DECL PgfSymbolLit {
    static const uint8_t tag = 1;
    size_t d;
    PgfLParam r;
};

struct PGF_INTERNAL_DECL PgfSymbolVar {
    static const uint8_t tag = 2;
    size_t d, r;
};

struct PGF_INTERNAL_DECL PgfSymbolKS {
    static const uint8_t tag = 3;
    PgfText token;
};

struct PGF_INTERNAL_DECL PgfSymbolKP {
    static const uint8_t tag = 4;
};

struct PGF_INTERNAL_DECL PgfSymbolBIND {
    static const uint8_t tag = 5;
};

struct PGF_INTERNAL_DECL PgfSymbolSOFTBIND {
    static const uint8_t tag = 6;
};

struct PGF_INTERNAL_DECL PgfSymbolNE {
    static const uint8_t tag = 7;
};

struct PGF_INTERNAL_DECL PgfSymbolSOFTSPACE {
    static const uint8_t tag = 8;
};

struct PGF_INTERNAL_DECL PgfSymbolCAPIT {
    static const uint8_t tag = 9;
};

struct PGF_INTERNAL_DECL PgfSymbolALLCAPIT {
    static const uint8_t tag = 10;
};

struct PGF_INTERNAL_DECL PgfConcrLin {
    size_t ref_count;

    ref<PgfAbsFun> absfun;

    ref<Vector<PgfPArg>> args;
    ref<Vector<ref<PgfLParam>>> res;
    ref<Vector<ref<Vector<PgfSymbol>>>> seqs;

    PgfText name;

    static void release(ref<PgfConcrLin> lin);
};

struct PGF_INTERNAL_DECL PgfConcrPrintname {
    size_t ref_count;
    ref<PgfText> printname;
    PgfText name;

    static void release(ref<PgfConcrPrintname> printname);
};

struct PGF_INTERNAL_DECL PgfConcr {
    size_t ref_count;
    size_t ref_count_ex;
    Namespace<PgfFlag> cflags;
    Namespace<PgfConcrLin> lins;
    Namespace<PgfConcrLincat> lincats;
    Namespace<PgfConcrPrintname> printnames;

    // If there are references from the host language to this concrete,
    // then it is included in a double-linked list. If a process
    // dies without releasing the reference, it will be released by
    // the first process who have an exclusive access to the database.
    ref<PgfConcr> prev, next;

    PgfText name;

    static void release(ref<PgfConcr> pgf);
};

struct PGF_INTERNAL_DECL PgfPGF {
    size_t ref_count;

	uint16_t major_version;
	uint16_t minor_version;
	Namespace<PgfFlag> gflags;
	PgfAbstr abstract;
    Namespace<PgfConcr> concretes;

    // If the revision is transient, then it is in a double-linked list
    // with all other transient revisions.
    ref<PgfPGF> prev, next;

    // The name lets the user to find a particular revision in
    // the database.
    PgfText name;

    static void release(ref<PgfPGF> pgf);
};

extern PGF_INTERNAL_DECL size_t master_size;
extern PGF_INTERNAL_DECL char master_text[];

#endif
