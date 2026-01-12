#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <string.h>
#include <assert.h>
#include <exception>
#include <stdexcept>
#include <functional>
#include <queue>
#include <map>
#include <set>

#include "pgf.h"

#ifdef _WIN32
#include <windows.h>
typedef SSIZE_T ssize_t;
#endif

#ifdef __APPLE__
#include <sys/errno.h>
#endif

#ifdef EMSCRIPTEN
#include <errno.h>
#include <stdio.h>
#endif

#ifdef _MSC_VER
#include <malloc.h>
#define alloca _alloca
#define strdup _strdup
#pragma warning(disable : 4996)
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

struct PgfPGF;
struct PgfAbsFun;
struct PgfConcr;

#include "db.h"
#include "text.h"
#include "vector.h"
#include "namespace.h"
#include "probspace.h"
#include "expr.h"
#include "intervalmap.h"

struct PGF_INTERNAL_DECL PgfFlag {
    PgfLiteral value;
    PgfText name;

    static void release(ref<PgfFlag> pgf);
};

struct PGF_INTERNAL_DECL PgfAbsFun {
    ref<PgfDTyp> type;
	int arity;
    ref<char> bytecode;
    prob_t prob;
    PgfText name;

    static void release(ref<PgfAbsFun> cat);
};

struct PGF_INTERNAL_DECL PgfAbsCat {
	vector<PgfHypo> context;
	prob_t prob;
    PgfText name;

    static void release(ref<PgfAbsCat> cat);
};

typedef struct {
	ref<PgfText> name;
    Namespace<PgfFlag> aflags;
    Namespace<PgfAbsFun> funs;
    Namespace<PgfAbsCat> cats;
    PgfProbspace funs_by_cat;
} PgfAbstr;

typedef struct {
    size_t factor;
    size_t var;
} term;

struct PGF_INTERNAL_DECL PgfLParam {
    size_t i0;
    size_t n_terms;
    term terms[];

    static void release(ref<PgfLParam> param);
};

struct PGF_INTERNAL_DECL PgfVariableRange {
    size_t var;
    size_t range;
};

struct PGF_INTERNAL_DECL PgfPArg {
    ref<PgfLParam> param;
};

struct PGF_INTERNAL_DECL PgfPResult {
    vector<PgfVariableRange> vars; 
    PgfLParam param;

    static void release(ref<PgfPResult> res);
};

typedef object PgfSymbol;

struct PGF_INTERNAL_DECL PgfSequenceBackref {
    object container;
    size_t seq_index;
};

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

struct PGF_INTERNAL_DECL PgfAlternative {
	vector<PgfSymbol> form;
	/**< The form of this variant as a list of tokens. */

	vector<ref<PgfText>> prefixes;
	/**< The prefixes of the following symbol that trigger this
	 * form. */
};

struct PGF_INTERNAL_DECL PgfSymbolKP {
    static const uint8_t tag = 4;
    vector<PgfSymbol> default_form;
    inline_vector<PgfAlternative> alts;
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

struct PGF_INTERNAL_DECL PgfConcrRule {
    vector<PgfVariableRange> vars;
    ref<PgfLParam> res;
    object container;
    vector<ref<PgfLParam>> args;
    ref<PgfLParam> lin_idx;
    inline_vector<PgfSymbol> syms;

    static void release(ref<PgfConcrRule> seq);
};

struct PGF_INTERNAL_DECL PgfConcrLincat {
    static const uint8_t tag = 0;

    ref<PgfAbsCat> abscat;

    size_t n_lindefs;
    vector<ref<PgfConcrRule>> rules;
    vector<ref<PgfText>> fields;

    PgfText name;

    static void release(ref<PgfConcrLincat> lincat);
};

struct PGF_INTERNAL_DECL PgfConcrLin {
    static const uint8_t tag = 1;

    ref<PgfAbsFun> absfun;
    ref<PgfConcrLincat> lincat;

    vector<ref<PgfConcrRule>> rules;

    PgfText name;

    static void release(ref<PgfConcrLin> lin);
};

struct PGF_INTERNAL_DECL PgfSymbolACat {
    static const uint8_t tag = 11;
    PgfText name;
};

struct PGF_INTERNAL_DECL PgfSymbolCCat {
    static const uint8_t tag = 12;
    ref<PgfConcrLincat> lincat;
    interval_t value;
    interval_t lin_idx;
    PgfMetaId fid;
};

struct PGF_INTERNAL_DECL PgfConcrPrintname {
    ref<PgfText> printname;
    PgfText name;

    static void release(ref<PgfConcrPrintname> printname);
};

#define containerof(T,field,p) (T*) (((char*) p)-offsetof(T,field))

#include "phrasetable.h"

struct PGF_INTERNAL_DECL PgfConcr {
    Namespace<PgfFlag> cflags;
    Namespace<PgfConcrLin> lins;
    Namespace<PgfConcrLincat> lincats;
    PgfPhrasetable phrasetable;
    Namespace<PgfConcrPrintname> printnames;
    PgfMetaId last_fid;

    PgfText name;

    static void release(ref<PgfConcr> pgf);
};

struct PGF_INTERNAL_DECL PgfPGF {
	uint16_t major_version;
	uint16_t minor_version;
	Namespace<PgfFlag> gflags;
	PgfAbstr abstract;
    Namespace<PgfConcr> concretes;

    static void release(ref<PgfPGF> pgf);
};

#endif
