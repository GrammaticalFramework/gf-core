#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <string.h>
#include <assert.h>
#include <exception>
#include <stdexcept>

#include "pgf.h"

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
	PgfVector<PgfPatt> patts;
} PgfEquation;

struct PGF_INTERNAL_DECL PgfAbsFun {
    size_t ref_count;

    ref<PgfDTyp> type;
	int arity;
    ref<PgfVector<ref<PgfEquation>>> defns;
    PgfExprProb ep;
    PgfText name;

    static void release(ref<PgfAbsFun> cat);
};

struct PGF_INTERNAL_DECL PgfAbsCat {
    size_t ref_count;

	ref<PgfVector<PgfHypo>> context;
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

struct PGF_INTERNAL_DECL PgfPGF {
    size_t ref_count;

	uint16_t major_version;
	uint16_t minor_version;
	Namespace<PgfFlag> gflags;
	PgfAbstr abstract;
	//PgfConcrs* concretes;

    // If the revision is transient, then it is in a double-linked list
    // with all other transient revisions.
    ref<PgfPGF> prev, next;

    // The name lets the user to find a particular revision in
    // the database.
    PgfText name;

    static void release(ref<PgfPGF> pgf);
};

extern PGF_INTERNAL_DECL
PgfText master;

#endif
