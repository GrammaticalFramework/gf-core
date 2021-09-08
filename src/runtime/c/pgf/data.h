#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <string.h>
#include <assert.h>
#include <iostream>
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
    PgfLiteral value;
    PgfText name;
};

// PgfPatt

typedef object PgfPatt;

struct PGF_INTERNAL_DECL PgfPattApp {
    static const uint8_t tag = 0;

	ref<PgfText> ctor;
    PgfVector<PgfPatt> args;
};

struct PGF_INTERNAL_DECL PgfPattVar {
    static const uint8_t tag = 1;

	PgfText name;
};

struct PGF_INTERNAL_DECL PgfPattAs {
    static const uint8_t tag = 2;

	PgfPatt patt;
	PgfText name;
};

struct PGF_INTERNAL_DECL PgfPattWild {
    static const uint8_t tag = 3;
};

struct PGF_INTERNAL_DECL PgfPattLit {
    static const uint8_t tag = 4;

	PgfLiteral lit;
};

struct PGF_INTERNAL_DECL PgfPattImplArg {
    static const uint8_t tag = 5;

	PgfPatt patt;
};

struct PGF_INTERNAL_DECL PgfPattTilde {
    static const uint8_t tag = 6;

	PgfExpr expr;
};

typedef struct {
	PgfExpr body;
	PgfVector<PgfPatt> patts;
} PgfEquation;

struct PGF_INTERNAL_DECL PgfAbsFun {
    ref<PgfDTyp> type;
	int arity;
    ref<PgfVector<ref<PgfEquation>>> defns;
    PgfExprProb ep;
    PgfText name;
};

typedef struct {
	ref<PgfVector<PgfHypo>> context;
	prob_t prob;
    PgfText name;
} PgfAbsCat;

typedef struct {
	ref<PgfText> name;
    Namespace<PgfFlag> aflags;
    Namespace<PgfAbsFun> funs;
    Namespace<PgfAbsCat> cats;
} PgfAbstr;

struct PGF_INTERNAL_DECL PgfPGF {
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
};

extern PGF_INTERNAL_DECL
PgfText master;

#endif
