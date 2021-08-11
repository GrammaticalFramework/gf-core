#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <string.h>
#include <assert.h>
#include <iostream>
#include <exception>
#include <stdexcept>

#include "pgf.h"
#include "db.h"
#include "text.h"
#include "vector.h"
#include "namespace.h"
#include "expr.h"

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

struct PGF_INTERNAL_DECL PgfFlag {
    PgfLiteral value;
    PgfText name;
};

// PgfPatt

typedef variant PgfPatt;

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
    ref<PgfType> type;
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

struct PGF_INTERNAL_DECL PgfPGFRoot {
	uint16_t major_version;
	uint16_t minor_version;
	Namespace<PgfFlag> gflags;
	PgfAbstr abstract;
	//PgfConcrs* concretes;
};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wattributes"

struct PgfPGF : DB {
    PGF_INTERNAL_DECL PgfPGF(const char* fpath, int flags, int mode,
                             PgfUnmarshaller *unmarshaller)
                         : DB(fpath, flags, mode)
    { u = unmarshaller; };

    PGF_INTERNAL_DECL ~PgfPGF()
    { u->free_me(u); };

    PgfUnmarshaller *u;
};

#pragma GCC diagnostic pop

#endif
