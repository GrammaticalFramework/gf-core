#ifndef PGF_DATA_H_
#define PGF_DATA_H_

#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <iostream>
#include <exception>
#include <stdexcept>

#include "pgf.h"
#include "db.h"
#include "text.h"
#include "namespace.h"
#include "expr.h"

class PGF_INTERNAL_DECL pgf_error : public std::runtime_error {
public:
    pgf_error(const char *msg) : std::runtime_error(msg)
    {
        this->msg = msg;
    }

    const char *what() const throw ()
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

typedef struct {
	ref<char> name;
    Namespace<PgfFlag> aflags;
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

struct PgfPGF : public PgfPGFRoot {
    DB db;

    PGF_INTERNAL_DECL PgfPGF(const char* fpath) : db(fpath) {};
    PGF_INTERNAL_DECL ~PgfPGF() {};

    PGF_INTERNAL_DECL void set_root();
};

#pragma GCC diagnostic pop

#endif
