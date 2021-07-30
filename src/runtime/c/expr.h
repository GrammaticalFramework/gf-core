#ifndef EXPR_H_
#define EXPR_H_

#include "variant.h"

// PgfLiteral

typedef variant PgfLiteral;


typedef enum {
	PGF_LITERAL_STR,
	PGF_LITERAL_INT,
	PGF_LITERAL_FLT,
	PGF_LITERAL_NUM_TAGS
} PgfLiteralTag;

typedef struct {
	char val[0];  // a flexible array that contains the value
} PgfLiteralStr;

typedef struct {
	int val;
} PgfLiteralInt;

typedef struct {
	double val;
} PgfLiteralFlt;

#endif /* EXPR_H_ */
