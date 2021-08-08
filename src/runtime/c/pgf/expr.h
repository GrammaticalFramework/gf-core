#ifndef EXPR_H_
#define EXPR_H_

/// An abstract syntax tree
typedef variant PgfExpr;

struct PgfHypo;
struct PgfType;

typedef int PgfMetaId;

typedef enum {
	PGF_BIND_TYPE_EXPLICIT,
	PGF_BIND_TYPE_IMPLICIT
} PgfBindType;

/// A literal for an abstract syntax tree
typedef variant PgfLiteral;

struct PgfLiteralStr {
    static const uint8_t tag = 0;

	PgfText val;
} ;

struct PgfLiteralInt {
    static const uint8_t tag = 1;

	int val;
} ;

struct PgfLiteralFlt {
    static const uint8_t tag = 2;

	double val;
};

struct PgfHypo {
	PgfBindType bind_type;
	ref<PgfText> cid;
	ref<PgfType> type;
};

struct PgfType {
	ref<PgfVector<PgfHypo>> hypos;
    ref<PgfVector<PgfExpr>> exprs;
	PgfText name;
};

struct PgfExprAbs {
    static const uint8_t tag = 0;
    
	PgfBindType bind_type;
	PgfExpr body;
	PgfText name;
};

struct PgfExprApp {
    static const uint8_t tag = 1;

	PgfExpr fun;
	PgfExpr arg;
};

struct PgfExprLit {
    static const uint8_t tag = 2;

	PgfLiteral lit;
};

struct PgfExprMeta {
    static const uint8_t tag = 3;

	PgfMetaId id;
};

struct PgfExprFun {
    static const uint8_t tag = 4;

	PgfText name;
};

struct PgfExprVar {
    static const uint8_t tag = 5;
    
	int var;
};

struct PgfExprTyped {
    static const uint8_t tag = 6;

	PgfExpr expr;
	ref<PgfType> type;
};

struct PgfExprImplArg {
    static const uint8_t tag = 7;

	PgfExpr expr;
};

typedef float prob_t;

typedef struct {
	prob_t prob;
	PgfExpr expr;
} PgfExprProb;

#endif /* EXPR_H_ */
