#ifndef TYPECHECKER_H
#define TYPECHECKER_H

class PGF_INTERNAL_DECL PgfTypechecker {
    ref<PgfPGF> gr;
    PgfMarshaller *m;
    PgfUnmarshaller *u;
    PgfExn* err;

    class Unmarshaller1;
    class Unmarshaller2;

    struct Pi;
    struct Cat;

    struct Type {
        virtual Pi  *is_pi()  { return NULL; }
        virtual Cat *is_cat() { return NULL; }
        virtual ~Type() {}
    };

    struct Pi : Type {
        PgfBindType bind_type;
        Type *arg, *res;
        PgfText var;

        void *operator new(size_t size, PgfBindType bind_type, PgfText *var, Type *arg, Type *res) {
            Pi *pi = (Pi *) malloc(size+var->size+1);
            pi->bind_type = bind_type;
            memcpy(&pi->var, var, sizeof(PgfText)+var->size+1);
            pi->arg = arg;
            pi->res = res;
            return pi;
        }
        virtual Pi *is_pi()  { return this; }

        virtual ~Pi() {
            delete arg;
            delete res;
        }
    };

    struct Cat : Type {
        PgfText name;

        void *operator new(size_t size, PgfText *name) {
            Cat *cat = (Cat *) malloc(size+name->size+1);
            memcpy(&cat->name, name, sizeof(PgfText)+name->size+1);
            return cat;
        }
        void *operator new(size_t size, const char *name) {
            size_t len = strlen(name);
            Cat *cat = (Cat *) malloc(size+len+1);
            cat->name.size = len;
            memcpy(&cat->name.text, name, len+1);
            return cat;
        }
        virtual Cat *is_cat() { return this; }
    };

    PgfType marshall_type(Type *ty, PgfUnmarshaller *u);
    std::vector<Type*> temps;

    bool unifyTypes(Type *ty1, Type *ty2);

    struct Scope {
        Scope *tail;
        PgfText *var;
        Type *ty;
    };

    struct Context : public PgfUnmarshaller {
        PgfTypechecker *tc;

        Scope *scope;
        PgfBindType bind_type;
        Type *exp_type;
        Type *inf_type;

        bool checkImplArgument();
        bool unifyTypes(PgfExpr *e);

    public:
        Context(PgfTypechecker *tc, Scope *scope, Type *exp_type = NULL, PgfBindType bind_type = PGF_BIND_TYPE_EXPLICIT);

        virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body);
        virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg);
        virtual PgfExpr elit(PgfLiteral lit);
        virtual PgfExpr emeta(PgfMetaId meta);
        virtual PgfExpr efun(PgfText *name);
        virtual PgfExpr evar(int index);
        virtual PgfExpr etyped(PgfExpr expr, PgfType typ);
        virtual PgfExpr eimplarg(PgfExpr expr);
        virtual PgfLiteral lint(size_t size, uintmax_t *v);
        virtual PgfLiteral lflt(double v);
        virtual PgfLiteral lstr(PgfText *v);
        virtual PgfType dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                             PgfText *cat,
                             size_t n_exprs, PgfExpr *exprs);
        virtual void free_ref(object x);
    };

    PgfDBMarshaller db_m;
    bool type_error(const char *fmt, ...)
#ifdef __GNUC__
    __attribute__ ((format (printf, 2, 3)));
#endif

public:
    PgfTypechecker(ref<PgfPGF> gr, PgfMarshaller *m, PgfUnmarshaller *u, PgfExn* err);
    ~PgfTypechecker();
    
    PgfExpr check_expr(PgfExpr expr, PgfType type);
    PgfType infer_expr(PgfExpr *pe);
    PgfType check_type(PgfType type);
};

#endif
