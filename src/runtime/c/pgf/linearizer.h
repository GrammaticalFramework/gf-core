#ifndef LINEARIZER_H
#define LINEARIZER_H

class PGF_INTERNAL_DECL PgfLinearizationOutput : public PgfLinearizationOutputIface {
    PgfPrinter printer;
    bool bind;
    bool nonexist;

public:
    PgfLinearizationOutput();

    PgfText *get_text();

    virtual void symbol_token(PgfText *tok);
    virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
    virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
    virtual void symbol_ne();
    virtual void symbol_bind();
    virtual void flush();
};

class PGF_INTERNAL_DECL PgfLinearizer : public PgfUnmarshaller {
    // List of free variables in order reverse to the order of binding
    PgfPrinter printer;

    ref<PgfConcr> concr;
    PgfMarshaller *m;

    struct Item {
        ref<PgfConcrRule> rule;

        struct {
            size_t &operator[](int i) {
                Item *item = containerof(Item,vars,this);
                return ((size_t*) (item+1))[i];
            }
            size_t size() {
                Item *item = containerof(Item,vars,this);
                return item->rule->ranges.size();
            }
        } vars;

        void *operator new(size_t sz, ref<PgfConcrRule> rule)
        {
            size_t sz2 = rule->ranges.size()*sizeof(size_t);
            Item *new_item = (Item *) malloc(sz+sz2);
            memset(new_item, 0, sz+sz2);
            return new_item;
        }

        void *operator new(size_t sz, Item *item)
        {
            size_t sz2 = item->vars.size()*sizeof(size_t);
            Item *new_item = (Item *) malloc(sz+sz2);
            memcpy(new_item, item, sz+sz2);
            return new_item;
        }

        void operator delete(void *p)
        {
            free(p);
        }
        
        Item() {
        }

        bool instantiate(ref<PgfLParam> lparam,size_t value);
        size_t eval(ref<PgfLParam> lparam);
    };

    struct TreeNode {
        TreeNode *next;
        TreeNode *next_arg;
        TreeNode *args;

        int fid;

        size_t value;

        size_t n_hoas_vars;
        PgfText **hoas_vars;

        TreeNode(PgfLinearizer *linearizer);
        virtual bool resolve(PgfLinearizer *linearizer) { return true; };
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat)=0;
        virtual void linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r);
        virtual void linearize_var(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r);
        virtual void linearize_item(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, Item *item, vector<PgfSymbol> syms);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)=0;
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer)=0;
        virtual ~TreeNode() { free(hoas_vars); };
    };

    struct TreeLinNode : public TreeNode {
        ref<PgfConcrLin> lin;
        size_t rule_index;
        Item **items;

        TreeLinNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        virtual ~TreeLinNode();
    };

    struct TreeLindefNode : public TreeNode {
        ref<PgfConcrLincat> lincat;
        size_t rule_index;
        Item **items;
        PgfText *fun;
        PgfText *literal;

        TreeLindefNode(PgfLinearizer *linearizer, PgfText *fun, PgfText *lit);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, PgfLParam *r);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        ~TreeLindefNode();
    };

    struct TreeLinrefNode : public TreeNode {
        size_t rule_index;
        Item *item;

        TreeLinrefNode(PgfLinearizer *linearizer, TreeNode *root);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat) {};
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        ~TreeLinrefNode();
    };

    struct TreeLitNode : public TreeNode {
        ref<PgfConcrLincat> lincat;
        PgfText *literal;

        TreeLitNode(PgfLinearizer *linearizer, ref<PgfConcrLincat> lincat, PgfText *lit);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
        ~TreeLitNode() { free(literal); };
    };

    struct TreeChunksNode : public TreeNode {
        TreeChunksNode(PgfLinearizer *linearizer);
        virtual bool resolve(PgfLinearizer *linearizer);
        virtual void check_category(PgfLinearizer *linearizer, PgfText *cat);
        virtual void linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex);
        virtual ref<PgfConcrLincat> get_lincat(PgfLinearizer *linearizer);
    };

    TreeNode *prev;
    TreeNode *next;
    TreeNode *args;

    enum CapitState { CAPIT_NONE, CAPIT_FIRST, CAPIT_ALL };

    CapitState capit;

    struct BracketStack {
        BracketStack *next;
        bool begin;
        int fid;
        PgfText *cat;
        PgfText *field;
        PgfText *fun;

        void flush(PgfLinearizationOutputIface *out);
    };

    struct PreStack {
        PreStack *next;
        TreeNode *node;
        Item *item;
        ref<PgfSymbolKP> sym_kp;
        bool bind;
        CapitState capit;
        BracketStack *bracket_stack;
    };

    PreStack *pre_stack;
    void flush_pre_stack(PgfLinearizationOutputIface *out, PgfText *token);

    PgfText *wild;

public:
    PgfLinearizer(PgfPrintContext *ctxt, ref<PgfConcr> concr, PgfMarshaller *m);

    bool resolve();
    void reverse_and_label(bool add_linref);
    void linearize(PgfLinearizationOutputIface *out, size_t lindex) {
        prev->linearize(out, this, lindex);
        flush_pre_stack(out, NULL);
    }
    ref<PgfConcrLincat> get_lincat() {
        return prev->get_lincat(this);
    }

    ~PgfLinearizer();

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

#endif
