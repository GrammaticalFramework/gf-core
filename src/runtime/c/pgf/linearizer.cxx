#include "data.h"
#include "printer.h"
#include "linearizer.h"

bool PgfLinearizer::Item::instantiate(ref<PgfLParam> lparam,size_t value)
{
    if (value < lparam->i0)
        return false;
    value -= lparam->i0;

    for (size_t j = 0; j < lparam->n_terms; j++) {
        term t = lparam->terms[j];
        if (vars[t.var] > 0) {
            if (value < vars[t.var]-1)
                return false;
            value -= vars[t.var]-1;
        }
    }

    for (size_t j = 0; j < lparam->n_terms; j++) {
        term t = lparam->terms[j];
        if (vars[t.var] == 0) {
            size_t v_val = value / t.factor;
            if (v_val >= rule->ranges[t.var])
                return false;
            vars[t.var] = v_val + 1;
            value %= t.factor;
        }
    }

    return (value == 0);
}

size_t PgfLinearizer::Item::eval(ref<PgfLParam> lparam)
{
    size_t value = lparam->i0;
    for (size_t i = 0; i < lparam->n_terms; i++) {
        value += lparam->terms[i].factor * (vars[lparam->terms[i].var]-1);
    }
    return value;
}

PgfLinearizer::TreeNode::TreeNode(PgfLinearizer *linearizer)
{
    this->next     = linearizer->prev;
    this->next_arg = NULL;
    this->args     = linearizer->args;

    this->fid       = 0;

    this->value     = 0;

    this->n_hoas_vars = 0;
    this->hoas_vars   = NULL;

    linearizer->prev = this;
}

bool PgfLinearizer::TreeNode::linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r)
{
    TreeNode *arg = args;
    while (d > 0) {
        arg = arg->next_arg;
        if (arg == NULL)
            break;
        d--;
    }
    if (arg == NULL)
        throw pgf_error("Missing argument");
    return arg->linearize(out, linearizer, r);
}

void PgfLinearizer::TreeNode::linearize_var(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r)
{
    TreeNode *arg = args;
    while (d > 0) {
        arg = arg->next_arg;
        if (arg == 0)
            break;
        d--;
    }
    if (arg == 0)
        throw pgf_error("Missing argument");
    if (r >= arg->n_hoas_vars)
        throw pgf_error("Missing lambda variable");
    linearizer->printer.efun(arg->hoas_vars[r]);
    out->symbol_token(linearizer->printer.get_text());
}

bool PgfLinearizer::TreeNode::linearize_item(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, Item *item, vector<PgfSymbol> syms)
{
    for (size_t i = 0; i < syms.size(); i++) {
        PgfSymbol sym = syms[i];

        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolCat::tag: {
            auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
            size_t r = item->eval(ref<PgfLParam>::from_ptr(&sym_cat->r));
            if (!linearize_arg(out, linearizer, sym_cat->d, r))
                return false;
            break;
        }
        case PgfSymbolLit::tag: {
            auto sym_lit = ref<PgfSymbolLit>::untagged(sym);
            size_t r = item->eval(ref<PgfLParam>::from_ptr(&sym_lit->r));
            if (!linearize_arg(out, linearizer, sym_lit->d, r))
                return false;
            break;
        }
        case PgfSymbolVar::tag: {
            auto sym_var = ref<PgfSymbolVar>::untagged(sym);
            linearize_var(out, linearizer, sym_var->d, sym_var->r);
            break;
        }
        case PgfSymbolKS::tag: {
            auto sym_ks = ref<PgfSymbolKS>::untagged(sym);

            linearizer->flush_pre_stack(out, &sym_ks->token);

            switch (linearizer->capit) {
            case CAPIT_NONE:
                out->symbol_token(&sym_ks->token);
                break;
            case CAPIT_FIRST: {
                PgfText *cap = (PgfText *) alloca(sizeof(PgfText)+sym_ks->token.size+6);

                const uint8_t *p   = (const uint8_t *) sym_ks->token.text;
                const uint8_t *end = p + sym_ks->token.size;

                uint8_t *q = (uint8_t *) cap->text;

                uint32_t ucs = pgf_utf8_decode(&p);
                ucs = pgf_utf8_to_upper(ucs);
                pgf_utf8_encode(ucs,&q);

                memcpy(q, p, (end - p)+1);
                q += (end - p);

                cap->size = q - (uint8_t *) cap->text;
                out->symbol_token(cap);

                linearizer->capit = CAPIT_NONE;
                break;
            }
            case CAPIT_ALL: {
                PgfText *cap = (PgfText *) alloca(sizeof(PgfText)+sym_ks->token.size*6);

                const uint8_t *p   = (const uint8_t *) sym_ks->token.text;
                const uint8_t *end = p + sym_ks->token.size;

                uint8_t *q = (uint8_t *) cap->text;

                while (p != end) {
                    uint32_t ucs = pgf_utf8_decode(&p);
                    ucs = pgf_utf8_to_upper(ucs);
                    pgf_utf8_encode(ucs,&q);
                }

                cap->size = q - (uint8_t *) cap->text;
                *q = 0;

                out->symbol_token(cap);

                linearizer->capit = CAPIT_NONE;
                break;
            }
            }
            break;
        }
        case PgfSymbolKP::tag: {
            auto sym_kp = ref<PgfSymbolKP>::untagged(sym);
            PreStack *pre = new PreStack();
            pre->next   = linearizer->pre_stack;
            pre->node   = this;
            pre->item   = item;
            pre->sym_kp = sym_kp;
            pre->bind   = false;
            pre->capit  = CAPIT_NONE;
            pre->bracket_stack = NULL;
            linearizer->pre_stack = pre;
            break;
        }
        case PgfSymbolBIND::tag:
        case PgfSymbolSOFTBIND::tag:
            if (linearizer->pre_stack == NULL)
                out->symbol_bind();
            else
                linearizer->pre_stack->bind = true;
            break;
        case PgfSymbolNE::tag:
            out->symbol_ne();
            break;
        case PgfSymbolSOFTSPACE::tag:
            // Nothing to do
            break;
        case PgfSymbolCAPIT::tag:
            if (linearizer->pre_stack == NULL)
                linearizer->capit = CAPIT_FIRST;
            else
                linearizer->pre_stack->capit = CAPIT_FIRST;
            break;
        case PgfSymbolALLCAPIT::tag:
            if (linearizer->pre_stack == NULL)
                linearizer->capit = CAPIT_ALL;
            else
                linearizer->pre_stack->capit = CAPIT_ALL;
            break;
        }
    }

    return true;
}

PgfLinearizer::TreeLinNode::TreeLinNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin)
  : TreeNode(linearizer)
{
    this->lin        = lin;
    this->rule_index = 0;
    this->items = new Item*[lin->lincat->fields.size()]();
}

bool PgfLinearizer::TreeLinNode::resolve(PgfLinearizer *linearizer)
{
    while (rule_index < lin->rules.size()) {
        Item *item = new (lin->rules[rule_index]) Item();
        item->rule = lin->rules[rule_index];

        int i = 0;
        TreeNode *arg = args;
        while (arg != NULL) {
            if (!item->instantiate(item->rule->args[i], arg->value))
                goto next;

            arg = arg->next_arg;  i++;
        }

        {
            size_t max_value = 1;
            for (size_t i = 0; i < item->vars.size(); i++) {
                if (item->vars[i] == 0)
                    max_value *= item->rule->ranges[i];
            }

            for (size_t value = 0; value < max_value; value++) {
                Item *new_item = new (item) Item;

                size_t v = value;
                for (size_t i = 0; i < new_item->vars.size(); i++) {
                    if (new_item->vars[i] == 0) {
                        size_t range = new_item->rule->ranges[i];
                        new_item->vars[i] = (v % range)+1;
                        v = v / range;
                    }
                }

                size_t lin_idx = new_item->eval(new_item->rule->lin_idx);
                items[lin_idx] = new_item;

                this->value = new_item->eval(new_item->rule->res);
            }
        }
    next:
        delete item;

        rule_index++;
    }

    return true;
}

bool PgfLinearizer::TreeLinNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    return (textcmp(&lin->absfun->type->name, cat) == 0);
}

bool PgfLinearizer::TreeLinNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    if (items[lindex] == NULL)
        return false;

    PgfText *cat = &lin->absfun->type->name;
    PgfText *field = &*lin->lincat->fields[lindex];

    if (linearizer->pre_stack == NULL)
        out->begin_phrase(cat, fid, field, &lin->name);
    else {
        BracketStack *bracket = new BracketStack();
        bracket->next  = linearizer->pre_stack->bracket_stack;
        bracket->begin = true;
        bracket->fid   = fid;
        bracket->cat   = cat;
        bracket->field = field;
        bracket->fun   = &lin->name;
        linearizer->pre_stack->bracket_stack = bracket;
    }

    if (!linearize_item(out, linearizer,
                        items[lindex],items[lindex]->rule->syms.as_vector()))
        return false;

    if (linearizer->pre_stack == NULL)
        out->end_phrase(cat, fid, field, &lin->name);
    else {
        BracketStack *bracket = new BracketStack();
        bracket->next  = linearizer->pre_stack->bracket_stack;
        bracket->begin = false;
        bracket->fid   = fid;
        bracket->cat   = cat;
        bracket->field = field;
        bracket->fun   = &lin->name;
        linearizer->pre_stack->bracket_stack = bracket;
    }

    return true;
}

ref<PgfConcrLincat> PgfLinearizer::TreeLinNode::get_lincat(PgfLinearizer *linearizer)
{
    return namespace_lookup(linearizer->concr->lincats, &lin->absfun->type->name);
}

PgfLinearizer::TreeLinNode::~TreeLinNode()
{
    size_t n_fields = lin->lincat->fields.size();
    for (size_t i = 0; i < n_fields; i++) {
        if (items[i] != NULL)
            delete items[i];
    }
    delete[] items;
};

PgfLinearizer::TreeLindefNode::TreeLindefNode(PgfLinearizer *linearizer, PgfText *fun, PgfText *literal)
  : TreeNode(linearizer)
{
    this->lincat    = 0;
    this->rule_index= 0;
    this->items     = NULL;
    this->fun       = fun;
    this->literal   = literal;

    TreeNode *prev = linearizer->prev;

    TreeNode *arg = args;
    TreeNode **plast = &args;
    while (arg != NULL) {
        TreeNode *next = arg->next_arg;
        arg->next_arg = NULL;

        TreeLinrefNode *new_arg = new TreeLinrefNode(linearizer, arg);
        new_arg->next = arg->next;
        arg->next = new_arg;
        *plast = new_arg;
        plast = &new_arg->next_arg;

        linearizer->prev = prev;

        arg = next;
    }
}

bool PgfLinearizer::TreeLindefNode::resolve(PgfLinearizer *linearizer)
{
    if (lincat == 0)
        return true;

    while (rule_index < lincat->n_lindefs) {
        ref<PgfConcrRule> rule = lincat->rules[rule_index];
        Item *item = new (rule) Item();
        item->rule = rule;

        size_t max_value = 1;
        for (size_t i = 0; i < item->vars.size(); i++) {
            if (item->vars[i] == 0)
                max_value *= item->rule->ranges[i];
        }

        for (size_t value = 0; value < max_value; value++) {
            Item *new_item = new (item) Item;

            size_t v = value;
            for (size_t i = 0; i < new_item->vars.size(); i++) {
                if (new_item->vars[i] == 0) {
                    size_t range = new_item->rule->ranges[i];
                    new_item->vars[i] = (v % range)+1;
                    v = v / range;
                }
            }

            size_t lin_idx = new_item->eval(new_item->rule->lin_idx);
            items[lin_idx] = new_item;

            this->value = new_item->eval(new_item->rule->res);
        }
        delete item;

        rule_index++;
    }

    return true;
}

bool PgfLinearizer::TreeLindefNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    lincat = namespace_lookup(linearizer->concr->lincats, cat);
    if (lincat != 0)
        this->items = new Item*[lincat->fields.size()]();
    return true;
}

bool PgfLinearizer::TreeLindefNode::linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, size_t r)
{
    linearizer->flush_pre_stack(out, literal);
    out->symbol_token(literal);

    TreeNode *arg = args;
    while (arg != NULL) {
        if (!arg->linearize(out,linearizer,0))
            return false;
        arg = arg->next_arg;
    }
    return true;
}

bool PgfLinearizer::TreeLindefNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    if (lincat==0) {
        return linearize_arg(out, linearizer, 0, 0);
    }

    PgfText *cat = &lincat->name;
    PgfText *field = &*lincat->fields[lindex];

    if (linearizer->pre_stack == NULL)
        out->begin_phrase(cat, fid, field, linearizer->wild);
    else {
        BracketStack *bracket = new BracketStack();
        bracket->next  = linearizer->pre_stack->bracket_stack;
        bracket->begin = true;
        bracket->fid   = fid;
        bracket->cat   = cat;
        bracket->field = field;
        bracket->fun   = linearizer->wild;
        linearizer->pre_stack->bracket_stack = bracket;
    }

    if (!linearize_item(out, linearizer,
                        items[lindex],items[lindex]->rule->syms.as_vector()))
        return false;

    if (linearizer->pre_stack == NULL)
        out->end_phrase(cat, fid, field, linearizer->wild);
    else {
        BracketStack *bracket = new BracketStack();
        bracket->next  = linearizer->pre_stack->bracket_stack;
        bracket->begin = false;
        bracket->fid   = fid;
        bracket->cat   = cat;
        bracket->field = field;
        bracket->fun   = linearizer->wild;
        linearizer->pre_stack->bracket_stack = bracket;
    }
    return true;
}

ref<PgfConcrLincat> PgfLinearizer::TreeLindefNode::get_lincat(PgfLinearizer *linearizer)
{
    return lincat;
}

PgfLinearizer::TreeLindefNode::~TreeLindefNode()
{
    if (lincat && items != NULL) {
        size_t n_fields = lincat->fields.size();
        for (size_t i = 0; i < n_fields; i++) {
            if (items[i] != NULL)
                delete items[i];
        }
        delete[] items;
    }

    free(fun);
    free(literal);
};

PgfLinearizer::TreeLinrefNode::TreeLinrefNode(PgfLinearizer *linearizer, TreeNode *root)
  : TreeNode(linearizer)
{
    args = root;
    rule_index=0;
    item = NULL;
}

bool PgfLinearizer::TreeLinrefNode::resolve(PgfLinearizer *linearizer)
{
    TreeNode *root = args;
    ref<PgfConcrLincat> lincat = root->get_lincat(linearizer);
    if (lincat == 0)
        return (rule_index = !rule_index);

    while (rule_index < lincat->rules.size()) {
        Item *item = new (lincat->rules[lincat->n_lindefs+rule_index]) Item();
        item->rule = lincat->rules[lincat->n_lindefs+rule_index];

        if (!item->instantiate(item->rule->args[0], root->value)) {
            rule_index++;
            continue;
        }

        size_t max_value = 1;
        for (size_t i = 0; i < item->vars.size(); i++) {
            if (item->vars[i] == 0)
                max_value *= item->rule->ranges[i];
        }

        for (size_t value = 0; value < max_value; value++) {
            size_t v = value;
            for (size_t i = 0; i < item->vars.size(); i++) {
                if (item->vars[i] == 0) {
                    size_t range = item->rule->ranges[i];
                    item->vars[i] = v % range;
                    v = v / range;
                }
            }

            this->item = new (item) Item;
            this->value = item->eval(this->item->rule->res);
        }
        delete item;

        break;
    }

    if (item == NULL) {
        rule_index = 0;
        return false;
    }

    return true;
}

bool PgfLinearizer::TreeLinrefNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    ref<PgfConcrLincat> lincat = args->get_lincat(linearizer);
    if (lincat != 0) {
        return linearize_item(out, linearizer, item, item->rule->syms.as_vector());
    } else {
        return args->linearize(out, linearizer, lindex);
    }
}

ref<PgfConcrLincat> PgfLinearizer::TreeLinrefNode::get_lincat(PgfLinearizer *linearizer)
{
    return 0;
}

PgfLinearizer::TreeLinrefNode::~TreeLinrefNode()
{
    delete item;
}

PgfLinearizer::TreeLitNode::TreeLitNode(PgfLinearizer *linearizer, ref<PgfConcrLincat> lincat, PgfText *lit)
  : TreeNode(linearizer)
{
    this->lincat  = lincat;
    this->literal = lit;
}

bool PgfLinearizer::TreeLitNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    return (textcmp(&lincat->name, cat) == 0);
}

bool PgfLinearizer::TreeLitNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    PgfText *field = NULL;
    if (lincat != 0) {
        field = &*lincat->fields[lindex];
    }

    linearizer->flush_pre_stack(out, literal);

    if (lincat != 0)
        out->begin_phrase(&lincat->name, fid, field, linearizer->wild);
    out->symbol_token(literal);
    if (lincat != 0)
        out->end_phrase(&lincat->name, fid, field, linearizer->wild);

    return true;
}

ref<PgfConcrLincat> PgfLinearizer::TreeLitNode::get_lincat(PgfLinearizer *linearizer)
{
    return lincat;
}

PgfLinearizer::PgfLinearizer(PgfPrintContext *ctxt, ref<PgfConcr> concr, PgfMarshaller *m)
  : printer(ctxt,0,m)
{
    this->concr = concr;
    this->m = m;
    this->prev  = NULL;
    this->next  = NULL;
    this->args  = NULL;
    this->capit = CAPIT_NONE;
    this->pre_stack = NULL;
    this->type_error = false;
    this->wild = (PgfText*) malloc(sizeof(PgfText)+2);
    this->wild->size = 1;
    this->wild->text[0] = '_';
    this->wild->text[1] = 0;
};

PgfLinearizer::~PgfLinearizer()
{
    while (prev != NULL) {
        TreeNode *prev_next = prev->next;
        delete prev;
        prev = prev_next;
    }

    while (next != NULL) {
        TreeNode *next_next = next->next;
        delete next;
        next = next_next;
    }

    while (pre_stack != NULL) {
        PreStack *next = pre_stack->next;

        while (pre_stack->bracket_stack != NULL) {
            BracketStack *next = pre_stack->bracket_stack->next;
            delete pre_stack->bracket_stack;

            pre_stack->bracket_stack = next;
        }

        delete pre_stack;
        pre_stack = next;
    }

    free(this->wild);
}

bool PgfLinearizer::resolve()
{
    if (type_error) {
        throw pgf_error("An attempt to linearize an expression which is not type correct");
    }

    for (;;) {
        if (!prev || prev->resolve(this)) {
            if (next == NULL)
                return true;
            TreeNode *next_next = next->next;
            next->next = prev;
            prev = next;
            next = next_next;
        } else {
            TreeNode *prev_next = prev->next;
            prev->next = next;
            next = prev;
            prev = prev_next;
            if (prev == NULL)
                return false;
        }
    }
}

void PgfLinearizer::reverse_and_label(bool add_linref)
{
    if (add_linref)
        new TreeLinrefNode(this, prev);

    // Reverse the list of nodes and label them with fid;
    int fid = 0;
    while (prev != NULL) {
        TreeNode *tmp = prev->next;

        prev->fid  = fid++;
        prev->next = next;

        next = prev;
        prev = tmp;
    }
}

PGF_INTERNAL_DECL bool
pgf_is_case_sensitive(ref<PgfConcr> concr);

void PgfLinearizer::flush_pre_stack(PgfLinearizationOutputIface *out, PgfText *token)
{
    bool (*cmp)(PgfText *t, PgfText *prefix) =
        pgf_is_case_sensitive(concr) ? textstarts : textistarts;

    while (pre_stack != NULL) {
        PreStack *pre = pre_stack;
        pre_stack = pre->next;

        if (token != NULL) {
            for (size_t i = 0; i < pre->sym_kp->alts.size(); i++) {
                ref<PgfAlternative> alt = pre->sym_kp->alts.elem(i);
                for (ref<PgfText> prefix : alt->prefixes) {
                    if (cmp(token, &(*prefix))) {
                        pre->node->linearize_item(out, this, pre->item, alt->form);
                        goto done;
                    }
                }
            }
        }

        pre->node->linearize_item(out, this, pre->item, pre->sym_kp->default_form);

    done:
        if (pre->bracket_stack != NULL)
            pre->bracket_stack->flush(out);

        if (pre->bind)
            out->symbol_bind();

        capit    = pre->capit;

        delete pre;
    }
}

void PgfLinearizer::BracketStack::flush(PgfLinearizationOutputIface *out)
{
    if (next != NULL)
        next->flush(out);

    if (begin)
        out->begin_phrase(cat, fid, field, fun);
    else
        out->end_phrase(cat, fid, field, fun);
}

PgfExpr PgfLinearizer::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    printer.push_variable(name);

    TreeNode *node = (TreeNode *) m->match_expr(this, body);

    PgfText** hoas_vars = (PgfText**) malloc((node->n_hoas_vars+1)*sizeof(PgfText*));
    hoas_vars[0] = textdup(name);
    memcpy(hoas_vars+1, node->hoas_vars, node->n_hoas_vars*sizeof(PgfText*));
    free(node->hoas_vars);
    node->n_hoas_vars++;
    node->hoas_vars = hoas_vars;

    printer.pop_variable();
    return (PgfExpr) node;
}

PgfExpr PgfLinearizer::eapp(PgfExpr fun, PgfExpr arg)
{
    TreeNode *args = this->args;
    this->args = NULL;
    TreeNode *node = (TreeNode*) m->match_expr(this, arg);
    node->next_arg = args;
    this->args = node;

    return m->match_expr(this, fun);
}

PgfExpr PgfLinearizer::elit(PgfLiteral lit)
{
    return m->match_lit(this, lit);
}

PgfExpr PgfLinearizer::emeta(PgfMetaId meta)
{
    printer.emeta(meta);
    return (PgfExpr) new TreeLindefNode(this, textdup(wild),
                                        printer.get_text());
}

PgfExpr PgfLinearizer::efun(PgfText *name)
{
    ref<PgfConcrLin> lin = namespace_lookup(concr->lins, name);
    if (lin != 0) {
        TreeNode *node = args;
        size_t i = 0;
        vector<PgfHypo> hypos = lin->absfun->type->hypos;
        while (node != NULL) {
            if (!node->check_category(this, &hypos[i].type->name)) {
                type_error = true;
            }
            node = node->next_arg; i++;
        }

        return (PgfExpr) new TreeLinNode(this, lin);
    } else {
        printer.puts("[");
        printer.efun(name);
        printer.puts("]");
        return (PgfExpr) new TreeLindefNode(this, textdup(name), printer.get_text());
    }
}

PgfExpr PgfLinearizer::evar(int index)
{
    printer.evar(index);
    PgfText *name = printer.get_text();
    return (PgfExpr) new TreeLindefNode(this, textdup(name), name);
}

PgfExpr PgfLinearizer::etyped(PgfExpr expr, PgfType ty)
{
    return m->match_expr(this, expr);
}

PgfExpr PgfLinearizer::eimplarg(PgfExpr expr)
{
    return m->match_expr(this, expr);
}

PgfLiteral PgfLinearizer::lint(size_t size, uintmax_t *v)
{
    PgfText *cat = (PgfText *) alloca(sizeof(PgfText)+4);
    cat->size = 3;
    strcpy(cat->text, "Int");
    ref<PgfConcrLincat> lincat = namespace_lookup(concr->lincats, cat);

    printer.lint(size,v);

    return (PgfExpr) new TreeLitNode(this, lincat, printer.get_text());
}

PgfLiteral PgfLinearizer::lflt(double v)
{    
    PgfText *cat = (PgfText *) alloca(sizeof(PgfText)+6);
    cat->size = 5;
    strcpy(cat->text, "Float");
    ref<PgfConcrLincat> lincat = namespace_lookup(concr->lincats, cat);

    printer.lflt(v);

    return (PgfExpr) new TreeLitNode(this, lincat, printer.get_text());
}

PgfLiteral PgfLinearizer::lstr(PgfText *v)
{
    PgfText *cat = (PgfText *) alloca(sizeof(PgfText)+7);
    cat->size = 6;
    strcpy(cat->text, "String");
    ref<PgfConcrLincat> lincat = namespace_lookup(concr->lincats, cat);

    return (PgfExpr) new TreeLitNode(this, lincat, textdup(v));
}

PgfType PgfLinearizer::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                             PgfText *cat,
                             size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

void PgfLinearizer::free_ref(object x)
{
}

PgfLinearizationOutput::PgfLinearizationOutput() : printer(NULL,0,NULL)
{
    bind = true;
    nonexist = false;
}

PgfText *PgfLinearizationOutput::get_text()
{
    if (nonexist) {
        free(printer.get_text());
        nonexist = false;
        return NULL;
    }
    bind = true;
    return printer.get_text();
}

void PgfLinearizationOutput::symbol_token(PgfText *tok)
{
    if (!bind) {
        printer.puts(" ");
    }
    bind = false;

    printer.puts(tok);
}

void PgfLinearizationOutput::begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
}

void PgfLinearizationOutput::end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
}

void PgfLinearizationOutput::symbol_ne()
{
    nonexist = true;
}

void PgfLinearizationOutput::symbol_bind()
{
    bind = true;
}

void PgfLinearizationOutput::flush()
{
}
