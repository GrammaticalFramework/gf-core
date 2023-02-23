#include "data.h"
#include "printer.h"
#include "linearizer.h"

PgfLinearizer::TreeNode::TreeNode(PgfLinearizer *linearizer)
{
    this->next     = linearizer->prev;
    this->next_arg = NULL;
    this->args     = linearizer->args;

    this->fid       = 0;

    this->value     = 0;
    this->var_count = 0;
    this->var_values= NULL;

    this->n_hoas_vars = 0;
    this->hoas_vars   = NULL;

    linearizer->prev = this;
}

void PgfLinearizer::TreeNode::linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, PgfLParam *r)
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
    size_t lindex = eval_param(r);
    arg->linearize(out, linearizer, lindex);
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

void PgfLinearizer::TreeNode::linearize_seq(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, ref<PgfSequence> seq)
{
    for (size_t i = 0; i < seq->syms.len; i++) {
        PgfSymbol sym = *vector_elem(&seq->syms, i);

        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolCat::tag: {
            auto sym_cat = ref<PgfSymbolCat>::untagged(sym);
            linearize_arg(out, linearizer, sym_cat->d, &sym_cat->r);
            break;
        }
        case PgfSymbolLit::tag: {
            auto sym_lit = ref<PgfSymbolLit>::untagged(sym);
            linearize_arg(out, linearizer, sym_lit->d, &sym_lit->r);
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
}

size_t PgfLinearizer::TreeNode::eval_param(PgfLParam *param)
{
    size_t value = param->i0;
    for (size_t j = 0; j < param->n_terms; j++) {
        size_t factor = param->terms[j].factor;
        size_t var    = param->terms[j].var;

        if (var < var_count && var_values[var] != (size_t) -1) {
            value += factor * var_values[var];
        } else {
            throw pgf_error("Unbound variable in resolving a linearization");
        }
    }
    return value;
}

PgfLinearizer::TreeLinNode::TreeLinNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin)
  : TreeNode(linearizer)
{
    this->lin       = lin;
    this->lin_index = 0;
}

bool PgfLinearizer::TreeLinNode::resolve(PgfLinearizer *linearizer)
{
    ref<Vector<PgfHypo>> hypos = lin->absfun->type->hypos;
    size_t n_args = lin->args->len / lin->res->len;

    while (lin_index < lin->res->len) {
        size_t offset = lin_index*n_args;

        ref<PgfPResult> pres = *vector_elem(lin->res,  lin_index);

        // Unbind all variables
        for (size_t j = 0; j < var_count; j++) {
            var_values[j] = (size_t) -1;
        }

        int i = 0;
        TreeNode *arg = args;
        while (arg != NULL) {
            ref<PgfPArg> parg = vector_elem(lin->args, offset+i);
            arg->check_category(linearizer, &vector_elem(hypos,i)->type->name);

            if (arg->value < parg->param->i0)
                break;

            size_t value = arg->value - parg->param->i0;
            for (size_t j = 0; j < parg->param->n_terms; j++) {
                size_t factor    = parg->param->terms[j].factor;
                size_t var       = parg->param->terms[j].var;
                size_t var_value;

                if (var < var_count && var_values[var] != (size_t) -1) {
                    // The variable already has a value
                    var_value = var_values[var];
                } else {
                    // The variable is not assigned yet
                    var_value = value / factor;

                    // find the range for the variable
                    size_t range = 0;
                    for (size_t k = 0; k < pres->vars->len; k++) {
                        ref<PgfVariableRange> var_range = vector_elem(pres->vars, k);
                        if (var_range->var == var) {
                            range = var_range->range;
                            break;
                        }
                    }
                    if (range == 0)
                        throw pgf_error("Unknown variable in resolving a linearization");

                    if (var_value >= range)
                        break;

                    // Assign the variable;
                    if (var >= var_count) {
                        var_values = (size_t*)
                            realloc(var_values, (var+1)*sizeof(size_t));
                        while (var_count < var) {
                            var_values[var_count++] = (size_t) -1;
                        }
                        var_count++;
                    }
                    var_values[var] = var_value;
                }

                value -= var_value * factor;
            }

            if (value != 0)
                break;

            arg = arg->next_arg;
            i++;
        }

        lin_index++;

        if (arg == NULL) {
            value = eval_param(&pres->param);
            return true;
        }
    }

    lin_index = 0;
    return false;
}

void PgfLinearizer::TreeLinNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    if (textcmp(&lin->absfun->type->name, cat) != 0)
        throw pgf_error("An attempt to linearize an expression which is not type correct");
}

void PgfLinearizer::TreeLinNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    PgfText *cat = &lin->absfun->type->name;
    PgfText *field = &*(vector_elem(lin->lincat->fields, lindex)->name);

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

    size_t n_seqs = lin->seqs->len / lin->res->len;
    ref<PgfSequence> seq = *vector_elem(lin->seqs, (lin_index-1)*n_seqs + lindex);
    linearize_seq(out, linearizer, seq);

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
}

ref<PgfConcrLincat> PgfLinearizer::TreeLinNode::get_lincat(PgfLinearizer *linearizer)
{
    return namespace_lookup(linearizer->concr->lincats, &lin->absfun->type->name);
}

PgfLinearizer::TreeLindefNode::TreeLindefNode(PgfLinearizer *linearizer, PgfText *fun, PgfText *literal)
  : TreeNode(linearizer)
{
    this->lincat    = 0;
    this->lin_index = 0;
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
    if (lincat == 0) {
        return (lin_index = !lin_index);
    } else {
        ref<PgfPResult> pres = *vector_elem(lincat->res,  lin_index);
        value = eval_param(&pres->param);
        lin_index++;
        if (lin_index <= lincat->n_lindefs)
            return true;
        lin_index = 0;
        return false;
    }
}

void PgfLinearizer::TreeLindefNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    lincat = namespace_lookup(linearizer->concr->lincats, cat);
    if (lincat == 0)
        throw pgf_error("Cannot find a lincat for a category");
}

void PgfLinearizer::TreeLindefNode::linearize_arg(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t d, PgfLParam *r)
{
    linearizer->flush_pre_stack(out, literal);
    out->symbol_token(literal);

    TreeNode *arg = args;
    while (arg != NULL) {
        arg->linearize(out,linearizer,0);
        arg = arg->next_arg;
    }
}

void PgfLinearizer::TreeLindefNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    if (lincat != 0) {
        PgfText *field = &*(vector_elem(lincat->fields, lindex)->name);
        if (linearizer->pre_stack == NULL)
            out->begin_phrase(&lincat->name, fid, field, fun);
        else {
            BracketStack *bracket = new BracketStack();
            bracket->next  = linearizer->pre_stack->bracket_stack;
            bracket->begin = true;
            bracket->fid   = fid;
            bracket->cat   = &lincat->name;
            bracket->field = field;
            bracket->fun   = fun;
            linearizer->pre_stack->bracket_stack = bracket;
        }

        ref<PgfSequence> seq = *vector_elem(lincat->seqs, (lin_index-1)*lincat->fields->len + lindex);
        linearize_seq(out, linearizer, seq);

        if (linearizer->pre_stack == NULL)
            out->end_phrase(&lincat->name, fid, field, fun);
        else {
            BracketStack *bracket = new BracketStack();
            bracket->next  = linearizer->pre_stack->bracket_stack;
            bracket->begin = false;
            bracket->fid   = fid;
            bracket->cat   = &lincat->name;
            bracket->field = field;
            bracket->fun   = fun;
            linearizer->pre_stack->bracket_stack = bracket;
        }
    } else {
        linearize_arg(out, linearizer, 0, NULL);
    }
}

ref<PgfConcrLincat> PgfLinearizer::TreeLindefNode::get_lincat(PgfLinearizer *linearizer)
{
    return lincat;
}

PgfLinearizer::TreeLinrefNode::TreeLinrefNode(PgfLinearizer *linearizer, TreeNode *root)
  : TreeNode(linearizer)
{
    args = root;
    lin_index=0;
}

bool PgfLinearizer::TreeLinrefNode::resolve(PgfLinearizer *linearizer)
{
    TreeNode *root = args;
    ref<PgfConcrLincat> lincat = root->get_lincat(linearizer);
    if (lincat == 0)
        return (lin_index = !lin_index);

    while (lincat->n_lindefs+lin_index < lincat->res->len) {
        // Unbind all variables
        for (size_t j = 0; j < var_count; j++) {
            var_values[j] = (size_t) -1;
        }

        ref<PgfPResult> pres = *vector_elem(lincat->res, lincat->n_lindefs+lin_index);
        ref<PgfPArg> parg = vector_elem(lincat->args, lincat->n_lindefs+lin_index);

        if (root->value < parg->param->i0)
            break;

        size_t value = root->value - parg->param->i0;
        for (size_t j = 0; j < parg->param->n_terms; j++) {
            size_t factor    = parg->param->terms[j].factor;
            size_t var       = parg->param->terms[j].var;
            size_t var_value;

            if (var < var_count && var_values[var] != (size_t) -1) {
                // The variable already has a value
                var_value = var_values[var];
            } else {
                // The variable is not assigned yet
                var_value = value / factor;

                // find the range for the variable
                size_t range = 0;
                for (size_t k = 0; k < pres->vars->len; k++) {
                    ref<PgfVariableRange> var_range = vector_elem(pres->vars, k);
                    if (var_range->var == var) {
                        range = var_range->range;
                        break;
                    }
                }
                if (range == 0)
                    throw pgf_error("Unknown variable in resolving a linearization");

                if (var_value >= range)
                    break;

                // Assign the variable;
                if (var >= var_count) {
                    var_values = (size_t*)
                        realloc(var_values, (var+1)*sizeof(size_t));
                    while (var_count < var) {
                        var_values[var_count++] = (size_t) -1;
                    }
                    var_count++;
                }
                var_values[var] = var_value;
            }

            value -= var_value * factor;
        }

        lin_index++;
        if (value == 0) {
            value = eval_param(&pres->param);
            return true;
        }
    }

    lin_index = 0;
    return false;
}

void PgfLinearizer::TreeLinrefNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{
    ref<PgfConcrLincat> lincat = args->get_lincat(linearizer);
    if (lincat != 0) {
        size_t i = lincat->n_lindefs*lincat->fields->len + (lin_index-1);
        ref<PgfSequence> seq = *vector_elem(lincat->seqs, i);
        linearize_seq(out, linearizer, seq);
    } else {
        args->linearize(out, linearizer, lindex);
    }
}

ref<PgfConcrLincat> PgfLinearizer::TreeLinrefNode::get_lincat(PgfLinearizer *linearizer)
{
    return 0;
}

PgfLinearizer::TreeLitNode::TreeLitNode(PgfLinearizer *linearizer, ref<PgfConcrLincat> lincat, PgfText *lit)
  : TreeNode(linearizer)
{
    this->lincat  = lincat;
    this->literal = lit;
}

void PgfLinearizer::TreeLitNode::check_category(PgfLinearizer *linearizer, PgfText *cat)
{
    if (textcmp(&lincat->name, cat) != 0)
        throw pgf_error("An attempt to linearize an expression which is not type correct");
}

void PgfLinearizer::TreeLitNode::linearize(PgfLinearizationOutputIface *out, PgfLinearizer *linearizer, size_t lindex)
{    
    PgfText *field = NULL;
    if (lincat != 0) {
        field = &*(vector_elem(lincat->fields, lindex)->name);
    }

    linearizer->flush_pre_stack(out, literal);

    if (lincat != 0)
        out->begin_phrase(&lincat->name, fid, field, linearizer->wild);
    out->symbol_token(literal);
    if (lincat != 0)
        out->end_phrase(&lincat->name, fid, field, linearizer->wild);
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
            for (size_t i = 0; i < pre->sym_kp->alts.len; i++) {
                PgfAlternative *alt = &pre->sym_kp->alts.data[i];
                for (size_t j = 0; j < alt->prefixes->len; j++) {
                    ref<PgfText> prefix = *vector_elem(alt->prefixes,j);
                    if (cmp(token, &(*prefix))) {
                        pre->node->linearize_seq(out, this, alt->form);
                        goto done;
                    }
                }
            }
        }

        pre->node->linearize_seq(out, this, pre->sym_kp->default_form);

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
    if (lin != 0)
        return (PgfExpr) new TreeLinNode(this, lin);
    else {
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
