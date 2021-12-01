#include "data.h"
#include "printer.h"
#include "linearizer.h"

PgfLinearizer::TreeNode::TreeNode(PgfLinearizer *linearizer, ref<PgfConcrLin> lin) {
    this->next     = NULL;
    this->next_arg = NULL;
    this->args     = linearizer->args;

    this->lin       = lin;
    this->lin_index = 0;

    this->value     = 0;
    this->var_count = 0;
    this->var_values= NULL;

    if (linearizer->first == NULL) {
        linearizer->first = this;
        linearizer->root  = this;
    } else {
        linearizer->root->next = this;
        linearizer->root = this;
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

PgfLinearizer::PgfLinearizer(ref<PgfConcr> concr, PgfMarshaller *m) {
    this->concr = concr;
    this->m = m;
    this->root  = NULL;
    this->first = NULL;
    this->args  = NULL;
};

PgfLinearizer::~PgfLinearizer()
{
    while (first != NULL) {
        TreeNode *next = first->next;
        delete first;
        first = next;
    }
}

bool PgfLinearizer::resolve()
{
    TreeNode *node = first;
    while (node != NULL) {
        size_t n_args = node->lin->args->len / node->lin->res->len;

        while (node->lin_index < node->lin->res->len) {
            size_t offset = node->lin_index*n_args;

            ref<PgfPResult> pres = *vector_elem(node->lin->res,  node->lin_index);

            int i = 0;
            TreeNode *arg = node->args;
            while (arg != NULL) {
                ref<PgfPArg> parg = vector_elem(node->lin->args, offset+i);

                if (arg->value < parg->param->i0)
                    break;

                size_t value = arg->value - parg->param->i0;
                for (size_t j = 0; j < parg->param->n_terms; j++) {
                    size_t factor    = parg->param->terms[j].factor;
                    size_t var       = parg->param->terms[j].var;
                    size_t var_value;

                    if (var < node->var_count && node->var_values[var] != (size_t) -1) {
                        // The variable already has a value
                        var_value = node->var_values[var];
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

                        if (var_value > range)
                            break;

                        // Assign the variable;
                        if (var >= node->var_count) {
                            node->var_values = (size_t*)
                                realloc(node->var_values, (var+1)*sizeof(size_t));
                            while (node->var_count < var) {
                                node->var_values[node->var_count++] = (size_t) -1;
                            }
                            node->var_count++;
                        }
                        node->var_values[var] = var_value;
                    }

                    value -= var_value * factor;
                }

                if (value != 0)
                    break;

                arg = arg->next_arg;
                i++;
            }

            node->lin_index++;

            if (arg == NULL) {
                node->value = node->eval_param(&pres->param);
                break;
            }

            // Unbind all variables
            for (size_t j = 0; j < node->var_count; j++) {
                node->var_values[j] = (size_t) -1;
            }
        }

        node = node->next;
    }

    return true;
}

void PgfLinearizer::linearize(PgfLinearizationOutputIface *out, TreeNode *node, ref<Vector<PgfSymbol>> syms)
{
    ref<Vector<PgfHypo>> hypos = node->lin->absfun->type->hypos;

    for (size_t i = 0; i < syms->len; i++) {
        PgfSymbol sym = *vector_elem(syms, i);

        switch (ref<PgfSymbol>::get_tag(sym)) {
        case PgfSymbolCat::tag: {
            auto sym_cat = ref<PgfSymbolCat>::untagged(sym);

            size_t d = sym_cat->d;
            TreeNode *arg = node->args;
            while (d > 0) {
                arg = arg->next_arg;
                if (arg == 0)
                    throw pgf_error("Found inconsistency in the PMCFG representation");
                d--;
            }
            size_t lindex = node->eval_param(&sym_cat->r);
            PgfText *cat = &vector_elem(hypos, sym_cat->d)->type->name;

            out->begin_phrase(cat, 0, NULL, &node->lin->name);
            linearize(out, arg, lindex);
            out->end_phrase(cat, 0, NULL, &node->lin->name);
            break;
        }
        case PgfSymbolLit::tag: {
            auto sym_lit = ref<PgfSymbolLit>::untagged(sym);
            break;
        }
        case PgfSymbolVar::tag: {
            auto sym_var = ref<PgfSymbolVar>::untagged(sym);
            break;
        }
        case PgfSymbolKS::tag: {
            auto sym_ks = ref<PgfSymbolKS>::untagged(sym);
            out->symbol_token(&sym_ks->token);
            break;
        }
        case PgfSymbolKP::tag: {
            auto sym_kp = ref<PgfSymbolKP>::untagged(sym);
            linearize(out, node, sym_kp->default_form);
            break;
        }
        case PgfSymbolBIND::tag:
            out->symbol_bind();
            break;
        case PgfSymbolSOFTBIND::tag:
            out->symbol_bind();
            break;
        case PgfSymbolNE::tag:
            out->symbol_ne();
            break;
        case PgfSymbolSOFTSPACE::tag:
            // Nothing to do
            break;
        case PgfSymbolCAPIT::tag:
            out->symbol_capit();
            break;
        case PgfSymbolALLCAPIT::tag:
            out->symbol_allcapit();
            break;
        }
    }
}

void PgfLinearizer::linearize(PgfLinearizationOutputIface *out, TreeNode *node, size_t lindex)
{
    size_t n_seqs = node->lin->seqs->len / node->lin->res->len;
    ref<Vector<PgfSymbol>> syms = *vector_elem(node->lin->seqs, (node->lin_index-1)*n_seqs + lindex);
    linearize(out, node, syms);
}

PgfExpr PgfLinearizer::eabs(PgfBindType btype, PgfText *name, PgfExpr body)
{
    return 0;
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
    return 0;
}

PgfExpr PgfLinearizer::efun(PgfText *name)
{
    ref<PgfConcrLin> lin = namespace_lookup(concr->lins, name);
    TreeNode *node = new TreeNode(this, lin);
    return (PgfExpr) node;
}

PgfExpr PgfLinearizer::evar(int index)
{
    return 0;
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
    return 0;
}

PgfLiteral PgfLinearizer::lflt(double v)
{
    return 0;
}

PgfLiteral PgfLinearizer::lstr(PgfText *v)
{
    return 0;
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
    capit = false;
    allcapit = false;
}

void PgfLinearizationOutput::symbol_token(PgfText *tok)
{
    if (!bind) {
        printer.puts(" ");
    }
    bind = false;

    if (capit) {
        //TODO
        capit = false;
    } else if (allcapit) {
        //TODO
        allcapit = false;
    } else {
        printer.puts(tok);
    }
}

void PgfLinearizationOutput::begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
}

void PgfLinearizationOutput::end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
}

void PgfLinearizationOutput::symbol_ne()
{
}

void PgfLinearizationOutput::symbol_bind()
{
    bind = true;
}

void PgfLinearizationOutput::symbol_capit()
{
    capit = true;
}

void PgfLinearizationOutput::symbol_allcapit()
{
    allcapit = true;
}

void PgfLinearizationOutput::symbol_meta(PgfMetaId id)
{
    printer.nprintf(32, "?%d", id);
}
