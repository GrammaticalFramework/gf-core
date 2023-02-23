#include "data.h"
#include "printer.h"
#include "linearizer.h"
#include "graphviz.h"


PgfLinearizationGraphvizOutput::PgfLinearizationGraphvizOutput()
{
	parent = NULL;
	level = 0;
    n_internals = 0;
    internals = NULL;
    leaves.n_nodes = 0;
    leaves.nodes = NULL;
    nonexist = false;

    meta = (PgfText*) malloc(sizeof(PgfText)+2);
    meta->text[0] = '?';
    meta->text[1] = 0;
}

PgfLinearizationGraphvizOutput::~PgfLinearizationGraphvizOutput()
{
    for (size_t i = 0; i < n_internals; i++) {
        ParseLevel *internal = internals[i];
        for (size_t j = 0; j < internal->n_nodes; j++) {
            delete internal->nodes[j];
        }
        delete(internal->nodes);
        delete[] internal;
    }

    for (size_t j = 0; j < leaves.n_nodes; j++) {
        delete leaves.nodes[j];
    }
    delete(leaves.nodes);

    free(meta);
}

PgfLinearizationGraphvizOutput::ParseNode::ParseNode(ParseLevel *level,
                                                     int id,
                                                     ParseNode *parent,
                                                     PgfText *fun,
                                                     PgfText *label)
{
	this->id     = id;
	this->parent = parent;
	this->fun    = fun;
	this->label  = label;

    level->nodes = (ParseNode**)
        realloc(level->nodes, (level->n_nodes+1)*sizeof(ParseNode*));
    level->nodes[level->n_nodes++] = this;
}

void PgfLinearizationGraphvizOutput::symbol_token(PgfText *tok)
{
	new ParseNode(&leaves, 100000 + leaves.n_nodes, parent, NULL, tok);
}

void PgfLinearizationGraphvizOutput::begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
	if (cat->size == 1 && cat->text[0] == '_')
		return;

	level++;

	ParseLevel *internal;
	if (level < n_internals) {
		internal = internals[level];

        for (size_t i = 0; i < internal->n_nodes; i++) {
            ParseNode *node = internal->nodes[i];
            if (node->id == fid) {
                parent = node;
                return;
            }
        }
	} else {
        internal = new ParseLevel();
        internal->n_nodes = 0;
        internal->nodes   = NULL;
        internals = (ParseLevel**)
            realloc(internals, (n_internals+1)*sizeof(ParseLevel*));
		internals[n_internals++] = internal;
	}

	parent = new ParseNode(internal, fid, parent, fun, cat);
}

void PgfLinearizationGraphvizOutput::end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
	if (cat->size == 1 && cat->text[0] == '_')
		return;

	level--;
	parent = parent->parent;
}

void PgfLinearizationGraphvizOutput::symbol_ne()
{
    nonexist = true;
}

void PgfLinearizationGraphvizOutput::symbol_bind()
{
}

void PgfLinearizationGraphvizOutput::flush()
{
}

void PgfLinearizationGraphvizOutput::generate_graphviz_level(PgfPrinter *printer, PgfGraphvizOptions* opts, ParseLevel *level)
{
	printer->puts("\n  subgraph {\n    rank=same;\n");
	
	if (level->n_nodes > 1)
		printer->puts("    edge[style=invis]\n");

	for (size_t i = 0; i < level->n_nodes; i++) {
		ParseNode* node = level->nodes[i];
		if (node->fun != NULL) {
			printer->nprintf(32, "    n%d[label=\"", node->id);
			if (!opts->noFun)
				printer->efun(node->fun);
			if (!opts->noFun && !opts->noCat)
				printer->puts(" : ");
			if (!opts->noCat)
				printer->efun(node->label);
			printer->puts("\"");
			if (opts->nodeColor != NULL && *opts->nodeColor)
				printer->nprintf(40, ", fontcolor = \"%s\"", opts->nodeColor);
			if (opts->nodeFont != NULL && *opts->nodeFont)
				printer->nprintf(40, ", fontname = \"%s\"", opts->nodeFont);
			printer->puts("]\n");
		} else {
			if (opts->noLeaves)
				printer->nprintf(25, "    n%d[label=\"\"]\n", node->id);
			else {
				printer->nprintf(25, "    n%d[label=\"", node->id);
                printer->puts(node->label);
                printer->puts("\"");
				if (opts->leafColor != NULL && *opts->leafColor)
					printer->nprintf(40, ", fontcolor = \"%s\"", opts->leafColor);
				if (opts->leafFont != NULL && *opts->leafFont)
					printer->nprintf(40, ", fontname = \"%s\"", opts->leafFont);
				printer->puts("]\n");
			}
		}
	}

	if (level->n_nodes > 1) {
		for (size_t i = 0; i < level->n_nodes; i++) {
			ParseNode* node = level->nodes[i];		
		
			printer->puts((i == 0) ? "    " : " -- ");
			printer->nprintf(32, "n%d", node->id);
		}
		printer->puts("\n");
	}
	
	printer->puts("  }\n");

	for (size_t i = 0; i < level->n_nodes; i++) {
        ParseNode* node = level->nodes[i];
		if (node->parent != NULL) {
			printer->nprintf(40, "  n%d -- n%d", node->parent->id, node->id);

			const char *edgeStyle, *color;
			if (node->fun == NULL) {
				edgeStyle = opts->leafEdgeStyle;
				color     = opts->leafColor;
			} else {
				edgeStyle = opts->nodeEdgeStyle;
				color     = opts->nodeColor;
			}

			if (edgeStyle != NULL && *edgeStyle && color != NULL && *color)
				printer->nprintf(50, " [style = \"%s\", color = \"%s\"]", edgeStyle, color);
			else if (edgeStyle != NULL && *edgeStyle)
				printer->nprintf(40, " [style = \"%s\"]", edgeStyle);
			else if (color != NULL && *color)
				printer->nprintf(40, " [color = \"%s\"]", color);

			printer->puts("\n");			
		}
	}
}

PgfText *PgfLinearizationGraphvizOutput::generate_graphviz(PgfGraphvizOptions* opts)
{
    if (nonexist)
        return NULL;

    PgfPrinter printer(NULL, 0, NULL);

	printer.puts("graph {\n");
	printer.puts("  node[shape=plaintext]\n");

	for (size_t i = 0; i < n_internals; i++) {
		ParseLevel* level = internals[i];
        generate_graphviz_level(&printer, opts, level);
	}
    generate_graphviz_level(&printer, opts, &leaves);

	printer.puts("}");

    return printer.get_text();
}

PgfAbstractGraphvizOutput::PgfAbstractGraphvizOutput(PgfAbstr *abstr, PgfGraphvizOptions* opts, PgfMarshaller *m)
    : printer(NULL, 0, NULL)
{
    this->abstr  = abstr;
    this->opts   = opts;
    this->m      = m;

    this->id     = 0;
    this->n_vars = 0;
}

PgfText *PgfAbstractGraphvizOutput::generate_graphviz(PgfExpr expr)
{
    printer.puts("graph {\n");
    m->match_expr(this, expr);
	printer.puts("}");
    return printer.get_text();
}

PgfExpr PgfAbstractGraphvizOutput::eabs(PgfBindType bind_type, PgfText *name, PgfExpr body)
{
    n_vars++;
    printer.push_variable(name);
    PgfExpr res = m->match_expr(this, body);
    printer.pop_variable();
    return res;
}

PgfExpr PgfAbstractGraphvizOutput::eapp(PgfExpr fun, PgfExpr arg)
{
    int fun_id = (int) m->match_expr(this, fun);
	int arg_id = (int) m->match_expr(this, arg);

    printer.nprintf(32, "n%d -- n%d", fun_id, arg_id);
    if (opts->nodeEdgeStyle != NULL && *opts->nodeEdgeStyle && opts->nodeColor != NULL && *opts->nodeColor)
        printer.nprintf(50, " [style = \"%s\", color = \"%s\"]", opts->nodeEdgeStyle, opts->nodeColor);
    else if (opts->nodeEdgeStyle != NULL && *opts->nodeEdgeStyle)
        printer.nprintf(30, " [style = \"%s\"]", opts->nodeEdgeStyle);
    else if (opts->nodeColor != NULL && *opts->nodeColor)
		printer.nprintf(30, " [color = \"%s\"]", opts->nodeColor);
    printer.puts("\n");

    return fun_id;
}

PgfExpr PgfAbstractGraphvizOutput::elit(PgfLiteral lit)
{
    int id = this->id++;
	printer.nprintf(20, "n%d[label = \"", id);
    m->match_lit(this, lit);
    printer.puts("\", style = \"solid\", shape = \"plaintext\"]\n");
    return id;
}

PgfExpr PgfAbstractGraphvizOutput::emeta(PgfMetaId meta_id)
{
    int id = this->id++;
	printer.nprintf(20, "n%d[label = \"", id);
    PgfExpr res = printer.emeta(meta_id);
    printer.puts(", style = \"solid\", shape = \"plaintext\"]\n");
    return res;
}

PgfExpr PgfAbstractGraphvizOutput::efun(PgfText *name)
{
    int id = this->id++;
	if (opts->noFun && opts->noCat) {
        printer.nprintf(32, "n%d[shape = \"point\"]\n", id);
    } else {
		printer.nprintf(20, "n%d[label = \"", id);
        ref<PgfAbsFun> absfun =
            (opts->noCat) ? 0 : namespace_lookup(abstr->funs, name);
        if (!opts->noFun) {
            if (n_vars > 0) {
                printer.puts("\\\\");
                printer.bindings(n_vars);
                printer.puts(" . ");
                n_vars = 0;
            }
            printer.efun(name);
        }
        if (!opts->noFun && absfun != 0)
            printer.puts(" : ");
        if (absfun != 0)
            printer.efun(&absfun->type->name);
        printer.puts("\", shape = \"plaintext\", style = \"solid\"");
        if (opts->nodeColor != NULL && *opts->nodeColor)
            printer.nprintf(40, ", fontcolor = \"%s\"", opts->nodeColor);
        if (opts->nodeFont != NULL && *opts->nodeFont)
            printer.nprintf(40, ", fontname = \"%s\"", opts->nodeFont);
        printer.puts("]\n");
    }

    return id;
}

PgfExpr PgfAbstractGraphvizOutput::evar(int index)
{
    int id = this->id++;
	if (opts->noFun && opts->noCat) {
        printer.nprintf(32, "n%d[shape = \"point\"]\n", id);
    } else {
		printer.nprintf(20, "n%d[label = \"", id);
        if (!opts->noFun) {
            if (n_vars > 0) {
                printer.puts("\\\\");
                printer.bindings(n_vars);
                printer.puts(" . ");
                n_vars = 0;
            }
            printer.evar(index);
        }
        printer.puts("\", shape = \"plaintext\", style = \"solid\"");
        if (opts->nodeColor != NULL && *opts->nodeColor)
            printer.nprintf(40, ", fontcolor = \"%s\"", opts->nodeColor);
        if (opts->nodeFont != NULL && *opts->nodeFont)
            printer.nprintf(40, ", fontname = \"%s\"", opts->nodeFont);
        printer.puts("]\n");
    }

    return id;
}

PgfExpr PgfAbstractGraphvizOutput::etyped(PgfExpr expr, PgfType ty)
{
    return m->match_expr(this, expr);
}

PgfExpr PgfAbstractGraphvizOutput::eimplarg(PgfExpr expr)
{
    return m->match_expr(this, expr);
}

PgfLiteral PgfAbstractGraphvizOutput::lint(size_t size, uintmax_t *val)
{
    return printer.lint(size, val);
}

PgfLiteral PgfAbstractGraphvizOutput::lflt(double val)
{
    return printer.lflt(val);
}

PgfLiteral PgfAbstractGraphvizOutput::lstr(PgfText *val)
{
    printer.puts("\\\"");
    printer.puts(val);
    printer.puts("\\\"");
    return 0;
}

PgfType PgfAbstractGraphvizOutput::dtyp(size_t n_hypos, PgfTypeHypo *hypos,
                                        PgfText *cat,
                                        size_t n_exprs, PgfExpr *exprs)
{
    return 0;
}

void PgfAbstractGraphvizOutput::free_ref(object x)
{
}
