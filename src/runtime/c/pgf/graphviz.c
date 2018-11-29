#include "data.h"
#include "graphviz.h"
#include "linearizer.h"

PgfGraphvizOptions pgf_default_graphviz_options[1] =
  { {0, 0, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL} } ;

static int
pgf_graphviz_abstract_tree_(PgfPGF* pgf, PgfExpr expr, int *pid,
                            PgfGraphvizOptions* opts,
                            GuOut* out, GuExn* err)
{
	int id = -1;

	GuVariantInfo ei = gu_variant_open(expr);
	switch (ei.tag) {
	case PGF_EXPR_ABS:
		gu_impossible();
		break;
	case PGF_EXPR_APP: {
		PgfExprApp* app = ei.data;
		id = pgf_graphviz_abstract_tree_(pgf, app->fun, pid, opts, out, err);
		int arg_id = pgf_graphviz_abstract_tree_(pgf, app->arg, pid, opts, out, err);
		gu_printf(out, err, "n%d -- n%d", id, arg_id);
		if (opts->nodeEdgeStyle != NULL && *opts->nodeEdgeStyle && opts->nodeColor != NULL && *opts->nodeColor)
			gu_printf(out, err, " [style = \"%s\", color = \"%s\"]", opts->nodeEdgeStyle, opts->nodeColor);
		else if (opts->nodeEdgeStyle != NULL && *opts->nodeEdgeStyle)
				gu_printf(out, err, " [style = \"%s\"]", opts->nodeEdgeStyle);
		else if (opts->nodeColor != NULL && *opts->nodeColor)
				gu_printf(out, err, " [color = \"%s\"]", opts->nodeColor);
		gu_printf(out, err, "\n", id, arg_id);
		break;
	}
	case PGF_EXPR_LIT: {
		PgfExprLit* lit = ei.data;
		id = (*pid)++;
		gu_printf(out, err, "n%d[label = \"", id);
		
		GuVariantInfo ei = gu_variant_open(lit->lit);
		switch (ei.tag) {
		case PGF_LITERAL_STR: {
			PgfLiteralStr* lit = ei.data;
			gu_puts("\\\"", out, err);
			gu_string_write(lit->val, out, err);
			gu_puts("\\\"", out, err);
			break;
		}
		case PGF_LITERAL_INT: {
			PgfLiteralInt* lit = ei.data;
			gu_printf(out, err, "%d", lit->val);
			break;
		}
		case PGF_LITERAL_FLT: {
			PgfLiteralFlt* lit = ei.data;
			gu_printf(out, err, "%lf", lit->val);
			break;
		}
		default:
			gu_impossible();
		}

		gu_puts("\", style = \"solid\", shape = \"plaintext\"]\n", out, err);
		break;
	}
	case PGF_EXPR_META:
		id = (*pid)++;
		gu_printf(out, err, "n%d[label = \"?\", style = \"solid\", shape = \"plaintext\"]\n", id);
		break;
	case PGF_EXPR_FUN: {
		PgfExprFun* fun = ei.data;
		id = (*pid)++;
		if (opts->noFun && opts->noCat) {
			gu_printf(out, err, "n%d[shape = \"point\"]\n", id);
		} else {
			gu_printf(out, err, "n%d[label = \"", id);
			PgfType* ty = (opts->noCat) ? NULL : pgf_function_type(pgf, fun->fun);
			if (!opts->noFun)
				gu_string_write(fun->fun, out, err);
			if (!opts->noFun && ty != NULL)
				gu_puts(" : ", out,err);
			if (ty != NULL)
				gu_string_write(ty->cid, out, err);
			gu_puts("\", shape = \"plaintext\", style = \"solid\"", out, err);
			if (opts->nodeColor != NULL && *opts->nodeColor)
				gu_printf(out, err, ", fontcolor = \"%s\"", opts->nodeColor);
			if (opts->nodeFont != NULL && *opts->nodeFont)
				gu_printf(out, err, ", fontname = \"%s\"", opts->nodeFont);
			gu_puts("]\n", out, err);
		}
		break;
	}
	case PGF_EXPR_VAR:
		gu_impossible();
		break;
	case PGF_EXPR_TYPED: {
		PgfExprTyped* typed = ei.data;
		id = pgf_graphviz_abstract_tree_(pgf, typed->expr, pid, opts, out, err);
		break;
	}
	case PGF_EXPR_IMPL_ARG: {
		PgfExprImplArg* implarg = ei.data;
		id = pgf_graphviz_abstract_tree_(pgf, implarg->expr, pid, opts, out, err);
		break;
	}
	default:
		gu_impossible();
	}

	return id;
}

PGF_API void
pgf_graphviz_abstract_tree(PgfPGF* pgf, PgfExpr expr, PgfGraphvizOptions* opts, GuOut* out, GuExn* err)
{
	int id = 0;

	gu_puts("graph {\n", out, err);
	pgf_graphviz_abstract_tree_(pgf, expr, &id, opts, out, err);
	gu_puts("}", out, err);
}

typedef struct PgfParseNode PgfParseNode;
	
struct PgfParseNode {
	int id;
	PgfParseNode* parent;
	GuString fun;
	GuString label;
};

typedef struct {
	PgfLinFuncs* funcs;

	GuPool* pool;
	GuOut* out;
	GuExn* err;

	PgfParseNode* parent;
	size_t level;
	GuBuf* internals;
	GuBuf* leaves;
} PgfBracketLznState;

static void
pgf_bracket_lzn_symbol_token(PgfLinFuncs** funcs, PgfToken tok)
{
	PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);

	PgfParseNode* node = gu_new(PgfParseNode, state->pool);
	node->id     = 100000 + gu_buf_length(state->leaves);
	node->parent = state->parent;
	node->fun    = NULL;
	node->label  = tok;
	gu_buf_push(state->leaves, PgfParseNode*, node);
}

static void
pgf_bracket_lzn_begin_phrase(PgfLinFuncs** funcs, PgfCId cat, int fid, size_t lindex, PgfCId fun)
{
	PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);

	if (strcmp(cat, "_") == 0)
		return;
	
	state->level++;

	GuBuf* level;
	if (state->level < gu_buf_length(state->internals))
		level = gu_buf_get(state->internals, GuBuf*, state->level);
	else {
		level = gu_new_buf(PgfParseNode*, state->pool);
		gu_buf_push(state->internals, GuBuf*, level);
	}

	size_t len = gu_buf_length(level);
	for (size_t i = 0; i < len; i++) {
		PgfParseNode* node = gu_buf_get(level, PgfParseNode*, i);
		if (node->id == fid) {
			state->parent = node;
			return;
		}
	}
	
	PgfParseNode* node = gu_new(PgfParseNode, state->pool);
	node->id     = fid;
	node->parent = state->parent;
	node->fun    = fun;
	node->label  = cat;
	gu_buf_push(level, PgfParseNode*, node);

	state->parent = node;
}

static void
pgf_bracket_lzn_end_phrase(PgfLinFuncs** funcs, PgfCId cat, int fid, size_t lindex, PgfCId fun)
{
	PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);

	if (strcmp(cat, "_") == 0)
		return;

	state->level--;
	state->parent = state->parent->parent;
}

static void
pgf_bracket_lzn_symbol_meta(PgfLinFuncs** funcs, PgfMetaId meta_id)
{
	PgfBracketLznState* state = gu_container(funcs, PgfBracketLznState, funcs);

	PgfParseNode* node = gu_new(PgfParseNode, state->pool);
	node->id     = 100000 + gu_buf_length(state->leaves);
	node->parent = state->parent;
	node->fun    = NULL;
	node->label  = "?";
	gu_buf_push(state->leaves, PgfParseNode*, node);
}

static PgfLinFuncs pgf_bracket_lin_funcs = {
	.symbol_token  = pgf_bracket_lzn_symbol_token,
	.begin_phrase  = pgf_bracket_lzn_begin_phrase,
	.end_phrase    = pgf_bracket_lzn_end_phrase,
	.symbol_ne     = NULL,
	.symbol_bind   = NULL,
	.symbol_capit  = NULL,
	.symbol_meta   = pgf_bracket_lzn_symbol_meta
};

static void
pgf_graphviz_parse_level(GuBuf* level, PgfGraphvizOptions* opts, GuOut* out, GuExn* err)
{
	gu_puts("\n  subgraph {rank=same;\n", out, err);

	size_t len = gu_buf_length(level);
	
	if (len > 1)
		gu_puts("    edge[style=invis]\n", out, err);

	for (size_t i = 0; i < len; i++) {
		PgfParseNode* node = gu_buf_get(level, PgfParseNode*, i);
		if (node->fun != NULL) {
			gu_printf(out, err, "    n%d[label=\"", node->id);
			if (!opts->noFun)
				gu_string_write(node->fun, out, err);
			if (!opts->noFun && !opts->noCat)
				gu_puts(" : ", out, err);
			if (!opts->noCat)
				gu_string_write(node->label, out, err);
			gu_puts("\"", out, err);
			if (opts->nodeColor != NULL && *opts->nodeColor)
				gu_printf(out, err, ", fontcolor = \"%s\"", opts->nodeColor);
			if (opts->nodeFont != NULL && *opts->nodeFont)
				gu_printf(out, err, ", fontname = \"%s\"", opts->nodeFont);
			gu_puts("]\n", out, err);
		} else {
			if (opts->noLeaves)
				gu_printf(out, err, "    n%d[label=\"\"]\n", node->id);
			else {
				gu_printf(out, err, "    n%d[label=\"%s\"", node->id, node->label);
				if (opts->leafColor != NULL && *opts->leafColor)
					gu_printf(out, err, ", fontcolor = \"%s\"", opts->leafColor);
				if (opts->leafFont != NULL && *opts->leafFont)
					gu_printf(out, err, ", fontname = \"%s\"", opts->leafFont);
				gu_puts("]\n", out, err);
			}
		}
	}
	
	if (len > 1) {
		for (size_t i = 0; i < len; i++) {
			PgfParseNode* node = gu_buf_get(level, PgfParseNode*, i);		
		
			gu_puts((i == 0) ? "    " : " -- ", out, err);
			gu_printf(out, err, "n%d", node->id);
		}
		gu_puts("\n", out, err);
	}
	
	gu_puts("  }\n", out, err);

	for (size_t i = 0; i < len; i++) {
		PgfParseNode* node = gu_buf_get(level, PgfParseNode*, i);		
		if (node->parent != NULL) {
			gu_printf(out, err, "  n%d -- n%d", node->parent->id, node->id);

			GuString edgeStyle, color;
			if (node->fun == NULL) {
				edgeStyle = opts->leafEdgeStyle;
				color     = opts->leafColor;
			} else {
				edgeStyle = opts->nodeEdgeStyle;
				color     = opts->nodeColor;
			}

			if (edgeStyle != NULL && *edgeStyle && color != NULL && *color)
				gu_printf(out, err, " [style = \"%s\", color = \"%s\"]", edgeStyle, color);
			else if (edgeStyle != NULL && *edgeStyle)
				gu_printf(out, err, " [style = \"%s\"]", edgeStyle);
			else if (color != NULL && *color)
				gu_printf(out, err, " [color = \"%s\"]", color);

			gu_putc('\n', out, err);			
		}
	}
}

PGF_API void
pgf_graphviz_parse_tree(PgfConcr* concr, PgfExpr expr, PgfGraphvizOptions* opts, GuOut* out, GuExn* err)
{
	GuPool* tmp_pool = gu_local_pool();
	
	GuEnum* cts = 
		pgf_lzr_concretize(concr, expr, err, tmp_pool);
	if (!gu_ok(err)) {
		gu_pool_free(tmp_pool);
		return;
	}

	PgfCncTree ctree = gu_next(cts, PgfCncTree, tmp_pool);
	if (gu_variant_is_null(ctree)) {
		gu_pool_free(tmp_pool);
		return;
	}

	gu_puts("graph {\n", out, err);
	gu_puts("  node[shape=plaintext]\n", out, err);

	PgfBracketLznState state;
	state.funcs = &pgf_bracket_lin_funcs;
	state.pool = tmp_pool;
	state.out = out;
	state.err = err;

	state.parent    = NULL;
	state.level     = -1;
	state.internals = gu_new_buf(GuBuf*, tmp_pool);
	state.leaves    = gu_new_buf(PgfParseNode*, tmp_pool);
	pgf_lzr_linearize(concr, ctree, 0, &state.funcs, tmp_pool);

	size_t len = gu_buf_length(state.internals);
	for (size_t i = 0; i < len; i++) {
		GuBuf* level = gu_buf_get(state.internals, GuBuf*, i);
		pgf_graphviz_parse_level(level, opts, out, err);
	}
	pgf_graphviz_parse_level(state.leaves, opts, out, err);

	gu_puts("}", out, err);

	gu_pool_free(tmp_pool);
}


PGF_API_DECL void
pgf_graphviz_word_alignment(PgfConcr** concrs, size_t n_concrs, PgfExpr expr, PgfGraphvizOptions* opts, GuOut* out, GuExn* err)
{
	GuPool* tmp_pool = gu_local_pool();
	
	gu_puts("digraph {\n", out, err);
	gu_puts("rankdir=LR ;\n", out, err);
	gu_puts("node [shape = record", out, err);
	if (opts->leafFont != NULL && *opts->leafFont)
		gu_printf(out, err, ", fontname = \"%s\"", opts->leafFont);
	if (opts->leafColor != NULL && *opts->leafColor)
		gu_printf(out, err, ", fontcolor = \"%s\"", opts->leafColor);
	gu_puts("] ;\n\n", out, err);
	if (opts->leafEdgeStyle != NULL && *opts->leafEdgeStyle)
		gu_printf(out, err, "edge [style = %s];\n", opts->leafEdgeStyle);
	gu_puts("\n", out, err);

	GuSeq* alignment = NULL;
	GuSeq* last_alignment = NULL;
	for (size_t i = 0; i < n_concrs; i++) {
		alignment = pgf_align_words(concrs[i], expr, err, tmp_pool);
		gu_printf(out, err, "  struct%d[label=\"", i);

		size_t n_tokens = gu_seq_length(alignment);
		for (size_t j = 0; j < n_tokens; j++) {
			PgfAlignmentPhrase* phrase = gu_seq_get(alignment, PgfAlignmentPhrase*, j);
			if (j > 0)
				gu_puts(" | ", out, err);
			gu_printf(out, err, "<n%d> %s", j, phrase->phrase);
		}

		gu_puts("\"] ;\n", out, err);

		if (last_alignment != NULL) {
			size_t n_last_tokens = gu_seq_length(last_alignment);

			for (size_t j = 0; j < n_tokens; j++) {
				PgfAlignmentPhrase* phrase = gu_seq_get(alignment, PgfAlignmentPhrase*, j);

				for (size_t k = 0; k < phrase->n_fids; k++) {
					int fid = phrase->fids[k];

					for (size_t l = 0; l < n_last_tokens; l++) {
						PgfAlignmentPhrase* last_phrase = gu_seq_get(last_alignment, PgfAlignmentPhrase*, l);
						
						for (size_t r = 0; r < last_phrase->n_fids; r++) {
							int last_fid = last_phrase->fids[r];
							if (fid == last_fid) {
								gu_printf(out, err, "struct%d:n%d:e -> struct%d:n%d:w ;\n",i,j,i-1,l);
							}
						}
					}
				}
			}
		}

		last_alignment = alignment;
	}

	gu_puts("}", out, err);

	gu_pool_free(tmp_pool);
}

typedef struct {
	PgfPGF* pgf;
	int next_fid;
	GuBuf* anchors;
	GuBuf* heads;
    GuPool* pool;
} PgfDepGenState;

typedef struct {
	int fid;
	int visit;
	PgfExpr expr;
	GuBuf* edges;
} PgfDepNode;

typedef struct {
	GuString label;
	PgfDepNode* node;
} PgfDepEdge;

typedef struct {
	bool   solved;
	size_t start;
	size_t end;
	GuString label;
} PgfDepStackRange;

static void
pgf_graphviz_dependency_graph_(PgfDepGenState* state,
                               size_t parents_start,size_t parents_end,
                               GuString head_label, GuString mod_label,
                               PgfExpr expr);

static bool
pgf_graphviz_dependency_graph_apply(PgfDepGenState* state,
                                    size_t parents_start,size_t parents_end,
                                    GuString head_label, GuString mod_label,
                                    GuBuf* args, GuSeq* pragmas)
{
	size_t n_args    = gu_buf_length(args);
	size_t n_pragmas = pragmas ? gu_seq_length(pragmas) : 0;

	size_t n_count = (n_args <= n_pragmas) ? n_args : n_pragmas;
	PgfDepStackRange ranges[n_count+1];
	for (size_t i = 0; i <= n_count; i++) {
		ranges[i].solved = false;
		ranges[i].label =
			(i > 0) ? gu_seq_index(pragmas, PgfDepPragma, i-1)->label
					: NULL;
	}

	ranges[0].start = gu_buf_length(state->heads);
	ranges[0].end   = gu_buf_length(state->heads);

    bool rel_solved = false;
	size_t n_solved = 0;
	size_t count = 0;
	while (n_solved < n_count) {
		if (!ranges[0].solved) {
			ranges[0].start = gu_buf_length(state->heads);
		}

		for (size_t i = 0; i < n_count; i++) {
			if (ranges[i+1].solved)
				continue;

			PgfExpr       arg    = gu_buf_get(args, PgfExpr, n_args-i-1);
			PgfDepPragma* pragma = gu_seq_index(pragmas, PgfDepPragma, i);

			switch (pragma->tag) {
			case PGF_DEP_PRAGMA_MOD:
				assert(pragma->index <= n_count);
				if (ranges[0].solved && ranges[pragma->index].solved) {
					ranges[i+1].start = gu_buf_length(state->heads);
					pgf_graphviz_dependency_graph_(state,
												   ranges[pragma->index].start, ranges[pragma->index].end,
												   NULL, ranges[i+1].label,
												   arg);
					ranges[i+1].end   = gu_buf_length(state->heads);
					ranges[i+1].solved= true;
					n_solved++;
				}
				break;
			case PGF_DEP_PRAGMA_REL:
				ranges[i+1].solved = true;
				ranges[i+1].start  = 0;
				ranges[i+1].end    = 0;
				n_solved++;

				GuPool *tmp_pool = gu_local_pool();

				GuStringBuf* sbuf =
					gu_new_string_buf(tmp_pool);
				GuOut* out = gu_string_buf_out(sbuf);
				GuExn* err = gu_new_exn(tmp_pool);

				pgf_print_expr(arg, NULL, 0, out, err);

				ranges[pragma->index].label =
					gu_string_buf_freeze(sbuf, state->pool);

				gu_pool_free(tmp_pool);
				break;
			case PGF_DEP_PRAGMA_SKIP:
				ranges[i+1].solved = true;
				n_solved++;
				break;
			case PGF_DEP_PRAGMA_ANCH:
				if (ranges[0].solved) {
					ranges[i+1].start = gu_buf_length(state->heads);
					pgf_graphviz_dependency_graph_(state,0,0,"ROOT","ROOT",arg);
					ranges[i+1].end   = gu_buf_length(state->heads);
					ranges[i+1].solved= true;
					n_solved++;
					count++;
				}
				break;
			case PGF_DEP_PRAGMA_HEAD:
				if (!rel_solved)
					break;

				if (!ranges[0].solved) {
					GuString new_head_label = head_label;
					GuString new_mod_label = mod_label;
					if (pragma->label != NULL && *pragma->label && pragma->index == 0) {
						new_head_label = pragma->label;
						new_mod_label = "ROOT";
					}
					if (ranges[0].label != NULL)
						new_mod_label = ranges[0].label;
					ranges[i+1].start = gu_buf_length(state->heads);
					pgf_graphviz_dependency_graph_(state,
												   parents_start,parents_end,
												   new_head_label, new_mod_label,
												   arg);
					ranges[i+1].end   = gu_buf_length(state->heads);
					if (pragma->index == 0) {
						ranges[i+1].solved = true;
						n_solved++;
					}
					count++;
				}
				if (pragma->index != 0 && ranges[pragma->index].solved) {
					for (size_t j = ranges[pragma->index].start; j < ranges[pragma->index].end; j++) {
						PgfDepNode* parent = gu_buf_get(state->heads, PgfDepNode*, j);
						for (size_t k = ranges[i+1].start; k < ranges[i+1].end; k++) {
							PgfDepNode* child = gu_buf_get(state->heads, PgfDepNode*, k);
							PgfDepEdge* edge = gu_buf_extend(parent->edges);
							edge->label = pragma->label;
							edge->node  = child;
						}
					}
					ranges[i+1].solved = true;
					n_solved++;
				}
				break;
			default:
				gu_impossible();
			}
		}

		if (rel_solved) {
			if (!ranges[0].solved) {
				ranges[0].end = gu_buf_length(state->heads);
				ranges[0].solved = true;
			}
		} else {
			rel_solved = true;
		}
	}

	gu_buf_trim_n(state->heads, gu_buf_length(state->heads)-ranges[0].end);

	return (count > 0);
}

static void
pgf_graphviz_dependency_graph_(PgfDepGenState* state,
                               size_t parents_start,size_t parents_end,
                               GuString head_label, GuString mod_label,
                               PgfExpr expr)
{
	PgfExpr e = expr;
	GuBuf* args = gu_new_buf(PgfDepNode*, state->pool);

	for (;;) {
		GuVariantInfo ei = gu_variant_open(e);
		switch (ei.tag) {
		case PGF_EXPR_APP: {
			PgfExprApp* app = ei.data;
			gu_buf_push(args, PgfExpr, app->arg);
			e = app->fun;
			break;
		}
		case PGF_EXPR_TYPED: {
			PgfExprTyped* typed = ei.data;
			e = typed->expr;
			break;
		}
		case PGF_EXPR_IMPL_ARG: {
			PgfExprImplArg* implarg = ei.data;
			e = implarg->expr;
			break;
		}
		case PGF_EXPR_FUN: {
			PgfExprFun* fun = ei.data;
			PgfAbsFun* absfun =
				gu_seq_binsearch(state->pgf->abstract.funs, pgf_absfun_order, PgfAbsFun, fun->fun);

			if (pgf_graphviz_dependency_graph_apply(state,
			                                        parents_start,parents_end,
			                                        head_label,mod_label,
			                                        args,absfun ? absfun->pragmas : NULL))
				return;
			// continue to default
		}
		default: {
			PgfDepNode* node = gu_new(PgfDepNode, state->pool);
			node->fid   = state->next_fid++;
			node->visit = 0;
			node->expr  = expr;
			node->edges = gu_new_buf(PgfDepEdge, state->pool);

			for (size_t i = parents_start; i < parents_end; i++) {
				PgfDepNode* parent = gu_buf_get(state->heads, PgfDepNode*, i);
				if (head_label == NULL) {
					PgfDepEdge* edge = gu_buf_extend(parent->edges);
					edge->label = mod_label;
					edge->node  = node;
				} else {
					PgfDepEdge* edge = gu_buf_extend(node->edges);
					edge->label = head_label;
					edge->node  = parent;
				}
			}

			gu_buf_push(state->heads, PgfDepNode*, node);
			if (head_label != NULL)
				gu_buf_push(state->anchors, PgfDepNode*, node);

			return;
		}
		}
	}
}

static void
pgf_graphviz_print_graph(PgfGraphvizOptions* opts, PgfDepNode* node,
                         GuOut* out, GuExn* err)
{
	if (node->visit++ > 0)
		return;

	gu_printf(out, err, "  n%d[label = \"", node->fid);
	pgf_print_expr(node->expr, NULL, 0, out, err);
	if (opts->nodeColor != NULL && *opts->nodeColor)
		gu_printf(out, err, ", fontcolor = \"%s\"", opts->nodeColor);
	if (opts->nodeFont != NULL && *opts->nodeFont)
		gu_printf(out, err, ", fontname = \"%s\"", opts->nodeFont);
	gu_puts("\"]\n", out, err);

	size_t n_children = gu_buf_length(node->edges);
	for (size_t i = 0; i < n_children; i++) {
		PgfDepEdge* edge = gu_buf_index(node->edges, PgfDepEdge, n_children-i-1);
		gu_printf(out, err, "  n%d -> n%d [label = \"%s\"",
		          node->fid, edge->node->fid, edge->label);
		if (opts->nodeEdgeStyle != NULL && *opts->nodeEdgeStyle)
			gu_printf(out, err, ", style = \"%s\"", opts->nodeEdgeStyle);
		if (opts->nodeColor != NULL && *opts->nodeColor)
			gu_printf(out, err, ", color = \"%s\"", opts->nodeColor);
		gu_puts("]\n", out, err);

		if (edge->node->fid > node->fid)
			pgf_graphviz_print_graph(opts, edge->node, out, err);
	}
}

void
pgf_graphviz_dependency_graph(PgfPGF* pgf, PgfExpr expr,
                              PgfGraphvizOptions* opts,
                              GuOut* out, GuExn* err,
                              GuPool* pool)
{
	PgfDepGenState state;
	state.pgf  = pgf;
	state.next_fid = 1;
	state.pool = pool;
	state.anchors = gu_new_buf(PgfDepNode*, pool);
	state.heads   = gu_new_buf(PgfDepNode*, pool);

	pgf_graphviz_dependency_graph_(&state, 0, 0, "ROOT", "ROOT", expr);

	gu_puts("digraph {\n", out, err);
	size_t n_anchors = gu_buf_length(state.anchors);
	for (size_t i = 0; i < n_anchors; i++) {
		PgfDepNode* node = gu_buf_get(state.anchors, PgfDepNode*, i);
		pgf_graphviz_print_graph(opts,node,out,err);
	}
	gu_puts("}", out, err);
}
