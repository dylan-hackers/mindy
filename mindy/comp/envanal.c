/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/comp/envanal.c,v 1.1 1998/05/03 19:55:07 andreas Exp $
*
* This file performs environment analysis.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "src.h"
#include "lexenv.h"
#include "envanal.h"
#include "sym.h"
#include "lose.h"

static void analyze_expr(struct expr *expr, struct lexenv *lexenv);
static void analyze_body(struct body *body, struct lexenv *lexenv);


/* Utilities */

static struct binding
    *find_and_maybe_close_over(struct id *id, struct lexenv *lexenv,
			       struct closes_over **over_ptr)
{
    struct method *method = lexenv->method;
    struct binding *binding;
    struct closes_over *over;

    for (binding = lexenv->bindings; binding != NULL; binding = binding->next)
	if (binding->id->symbol == id->symbol
	    && binding->id->internal == id->internal)
	    break;

    if (binding == NULL)
	return NULL;

    if (binding->home != method) {
	/* It is a closure var. */
	binding->closed_over = TRUE;

	do {
	    for (over = method->closes_over; over != NULL; over = over->next)
		if (over->binding == binding) {
		    /* We already know that this method closes over this */
		    /* binding.  */
		    *over_ptr = over;
		    return binding;
		}

	    over = malloc(sizeof(struct closes_over));
	    over->binding = binding;
	    over->over = NULL;
	    over->offset = method->lexenv_size++;
	    over->next = method->closes_over;
	    method->closes_over = over;
	    method->nargs++;

	    *over_ptr = over;
	    over_ptr = &over->over;

	    method = method->parent;
	} while (method != binding->home);
	*over_ptr = NULL;
	return binding;
    }
    else
	return binding;
}

static void analyze_method(struct method *method, struct lexenv *lexenv)
{
    struct method *parent = lexenv->method;
    struct binding *bindings = lexenv->bindings;
    int offset = 0;
    struct param_list *params = method->params;
    struct param *p;
    struct keyword_param *k;

    if (parent) {
	method->parent = parent;
	method->next = parent->kids;
	parent->kids = method;
    }

    analyze_expr(method->specializers, lexenv);
    if (method->rettypes) {
	analyze_expr(method->rettypes->req_types_list, lexenv);
	if (method->rettypes->rest_temp_varref)
	    analyze_expr(method->rettypes->rest_temp_varref, lexenv);
    }

    for (p = params->required_params; p != NULL; p = p->next)
	bindings = make_binding(p->id, p->type_temp, TRUE, offset++, method,
				bindings);
    if (params->rest_param)
	bindings = make_binding(params->rest_param, NULL, TRUE, offset++,
				method, bindings);
    for (k = params->keyword_params; k != NULL; k = k->next)
	bindings = make_binding(k->id, NULL, TRUE, offset++, method, bindings);
    if (params->next_param)
	bindings = make_binding(params->next_param, NULL, TRUE, offset,
				method, bindings);

    method->nargs = offset+1;
    method->lexenv = make_lexenv(method, bindings, 0);

    analyze_body(method->body, method->lexenv);
}


/* Expression analysis */


static void analyze_varref_expr(struct varref_expr *expr,
				struct lexenv *lexenv)
{
    struct binding *binding
	= find_and_maybe_close_over(expr->var, lexenv, &expr->over);

    expr->home = lexenv->method;
    expr->binding = binding;

    if (binding == NULL && expr->var->symbol == sym_NextMethod)
	warn(expr->var->line, "next-method not bound -- Mindy requires #next");
}

static void analyze_literal_expr(struct literal_expr *expr,
				 struct lexenv *lexenv)
{
    /* Nothing to do. */
}

static void analyze_call_expr(struct call_expr *expr,
			      struct lexenv *lexenv)
{
    struct argument *arg;

    analyze_expr(expr->func, lexenv);
    for (arg = expr->args; arg != NULL; arg = arg->next)
	analyze_expr(arg->expr, lexenv);
}

static void analyze_method_expr(struct method_expr *expr,
				struct lexenv *lexenv)
{
    analyze_method(expr->method, lexenv);
}

static void analyze_dot_expr(struct dot_expr *expr,
			     struct lexenv *lexenv)
{
    analyze_expr(expr->arg, lexenv);
    analyze_expr(expr->func, lexenv);
}

static void analyze_body_expr(struct body_expr *expr,
			      struct lexenv *lexenv)
{
    analyze_body(expr->body, lexenv);
}

static void analyze_block_expr(struct block_expr *expr,
			       struct lexenv *lexenv)
{
    lose("block expression made it though expand?");
}

static void analyze_case_expr(struct case_expr *expr,
			      struct lexenv *lexenv)
{
    lose("case expression made it though expand?");
}

static void analyze_if_expr(struct if_expr *expr,
			    struct lexenv *lexenv)
{
    analyze_expr(expr->cond, lexenv);
    analyze_body(expr->consequent, lexenv);
    analyze_body(expr->alternate, lexenv);
}

static void analyze_for_expr(struct for_expr *expr,
			     struct lexenv *lexenv)
{
    lose("for expression made it though expand?");
}

static void analyze_select_expr(struct select_expr *expr,
				struct lexenv *lexenv)
{
    lose("select expression made it though expand?");
}

static void analyze_varset_expr(struct varset_expr *expr,
				struct lexenv *lexenv)
{
    struct binding *binding
	= find_and_maybe_close_over(expr->var, lexenv, &expr->over);

    if (binding) {
	binding->set = TRUE;
	if (binding->type) {
	    struct expr *type = make_varref(id(binding->type));
	    expr->type = (struct varref_expr *)type;
	    analyze_expr(type, lexenv);
	}
    }

    expr->home = lexenv->method;
    expr->binding = binding;

    analyze_expr(expr->value, lexenv);
}

static void analyze_binop_series_expr(struct binop_series_expr *expr,
				      struct lexenv *lexenv)
{
    lose("binop_series expression made it though expand?");
}

static void analyze_loop_expr(struct loop_expr *expr,
			      struct lexenv *lexenv)
{
    analyze_body(expr->body, lexenv);
}

static void analyze_repeat_expr(struct repeat_expr *expr,
				struct lexenv *lexenv)
{
    /* Nothing to do. */
}

static void analyze_error_expr(struct expr *expr, struct lexenv *lexenv)
{
    lose("Called environment on a parse tree with errors?");
}

static void (*ExprAnalyzers[])() = {
    analyze_varref_expr, analyze_literal_expr, analyze_call_expr,
    analyze_method_expr, analyze_dot_expr, analyze_body_expr,
    analyze_block_expr, analyze_case_expr, analyze_if_expr, analyze_for_expr,
    analyze_select_expr, analyze_varset_expr, analyze_binop_series_expr,
    analyze_loop_expr, analyze_repeat_expr, analyze_error_expr
};

static void analyze_expr(struct expr *expr, struct lexenv *lexenv)
{
    if (expr->analyzed)
	lose("Analyzing an expression we have already analyzed?");
    else {
	expr->analyzed = TRUE;
	(*ExprAnalyzers[(int)expr->kind])(expr, lexenv);
    }
}



/* Constituent analysis */

static void analyze_defconst_constituent(struct defconst_constituent *c,
					 struct lexenv *lexenv)
{
    analyze_method(c->tlf, lexenv);
}

static void analyze_defvar_constituent(struct defvar_constituent *c,
				       struct lexenv *lexenv)
{
    analyze_method(c->tlf, lexenv);
}

static void analyze_defmethod_constituent(struct defmethod_constituent *c,
					  struct lexenv *lexenv)
{
    analyze_method(c->tlf, lexenv);
}

static void analyze_defdomain_constituent(struct defdomain_constituent *c,
					  struct lexenv *lexenv)
{
    analyze_method(c->tlf, lexenv);
}

static void analyze_defgeneric_constituent(struct defgeneric_constituent *c,
					   struct lexenv *lexenv)
{
    analyze_method(c->tlf, lexenv);
}

static void analyze_defclass_constituent(struct defclass_constituent *c,
					 struct lexenv *lexenv)
{
    analyze_method(c->tlf1, lexenv);
    analyze_method(c->tlf2, lexenv);
}

static void analyze_expr_constituent(struct expr_constituent *c,
				     struct lexenv *lexenv)
{
    analyze_expr(c->expr, lexenv);
}

static void analyze_local_constituent(struct local_constituent *c,
				      struct lexenv *lexenv)
{
    struct method *home = lexenv->method;
    struct binding *bindings = NULL;
    struct binding **prev = &bindings;
    struct method *method;
    int offset = lexenv->depth;

    c->offset = offset;

    for (method = c->methods; method != NULL; method = method->next_local) {
	struct binding *new = make_binding(method->name, NULL, FALSE, offset++,
					   home, NULL);
	new->function = TRUE;
	new->set = TRUE;
	*prev = new;
	prev = &new->next;
    }

    if (offset > home->frame_size)
	home->frame_size = offset;

    *prev = lexenv->bindings;
    c->lexenv = make_lexenv(home, bindings, offset);

    for (method = c->methods; method != NULL; method = method->next_local)
	analyze_method(method, c->lexenv);

    analyze_body(c->body, c->lexenv);
}

static void analyze_handler_constituent(struct handler_constituent *c,
					struct lexenv *lexenv)
{
    analyze_body(c->body, lexenv);
}

static void analyze_let_constituent(struct let_constituent *let,
				    struct lexenv *lexenv)
{
    struct method *home = lexenv->method;
    struct binding *bindings = lexenv->bindings;
    int offset = lexenv->depth;
    int req = 0;
    struct param_list *params = let->bindings->params;
    struct param *param;

    analyze_expr(let->bindings->expr, lexenv);

    let->inside = bindings;
    let->offset = offset;

    for (param = params->required_params; param != NULL; param = param->next) {
	bindings = make_binding(param->id, param->type_temp, FALSE, offset++,
				home, bindings);
	req++;
    }
    let->required = req;

    if (params->rest_param)
	bindings = make_binding(params->rest_param, NULL, FALSE, offset++,
				home, bindings);

    let->lexenv = make_lexenv(home, bindings, offset);

    if (offset > home->frame_size)
	home->frame_size = offset;

    analyze_body(let->body, let->lexenv);
}

static void analyze_tlf_constituent(struct tlf_constituent *c,
				    struct lexenv *lexenv)
{
    analyze_method(c->form, lexenv);
}

static void analyze_error_constituent(struct constituent *c,
				      struct lexenv *lexenv)
{
    lose("Called environment on a parse tree with errors?");
}

static void analyze_defmodule_constituent(struct constituent *c,
					  struct lexenv *lexenv)
{
    /* Do nothing. */
}

static void analyze_deflibrary_constituent(struct constituent *c,
					   struct lexenv *lexenv)
{
    /* Do nothing. */
}

static void (*AnalyzeConstituents[])() = {
    analyze_defconst_constituent, analyze_defvar_constituent,
    analyze_defmethod_constituent, analyze_defdomain_constituent,
    analyze_defgeneric_constituent, analyze_defclass_constituent,
    analyze_expr_constituent, analyze_local_constituent,
    analyze_handler_constituent, analyze_let_constituent,
    analyze_tlf_constituent, analyze_error_constituent,
    analyze_defmodule_constituent, analyze_deflibrary_constituent
};

static void analyze_body(struct body *body, struct lexenv *lexenv)
{
    struct constituent *c;

    for (c = body->head; c != NULL; c = c->next)
	(*AnalyzeConstituents[(int)c->kind])(c, lexenv);
}

void environment_analysis(struct body *body)
{
    struct lexenv *lexenv = make_lexenv(NULL, NULL, 0);

    analyze_body(body, lexenv);

    free(lexenv);
}
