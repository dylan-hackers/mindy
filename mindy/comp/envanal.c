/**********************************************************************\
*
*  Copyright (C) 1994, Carnegie Mellon University
*  All rights reserved.
*
*  This code was produced by the Gwydion Project at Carnegie Mellon
*  University.  If you are interested in using this code, contact
*  "Scott.Fahlman@cs.cmu.edu" (Internet).
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/envanal.c,v 1.6 1994/04/14 19:16:12 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindycomp.h"
#include "src.h"
#include "lexenv.h"
#include "envanal.h"
#include "sym.h"
#include "lose.h"

static void analize_expr(struct expr *expr, struct lexenv *lexenv);
static void analize_body(struct body *body, struct lexenv *lexenv);


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

static void analize_method(struct method *method, struct lexenv *lexenv)
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

    analize_expr(method->specializers, lexenv);
    if (method->rettypes) {
	analize_expr(method->rettypes->req_types_list, lexenv);
	if (method->rettypes->rest_type)
	    analize_expr(method->rettypes->rest_type, lexenv);
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

    analize_body(method->body, method->lexenv);
}


/* Expression analysis */


static void analize_varref_expr(struct varref_expr *expr,
				struct lexenv *lexenv)
{
    struct binding *binding
	= find_and_maybe_close_over(expr->var, lexenv, &expr->over);

    expr->home = lexenv->method;
    expr->binding = binding;

    if (binding == NULL && expr->var->symbol == symbol("next-method"))
	warn(expr->var->line, "next-method not bound -- Mindy requires #next");
}

static void analize_literal_expr(struct literal_expr *expr,
				 struct lexenv *lexenv)
{
    /* Nothing to do. */
}

static void analize_call_expr(struct call_expr *expr,
			      struct lexenv *lexenv)
{
    struct argument *arg;

    analize_expr(expr->func, lexenv);
    for (arg = expr->args; arg != NULL; arg = arg->next)
	analize_expr(arg->expr, lexenv);
}

static void analize_method_expr(struct method_expr *expr,
				struct lexenv *lexenv)
{
    analize_method(expr->method, lexenv);
}

static void analize_dot_expr(struct dot_expr *expr,
			     struct lexenv *lexenv)
{
    analize_expr(expr->arg, lexenv);
    analize_expr(expr->func, lexenv);
}

static void analize_body_expr(struct body_expr *expr,
			      struct lexenv *lexenv)
{
    analize_body(expr->body, lexenv);
}

static void analize_block_expr(struct block_expr *expr,
			       struct lexenv *lexenv)
{
    lose("block expression made it though expand?");
}

static void analize_case_expr(struct case_expr *expr,
			      struct lexenv *lexenv)
{
    lose("case expression made it though expand?");
}

static void analize_if_expr(struct if_expr *expr,
			    struct lexenv *lexenv)
{
    analize_expr(expr->cond, lexenv);
    analize_body(expr->consequent, lexenv);
    analize_body(expr->alternate, lexenv);
}

static void analize_for_expr(struct for_expr *expr,
			     struct lexenv *lexenv)
{
    lose("for expression made it though expand?");
}

static void analize_select_expr(struct select_expr *expr,
				struct lexenv *lexenv)
{
    lose("select expression made it though expand?");
}

static void analize_varset_expr(struct varset_expr *expr,
				struct lexenv *lexenv)
{
    struct binding *binding
	= find_and_maybe_close_over(expr->var, lexenv, &expr->over);

    if (binding) {
	binding->set = TRUE;
	if (binding->type) {
	    struct expr *type = make_varref(id(binding->type));
	    expr->type = (struct varref_expr *)type;
	    analize_expr(type, lexenv);
	}
    }

    expr->home = lexenv->method;
    expr->binding = binding;

    analize_expr(expr->value, lexenv);
}

static void analize_binop_series_expr(struct binop_series_expr *expr,
				      struct lexenv *lexenv)
{
    lose("binop_series expression made it though expand?");
}

static void analize_loop_expr(struct loop_expr *expr,
			      struct lexenv *lexenv)
{
    analize_body(expr->body, lexenv);
}

static void analize_repeat_expr(struct repeat_expr *expr,
				struct lexenv *lexenv)
{
    /* Nothing to do. */
}

static void analize_error_expr(struct expr *expr, struct lexenv *lexenv)
{
    lose("Called environment on a parse tree with errors?");
}

static void (*ExprAnalizers[])() = {
    analize_varref_expr, analize_literal_expr, analize_call_expr,
    analize_method_expr, analize_dot_expr, analize_body_expr,
    analize_block_expr, analize_case_expr, analize_if_expr, analize_for_expr,
    analize_select_expr, analize_varset_expr, analize_binop_series_expr,
    analize_loop_expr, analize_repeat_expr, analize_error_expr
};

static void analize_expr(struct expr *expr, struct lexenv *lexenv)
{
    if (expr->analized)
	lose("Analizing an expression we have already analized?");
    else {
	expr->analized = TRUE;
	(*ExprAnalizers[(int)expr->kind])(expr, lexenv);
    }
}



/* Constituent analysis */

static void analize_defconst_constituent(struct defconst_constituent *c,
					 struct lexenv *lexenv)
{
    analize_method(c->tlf, lexenv);
}

static void analize_defvar_constituent(struct defvar_constituent *c,
				       struct lexenv *lexenv)
{
    analize_method(c->tlf, lexenv);
}

static void analize_defmethod_constituent(struct defmethod_constituent *c,
					  struct lexenv *lexenv)
{
    analize_method(c->tlf, lexenv);
}

static void analize_defgeneric_constituent(struct defgeneric_constituent *c,
					   struct lexenv *lexenv)
{
    analize_method(c->tlf, lexenv);
}

static void analize_defclass_constituent(struct defclass_constituent *c,
					 struct lexenv *lexenv)
{
    analize_method(c->tlf, lexenv);
}

static void analize_expr_constituent(struct expr_constituent *c,
				     struct lexenv *lexenv)
{
    analize_expr(c->expr, lexenv);
}

static void analize_local_constituent(struct local_constituent *c,
				      struct lexenv *lexenv)
{
    struct method *home = lexenv->method;
    struct binding *bindings = lexenv->bindings;
    struct method *method;
    int offset = lexenv->depth;

    c->offset = offset;

    for (method = c->methods; method != NULL; method = method->next_local) {
	bindings = make_binding(method->name, NULL, FALSE, offset++, home,
				bindings);
	bindings->function = TRUE;
	bindings->set = TRUE;
    }

    if (offset > home->frame_size)
	home->frame_size = offset;

    c->lexenv = make_lexenv(home, bindings, offset);

    for (method = c->methods; method != NULL; method = method->next_local)
	analize_method(method, c->lexenv);

    analize_body(c->body, c->lexenv);
}

static void analize_handler_constituent(struct handler_constituent *c,
					struct lexenv *lexenv)
{
    analize_body(c->body, lexenv);
}

static void analize_let_constituent(struct let_constituent *let,
				    struct lexenv *lexenv)
{
    struct method *home = lexenv->method;
    struct binding *bindings = lexenv->bindings;
    int offset = lexenv->depth;
    int req = 0;
    struct param_list *params = let->bindings->params;
    struct param *param;

    analize_expr(let->bindings->expr, lexenv);

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

    analize_body(let->body, let->lexenv);
}

static void analize_tlf_constituent(struct tlf_constituent *c,
				    struct lexenv *lexenv)
{
    analize_method(c->form, lexenv);
}

static void analize_error_constituent(struct constituent *c,
				      struct lexenv *lexenv)
{
    lose("Called environment on a parse tree with errors?");
}

static void analize_defmodule_constituent(struct constituent *c,
					  struct lexenv *lexenv)
{
    /* Do nothing. */
}

static void analize_deflibrary_constituent(struct constituent *c,
					   struct lexenv *lexenv)
{
    /* Do nothing. */
}

static void (*AnalizeConstituents[])() = {
    analize_defconst_constituent, analize_defvar_constituent,
    analize_defmethod_constituent, analize_defgeneric_constituent,
    analize_defclass_constituent, analize_expr_constituent,
    analize_local_constituent, analize_handler_constituent,
    analize_let_constituent, analize_tlf_constituent,
    analize_error_constituent, analize_defmodule_constituent,
    analize_deflibrary_constituent
};

static void analize_body(struct body *body, struct lexenv *lexenv)
{
    struct constituent *c;

    for (c = body->head; c != NULL; c = c->next)
	(*AnalizeConstituents[(int)c->kind])(c, lexenv);
}

void environment_analysis(struct body *body)
{
    struct lexenv *lexenv = make_lexenv(NULL, NULL, 0);

    analize_body(body, lexenv);

    free(lexenv);
}
