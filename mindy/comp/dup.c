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
* $Header: /scm/cvs/src/mindy/comp/dup.c,v 1.1 1998/05/03 19:55:06 andreas Exp $
*
* This file duplicates parts of the parse tree.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "src.h"
#include "literal.h"
#include "free.h"
#include "lose.h"
#include "dup.h"


/* Utilities. */

#if 0
static struct param_list *dup_params(struct param_list *params)
{
    struct param_list *res = make_param_list();
    struct param *param, **param_ptr;

    param_ptr = &res->required_params;
    for (param = params->required_params; param != NULL; param = param->next) {
	struct expr *type = param->type ? dup_expr(param->type) : NULL;
	struct param *new = make_param(dup_id(param->id), type);
	*param_ptr = new;
	param_ptr = &new->next;
    }
    *param_ptr = NULL;

    if (params->next_param)
	res->next_param = dup_id(params->next_param);
    if (params->rest_param)
	res->rest_param = dup_id(params->rest_param);

    if (params->allow_keys) {
	struct keyword_param *key;

	res->allow_keys = TRUE;
	for (key = params->keyword_params; key != NULL; key = key->next) {
	    struct expr *type = key->type ? dup_expr(key->type) : NULL;
	    struct expr *def = key->def ? dup_expr(key->def) : NULL;
	    struct keyword_param *new
		= make_keyword_param(key->keyword, dup_id(key->id), type, def);
	    add_keyword_param(res, new);
	}
    }

    return res;
}

static struct bindings *dup_bindings(struct bindings *bindings)
{
    return make_bindings(dup_params(bindings->params),
			 bindings->expr ? dup_expr(bindings->expr) : NULL);
}

static struct return_type_list *dup_rettypes(struct return_type_list *rettypes)
{
    struct return_type_list *res = make_return_type_list();
    struct return_type *r, *new;

    for (r = rettypes->req_types; r != NULL; r = next) {
	new = malloc(sizeof(*new));
	new->type = r->type ? dup_expr(r->type) : NULL;
	new->temp = r->temp;
	*nes->req_types_tail = new;
	nes->req_types_tail = &new->next;
    }
    *req_types_tail = NULL;

    if (rettypes->req_types_list)
	res->req_types_list = dup_expr(rettypes->req_types_list);
    if (rettypes->rest_type)
	res->rest_type = dup_expr(rettypes->rest_type);

    return res;
}

static void dup_plist(struct plist *plist)
{
    struct plist *res = make_property_list();
    struct property *prop, *new;

    for (prop = plist->head; prop != NULL; prop = prop->next) {
	new = malloc(sizeof(*new));
	new->keyword = prop->keyword;
	new->expr = dup_expr(prop->expr);
	*res->tail = new;
	res->tail = &new->next;
    }
    *res->tail = NULL;

    return res;
}

static struct condition_body *dup_condition_body(struct condition_body *body)
{
    if (body) {
	struct condition_body *res = malloc(sizeof(*res));
	struct condition *cond, **prev;

	res->clause = malloc(sizeof(*res->clause));
	prev = &res->clause->conditions;
	for (cond = body->clause->conditions; cond!=NULL; cond = cond->next) {
	    struct condition *new = malloc(sizeof(*new));
	    new->cond = dup_expr(cond->expr);
	    *prev = new;
	    prev = &new->next;
	}
	*prev = NULL;
	res->clause->body = dup_body(body->clause->body);

	res->next = dup_condition_body(body->next);

	return res;
    }
    else
	return NULL;
}

static struct method *dup_method(struct method *method)
{
    struct method *res
	= make_method_description(dup_params(method->params),
				  dup_rettypes(method->rettypes),
				  dup_body(method->body));
    if (method->name)
	res->name = dup_id(method->name);
    if (method->debug_name)
	res->debug_name = dup_literal(method->debug_name);
    if (method->specializers)
	res->specializers = dup_expr(method->specializers);

    return res;
}
#endif


/* Stuff to dup expressions. */

static struct expr *dup_varref_expr(struct varref_expr *e)
{
    return make_varref(dup_id(e->var));
}

static struct expr *dup_literal_expr(struct literal_expr *e)
{
    return make_literal_ref(dup_literal(e->lit));
}

static struct expr *dup_call_expr(struct call_expr *e)
{
    struct arglist *args = make_argument_list();
    struct argument *arg;
    struct expr *func = dup_expr(e->func);

    if (func == NULL)
	return NULL;

    for (arg = e->args; arg != NULL; arg = arg->next) {
	struct expr *new = dup_expr(arg->expr);
	if (new == NULL) {
	    free_expr(make_function_call(func, args));
	    return NULL;
	}
	add_argument(args, make_argument(new));
    }
    return make_function_call(func, args);
}

static struct expr *dup_method_expr(struct method_expr *e)
{
    return NULL;
#if 0
    return make_method_ref(dup_method(e->method));
#endif
}

static struct expr *dup_dot_expr(struct dot_expr *e)
{
    struct expr *arg, *func;

    if ((arg = dup_expr(e->arg)) == NULL)
	return NULL;
    if ((func = dup_expr(e->func)) == NULL) {
	free_expr(arg);
	return NULL;
    }

    return make_dot_operation(arg, func);
}

static struct expr *dup_body_expr(struct body_expr *e)
{
    struct body *new = dup_body(e->body);

    if (new == NULL)
	return NULL;
    else
	return make_body_expr(new);
}

#if 0
static void dup_exception_clauses(struct exception_clause *clauses)
{
    struct exception_clause *clause, *next;

    lose("###");

    for (clause = clauses; clause != NULL; clause = next) {
	dup_expr(clause->type);
	if (clause->condition)
	    dup_id(clause->condition);
	dup_plist(clause->plist);
	dup_body(clause->body);
	next = clause->next;
	dup(clause);
    }
}
#endif

static struct expr *dup_block_expr(struct block_expr *e)
{
    return NULL;
#if 0
    struct block_expr *res
	= (struct block_expr *)make_block(e->line,
					  NULL,
					  dup_body(e->body),
					  NULL);

    if (e->exit_fun)
	res->exit_fun = dup_id(e->exit_fun);
    res->inner = dup_exception_clauses(e->inner);
    if (e->cleanup)
	res->cleanup = dup_body(e->cleanup);
    res->outer = dup_exception_clauses(e->outer);

    return (struct expr *)res;
#endif
}

static struct expr *dup_case_expr(struct case_expr *e)
{
    return NULL;
#if 0
    return make_case(dup_condition_body(e->body));
#endif
}

static struct expr *dup_if_expr(struct if_expr *e)
{
    struct expr *cond;
    struct body *consequent;
    struct body *alternate;

    if ((cond = dup_expr(e->cond)) == NULL)
	return NULL;
    if ((consequent = dup_body(e->consequent)) == NULL) {
	free_expr(cond);
	return NULL;
    }
    if ((alternate = dup_body(e->alternate)) == NULL) {
	free_expr(cond);
	free_body(consequent);
	return NULL;
    }
    return make_if(cond, consequent, make_else(e->else_line, alternate));
}

static struct expr *dup_for_expr(struct for_expr *e)
{
    return NULL;
#if 0
    struct for_clause *clause, *next;

    lose("###");

    for (clause = e->clauses; clause != NULL; clause = next) {
	dup_params(clause->vars);
	switch (clause->kind) {
	  case for_EQUAL_THEN:
	    {
		struct equal_then_for_clause *c
		    = (struct equal_then_for_clause *)clause;
		dup_expr(c->equal);
		dup_expr(c->then);
		break;
	    }
	  case for_IN:
	    {
		struct in_for_clause *c = (struct in_for_clause *)clause;
		dup_expr(c->collection);
		break;
	    }
	  case for_FROM:
	    {
		struct from_for_clause *c = (struct from_for_clause *)clause;
		dup_expr(c->from);
		if (c->to)
		    dup_expr(c->to);
		if (c->by)
		    dup_expr(c->by);
	    }
	}
	next = clause->next;
	dup(clause);
    }
    if (e->until)
	dup_expr(e->until);
    dup_body(e->body);
    if (e->finally)
	dup_body(e->finally);
    dup(e);
#endif
}

static struct expr *dup_select_expr(struct select_expr *e)
{
    return NULL;
#if 0
    return make_select(dup_expr(e->expr),
		       e->by ? dup_expr(e->by) : NULL,
		       dup_condition_body(e->body));
#endif
}

static struct expr *dup_varset_expr(struct varset_expr *e)
{
    struct expr *value = dup_expr(e->value);

    if (value != NULL)
	return make_varset(dup_id(e->var), value);
    else
	return NULL;
}

static struct expr *dup_binop_series_expr(struct binop_series_expr *e)
{
    return NULL;
}

static struct expr *dup_loop_expr(struct loop_expr *e)
{
    return NULL;
#if 0
    return make_loop(dup_body(e->body));
#endif
}

static struct expr *dup_repeat_expr(struct repeat_expr *e)
{
    return NULL;
#if 0
    return make_repeat();
#endif
}

static struct expr *dup_error_expr(struct expr *e)
{
    return NULL;
#if 0
    return make_error_expr();
#endif
}

static struct expr *(*ExprDuprs[(int)expr_Kinds])() = {
    dup_varref_expr, dup_literal_expr, dup_call_expr,
    dup_method_expr, dup_dot_expr, dup_body_expr, dup_block_expr,
    dup_case_expr, dup_if_expr, dup_for_expr, dup_select_expr,
    dup_varset_expr, dup_binop_series_expr, dup_loop_expr,
    dup_repeat_expr, dup_error_expr
};

struct expr *dup_expr(struct expr *e)
{
    return (*ExprDuprs[(int)e->kind])(e);
}


/* Stuff to dup constituents. */

struct constituent *dup_constituent(struct constituent *c)
{
    if (c->kind == constituent_EXPR) {
	struct expr *expr = dup_expr(((struct expr_constituent *)c)->expr);

	if (expr == NULL)
	    return NULL;
	else
	    return make_expr_constituent(expr);
    }
    else
	return NULL;
}

struct body *dup_body(struct body *body)
{
    struct body *res = make_body();
    struct constituent *ptr, **prev;

    prev = &res->head;

    for (ptr = body->head; ptr != NULL; ptr = ptr->next) {
	struct constituent *new = dup_constituent(ptr);
	if (new == NULL) {
	    *prev = NULL;
	    free_body(res);
	    return NULL;
	}
	*prev = new;
	prev = &new->next;
    }
    *prev = NULL;

    return res;
}
