/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* This file frees parts of the parse tree.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "src.h"
#include "literal.h"
#include "free.h"
#include "lose.h"


/* Utilities. */

static void free_id(struct id *id)
{
    free(id);
}

static void free_param(struct param *param)
{
    free_id(param->id);
    if (param->type)
	free_expr(param->type);
    free(param);
}

static void free_params(struct param_list *params)
{
    struct param *param, *next_param;
    struct keyword_param *key, *next_key;

    for (param = params->required_params; param != NULL; param = next_param) {
	free_id(param->id);
	if (param->type)
	    free_expr(param->type);
	next_param = param->next;
	free(param);
    }
    if (params->next_param)
	free_id(params->next_param);
    if (params->rest_param)
	free_id(params->rest_param);
    for (key = params->keyword_params; key != NULL; key = next_key) {
	free_id(key->id);
	if (key->type)
	    free_expr(key->type);
	if (key->def)
	    free_expr(key->def);
	next_key = key->next;
	free(key);
    }
    free(params);
}

static void free_bindings(struct bindings *bindings)
{
    free_params(bindings->params);
    if (bindings->expr)
	free_expr(bindings->expr);
    free(bindings);
}

static void free_rettypes(struct return_type_list *rettypes)
{
    struct return_type *r, *next;

    for (r = rettypes->req_types; r != NULL; r = next) {
	if (r->type)
	    free_expr(r->type);
	next = r->next;
	free(r);
    }
    if (rettypes->req_types_list)
	free_expr(rettypes->req_types_list);
    if (rettypes->rest_type)
	free_expr(rettypes->rest_type);
    if (rettypes->rest_temp_varref)
	free_expr(rettypes->rest_temp_varref);
    free(rettypes);
}

static void free_plist(struct plist *plist)
{
    struct property *prop, *next;

    for (prop = plist->head; prop != NULL; prop = next) {
	free_expr(prop->expr);
	next = prop->next;
	free(prop);
    }
    free(plist);
}

static void free_condition_body(struct condition_body *body)
{
    struct condition_body *next;
    struct condition_clause *clause;
    struct condition *cond, *next_cond;

    while (body != NULL) {
	clause = body->clause;
	for (cond = clause->conditions; cond != NULL; cond = next_cond) {
	    free_expr(cond->cond);
	    next_cond = cond->next;
	    free(cond);
	}
	free_body(clause->body);
	free(clause);
	next = body->next;
	free(body);
	body = next;
    }
}

static void free_method(struct method *method)
{
    if (method->name)
	free_id(method->name);
    if (method->debug_name)
	free_literal(method->debug_name);
    free_params(method->params);
    if (method->specializers)
	free_expr(method->specializers);
    if (method->rettypes)
	free_rettypes(method->rettypes);
    free_body(method->body);
    /* ### Free the lexenv? */
    free(method);
}



/* Stuff to free expressions. */

static void free_varref_expr(struct varref_expr *e)
{
    free_id(e->var);
    free(e);
}

static void free_literal_expr(struct literal_expr *e)
{
    free_literal(e->lit);
    free(e);
}

static void free_call_expr(struct call_expr *e)
{
    struct argument *arg, *next;

    free_expr(e->func);
    for (arg = e->args; arg != NULL; arg = next) {
	next = arg->next;
	free_expr(arg->expr);
	free(arg);
    }
    free(e);
}

static void free_method_expr(struct method_expr *e)
{
    free_method(e->method);
    free(e);
}

static void free_dot_expr(struct dot_expr *e)
{
    free_expr(e->arg);
    free_expr(e->func);
    free(e);
}

static void free_body_expr(struct body_expr *e)
{
    free_body(e->body);
    free(e);
}

static void free_exception_clauses(struct exception_clause *clauses)
{
    struct exception_clause *clause, *next;

    for (clause = clauses; clause != NULL; clause = next) {
	free_expr(clause->type);
	if (clause->condition)
	    free_id(clause->condition);
	free_plist(clause->plist);
	free_body(clause->body);
	next = clause->next;
	free(clause);
    }
}

static void free_block_expr(struct block_expr *e)
{
    if (e->exit_fun)
	free_id(e->exit_fun);
    free_body(e->body);
    free_exception_clauses(e->inner);
    if (e->cleanup)
	free_body(e->cleanup);
    free_exception_clauses(e->outer);
    free(e);
}

static void free_case_expr(struct case_expr *e)
{
    free_condition_body(e->body);
    free(e);
}

static void free_if_expr(struct if_expr *e)
{
    free_expr(e->cond);
    free_body(e->consequent);
    free_body(e->alternate);
    free(e);
}

static void free_for_expr(struct for_expr *e)
{
    struct for_clause *clause, *next;

    for (clause = e->clauses; clause != NULL; clause = next) {
	free_params(clause->vars);
	switch (clause->kind) {
	  case for_EQUAL_THEN:
	    {
		struct equal_then_for_clause *c
		    = (struct equal_then_for_clause *)clause;
		free_expr(c->equal);
		free_expr(c->then);
		break;
	    }
	  case for_IN:
	    {
		struct in_for_clause *c = (struct in_for_clause *)clause;
		free_expr(c->collection);
		if (c->protocol) 
		    free_param(c->protocol);
		break;
	    }
	  case for_FROM:
	    {
		struct from_for_clause *c = (struct from_for_clause *)clause;
		free_expr(c->from);
		if (c->to)
		    free_expr(c->to);
		if (c->by)
		    free_expr(c->by);
		break;
	    }
	  default:
	    lose("Bogus for clause kind.");
	}
	next = clause->next;
	free(clause);
    }
    if (e->until)
	free_expr(e->until);
    free_body(e->body);
    if (e->finally)
	free_body(e->finally);
    free(e);
}

static void free_select_expr(struct select_expr *e)
{
    free_expr(e->expr);
    if (e->by)
	free_expr(e->by);
    free_condition_body(e->body);
    free(e);
}

static void free_varset_expr(struct varset_expr *e)
{
    free_id(e->var);
    free_expr(e->value);
    free(e);
}

static void free_binop_series_expr(struct binop_series_expr *e)
{
    struct binop *b, *next;

    free_expr(e->first_operand);
    for (b = e->first_binop; b != NULL; b = next) {
	free_id(b->op);
	free_expr(b->operand);
	next = b->next;
	free(b);
    }
    free(e);
}

static void free_loop_expr(struct loop_expr *e)
{
    free_body(e->body);
    free(e);
}

static void free_repeat_expr(struct repeat_expr *e)
{
    free(e);
}

static void free_error_expr(struct expr *e)
{
    free(e);
}

static void (*ExprFreers[(int)expr_Kinds])() = {
    free_varref_expr, free_literal_expr, free_call_expr,
    free_method_expr, free_dot_expr, free_body_expr, free_block_expr,
    free_case_expr, free_if_expr, free_for_expr, free_select_expr,
    free_varset_expr, free_binop_series_expr, free_loop_expr,
    free_repeat_expr, free_error_expr
};

void free_expr(struct expr *e)
{
    (*ExprFreers[(int)e->kind])(e);
}


/* Stuff to free constituents. */

static void free_defconst_constituent(struct defconst_constituent *c)
{
    free_bindings(c->bindings);
    if (c->tlf)
	free_method(c->tlf);
    free(c);
}

static void free_defvar_constituent(struct defvar_constituent *c)
{
    free_bindings(c->bindings);
    if (c->tlf)
	free_method(c->tlf);
    free(c);
}

static void free_defmethod_constituent(struct defmethod_constituent *c)
{
    if (c->tlf)
	free_method(c->tlf);
    else
	free_method(c->method);
    free(c);
}

static void free_defdomain_constituent(struct defdomain_constituent *c)
{
    free_id(c->name);

    if (c->types) {
	struct argument *arg, *next;

	for (arg = c->types; arg != NULL; arg = next) {
	    next = arg->next;
	    free_expr(arg->expr);
	    free(arg);
	}
    }

    if (c->tlf)
	free_method(c->tlf);

    free(c);
}

static void free_defgeneric_constituent(struct defgeneric_constituent *c)
{
    free_id(c->name);
    if (c->params)
	free_params(c->params);
    if (c->rettypes)
	free_rettypes(c->rettypes);
    if (c->plist)
	free_plist(c->plist);
    if (c->tlf)
	free_method(c->tlf);
    free(c);
}

static void free_defclass_constituent(struct defclass_constituent *c)
{
    /* ### free_defclass_constituent not written yet. */
}

static void free_expr_constituent(struct expr_constituent *c)
{
    free_expr(c->expr);
    free(c);
}

static void free_local_constituent(struct local_constituent *c)
{
    struct method *method, *next;

    free_body(c->body);
    for (method = c->methods; method != NULL; method = next) {
	next = method->next_local;
	free_method(method);
    }
    /* ### free the lexenv? */
    free(c);
}

static void free_handler_constituent(struct handler_constituent *c)
{
    free_body(c->body);
    if (c->type)
	free_expr(c->type);
    if (c->func)
	free_expr(c->func);
    if (c->plist)
	free_plist(c->plist);
    free(c);
}

static void free_let_constituent(struct let_constituent *c)
{
    free_body(c->body);
    free_bindings(c->bindings);
    /* ### Free c->lexenv? */
    free(c);
}

static void free_tlf_constituent(struct tlf_constituent *c)
{
    free_method(c->form);
    free(c);
}

static void free_error_constituent(struct constituent *c)
{
    free(c);
}

static void free_var_names(struct variable_names *names)
{
    struct variable_name *v, *next;

    for (v = names->head; v != NULL; v = next) {
	
	next = v->next;
	free(v);
    }
    free(names);
}

static void free_renamings(struct renamings *renamings)
{
    struct renaming *ren, *next;

    for (ren = renamings->head; ren != NULL; ren = next) {
	free_literal(ren->from);
	free_literal(ren->to);
	next = ren->next;
	free(ren);
    }
    free(renamings);
}

static void free_defnamespace_constituent(struct defnamespace_constituent *c)
{
    struct use_clause *use, *next;

    free_literal(c->name);

    for (use = c->use_clauses; use != NULL; use = next) {
	struct use_option *opt, *next_opt;

	for (opt = use->options; opt != NULL; opt = next_opt) {
	    switch (opt->kind) {
	      case useopt_PREFIX:
		free_literal(((struct prefix_option *)opt)->prefix);
		break;
	      case useopt_IMPORT:
		free_var_names(((struct import_option *)opt)->vars);
		free_renamings(((struct import_option *)opt)->renames);
		break;
	      case useopt_EXCLUDE:
		free_var_names(((struct exclude_option *)opt)->vars);
		break;
	      case useopt_RENAME:
		free_renamings(((struct rename_option *)opt)->renames);
		break;
	      case useopt_EXPORT:
		free_var_names(((struct export_option *)opt)->vars);
		break;
	      case useopt_IMPORT_ALL:
	      case useopt_EXPORT_ALL:
		break;
	    }
	    next_opt = opt->next;
	    free(opt);
	}

	if (use->import)
	    free_literal(use->import);
	if (use->exclude)
	    free_literal(use->exclude);
	if (use->prefix)
	    free_literal(use->prefix);
	if (use->rename)
	    free_literal(use->rename);
	if (use->export)
	    free_literal(use->export);

	next = use->next;

	free(use);
    }

    if (c->exported_variables)
	free_var_names(c->exported_variables);
    if (c->created_variables)
	free_var_names(c->created_variables);

    if (c->exported_literal)
	free_literal(c->exported_literal);
    if (c->created_literal)
	free_literal(c->created_literal);
}

static void (*ConstituentFreers[(int)constituent_Kinds])() = {
    free_defconst_constituent, free_defvar_constituent,
    free_defmethod_constituent, free_defdomain_constituent,
    free_defgeneric_constituent, free_defclass_constituent,
    free_expr_constituent, free_local_constituent, free_handler_constituent,
    free_let_constituent, free_tlf_constituent, free_error_constituent,
    free_defnamespace_constituent, free_defnamespace_constituent
};

void free_constituent(struct constituent *c)
{
    (*ConstituentFreers[(int)c->kind])(c);
}

void free_body(struct body *body)
{
    struct constituent *ptr, *next;

    for (ptr = body->head; ptr != NULL; ptr = next) {
	next = ptr->next;
	free_constituent(ptr);
    }
    free(body);
}
