/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
* $Header: /scm/cvs/src/mindy/comp/expand.c,v 1.5 2002/08/27 21:34:18 gabor Exp $
*
* This file does source-to-source expansions.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "src.h"
#include "literal.h"
#include "dup.h"
#include "free.h"
#include "sym.h"
#include "expand.h"
#include "info.h"
#include "lose.h"

static void expand_expr(struct expr **ptr);
static void expand_body(struct body *body, boolean top_level);


/* Utilities */

static struct body *chain_bodies(struct body *body1, struct body *body2)
{
    if (body1->head == NULL) {
	free(body1);
	return body2;
    }
    else {
	if (body2->head != NULL) {
	    *body1->tail = body2->head;
	    body1->tail = body2->tail;
	}
	free(body2);
	return body1;
    }
}

static void bind_params(struct body *body, struct param_list *vars,
			struct expr *expr)
{
    add_constituent(body, make_let(make_bindings(vars, expr)));
}

static void bind_param(struct body *body, struct param *var, struct expr *expr)
{
    bind_params(body, push_param(var, make_param_list()), expr);
}

static void bind_temp(struct body *body, struct id *id, struct expr *expr)
{
    bind_param(body, make_param(id, NULL), expr);
}

static void add_expr(struct body *body, struct expr *expr)
{
    add_constituent(body, make_expr_constituent(expr));
}

static void expand_param_list(struct param_list *params)
{
    struct param *p;
    struct keyword_param *k;

    for (p = params->required_params; p != NULL; p = p->next)
	if (p->type)
	    expand_expr(&p->type);
    for (k = params->keyword_params; k != NULL; k = k->next) {
	if (k->type)
	    expand_expr(&k->type);
	if (k->def)
	    expand_expr(&k->def);
    }
}

static void expand_bindings(struct bindings *bindings)
{
    expand_param_list(bindings->params);
    expand_expr(&bindings->expr);
}

static void bind_rettypes(struct body *body,
			  struct return_type_list *rettypes)
{
    struct return_type *r;
    struct arglist *list_args = make_argument_list();
    struct symbol *ctype = sym_CheckType;
    struct symbol *type_class = sym_Type;
    struct symbol *object = sym_Object;

    for (r = rettypes->req_types; r != NULL; r = r->next) {
	if (r->type) {
	    struct arglist *args = make_argument_list();
	    struct expr *type;

	    add_argument(args, make_argument(r->type));
	    r->type = NULL;
	    add_argument(args, make_argument(make_varref(id(type_class))));
	    type = make_function_call(make_varref(id(ctype)), args);
	    r->temp = gensym();
	    bind_temp(body, id(r->temp), type);
	    add_argument(list_args,make_argument(make_varref(id(r->temp))));
	}
	else
	    add_argument(list_args, make_argument(make_varref(id(object))));
    }
    rettypes->req_types_list
	= make_function_call(make_varref(id(sym_List)), list_args);

    if (rettypes->rest_type) {
	struct arglist *args = make_argument_list();
	add_argument(args, make_argument(rettypes->rest_type));
	rettypes->rest_type = NULL;
	add_argument(args, make_argument(make_varref(id(type_class))));
	rettypes->rest_temp = gensym();
	bind_temp(body, id(rettypes->rest_temp),
		  make_function_call(make_varref(id(ctype)), args));
	rettypes->rest_temp_varref = make_varref(id(rettypes->rest_temp));
    }
}

static void add_plist_arguments(struct arglist *args, struct plist *plist)
{
    struct property *prop, *next;

    for (prop = plist->head; prop != NULL; prop = next) {
	struct literal *key = make_symbol_literal(prop->keyword);
	add_argument(args, make_argument(make_literal_ref(key)));
	add_argument(args, make_argument(prop->expr));
	next = prop->next;
	free(prop);
    }
    free(plist);
}

static void change_to_setter(struct id *id)
{
    static char buf[256];
    char *ptr;
    struct symbol *sym = id->symbol;
    int len = strlen((char *)sym->name);

    if (len + 8 > sizeof(buf))
	ptr = malloc(len + 8);
    else
	ptr = buf;

    strcpy(ptr, (char *)sym->name);
    strcpy(ptr+len, "-setter");

    id->symbol = symbol(ptr);

    if (ptr != buf)
	free(ptr);
}

static struct argument *make_find_var_arg(struct id *var)
{
    struct arglist *args = make_argument_list();
    struct expr *expr;

    add_argument(args, make_argument(make_varref(dup_id(var))));
    expr = make_function_call(make_varref(id(sym_FindVariable)), args);

    return make_argument(expr);
}


/* Method expander */

static void add_method_wrap(struct body *body, struct method *method)
{
    struct param_list *params = method->params;
    struct param *p;
    struct keyword_param *k;
    struct arglist *list_args = make_argument_list();
    struct symbol *ctype = sym_CheckType;
    struct symbol *type_class = sym_Type;

    for (p = params->required_params; p != NULL; p = p->next) {
	if (p->type) {
	    struct arglist *args = make_argument_list();
	    struct expr *expr;

	    p->type_temp = gensym();
	    add_argument(args, make_argument(p->type));
	    add_argument(args, make_argument(make_varref(id(type_class))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	    bind_temp(body, id(p->type_temp), expr);
	    p->type = NULL;
	    expr = make_varref(id(p->type_temp));
	    add_argument(list_args, make_argument(expr));
	}
	else {
	    struct expr *expr = make_varref(id(sym_Object));
	    add_argument(list_args, make_argument(expr));
	}
    }
    method->specializers
	= make_function_call(make_varref(id(sym_List)), list_args);

    for (k = params->keyword_params; k != NULL; k = k->next) {
	if (k->type) {
	    struct arglist *args = make_argument_list();
	    struct expr *expr;

	    k->type_temp = gensym();
	    add_argument(args, make_argument(k->type));
	    add_argument(args, make_argument(make_varref(id(type_class))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	    bind_temp(body, id(k->type_temp), expr);
	    k->type = NULL;
	}
    }

    if (method->rettypes)
	bind_rettypes(body, method->rettypes);
}

static void bind_next_param(struct body *body, struct param_list *params)
{
    struct symbol *temp = gensym();
    struct arglist *args;
    struct expr *expr;
    struct param *p;

    /* Make sure there is a #rest parameter if there are #key params. */
    if (params->allow_keys && params->rest_param == NULL)
	params->rest_param = id(gensym());

    /* Build the argument list for the call to make-next-method-function */
    args = make_argument_list();
    expr = make_varref(id(sym_MakeNextMethodFunction));

    /* If there is a #rest param, we are going to be calling apply */
    if (params->rest_param)
	add_argument(args, make_argument(expr));

    /* Pass the list of next methods as the first argument. */
    add_argument(args, make_argument(make_varref(id(temp))));

    /* Pass all the required params. */
    for (p = params->required_params; p != NULL; p = p->next)
	add_argument(args, make_argument(make_varref(dup_id(p->id))));

    if (params->rest_param) {
	/* Pass the rest param, and call apply. */
	add_argument(args,
		     make_argument(make_varref(dup_id(params->rest_param))));
	expr = make_function_call(make_varref(id(sym_Apply)), args);
    }
    else
	/* Just call make-next-method-function */
	expr = make_function_call(expr, args);

    /* Bind the original next_param to the results of make-next-method-fun */
    bind_temp(body, params->next_param, expr);

    /* Change the next_param to the temp. */
    params->next_param = id(temp);
}

static void hairy_keyword(struct body *body, struct keyword_param *k)
{
    struct symbol *temp = gensym();
    struct param *p = make_param(k->id, NULL);
    int line = k->id->line;
    struct arglist *args;
    struct id *name;
    struct expr *expr;

    name = id(temp);
    name->line = line;
    expr = make_varref(name);

    if (k->def) {
	/* Bind the original id to:
	 *   if (temp == #unbound) default-expression else temp end
	 */
	args = make_argument_list();
	add_argument(args, make_argument(expr));
	expr = make_literal_ref(make_unbound_literal());
	add_argument(args, make_argument(expr));
	expr = make_function_call(make_varref(id(sym_Eq)), args);
	expr = make_if(expr, make_expr_body(k->def),
		       make_else(0, make_expr_body(make_varref(id(temp)))));
	k->def = make_literal_ref(make_unbound_literal());
    }

    if (k->type_temp) {
	/* Wrap it with a call to check-type if it is typed. */
	args = make_argument_list();
	add_argument(args, make_argument(expr));
	add_argument(args, make_argument(make_varref(id(k->type_temp))));
	expr = make_function_call(make_varref(id(sym_CheckType)), args);
	p->type_temp = k->type_temp;
    }

    bind_param(body, p, expr);
    
    /* Change the keyword id to the temp. */
    k->id = id(temp);
    k->id->line = line;
}

static struct body
    *check_rettypes(struct body *form, struct return_type_list *rettypes)
{
    struct param_list *params = make_param_list();
    struct param **param_tail = &params->required_params;
    struct return_type *r;
    struct arglist *values = make_argument_list();
    struct expr *fn;
    struct symbol *ctype = sym_CheckType;

    r = rettypes->req_types;

    if (rettypes->restp) {
	if (r == NULL && rettypes->rest_temp == NULL)
	    /* #rest <object> -- real easy to test. */
	    return form;
	add_argument(values, make_argument(make_varref(id(sym_Values))));
    }
    else {
	if (r == NULL) {
	    /* No results are returned -- hence it is easy to test their */
	    /* types. */
	    struct expr *expr = make_varref(id(sym_Values));
	    add_expr(form, make_function_call(expr, make_argument_list()));
	    return form;
	}
	else if (r->next == NULL) {
	    /* Only a single value is returned. */
	    struct arglist *args = make_argument_list();
	    struct body *body = make_body();
	    struct expr *expr;

	    add_argument(args, make_argument(make_body_expr(form)));
	    if (r->temp) {
		add_argument(args, make_argument(make_varref(id(r->temp))));
		expr = make_varref(id(ctype));
	    }
	    else
		expr = make_varref(id(sym_Values));
	    add_expr(body, make_function_call(expr, args));
	    return body;
	}
    }

    for (; r != NULL; r = r->next) {
	struct symbol *temp = gensym();
	struct param *param = make_param(id(temp), NULL);
	struct expr *expr = make_varref(id(temp));
	*param_tail = param;
	param_tail = &param->next;
	if (r->temp) {
	    struct arglist *args = make_argument_list();
	    add_argument(args, make_argument(expr));
	    add_argument(args, make_argument(make_varref(id(r->temp))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	}
	add_argument(values, make_argument(expr));
    }

    if (rettypes->restp) {
	struct symbol *rest_temp = gensym();

	set_rest_param(params, id(rest_temp));
	
	if (rettypes->rest_temp) {
	    struct symbol *val_temp = gensym();
	    struct param_list *meth_params;
	    struct arglist *args;
	    struct body *body;
	    struct method *method;
	    struct expr *expr;

	    args = make_argument_list();
	    add_argument(args, make_argument(make_varref(id(val_temp))));
	    add_argument(args,
			 make_argument(make_varref(id(rettypes->rest_temp))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	    add_expr(body = make_body(), expr);
	
	    meth_params = make_param_list();
	    meth_params = push_param(make_param(id(val_temp), NULL),
				     meth_params);
	    method = make_method_description(meth_params, NULL, body);

	    args = make_argument_list();
	    add_argument(args, make_argument(make_method_ref(method)));
	    add_argument(args, make_argument(make_varref(id(rest_temp))));
	    expr = make_function_call(make_varref(id(sym_Do)), args);

	    add_expr(body = make_body(), expr);
	    add_expr(body, make_varref(id(rest_temp)));

	    add_argument(values, make_argument(make_body_expr(body)));
	}
	else
	    add_argument(values, make_argument(make_varref(id(rest_temp))));

	fn = make_varref(id(sym_Apply));
    }
    else
	fn = make_varref(id(sym_Values));

    {
	struct body *body = make_body();
	struct bindings *bind = make_bindings(params, make_body_expr(form));
	add_constituent(body, make_let(bind));
	add_expr(body, make_function_call(fn, values));
	return body;
    }
}

static void expand_method_for_compile(struct method *method)
{
    struct param_list *params = method->params;
    struct keyword_param *k;
    struct body *body = make_body();

    if (params->next_param)
	bind_next_param(body, method->params);

    for (k = params->keyword_params; k != NULL; k = k->next)
	if ((k->def && k->def->kind != expr_LITERAL) || k->type_temp)
	    hairy_keyword(body, k);

    expand_param_list(params);

    if (method->rettypes)
	method->body = check_rettypes(method->body, method->rettypes);

    method->body = chain_bodies(body, method->body);

    expand_body(method->body, FALSE);
}


/* defvar/defconst initializer generation. */

static struct method *make_initializer(char *kind, struct bindings *bindings)
{
    struct param_list *params = bindings->params;
    struct param *param;
    struct symbol *init = sym_InitVariable;
    struct symbol *ctype = sym_CheckType;
    struct symbol *type_class = sym_Type;
    struct param_list *temps = make_param_list();
    struct param **tail = &temps->required_params;
    struct body *outer_body = make_body();
    struct body *inner_body = make_body();
    struct param *temp_param;
    struct arglist *args, *init_args;
    struct expr *expr;
    struct symbol *type_temp, *temp;
    int len;
    char *debug_name;
    struct method *res;
    boolean first;

    len = strlen(kind) + 1 - strlen(", ");
    for (param = params->required_params; param != NULL; param = param->next)
	len += strlen(", ") + strlen((char *)param->id->symbol->name);
    if (params->rest_param)
	len += strlen(", #rest ") + strlen((char *)params->rest_param->symbol->name);
    debug_name = malloc(len);
    strcpy(debug_name, kind);

    first = TRUE;
    for (param = params->required_params; param != NULL; param = param->next) {
	if (first)
	    first = FALSE;
	else
	    strcat(debug_name, ", ");
	strcat(debug_name, (char *)param->id->symbol->name);

	temp = gensym();
	temp_param = make_param(id(temp), NULL);
	*tail = temp_param;
	tail = &temp_param->next;

	if (param->type) {
	    type_temp = gensym();
	    args = make_argument_list();
	    add_argument(args, make_argument(param->type));
	    param->type = NULL;
	    add_argument(args, make_argument(make_varref(id(type_class))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	    bind_temp(outer_body, id(type_temp), expr);
	}
	else
	    type_temp = NULL;
	
	init_args = make_argument_list();
	add_argument(init_args, make_find_var_arg(param->id));
	expr = make_varref(id(temp));
	if (type_temp) {
	    args = make_argument_list();
	    add_argument(args, make_argument(expr));
	    add_argument(args, make_argument(make_varref(id(type_temp))));
	    expr = make_function_call(make_varref(id(ctype)), args);
	}
	add_argument(init_args, make_argument(expr));
	if (type_temp)
	    add_argument(init_args, make_argument(make_varref(id(type_temp))));
	else {
	    expr = make_literal_ref(make_false_literal());
	    add_argument(init_args, make_argument(expr));
	}
	add_expr(inner_body,
		 make_function_call(make_varref(id(init)), init_args));
    }

    if (params->rest_param) {
	if (first)
	    strcat(debug_name, "#rest ");
	else
	    strcat(debug_name, ", #rest ");
	strcat(debug_name, (char *)params->rest_param->symbol->name);
	temp = gensym();
	temps->rest_param = id(temp);
	init_args = make_argument_list();
	add_argument(init_args, make_find_var_arg(params->rest_param));
	expr = make_varref(id(temp));
	add_argument(init_args, make_argument(expr));
	expr = make_literal_ref(make_false_literal());
	add_argument(init_args, make_argument(expr));
	add_expr(inner_body,
		 make_function_call(make_varref(id(init)), init_args));
    }

    add_constituent(outer_body,
		    make_let(make_bindings(temps, bindings->expr)));
    bindings->expr = NULL;

    outer_body = chain_bodies(outer_body, inner_body);

    add_expr(outer_body, make_literal_ref(make_false_literal()));
    res = make_top_level_method(debug_name, outer_body);

    free(debug_name);

    return res;
}


/* define module and define library stuff. */

static struct literal *make_var_names_literal(struct variable_names *names)
{
    struct literal_list *guts = make_literal_list();
    struct list_literal *res;
    struct variable_name *name;

    for (name = names->head; name != NULL; name = name->next)
	add_literal(guts, name->name);
    res = (struct list_literal *)make_list_literal(guts);

    if (res->first) {
	struct literal **prev, *cur, *scan;

	prev = &res->first->next;
	while ((cur = *prev) != NULL) {
	    for (scan = res->first;
		 scan != cur;
		 scan = scan->next)
		if (((struct symbol_literal *)cur)->symbol
		    == ((struct symbol_literal *)scan)->symbol)
		    break;
	    if (cur == scan)
		prev = &cur->next;
	    else {
		*prev = cur->next;
		free(cur);
	    }
	}
    }

    return (struct literal *)res;
}

static void expand_useopt_prefix(struct use_clause *use,
				 struct prefix_option *option)
{
    use->prefix = option->prefix;
}

static void expand_useopt_import(struct use_clause *use,
				 struct import_option *option)
{
    use->import = make_var_names_literal(option->vars);

    if (option->renames->head != NULL) {
	struct literal_list *guts = make_literal_list();
	struct renaming *renaming;

	for (renaming = option->renames->head;
	     renaming != NULL;
	     renaming = renaming->next) {
	    struct literal_list *list = make_literal_list();
	    add_literal(list, renaming->from);
	    add_literal(guts, make_dotted_list_literal(list, renaming->to));
	}
	if (use->rename)
	    use->rename = make_dotted_list_literal(guts, use->rename);
	else
	    use->rename = make_list_literal(guts);
    }
}

static void expand_useopt_exclude(struct use_clause *use,
				  struct exclude_option *option)
{
    use->exclude = make_var_names_literal(option->vars);
}

static void expand_useopt_rename(struct use_clause *use,
				 struct rename_option *option)
{
    struct literal_list *guts = make_literal_list();
    struct renaming *renaming;

    for (renaming = option->renames->head;
	 renaming != NULL;
	 renaming = renaming->next)
	add_literal(guts,
		    make_dotted_list_literal(add_literal(make_literal_list(),
							 renaming->from),
					     renaming->to));
    if (use->rename)
	use->rename = make_dotted_list_literal(guts, use->rename);
    else
	use->rename = make_list_literal(guts);
}

static void expand_useopt_export(struct use_clause *use,
				 struct export_option *option)
{
    use->export = make_var_names_literal(option->vars);
}

static void expand_useopt_import_all(struct use_clause *use,
				     struct use_option *option)
{
    use->import = make_true_literal();
}

static void expand_useopt_export_all(struct use_clause *use,
				     struct use_option *option)
{
    use->export = make_true_literal();
}

static void (*UseOptionExpanders[])() = {
    expand_useopt_prefix, expand_useopt_import, expand_useopt_exclude,
    expand_useopt_rename, expand_useopt_export,
    expand_useopt_import_all, expand_useopt_export_all
};

static void expand_use_clause(struct use_clause *use)
{
    struct use_option *option, *next;

    for (option = use->options; option != NULL; option = next) {
	(*UseOptionExpanders[(int)option->kind])(use, option);
	next = option->next;
	free(option);
    }
    use->options = NULL;
    if (use->import == NULL)
	use->import = make_true_literal();
    if (use->exclude == NULL)
	use->exclude = make_list_literal(make_literal_list());
    if (use->prefix == NULL)
	use->prefix = make_false_literal();
    if (use->rename == NULL)
	use->rename = make_list_literal(make_literal_list());
    if (use->export == NULL)
	use->export = make_list_literal(make_literal_list());
}

static void expand_defnamespace(struct defnamespace_constituent *c)
{
    struct use_clause *use;

    for (use = c->use_clauses; use != NULL; use = use->next)
	expand_use_clause(use);
    c->exported_literal = make_var_names_literal(c->exported_variables);
    c->exported_variables = NULL;
    c->created_literal = make_var_names_literal(c->created_variables);
    c->created_variables = NULL;
}


/* Constituent expanders. */

static void expand_defconst_for_compile(struct defconst_constituent *c)
{
    c->tlf = make_initializer("Define Constant ", c->bindings);
    expand_method_for_compile(c->tlf);
}

static boolean expand_defconst_constituent(struct defconst_constituent **ptr,
					   boolean top_level)
{
    if (top_level)
	expand_defconst_for_compile(*ptr);
    else
	error((*ptr)->line, "define constant not at top-level");
    return FALSE;
}

static void expand_defvar_for_compile(struct defvar_constituent *c)
{
    c->tlf = make_initializer("Define Variable ", c->bindings);
    expand_method_for_compile(c->tlf);
}

static boolean expand_defvar_constituent(struct defvar_constituent **ptr,
					 boolean top_level)
{
    if (top_level)
	expand_defvar_for_compile(*ptr);
    else
	error((*ptr)->line, "define variable not at top-level");
    return FALSE;
}

static void expand_defmethod_for_compile(struct defmethod_constituent *c)
{
    struct method *method = c->method;
    char *name = (char *)method->name->symbol->name;
    char *debug_name = malloc(strlen(name) + sizeof("Define Method "));
    struct symbol *defmeth = sym_DefineMethod;
    struct body *body;
    struct arglist *args;
    struct expr *expr;

    body = make_body();
    add_method_wrap(body, method);
    args = make_argument_list();
    add_argument(args, make_find_var_arg(method->name));
    add_argument(args, make_argument(make_method_ref(c->method)));
    add_expr(body, make_function_call(make_varref(id(defmeth)), args));
    expr = make_literal_ref(make_symbol_literal(method->name->symbol));
    add_expr(body, expr);

    strcpy(debug_name, "Define Method ");
    strcat(debug_name, name);

    c->tlf = make_top_level_method(debug_name, body);

    free(debug_name);

    expand_method_for_compile(c->tlf);
}

static boolean expand_defmethod_constituent(struct defmethod_constituent **ptr,
					    boolean top_level)
{
    if (top_level)
	expand_defmethod_for_compile(*ptr);
    else
	error((*ptr)->method->line, "define method not at top-level");
    return FALSE;
}

static void expand_defdomain_for_compile(struct defdomain_constituent *c)
{
    char *name = (char *)c->name->symbol->name;
    char *debug_name = malloc(strlen(name) + sizeof("Define Sealed Domain "));
    struct body *body = make_body();
    struct arglist *init_args = make_argument_list();
    struct expr *expr;
    struct argument *arg, *next;

    strcpy(debug_name, "Define Sealed Domain ");
    strcat(debug_name, name);

    add_argument(init_args, make_argument(make_varref(c->name)));

    for (arg = c->types; arg != NULL; arg = next) {
	next = arg->next;
	add_argument(init_args, make_argument(arg->expr));
	free(arg);
    }
    c->types = NULL;

    expr = make_function_call(make_varref(id(sym_DefineDomain)), init_args);

    add_expr(body, expr);
    add_expr(body, make_literal_ref(make_symbol_literal(c->name->symbol)));

    c->tlf = make_top_level_method(debug_name, body);

    free(debug_name);

    expand_method_for_compile(c->tlf);
    
}

static boolean expand_defdomain_constituent(struct defdomain_constituent **ptr,
					    boolean top_level)
{
    if (top_level)
	expand_defdomain_for_compile(*ptr);
    else
	error((*ptr)->name->line, "define sealed domain not at top-level");
    return FALSE;
}

static void expand_defgeneric_for_compile(struct defgeneric_constituent *c)
{
    char *name = (char *)c->name->symbol->name;
    char *debug_name = malloc(strlen(name) + sizeof("Define Generic "));
    struct body *body = make_body();
    struct arglist *init_args = make_argument_list();
    struct expr *expr;

    strcpy(debug_name, "Define Generic ");
    strcat(debug_name, name);

    add_argument(init_args, make_find_var_arg(c->name));

    {
	struct arglist *list_args = make_argument_list();
	struct param *p;

	for (p = c->params->required_params; p != NULL; p = p->next)
	    if (p->type) {
		add_argument(list_args, make_argument(p->type));
		p->type = NULL;
	    }
	    else {
		expr = make_varref(id(sym_Object));
		add_argument(list_args, make_argument(expr));
	    }
	expr = make_function_call(make_varref(id(sym_List)), list_args);
	add_argument(init_args, make_argument(expr));
    }
    
    if (c->params->rest_param)
	expr = make_literal_ref(make_true_literal());
    else
	expr = make_literal_ref(make_false_literal());
    add_argument(init_args, make_argument(expr));

    if (c->params->allow_keys) {
	struct arglist *list_args = make_argument_list();
	struct keyword_param *k;

	for (k = c->params->keyword_params; k != NULL; k = k->next) {
	    expr = make_literal_ref(make_symbol_literal(k->keyword));
	    add_argument(list_args, make_argument(expr));
	}
	expr = make_function_call(make_varref(id(sym_List)), list_args);
	add_argument(init_args, make_argument(expr));
    }
    else {
	expr = make_literal_ref(make_false_literal());
	add_argument(init_args, make_argument(expr));
    }

    expr = make_literal_ref(c->params->all_keys
			    ? make_true_literal()
			    : make_false_literal());
    add_argument(init_args, make_argument(expr));

    if (c->rettypes) {
	bind_rettypes(body, c->rettypes);
	add_argument(init_args, make_argument(c->rettypes->req_types_list));
	if (c->rettypes->rest_temp)
	    expr = c->rettypes->rest_temp_varref;
	else if (c->rettypes->restp)
	    expr = make_literal_ref(make_true_literal());
	else
	    expr = make_literal_ref(make_false_literal());
	add_argument(init_args, make_argument(expr));
    }
    else {
	expr = make_literal_ref(make_list_literal(make_literal_list()));
	add_argument(init_args, make_argument(expr));
	expr = make_literal_ref(make_true_literal());
	add_argument(init_args, make_argument(expr));
    }
    if (c->plist) {
	add_plist_arguments(init_args, c->plist);
	c->plist = NULL;
    }

    expr = make_function_call(make_varref(id(sym_DefineGeneric)),
			      init_args);
    add_expr(body, expr);
    add_expr(body, make_literal_ref(make_symbol_literal(c->name->symbol)));

    c->tlf = make_top_level_method(debug_name, body);

    free(debug_name);

    expand_method_for_compile(c->tlf);
}

static boolean
    expand_defgeneric_constituent(struct defgeneric_constituent **ptr,
				  boolean top_level)
{
    if (top_level)
	expand_defgeneric_for_compile(*ptr);
    else
	error((*ptr)->name->line, "define generic not at top-level");
    return FALSE;
}

static void expand_slots(struct body *body,
			 struct arglist *defclass_args,
			 struct defclass_constituent *c)
{
    struct slot_spec *slot;
    struct arglist *list_args = make_argument_list();
    struct expr *expr;

    for (slot = c->slots; slot != NULL; slot = slot->next) {
	struct arglist *slot_args;
	boolean default_setter = TRUE;
	
	/* Extract the setter name, if there is one */
	if (slot->flags & flag_CONSTANT) {
	    default_setter = FALSE;
	} else if (slot->plist) {
	    struct property *prop, **prev;
	    prev = &slot->plist->head;
	    while ((prop = *prev) != NULL) {
		if (prop->keyword == sym_Setter) {
		    if (prop->expr->kind == expr_LITERAL
			&& ((struct literal_expr *) (prop->expr))
			->lit->kind == literal_FALSE) {
			default_setter = FALSE;
			*prev = prop->next;
			free(prop);
		    }
		    else if (prop->expr->kind != expr_VARREF) {
			error(prop->line, "Bogus %s in slot %s",
			      prop->keyword->name,
			      slot->name->symbol->name);
			prev = &prop->next;
		    }
		    else {
			struct varref_expr *v = (void *) prop->expr;
			slot->setter = v->var;
			*prev = prop->next;
			free(prop);
		    }
		}
		else
		  prev = &prop->next;
	    }
	}
	
	/* Bind the getter and setter names */
	slot->getter = slot->name;
	if (slot->setter == NULL && default_setter) {
	    slot->setter = dup_id(slot->name);
	    change_to_setter(slot->setter);
	}
	
	/* Make the call to %define-slot */

	slot_args = make_argument_list();
	add_argument(slot_args, make_find_var_arg(slot->getter));
	if (slot->setter)
	  add_argument(slot_args, make_find_var_arg(slot->setter));
	else {
	    expr = make_literal_ref(make_false_literal());
	    add_argument(slot_args, make_argument(expr));
	}
	expr = make_varref(id(sym_DefineSlot));
	add_expr(body, make_function_call(expr, slot_args));
	
	/* Make the call to make-slot */

	slot_args = make_argument_list();
	
	/* First argument: the slot name */
	expr = make_literal_ref(make_symbol_literal(slot->name->symbol));
	add_argument(slot_args, make_argument(expr));
	
	/* Second argument: the allocation. */
	expr = make_literal_ref(make_integer_literal((int) slot->alloc));
	add_argument(slot_args, make_argument(expr));
	
	/* Third argument: the getter. */
	add_argument(slot_args, make_argument(make_varref(slot->getter)));
	
	/* Fourth argument: the setter */
	if (slot->setter == NULL)
	  expr = make_literal_ref(make_false_literal());
	else
	  expr = make_varref(slot->setter);
	add_argument(slot_args, make_argument(expr));
	
	/* Fifth argument: the type. */
	if (slot->type)
	  add_argument(slot_args, make_argument(slot->type));
	else {
	    expr = make_literal_ref(make_false_literal());
	    add_argument(slot_args, make_argument(expr));
	}
	
	/* Sixth and on: the other properties. */
	if (slot->plist) {
	    add_plist_arguments(slot_args, slot->plist);
	    slot->plist = NULL;
	}
	
	expr = make_varref(id(sym_MakeSlot));
	expr = make_function_call(expr, slot_args);
	add_argument(list_args, make_argument(expr));
    }
    expr = make_function_call(make_varref(id(sym_List)), list_args);
    add_argument(defclass_args, make_argument(expr));
}

static void expand_initargs(struct body *body,
			    struct arglist *defclass_args,
			    struct defclass_constituent *c)
{
    struct initarg_spec *initarg;
    struct arglist *list_args = make_argument_list();
    struct expr *expr;

    for (initarg = c->initargs; initarg != NULL; initarg = initarg->next) {
	struct arglist *initarg_args = make_argument_list();
	
	/* Make the call to make-initarg */
	
	/* First argument: the slot name */
	expr = make_literal_ref(make_symbol_literal(initarg->keyword));
	add_argument(initarg_args, make_argument(expr));
	
	/* Second argument: required */
	if (initarg->required)
	    expr = make_literal_ref(make_true_literal());
	else
	    expr = make_literal_ref(make_false_literal());
	add_argument(initarg_args, make_argument(expr));

	/* Other arguments: properties */
	if (initarg->plist) {
	    add_plist_arguments(initarg_args, initarg->plist);
	    initarg->plist = NULL;
	}
	
	expr = make_varref(id(sym_MakeInitarg));
	expr = make_function_call(expr, initarg_args);
	add_argument(list_args, make_argument(expr));
    }
    expr = make_function_call(make_varref(id(sym_List)), list_args);
    add_argument(defclass_args, make_argument(expr));
}

static void expand_inheriteds(struct body *body,
			      struct arglist *defclass_args,
			      struct defclass_constituent *c)
{
    struct inherited_spec *inherited;
    struct arglist *list_args = make_argument_list();
    struct expr *expr;

    for (inherited = c->inheriteds; inherited != NULL;
	 inherited = inherited->next) {
	struct arglist *inherited_args = make_argument_list();
	
	/* Make the call to make-inherited */
	
	/* First argument: the slot name */
	expr = make_literal_ref(make_symbol_literal(inherited->name->symbol));
	add_argument(inherited_args, make_argument(expr));
	
	/* Other arguments: properties */
	if (inherited->plist) {
	    add_plist_arguments(inherited_args, inherited->plist);
	    inherited->plist = NULL;
	}
	
	expr = make_varref(id(sym_MakeInherited));
	expr = make_function_call(expr, inherited_args);
	add_argument(list_args, make_argument(expr));
    }
    expr = make_function_call(make_varref(id(sym_List)), list_args);
    add_argument(defclass_args, make_argument(expr));
}

static void expand_defclass_for_compile(struct defclass_constituent *c)
{
    char *name = (char *)c->name->symbol->name;
    char *debug_name = malloc(strlen(name) + sizeof("Define Class "));

    strcpy(debug_name, "Define Class ");
    strcat(debug_name, name);

    /* Phase I: Create the class with its superclasses. */

    {
	struct arglist *list_args = make_argument_list();
	struct arglist *defclass_args = make_argument_list();
	struct body *body = make_body();
	struct superclass *super;
	struct expr *expr;
	
	add_argument(defclass_args, make_argument(make_varref(c->name)));
	for (super = c->supers; super != NULL; super = super->next)
	    add_argument(list_args, make_argument(super->expr));
	expr = make_function_call(make_varref(id(sym_List)), list_args);
	add_argument(defclass_args, make_argument(expr));

	expr = make_varref(id(sym_DefineClass1));
	add_expr(body, make_function_call(expr, defclass_args));
	add_expr(body, make_literal_ref(make_symbol_literal(c->name->symbol)));

	c->tlf1 = make_top_level_method(debug_name, body);
    }

    /* Phase II: Create the slots, init args, and inherited slots. */

    {
	struct arglist *defclass_args = make_argument_list();
	struct body *body = make_body();
	struct expr *expr;
	struct expr *abstract_expr;
	
	add_argument(defclass_args, make_argument(make_varref(dup_id(c->name))));

	expand_slots(body, defclass_args, c);
	expand_initargs(body, defclass_args, c);
	expand_inheriteds(body, defclass_args, c);

	abstract_expr 
	    = make_literal_ref((c->flags & flag_ABSTRACT) 
			         ? make_true_literal() : make_false_literal());
	add_argument(defclass_args, make_argument(abstract_expr));

	expr = make_varref(id(sym_DefineClass2));
	add_expr(body, make_function_call(expr, defclass_args));
	add_expr(body, make_literal_ref(make_symbol_literal(c->name->symbol)));

	c->tlf2 = make_top_level_method(debug_name, body);
    }

    free(debug_name);

    expand_method_for_compile(c->tlf1);
    expand_method_for_compile(c->tlf2);
}

static boolean expand_defclass_constituent(struct defclass_constituent **ptr,
					   boolean top_level)
{
    if (top_level)
	expand_defclass_for_compile(*ptr);
    else
	error((*ptr)->name->line, "define class not at top-level");
    return FALSE;
}

static boolean expand_expr_constituent(struct constituent **ptr,
				       boolean top_level)
{
    struct expr_constituent *c = (struct expr_constituent *)*ptr;
    struct expr *expr = c->expr;

    if (top_level) {
	if (expr->kind == expr_BODY) {
	    struct body_expr *body_expr = (struct body_expr *)expr;
	    expand_body(body_expr->body, TRUE);
	    return FALSE;
	}
	else {
	    *ptr = make_top_level_form("Top Level Form",
				       (struct constituent *)c);
	    return TRUE;
	}
    }
    else {
	expand_expr(&c->expr);
	return FALSE;
    }
}

static boolean expand_local_constituent(struct constituent **ptr,
					boolean top_level)
{
    struct local_constituent *c = (struct local_constituent *)*ptr;
    struct method *method = c->methods;

    if (top_level) {
	*ptr = make_top_level_form("Top Level Form", (struct constituent *)c);
	return TRUE;
    }
    else if (method != NULL && method->specializers == NULL) {
	struct body *body = make_body();
	for (; method != NULL; method = method->next_local)
	    add_method_wrap(body, method);
	c->next = NULL;
	add_constituent(body, (struct constituent *)c);
	*ptr = make_expr_constituent(make_body_expr(body));
	return TRUE;
    }
    else {
	for (; method != NULL; method = method->next_local)
	    expand_method_for_compile(method);
	expand_body(c->body, FALSE);
	return FALSE;
    }
}

static boolean expand_handler_constituent(struct constituent **ptr,
					  boolean top_level)
{
    struct handler_constituent *h = (struct handler_constituent *)*ptr;
    struct body *body;
    struct arglist *args;

    if (top_level) {
	*ptr = make_top_level_form("Top Level Form", (struct constituent *)h);
	return TRUE;
    }

    body = make_body();
    args = make_argument_list();

    add_argument(args, make_argument(h->type));
    add_argument(args, make_argument(h->func));
    if (h->plist) {
	add_plist_arguments(args, h->plist);
	h->plist = NULL;
    }
    add_expr(body, make_function_call(make_varref(id(sym_PushHandler)),
				      args));


    /* Link the handler body into the body we have just made, and replace */
    /* the handler body with it. */
    h->body = chain_bodies(body, h->body);

    /* Clear out the type and func */
    h->type = NULL;
    h->func = NULL;

    /* Now expand that body. */
    expand_body(h->body, FALSE);

    return FALSE;
}

static boolean expand_let_constituent(struct constituent **ptr,
				      boolean top_level)
{
    struct let_constituent *let = (struct let_constituent *)*ptr;
    struct bindings *bindings = let->bindings;

    if (top_level) {
	*ptr = make_top_level_form("Top Level Form",(struct constituent *)let);
	return TRUE;
    }
    else {
	struct param_list *params = bindings->params;
	struct body *body = NULL;
	struct param *p;
	struct arglist *args;
	struct expr *expr;
	struct symbol *check_type = sym_CheckType;
	struct symbol *type_class = sym_Type;

	for (p = params->required_params; p != NULL; p = p->next)
	    if (p->type) {
		if (body == NULL)
		    body = make_body();
		p->type_temp = gensym();
		args = make_argument_list();
		add_argument(args, make_argument(p->type));
		add_argument(args, make_argument(make_varref(id(type_class))));
		expr = make_function_call(make_varref(id(check_type)), args);
		bind_temp(body, id(p->type_temp), expr);
		p->type = NULL;
	    }
	if (body != NULL) {
	    p = params->required_params;
	    if (p->next || params->rest_param) {
		/* There are multiple parameters, so we can't just wrap the */
		/* expression with check-type.  Therefore, we bind a bunch */
		/* of temps, and then bind the real variables to check-type */
		/* of the temps. */
		struct body *let_body = let->body;
		let->body = make_body();
		add_constituent(body, (struct constituent *)let);
		let->next = NULL;
		for (; p != NULL; p = p->next) {
		    if (p->type_temp) {
			struct symbol *temp = gensym();
			struct param *new_param = make_param(p->id, NULL);

			p->id = id(temp);
			args = make_argument_list();
			add_argument(args,
				     make_argument(make_varref(id(temp))));
			expr = make_varref(id(p->type_temp));
			add_argument(args, make_argument(expr));
			expr = make_function_call(make_varref(id(check_type)),
						  args);
			bind_param(body, new_param, expr);
		    }
		}
		add_expr(body, make_body_expr(let_body));
	    }
	    else {
		/* Wrap the expression with a call to check-type */
		args = make_argument_list();
		add_argument(args, make_argument(bindings->expr));
		add_argument(args,
			     make_argument(make_varref(id(p->type_temp))));
		expr = make_function_call(make_varref(id(check_type)), args);
		bindings->expr = expr;
		add_constituent(body, (struct constituent *)let);
		let->next = NULL;
	    }
	    *ptr = make_expr_constituent(make_body_expr(body));
	    return TRUE;
	}
	else {
	    expand_bindings(bindings);
	    expand_body(let->body, FALSE);
	    return FALSE;
	}
    }
}

static boolean expand_tlf_constituent(struct tlf_constituent **ptr,
				      boolean top_level)
{
    expand_method_for_compile((*ptr)->form);
    return FALSE;
}

static boolean expand_error_constituent(struct constituent **ptr)
{
    lose("Called expand on a parse tree with errors?");
    return FALSE;
}


static boolean
    expand_defnamespace_constituent(struct defnamespace_constituent **ptr,
				    boolean top_level)
{
    if (top_level)
	expand_defnamespace(*ptr);
    else
	error((*ptr)->name->line, "define %s not at top-level",
	      (*ptr)->kind == constituent_DEFMODULE ? "module" : "library");
    return FALSE;
}

static boolean (*ConstituentExpanders[])() = {
    expand_defconst_constituent, expand_defvar_constituent,
    expand_defmethod_constituent, expand_defdomain_constituent,
    expand_defgeneric_constituent, expand_defclass_constituent,
    expand_expr_constituent, expand_local_constituent,
    expand_handler_constituent, expand_let_constituent,
    expand_tlf_constituent, expand_error_constituent,
    expand_defnamespace_constituent, expand_defnamespace_constituent
};

static boolean expand_constituent(struct constituent **ptr, boolean top_level)
{
    return (*ConstituentExpanders[(int)(*ptr)->kind])(ptr, top_level);
}


/* Block expander */

/* block/exit-fun forms:

    block (exit-fun)
      body
    end

    =>

    catch(method (temp)
            local
	      method exit-fun (#rest rest)
	        apply(throw, temp, rest)
	      end;
	    body
	  end)

 */

static struct body *make_catch(int line, struct body *body,
			       struct id *exit_fun)
{
    struct symbol *temp = gensym();
    struct symbol *rest = gensym();
    struct param_list *params;
    struct arglist *args;
    struct body *new_body;
    struct method *method;
    struct local_methods *locals;
    struct expr *expr;
    struct id *name;

    /* Make the call to apply */
    args = make_argument_list();
    add_argument(args, make_argument(make_varref(id(sym_Throw))));
    add_argument(args, make_argument(make_varref(id(temp))));
    add_argument(args, make_argument(make_varref(id(rest))));
    expr = make_function_call(make_varref(id(sym_Apply)), args);

    /* Make the local method */
    params = set_rest_param(make_param_list(), id(rest));
    new_body = make_body();
    add_expr(new_body, expr);
    method = make_method_description(params, NULL, new_body);
    set_method_name(exit_fun, method);

    /* Make the local constituent, and add it to the outer body */
    locals = add_local_method(make_local_methods(), method);
    new_body = add_constituent(make_body(), make_local_constituent(locals));

    /* Chain the original body to the new body. */
    new_body = chain_bodies(new_body, body);

    /* Make the method arg to catch */
    params = push_param(make_param(id(temp), NULL), make_param_list());
    method = make_method_description(params, NULL, new_body);
    method->line = line;

    /* Make the call to catch */
    args = make_argument_list();
    add_argument(args, make_argument(make_method_ref(method)));
    name = id(sym_Catch);
    name->line = line;
    expr = make_function_call(make_varref(name), args);

    /* Return it. */
    return make_expr_body(expr);
}

/* block/exception forms:

   block ()
     block-body
   exception (symbol-1 :: type-1, plist-1...)
     exception-body-1
   exception (symbol-2 :: type-2, plist-2...)
     exception-body-2
   end

   get expanded into:

   block (done)
     block (do-handler)
       let handler (type-2, plist-2...)
         = method (symbol-2, ignore)
	     do-handler(method () exception-body-2 end)
	   end;
       let handler (type-1, plist-1...)
         = method (symbol-1, ignore)
	     do-handler(method () exception-body-1 end)
	   end;
       let (#rest results) = block-body;
       apply(done, results)
     end()
   end

 */

static struct body *make_handler_case(int line, struct body *block_body,
				      struct exception_clause *clauses)
{
    struct symbol *done = gensym();
    struct symbol *do_handler = gensym();
    struct symbol *results = gensym();
    struct exception_clause *next;
    struct expr *expr;
    struct param_list *params;
    struct arglist *args;
    struct method *method;
    struct body *handler_body;
    struct body *body = make_body();
    struct body *clause_body;
    
    while (clauses != NULL) {
	/* Wrap the exception body in a method */
	params = make_param_list();
	method = make_method_description(params, NULL, clauses->body);

	/* Make the handler method's body */
	args = make_argument_list();
	add_argument(args, make_argument(make_method_ref(method)));
	handler_body = make_body();
	add_expr(handler_body,
		 make_function_call(make_varref(id(do_handler)), args));

	/* And make the handler method itself. */
	params = make_param_list();
	push_param(make_param(id(gensym()), NULL), params);
	if (clauses->condition)
	    push_param(make_param(clauses->condition, NULL), params);
	else
	    push_param(make_param(id(gensym()), NULL), params);
	method = make_method_description(params, NULL, handler_body);

	/* Add the handler to the body. */
	clause_body = make_body();
	add_constituent(clause_body,
			make_handler(clauses->type,
				     make_method_ref(method),
				     clauses->plist));
	body = chain_bodies(clause_body, body);

	/* Advance to the next clause. */
	next = clauses->next;
	free(clauses);
	clauses = next;
    }
	
    /* Invoke the block-body for multiple values. */
    params = set_rest_param(make_param_list(), id(results));
    add_constituent(body,
		    make_let(make_bindings(params,
					   make_body_expr(block_body))));

    /* apply those results to the done exit function. */
    args = make_argument_list();
    args = add_argument(args, make_argument(make_varref(id(done))));
    args = add_argument(args, make_argument(make_varref(id(results))));
    expr = make_function_call(make_varref(id(sym_Apply)), args);
    add_expr(body, expr);

    /* make the do-handler block */
    expr = make_block(line, id(do_handler), body, NULL);

    /* Make a function call out of it. */
    expr = make_function_call(expr, make_argument_list());

    /* make the done block. */
    expr = make_block(line, id(done), make_expr_body(expr), NULL);

    /* And return it as a body. */
    return make_expr_body(expr);
}

static struct body *make_unwind_protect(struct body *body,struct body *cleanup)
{
    struct method *body_method
	= make_method_description(make_param_list(), NULL, body);
    struct method *cleanup_method
	= make_method_description(make_param_list(), NULL, cleanup);
    struct argument *body_arg
	= make_argument(make_method_ref(body_method));
    struct argument *cleanup_arg
	= make_argument(make_method_ref(cleanup_method));
    struct arglist *args
	= add_argument(add_argument(make_argument_list(), body_arg),
		       cleanup_arg);
    struct expr *expr
	= make_function_call(make_varref(id(sym_Uwp)), args);

    return make_expr_body(expr);
}

static boolean expand_block_expr(struct expr **ptr)
{
    struct block_expr *e = (struct block_expr *)*ptr;
    struct body *body = e->body;

    if (e->inner)
	body = make_handler_case(e->line, body, e->inner);
    if (e->cleanup)
	body = make_unwind_protect(body, e->cleanup);
    if (e->outer)
	body = make_handler_case(e->line, body, e->outer);
    if (e->exit_fun)
	body = make_catch(e->line, body, e->exit_fun);

    *ptr = make_body_expr(body);

    free(e);

    return TRUE;
}


/* Case expander */

static struct expr *make_case_condition(struct condition *conditions)
{
    struct expr *cond = conditions->cond;

    if (conditions->next) {
	struct body *true_body
	    = make_expr_body(make_literal_ref(make_true_literal()));
	struct body *rest_body
	    = make_expr_body(make_case_condition(conditions->next));

	free(conditions);

	return make_if(cond, true_body, make_else(0, rest_body));
    }
    else {
	free(conditions);
	return cond;
    }
}

static struct expr *expand_case_body(struct condition_body *body)
{
    if (body) {
	struct condition_clause *clause = body->clause;

	if (clause->conditions) {
	    struct expr *cond = make_case_condition(clause->conditions);
	    struct expr *rest = expand_case_body(body->next);
	
	    free(body);

	    return make_if(cond, clause->body,
			   make_else(0, make_expr_body(rest)));
	}
	else {
	    free(body);
	    return make_body_expr(clause->body);
	}
    }
    else
	return make_literal_ref(make_false_literal());
}

static boolean expand_case_expr(struct expr **ptr)
{
    struct case_expr *e = (struct case_expr *)*ptr;

    *ptr = expand_case_body(e->body);

    free(e);

    return TRUE;
}


/* For expander */

/* For loops expand into a body of code structured as follows:

   let temps;				<- outer body
   loop (repeat)
     let =/then & from vars;		<- middle body
     unless (implied-end-tests)		<- tests
       let in vars;			<- inner body
       unless (explicit-end-test)	<- until clause
         body;				<- step body
	 steps;
	 repeat
       end
     end
     finally
   end

*/         

struct for_info {
    struct body *outer_body;
    struct body *middle_body;
    struct expr *first_test;
    struct binop_series *more_tests;
    struct body *inner_body;
    struct body *step_body;
};

static void cache_types(struct param_list *params, struct for_info *info)
{
    struct param *param;

    for (param = params->required_params; param != NULL; param = param->next) {
	if (param->type) {
	    param->type_temp = gensym();
	    bind_temp(info->outer_body, id(param->type_temp), param->type);
	    param->type = NULL;
	}
    }
}

static void add_set(struct body *body, struct id *id, struct expr *expr)
{
    add_expr(body, make_varset(id, expr));
}

static void grovel_equal_then_for_clause(struct equal_then_for_clause *clause,
					 struct for_info *info)
{
    struct param_list *params = clause->vars;
    struct param *init_params_head = NULL;
    struct param **init_params_tail = &init_params_head;
    struct param_list *step_params = make_param_list();
    struct param *step_params_head = NULL;
    struct param **step_params_tail = &step_params_head;
    struct param *param, *next;

    bind_params(info->outer_body, params, clause->equal);
    bind_params(info->step_body, step_params, clause->then);

    for (param = params->required_params; param != NULL; param = next) {
	struct symbol *temp1 = gensym();
	struct symbol *temp2 = gensym();
	struct param *init_param = make_param(id(temp1), NULL);
	struct param *step_param = make_param(id(temp2), NULL);

	*init_params_tail = init_param;
	init_params_tail = &init_param->next;
	*step_params_tail = step_param;
	step_params_tail = &step_param->next;

	next = param->next;
	bind_param(info->middle_body, param, make_varref(id(temp1)));
	add_set(info->step_body, id(temp1), make_varref(id(temp2)));
    }
    params->required_params = init_params_head;
    step_params->required_params = step_params_head;

    if (params->rest_param) {
	struct id *rest = params->rest_param;
	struct symbol *temp1 = gensym();
	struct symbol *temp2 = gensym();

	params->rest_param = id(temp1);
	step_params->rest_param = id(temp2);

	bind_temp(info->middle_body, rest, make_varref(id(temp1)));
	add_set(info->step_body, id(temp1), make_varref(id(temp2)));
    }
}

static void add_test(struct expr *test, struct for_info *info)
{
    if (info->more_tests)
	info->more_tests
	    = add_binop(info->more_tests, make_binop(id(sym_Or)), test);
    else {
	info->more_tests = make_binop_series();
	info->first_test = test;
    }
}

static void grovel_in_for_clause(struct in_for_clause *clause,
				 struct for_info *info)
{
    struct param *var = clause->vars->required_params;
    struct param *keyed_by = var->next;
    struct param *protocol = clause->protocol;
    struct symbol *coll = gensym();
    struct symbol *state = gensym();
    struct symbol *limit = gensym();
    struct symbol *next = gensym();
    struct symbol *done = gensym();
    struct symbol *curkey = gensym();
    struct symbol *curel = gensym();
    struct param_list *params = make_param_list();
    struct expr *expr;
    struct arglist *args;
    struct bindings *bindings;

    /* Bind the collection. */
    bind_temp(info->outer_body, id(coll), clause->collection);

    /* Bind the iteration protocol */
    push_param(make_param(id(curel), NULL), params);
    push_param(make_param(id(curkey), NULL), params);
    push_param(make_param(id(done), NULL), params);
    push_param(make_param(id(next), NULL), params);
    push_param(make_param(id(limit), NULL), params);
    push_param(make_param(id(state), NULL), params);
    args = make_argument_list();
    add_argument(args, make_argument(make_varref(id(coll))));

    if (clause->protocol)
	expr = make_varref(protocol->id);
    else
	expr = make_varref(id(sym_ForwardIterationProtocol));

    bindings = make_bindings(params, make_function_call(expr, args));
    add_constituent(info->outer_body, make_let(bindings));

    /* Add the test for being done with the collection. */
    args = make_argument_list();
    add_argument(args, make_argument(make_varref(id(coll))));
    add_argument(args, make_argument(make_varref(id(state))));
    add_argument(args, make_argument(make_varref(id(limit))));
    add_test(make_function_call(make_varref(id(done)), args), info);

    /* Bind the users variable to the current element in the inner body. */
    args = make_argument_list();
    add_argument(args, make_argument(make_varref(id(coll))));
    add_argument(args, make_argument(make_varref(id(state))));
    expr = make_function_call(make_varref(id(curel)), args);
    bind_param(info->inner_body, var, expr);

    /* Bind the keyed_by variable if supplied. */
    if (keyed_by) {
	args = make_argument_list();
	add_argument(args, make_argument(make_varref(id(coll))));
	add_argument(args, make_argument(make_varref(id(state))));
	expr = make_function_call(make_varref(id(curkey)), args);
	bind_param(info->inner_body, keyed_by, expr);
    }

    /* Free the clauses param_list, because we've extracted the two params */
    /* from it. */
    free(clause->vars);

    /* Advance the state in the steps. */
    args = make_argument_list();
    add_argument(args, make_argument(make_varref(id(coll))));
    add_argument(args, make_argument(make_varref(id(state))));
    expr = make_function_call(make_varref(id(next)), args);
    add_set(info->step_body, id(state), expr);
}

static void grovel_from_for_clause(struct from_for_clause *clause,
				   struct for_info *info)
{
    struct symbol *temp = gensym();
    struct symbol *bound = NULL;
    struct symbol *by_temp = NULL;
    struct expr *by = NULL;
    struct arglist *args;
    struct expr *expr;

    /* Bind the start in the outer body. */
    bind_temp(info->outer_body, id(temp), clause->from);

    /* Bind the bound if there is one. */
    if (clause->to) {
	bound = gensym();
	bind_temp(info->outer_body, id(bound), clause->to);
    }

    /* Figure out what by should be, binding it if necessary. */
    if (clause->by) {
	by_temp = gensym();
	bind_temp(info->outer_body, id(by_temp), clause->by);
	by = make_varref(id(by_temp));
    }
    else if (clause->to_kind == to_ABOVE)
	by = make_literal_ref(make_integer_literal(-1));
    else
	by = make_literal_ref(make_integer_literal(1));
    
    /* Bind the user variable in the middle body. */
    bind_params(info->middle_body, clause->vars, make_varref(id(temp)));

    /* Add the end test. */
    switch (clause->to_kind) {
      case to_TO:
	if (by_temp) {
	    struct expr *when_negative, *when_positive;

	    args = make_argument_list();
	    add_argument(args, make_argument(make_varref(id(temp))));
	    add_argument(args, make_argument(make_varref(id(bound))));
	    when_negative
		= make_function_call(make_varref(id(sym_Less)), args);

	    args = make_argument_list();
	    add_argument(args, make_argument(make_varref(id(bound))));
	    add_argument(args, make_argument(make_varref(id(temp))));
	    when_positive
		= make_function_call(make_varref(id(sym_Less)), args);

	    args = make_argument_list();
	    add_argument(args, make_argument(make_varref(id(by_temp))));
	    expr = make_function_call(make_varref(id(sym_NegativeP)),
				      args);

	    add_test(make_if(expr, make_expr_body(when_negative),
			     make_else(0, make_expr_body(when_positive))),
		     info);
	}
	else {
	    args = make_argument_list();
	    add_argument(args, make_argument(make_varref(id(bound))));
	    add_argument(args, make_argument(make_varref(id(temp))));
	    add_test(make_function_call(make_varref(id(sym_Less)), args),
		     info);
	}
	break;

      case to_ABOVE:
	args = make_argument_list();
	add_argument(args, make_argument(make_varref(id(temp))));
	add_argument(args, make_argument(make_varref(id(bound))));
	add_test(make_function_call(make_varref(id(sym_LessEqual)), args),
		 info);
	break;

      case to_BELOW:
	args = make_argument_list();
	add_argument(args, make_argument(make_varref(id(bound))));
	add_argument(args, make_argument(make_varref(id(temp))));
	add_test(make_function_call(make_varref(id(sym_LessEqual)), args),
		 info);
	break;

      case to_UNBOUNDED:
	break;

      default:
	lose("Bogus to kind in from for clause"); 
    }

    /* Advance the count by by */
    args = make_argument_list();
    add_argument
	(args, make_argument(make_varref(clause->vars->required_params->id)));
    add_argument(args, make_argument(by));
    expr = make_function_call(make_varref(id(sym_Plus)), args);
    add_set(info->step_body, id(temp), expr);
}

static void (*ForClauseGrovelers[])() = {
    grovel_equal_then_for_clause,
    grovel_in_for_clause,
    grovel_from_for_clause
};

static boolean expand_for_expr(struct expr **ptr)
{
    struct for_expr *e = (struct for_expr *)*ptr;
    struct for_info info;
    struct repeat_expr *repeat;
    struct expr *expr;
    struct loop_expr *loop;
    struct for_clause *clause, *next;

    info.outer_body = make_body();
    info.middle_body = make_body();
    info.first_test = NULL;
    info.more_tests = NULL;
    info.inner_body = make_body();
    info.step_body = e->body;

    /* Grovel the clauses. */
    for (clause = e->clauses; clause != NULL; clause = next) {
	cache_types(clause->vars, &info);
	(*ForClauseGrovelers[(int)clause->kind])(clause, &info);
	next = clause->next;
	free(clause);
    }

    /* Add the call to repeat to the step body. */
    repeat = (struct repeat_expr *)make_repeat();
    add_expr(info.step_body, (struct expr *)repeat);

    /* Wrap the step body with the ``if (end-test) ...'' (if necessary) and */
    /* add it to the inner body. */
    if (e->until)
	expr = make_if(e->until, NULL, make_else(0, info.step_body));
    else
	expr = make_body_expr(info.step_body);
    add_expr(info.inner_body, expr);

    /* Wrap the inner body with the implicit end tests and add it to the */
    /* middle body */
    if (info.more_tests)
	expr = make_if(make_binop_series_expr(info.first_test,info.more_tests),
		       NULL,
		       make_else(0, info.inner_body));
    else
	expr = make_body_expr(info.inner_body);
    add_expr(info.middle_body, expr);

    /* Add the final part to the middle body */
    if (e->finally)
	add_expr(info.middle_body, make_body_expr(e->finally));

    /* Make the loop, and add it to the outer body. */
    loop = (struct loop_expr *)make_loop(info.middle_body);
    repeat->loop = loop;
    add_expr(info.outer_body, (struct expr *)loop);

    /* Change this expression into the outer body. */
    *ptr = make_body_expr(info.outer_body);

    /* Free the loop expression now that we are done with it. */
    free(e);

    return TRUE;
}


/* Select expander */

static struct expr
    *make_select_condition(struct condition *conditions,
			   struct symbol *val, struct symbol *by)
{
    struct arglist *args
	= add_argument(add_argument(make_argument_list(),
				    make_argument(make_varref(id(val)))),
		       make_argument(conditions->cond));
    struct expr *cond = make_function_call(make_varref(id(by)), args);

    if (conditions->next) {
	struct body *true_body
	    = make_expr_body(make_literal_ref(make_true_literal()));
	struct body *rest_body
	    = make_expr_body(make_select_condition(conditions->next, val, by));

	free(conditions);

	return make_if(cond, true_body, make_else(0, rest_body));
    }
    else {
	free(conditions);
	return cond;
    }
}

static struct expr *expand_select_body(struct condition_body *body,
				       struct symbol *val, struct symbol *by)
{
    if (body) {
	struct condition_clause *clause = body->clause;

	if (clause->conditions) {
	    struct expr *cond
		= make_select_condition(clause->conditions, val, by);
	    struct expr *rest = expand_select_body(body->next, val, by);
	
	    free(body);

	    return make_if(cond, clause->body,
			   make_else(0, make_expr_body(rest)));
	}
	else {
	    free(body);
	    return make_body_expr(clause->body);
	}
    }
    else {
	struct expr *expr
	    = make_literal_ref(make_string_literal("fell through select"));
	struct arglist *args
	    = add_argument(make_argument_list(), make_argument(expr));

	return make_function_call(make_varref(id(sym_Error)), args);
    }
}

static boolean expand_select_expr(struct expr **ptr)
{
    struct select_expr *e = (struct select_expr *)*ptr;
    struct symbol *valtemp = gensym();
    struct symbol *bytemp = e->by ? gensym() : sym_Eq;
    struct body *body = make_body();

    bind_temp(body, id(valtemp), e->expr);
    if (e->by)
	bind_temp(body, id(bytemp), e->by);

    add_expr(body, expand_select_body(e->body, valtemp, bytemp));

    *ptr = make_body_expr(body);

    free(e);

    return TRUE;
}


/* Binop series expander */

static struct expr *make_binary_fn_call(struct id *op, struct expr *left,
					struct expr *right)
{
    struct arglist *args
	= add_argument(add_argument(make_argument_list(),
				    make_argument(left)),
		       make_argument(right));
    return make_function_call(make_varref(op), args);
}

static boolean expand_binop_series_expr(struct expr **ptr)
{
    struct binop_series_expr *e = (struct binop_series_expr *)*ptr;
    struct binop *stack = NULL;
    struct expr *left = e->first_operand;
    struct binop *op = e->first_binop;
    struct expr *right = op->operand;
    struct binop *next = op->next;

    while (next) {
	if (op->left_assoc
	      ? (op->precedence >= next->precedence)
	      : (op->precedence > next->precedence)) {
	    /* We want to reduce left.op.right */
	    struct expr *new = make_binary_fn_call(op->op, left, right);
	    free(op);
	    if (stack) {
		/* We want to reduce into right and pop the stack. */
		right = new;
		op = stack;
		stack = stack->next;
		left = op->operand;
	    }
	    else {
		/* We want to reduce into left and pop next. */
		left = new;
		op = next;
		right = op->operand;
		next = next->next;
	    }
	}
	else {
	    /* We want to shift this onto the stack. */
	    op->operand = left;
	    op->next = stack;
	    stack = op;
	    left = right;
	    op = next;
	    right = op->operand;
	    next = next->next;
	}
    }
    while (1) {
	right = make_binary_fn_call(op->op, left, right);
	free(op);
	if (stack == NULL)
	    break;
	op = stack;
	left = op->operand;
	stack = stack->next;
    }

    free(e);

    *ptr = right;

    return TRUE;
}


/* Simple expression expanders. */

static boolean expand_varref_expr(struct varref_expr **ptr)
{
    /* Nothing to do. */
    return FALSE;
}

static boolean expand_literal_expr(struct literal_expr **ptr)
{
    /* Nothing to do. */
    return FALSE;
}

static boolean expand_call_expr(struct call_expr **ptr)
{
    struct call_expr *e = *ptr;
    struct argument *arg;

    if (e->info && e->info->srctran) {
	if (e->func->kind != expr_VARREF)
	    lose("Source-transforming a call where the function "
		 "isn't a varref?");
	if ((*e->info->srctran)(ptr))
	    return TRUE;
    }

    expand_expr(&e->func);
    for (arg = e->args; arg != NULL; arg = arg->next)
	expand_expr(&arg->expr);
    return FALSE;
}

static boolean expand_dot_expr(struct expr **ptr)
{
    struct dot_expr *e = (struct dot_expr *)*ptr;

    expand_expr(&e->arg);
    expand_expr(&e->func);

    return FALSE;
}

static struct literal *extract_literal(struct body *body)
{
    struct expr *expr;

    if (body->head == NULL)
	return make_false_literal();
    if (body->head->next != NULL)
	return NULL;
    if (body->head->kind != constituent_EXPR)
	return NULL;
    expr = ((struct expr_constituent *)body->head)->expr;
    if (expr->kind != expr_LITERAL)
	return NULL;
    else
	return ((struct literal_expr *)expr)->lit;
}

static boolean expand_if_expr(struct expr **ptr)
{
    struct if_expr *e = *(struct if_expr **)ptr;

    expand_expr(&e->cond);

    if (e->cond->kind == expr_LITERAL) {
	struct literal *lit = ((struct literal_expr *)e->cond)->lit;
	if (lit->kind == literal_FALSE) {
	    free_body(e->consequent);
	    *ptr = make_body_expr(e->alternate);
	}
	else {
	    *ptr = make_body_expr(e->consequent);
	    free_body(e->alternate);
	}
	free_expr(e->cond);
	free(e);
	return TRUE;
    }

    expand_body(e->consequent, FALSE);
    expand_body(e->alternate, FALSE);

    if (e->cond->kind == expr_IF) {
	struct if_expr *inner = (struct if_expr *)e->cond;
	struct literal *inner_consequent = extract_literal(inner->consequent);
	struct literal *inner_alternate = extract_literal(inner->alternate);

	if (inner_consequent && inner_alternate) {
	    if (inner_consequent->kind != literal_FALSE)
		if (inner_alternate->kind != literal_FALSE) {
		    /* They are both true.  So no matter what we are going */
		    /* to only do the consequent.  But we need to eval the */
		    /* condition none the less. */
		    struct constituent *c = make_expr_constituent(inner->cond);
		    c->next = e->consequent->head;
		    e->consequent->head = c;
		    if (c->next == NULL)
			e->consequent->tail = &c->next;
		    free_body(e->alternate);
		    *ptr = make_body_expr(e->consequent);
		    free(e);
		}
		else {
		    /* The inner consequent is true and the inner alternate */
		    /* is false.  So we just use the inner condition. */
		    e->cond = inner->cond;
		}
	    else
		if (inner_alternate->kind != literal_FALSE) {
		    /* The inner consequent is false and the inner alternate */
		    /* is true.  Therefore, we use the inner condition but */
		    /* which the consequent and alternate. */
		    struct body *temp = e->consequent;
		    e->cond = inner->cond;
		    e->consequent = e->alternate;
		    e->alternate = temp;
		}
		else {
		    /* Both are false, so we always do the alternate. */
		    struct constituent *c = make_expr_constituent(inner->cond);
		    c->next = e->alternate->head;
		    e->alternate->head = c;
		    if (c->next == NULL)
			e->alternate->tail = &c->next;
		    free_body(e->consequent);
		    *ptr = make_body_expr(e->alternate);
		    free(e);
		}
	    free_body(inner->consequent);
	    free_body(inner->alternate);
	    free(inner);
	    return FALSE;
	}
	else {
	    struct body *consequent = dup_body(e->consequent);
	    struct body *alternate = dup_body(e->alternate);
	    if (consequent != NULL && alternate != NULL) {
		e->cond = inner->cond;
		e->consequent
		    = make_expr_body(make_if(make_body_expr(inner->consequent),
					     e->consequent,
					     make_else(0, e->alternate)));
		e->alternate
		    = make_expr_body(make_if(make_body_expr(inner->alternate),
					     consequent,
					     make_else(0, alternate)));
		free(inner);

		return TRUE;
	    }
	    else {
		if (consequent)
		    free_body(consequent);
		if (alternate)
		    free_body(alternate);
		return FALSE;
	    }
	}
    }
    else
	return FALSE;
}

static boolean expand_varset_expr(struct varset_expr **ptr)
{
    struct varset_expr *e = *ptr;

    expand_expr(&e->value);

    return FALSE;
}

static boolean expand_body_expr(struct body_expr **ptr)
{
    expand_body((*ptr)->body, FALSE);
    return FALSE;
}

static boolean expand_method_expr(struct expr **ptr)
{
    struct method_expr *e = (struct method_expr *)*ptr;
    struct method *method = e->method;

    if (method->specializers) {
	expand_method_for_compile(method);
	return FALSE;
    }
    else {
	struct body *body = make_body();
	add_method_wrap(body, method);
	add_expr(body, (struct expr *)e);
	*ptr = make_body_expr(body);
	return TRUE;
    }
}

static boolean expand_loop_expr(struct loop_expr **ptr)
{
    expand_body((*ptr)->body, FALSE);
    return FALSE;
}

static boolean expand_repeat_expr(struct repeat_expr **ptr)
{
    /* No nothing. */
    return FALSE;
}

static boolean expand_error_expr(struct expr **ptr)
{
    lose("Called expand on a parse tree with errors?");
    return FALSE;
}

static boolean (*ExpressionExpanders[])() = {
    expand_varref_expr, expand_literal_expr, expand_call_expr,
    expand_method_expr, expand_dot_expr, expand_body_expr, expand_block_expr,
    expand_case_expr, expand_if_expr, expand_for_expr, expand_select_expr,
    expand_varset_expr, expand_binop_series_expr, expand_loop_expr,
    expand_repeat_expr, expand_error_expr
};

static void expand_expr(struct expr **ptr)
{
    struct expr *expr;

    do {
	expr = *ptr;
    } while ((*ExpressionExpanders[(int)expr->kind])(ptr));
}


/* Expand */

static void expand_body(struct body *body, boolean top_level)
{
    struct constituent **prev, *next;

    if (body->head == NULL)
	body->head
	    = make_expr_constituent(make_literal_ref(make_false_literal()));

    prev = &body->head;
    do {
	next = (*prev)->next;
	while (expand_constituent(prev, top_level))
	    ;
	prev = &(*prev)->next;
	*prev = next;
    } while (next);
}

void expand(struct body *body)
{
    expand_body(body, TRUE);
}


/* Call src->src transforms */

static void free_function_ref(struct expr *expr)
{
    struct varref_expr *varref = (struct varref_expr *)expr;

    free(varref->var);
    free(varref);
}

static boolean srctran_varref_assignment(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *args = e->args;
    struct varref_expr *varref = (struct varref_expr *)args->expr;
    struct argument *value = args->next;

    *ptr = make_varset(varref->var, value->expr);

    free(value);
    free(varref);
    free(args);
    free_function_ref(e->func);
    free(e);

    return TRUE;
}

static boolean srctran_call_assignment(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *args = e->args;
    struct call_expr *comb = (struct call_expr *)args->expr;
    struct argument *value = args->next;
    struct body *body;
    struct symbol *temp;
    struct id *var;

    if (comb->func->kind != expr_VARREF)
	return FALSE;
    var = ((struct varref_expr *)comb->func)->var;
    change_to_setter(var);
    comb->info = lookup_function_info(var, FALSE);

    temp = gensym();
    body = make_body();
    bind_temp(body, id(temp), value->expr);

    value->expr = make_varref(id(temp));
    value->next = comb->args;
    comb->args = value;
    add_expr(body, (struct expr *)comb);

    add_expr(body, make_varref(id(temp)));

    *ptr = make_body_expr(body);

    free(args);
    free_function_ref(e->func);
    free(e);

    return TRUE;
}

static boolean srctran_dot_assignment(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *lhs = e->args;
    struct dot_expr *dot = (struct dot_expr *)lhs->expr;
    struct argument *value = lhs->next;
    struct expr *func = dot->func;
    struct arglist *args;
    struct body *body;
    struct symbol *temp;

    if (func->kind != expr_VARREF)
	return FALSE;
    change_to_setter(((struct varref_expr *)func)->var);

    temp = gensym();
    body = make_body();
    bind_temp(body, id(temp), value->expr);

    value->expr = make_varref(id(temp));
    args = add_argument(make_argument_list(), value);
    args = add_argument(args, make_argument(dot->arg));
    add_expr(body, make_function_call(dot->func, args));

    add_expr(body, make_varref(id(temp)));

    *ptr = make_body_expr(body);

    free(dot);
    free(lhs);
    free_function_ref(e->func);
    free(e);

    return TRUE;
}

static boolean srctran_assignment(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *lhs = e->args;

    /* Make sure there are only two arguments. */
    if (lhs==NULL || lhs->next==NULL || lhs->next->next!=NULL) {
	struct varref_expr *func = (struct varref_expr *)e->func;
	error(func->var->line, ":= invoked with other than two arguments");
	return FALSE;
    }

    switch (lhs->expr->kind) {
      case expr_VARREF:
	return srctran_varref_assignment(ptr);

      case expr_CALL:
	return srctran_call_assignment(ptr);

      case expr_DOT:
	return srctran_dot_assignment(ptr);

      default:
	{
	    struct varref_expr *func = (struct varref_expr *)e->func;
	    error(func->var->line, ":= applied to non-assignable expression.");
	}
	return FALSE;
    }
}

static boolean srctran_and(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *arg = e->args;

    if (arg == NULL) {
	*ptr = make_literal_ref(make_false_literal());
	free_function_ref(e->func);
    }
    else if (arg->next == NULL) {
	*ptr = arg->expr;
	free_function_ref(e->func);
	free(arg);
    }
    else {
	e->args = arg->next;
	*ptr = make_if(arg->expr, make_expr_body((struct expr *)e), NULL);
	free(arg);
    }
    return TRUE;
}

static boolean srctran_or(struct expr **ptr)
{
    struct call_expr *e = (struct call_expr *)*ptr;
    struct argument *arg = e->args;

    if (arg == NULL) {
	*ptr = make_literal_ref(make_true_literal());
	free_function_ref(e->func);
    }
    else if (arg->next == NULL) {
	*ptr = arg->expr;
	free_function_ref(e->func);
	free(arg);
    }
    else {
	struct symbol *temp = gensym();
	struct body *body = make_body();

	e->args = arg->next;
	bind_temp(body, id(temp), arg->expr);
	add_expr(body,
		 make_if(make_varref(id(temp)),
			 make_expr_body(make_varref(id(temp))),
			 make_else(0, make_expr_body((struct expr *)e))));
	*ptr = make_body_expr(body);
	free(arg);
    }

    return TRUE;
}



/* Initialization stuff. */

static void set_srctran(char *name, boolean (*srctran)(), boolean internal)
{
    struct id *identifier = id(symbol(name));
    struct function_info *info;

    identifier->internal = internal;
    info = lookup_function_info(identifier, TRUE);
    info->srctran = srctran;

    free(identifier);
}

void init_expand(void)
{
    set_srctran(":=", srctran_assignment, TRUE);
    set_srctran(":=", srctran_assignment, FALSE);
    set_srctran("&", srctran_and, TRUE);
    set_srctran("&", srctran_and, FALSE);
    set_srctran("|", srctran_or, TRUE);
    set_srctran("|", srctran_or, FALSE);
}
