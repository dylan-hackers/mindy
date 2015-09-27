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
* This file prints out parts of the parse tree in a human readable
* format for debugging purposes.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "src.h"
#include "sym.h"
#include "literal.h"
#include "print.h"
#include "lose.h"

static char spaces[] = "                                                            ";
#define indent(x) (spaces+sizeof(spaces)-1-((x)%60))



/* Property lists. */

static void print_plist(struct plist *plist, int depth)
{
    struct property *prop;

    if (plist == NULL)
	return;

    printf("%sproperty list\n", indent(depth));
    for (prop = plist->head; prop != NULL; prop = prop->next) {
	printf("%s%s:\n", indent(depth+1), prop->keyword->name);
	print_expr(prop->expr, depth+2);
    }
    printf("%send property list\n", indent(depth));
}


/* Literal printing. */

static void print_symbol_literal(struct symbol_literal *l, int depth)
{
    printf("%ssymbol %s\n", indent(depth), l->symbol->name);
}

static void print_integer_literal(struct integer_literal *l, int depth)
{
    printf("%sinteger %ld\n", indent(depth), l->value);
}

static void print_single_float_literal(struct single_float_literal *l,
				       int depth)
{
    printf("%ssingle float %g\n", indent(depth), l->value);
}

static void print_double_float_literal(struct double_float_literal *l,
				       int depth)
{
    printf("%sdouble float %g\n", indent(depth), l->value);
}

static void print_extended_float_literal(struct extended_float_literal *l,
					 int depth)
{
    printf("%sextended float %g\n", indent(depth), (double)l->value);
}

static void print_char(int c, int quote)
{
    switch (c) {
    case '"':
    case '\'':
        if (c == quote)
            printf("\\%c", c);
        else
            putchar(c);
        break;
    case '\b': printf("\\b");	break;
    case '\f': printf("\\f");	break;
    case '\n': printf("\\n");	break;
    case '\r': printf("\\r");	break;
    case '\t': printf("\\t");	break;
    default:
        if (' ' <= c/* && c <= '~'*/)
            putchar(c);
        else
            printf("\\%03o", c);
        break;
    }
}

static void print_character_literal(struct character_literal *l, int depth)
{
    printf("%scharacter '", indent(depth));
    print_char(l->value, '\'');
    printf("'\n");
}

static void print_string_literal(struct string_literal *l, int depth)
{
    char *ptr;
    int i;

    printf("%sstring \"", indent(depth));
    ptr = (char *)l->chars;
    for (i = 0; i < l->length; i++)
	print_char(*ptr++, '"');
    printf("\"\n");
}

static void print_list_literal(struct list_literal *l, int depth)
{
    struct literal *piece;

    printf("%slist\n", indent(depth));
    for (piece = l->first; piece != NULL; piece = piece->next)
	print_literal(piece, depth+1);
    if (l->tail) {
	printf("%sdot\n", indent(depth));
	print_literal(l->tail, depth+1);
    }
    printf("%send list\n", indent(depth));
}

static void print_vector_literal(struct vector_literal *l, int depth)
{
    struct literal *piece;

    printf("%svector\n", indent(depth));
    for (piece = l->first; piece != NULL; piece = piece->next)
	print_literal(piece, depth+1);
    printf("%send vector\n", indent(depth));
}

static void print_true_literal(struct literal *l, int depth)
{
    printf("%s#t\n", indent(depth));
}

static void print_false_literal(struct literal *l, int depth)
{
    printf("%s#f\n", indent(depth));
}

static void print_unbound_literal(struct literal *l, int depth)
{
    printf("%s#unbound\n", indent(depth));
}

static void (*LiteralPrinters[(int)literal_Kinds])() = {
    print_symbol_literal, print_integer_literal,
    print_single_float_literal, print_double_float_literal,
    print_extended_float_literal, print_character_literal,
    print_string_literal, print_list_literal, print_vector_literal,
    print_true_literal, print_false_literal, print_unbound_literal
};

void print_literal(struct literal *literal, int depth)
{
    (LiteralPrinters[(int)literal->kind])(literal, depth);
}


/* Utility printers. */

static void print_param(struct param *param, int depth, char *kind, int index)
{
    if (param->type_temp)
	printf("%s%s %d: %s :: %s\n", indent(depth), kind, index,
	       param->id->symbol->name, param->type_temp->name);
    else {
	printf("%s%s %d: %s\n", indent(depth), kind, index,
	       param->id->symbol->name);
	if (param->type) {
	    printf("%styped\n", indent(depth+1));
	    print_expr(param->type, depth+2);
	}
    }
}

static void print_param_list(struct param_list *list, int depth)
{
    struct param *p;
    struct keyword_param *k;
    int i;

    for (p = list->required_params, i = 0; p != NULL; p = p->next, i++)
	print_param(p, depth, "param", i);
    if (list->next_param)
	printf("%s#next %s\n", indent(depth), list->next_param->symbol->name);
    if (list->rest_param)
	printf("%s#rest %s\n", indent(depth), list->rest_param->symbol->name);
    if (list->allow_keys) {
	printf("%s#key\n", indent(depth));
	for (k = list->keyword_params; k != NULL; k = k->next) {
	    if (k->id->symbol != k->keyword)
		printf("%s%s: %s", indent(depth+1), k->keyword->name,
		       k->id->symbol->name);
	    else
		printf("%s%s:", indent(depth+1), k->keyword->name);
	    if (k->type_temp)
		printf(" :: %s\n", k->type_temp->name);
	    else if (k->type) {
		printf("\n%styped\n", indent(depth+2));
		print_expr(k->type, depth+3);
	    }
	    else
		putchar('\n');
	    if (k->def) {
		printf("%sdefault\n", indent(depth+2));
		print_expr(k->def, depth+3);
	    }
	}
	if (list->all_keys)
	    printf("%s#all-keys\n", indent(depth));
    }
}

static void print_bindings(struct bindings *bindings, int depth)
{
    printf("%sbind\n", indent(depth));
    print_param_list(bindings->params, depth+1);
    if (bindings->expr) {
	printf("%sto\n", indent(depth));
	print_expr(bindings->expr, depth+1);
    }
}

static void print_return_type_list(struct return_type_list *l, int depth)
{
    int i;
    struct return_type *r;

    if (l == NULL)
	return;

    printf("%sreturns\n", indent(depth));
    for (i = 0, r = l->req_types; r != NULL; i++, r = r->next)
	if (r->temp)
	    printf("%sresult %d: %s\n", indent(depth+1), i, r->temp->name);
	else if (r->type) {
	    printf("%sresult %d:\n", indent(depth+1), i);
	    print_expr(r->type, depth+2);
	}
	else
	    printf("%sresult %d.\n", indent(depth+1), i);
    if (l->rest_temp)
	printf("%s#rest %s\n", indent(depth+1), l->rest_temp->name);
    else if (l->rest_type) {
	printf("%s#rest\n", indent(depth+1));
	print_expr(l->rest_type, depth+2);
    }
    else if (l->restp)
	printf("%s#rest foo :: <object>\n", indent(depth+1));
    printf("%send returns\n", indent(depth));
}

static char *debug_name_string(struct literal *literal)
{
    switch (literal->kind) {
      case literal_SYMBOL:
	return (char *)((struct symbol_literal *)literal)->symbol->name;
      case literal_STRING:
	return (char *)((struct string_literal *)literal)->chars;
      default:
	return "with strange debug name";
    }
}

static void print_method(struct method *method, int depth)
{
    fputs(indent(depth), stdout);
    if (method->top_level)
	fputs("top level ", stdout);
    if (method->debug_name)
	printf("method %s\n", debug_name_string(method->debug_name));
    else if (method->name)
	printf("method %s\n", method->name->symbol->name);
    else
	fputs("anonymous method\n", stdout);

    print_param_list(method->params, depth+1);
    if (method->specializers) {
	printf("%sspecializers\n", indent(depth));
	print_expr(method->specializers, depth+1);
    }
    print_return_type_list(method->rettypes, depth);
    printf("%sbody\n", indent(depth));
    print_body(method->body, depth+1);

    printf("%send ", indent(depth));
    if (method->top_level)
	fputs("top level ", stdout);
    if (method->debug_name)
	printf("method %s\n", debug_name_string(method->debug_name));
    else if (method->name)
	printf("method %s\n", method->name->symbol->name);
    else
	fputs("anonymous method\n", stdout);
}

static void print_exception_clause(struct exception_clause *clause, int depth)
{
    printf("%sexception\n%stype\n", indent(depth), indent(depth+1));
    print_expr(clause->type, depth+2);
    if (clause->condition)
	printf("%scondition %s\n", indent(depth+1),
	       clause->condition->symbol->name);
    print_plist(clause->plist, depth+1);
    printf("%shandler\n", indent(depth+1));
    print_body(clause->body, depth+2);
}

static void print_condition_body(struct condition_body *body, int depth)
{
    struct condition *c;

    while (body != NULL) {
	c = body->clause->conditions;
	if (c) {
	    printf("%swhen\n", indent(depth));
	    while (1) {
		print_expr(c->cond, depth+1);
		c = c->next;
		if (c == NULL)
		    break;
		printf("%sor\n", indent(depth));
	    }
	    printf("%s=>\n", indent(depth));
	}
	else
	    printf("%sotherwise\n", indent(depth));
	print_body(body->clause->body, depth+1);
	body = body->next;
    }
}


/* Expr printing stuff. */

static void print_varref_expr(struct varref_expr *e, int depth)
{
    printf("%svarref %s\n", indent(depth), e->var->symbol->name);
}

static void print_literal_expr(struct literal_expr *e, int depth)
{
    print_literal(e->lit, depth);
}

static void print_call_expr(struct call_expr *e, int depth)
{
    int i;
    struct argument *arg;

    printf("%scall\n%sfunction:\n", indent(depth), indent(depth+1));
    print_expr(e->func, depth+2);
    for (i = 0, arg = e->args; arg != NULL; i++, arg = arg->next) {
	printf("%sargument %d\n", indent(depth+1), i);
	print_expr(arg->expr, depth+2);
    }
    printf("%send call\n", indent(depth));
}

static void print_method_expr(struct method_expr *e, int depth)
{
    print_method(e->method, depth);
}

static void print_dot_expr(struct dot_expr *e, int depth)
{
    printf("%sdot operator\n%sargument\n", indent(depth), indent(depth+1));
    print_expr(e->arg, depth+2);
    printf("%sfunction\n", indent(depth+1));
    print_expr(e->func, depth+2);
    printf("%send dot operator\n", indent(depth));
}

static void print_body_expr(struct body_expr *e, int depth)
{
    printf("%sbegin\n", indent(depth));
    print_body(e->body, depth+1);
    printf("%send\n", indent(depth));
}

static void print_block_expr(struct block_expr *e, int depth)
{
    struct exception_clause *clause;

    if (e->exit_fun)
	printf("%sblock (%s)\n", indent(depth), e->exit_fun->symbol->name);
    else
	printf("%sblock <no exit function>\n", indent(depth));
    print_body(e->body, depth+1);
    if (e->inner)
	for (clause = e->inner; clause != NULL; clause = clause->next)
	    print_exception_clause(clause, depth);
    if (e->cleanup) {
	printf("%scleanup\n", indent(depth));
	print_body(e->cleanup, depth+1);
    }
    if (e->outer)
	for (clause = e->outer; clause != NULL; clause = clause->next)
	    print_exception_clause(clause, depth);
    printf("%send block\n", indent(depth));
}

static void print_case_expr(struct case_expr *e, int depth)
{
    print_condition_body(e->body, depth);
    printf("%send case\n", indent(depth));
}

static void print_if_expr(struct if_expr *e, int depth)
{
    printf("%sif\n", indent(depth));
    print_expr(e->cond, depth+1);
    printf("%sconsequent\n", indent(depth));
    print_body(e->consequent, depth+1);
    printf("%salternate\n", indent(depth));
    print_body(e->alternate, depth+1);
    printf("%send if\n", indent(depth));
}

static void print_for_expr(struct for_expr *e, int depth)
{
    struct for_clause *clause;
    int i;

    printf("%sfor\n", indent(depth));
    for (clause=e->clauses, i=0; clause != NULL; clause=clause->next, i++) {
	print_param_list(clause->vars, depth+1);
	switch (clause->kind) {
	  case for_EQUAL_THEN:
	    {
		struct equal_then_for_clause *c
		    = (struct equal_then_for_clause *)clause;
		printf("%sequal\n", indent(depth+2));
		print_expr(c->equal, depth+3);
		printf("%sthen\n", indent(depth+2));
		print_expr(c->then, depth+3);
	    }
	    break;
	  case for_IN:
	    {
		struct in_for_clause *c = (struct in_for_clause *)clause;
		printf("%sin\n", indent(depth+2));
		print_expr(c->collection, depth+3);
	    }
	    break;
	  case for_FROM:
	    {
		struct from_for_clause *c = (struct from_for_clause *)clause;
		printf("%sfrom\n", indent(depth+2));
		print_expr(c->from, depth+3);
		if (c->to) {
		    static char *to_kinds[] = {"to", "above", "below"};
		    printf("%s%s\n", indent(depth+2),
			   to_kinds[(int)c->to_kind]);
		    print_expr(c->to, depth+3);
		}
		if (c->by) {
		    printf("%sby\n", indent(depth+2));
		    print_expr(c->by, depth+3);
		}
	    }
	    break;
	  default:
	    lose("Bogus for clause kind.");
	}
    }
    if (e->until) {
	printf("%suntil\n", indent(depth));
	print_expr(e->until, depth+1);
    }
    printf("%sdo\n", indent(depth));
    print_body(e->body, depth+1);
    if (e->finally) {
	printf("%sfinally\n", indent(depth));
	print_body(e->finally, depth+1);
    }
    printf("%send for\n", indent(depth));
}

static void print_select_expr(struct select_expr *e, int depth)
{
    printf("%sselect\n", indent(depth));
    print_expr(e->expr, depth+1);
    if (e->by) {
	printf("%sby\n", indent(depth));
	print_expr(e->by, depth+1);
    }
    print_condition_body(e->body, depth);
    printf("%send select\n", indent(depth));
}

static void print_varset_expr(struct varset_expr *e, int depth)
{
    printf("%svarset %s\n", indent(depth), e->var->symbol->name);
    print_expr(e->value, depth+1);
    printf("%send varset\n", indent(depth));
}

static void print_binop_series_expr(struct binop_series_expr *e, int depth)
{
    struct binop *b;

    printf("%sbinop series\n", indent(depth));
    print_expr(e->first_operand, depth+1);
    for (b = e->first_binop; b != NULL; b = b->next) {
	printf("%sbinop %s\n", indent(depth), b->op->symbol->name);
	print_expr(b->operand, depth+1);
    }
    printf("%send binop series\n", indent(depth));
}

static void print_loop_expr(struct loop_expr *e, int depth)
{
    printf("%sloop\n", indent(depth));
    print_body(e->body, depth+1);
    printf("%send loop\n", indent(depth));
}

static void print_repeat_expr(struct repeat_expr *e, int depth)
{
    printf("%srepeat\n", indent(depth));
}

static void print_error_expr(struct expr *e, int depth)
{
    printf("%serror expr\n", indent(depth));
}

static void (*ExprPrinters[(int)expr_Kinds])() = {
    print_varref_expr, print_literal_expr, print_call_expr,
    print_method_expr, print_dot_expr, print_body_expr, print_block_expr,
    print_case_expr, print_if_expr, print_for_expr, print_select_expr,
    print_varset_expr, print_binop_series_expr, print_loop_expr,
    print_repeat_expr, print_error_expr
};

void print_expr(struct expr *e, int depth)
{
    (*ExprPrinters[(int)e->kind])(e, depth);
}


/* Constituent printing stuff. */

static void
    print_defconst_constituent(struct defconst_constituent *c, int depth)
{
    printf("%sdefine constant\n", indent(depth));
    print_bindings(c->bindings, depth+1);
    if (c->tlf) {
	printf("%sinitializer\n", indent(depth));
	print_method(c->tlf, depth+1);
    }
    printf("%send define constant\n", indent(depth));
}

static void print_defvar_constituent(struct defvar_constituent *c, int depth)
{
    printf("%sdefine variable\n", indent(depth));
    print_bindings(c->bindings, depth+1);
    if (c->tlf) {
	printf("%sinitializer\n", indent(depth));
	print_method(c->tlf, depth+1);
    }
    printf("%send define variable\n", indent(depth));
}

static void
    print_defmethod_constituent(struct defmethod_constituent *c, int depth)
{
    printf("%sdefine method\n", indent(depth));
    if (c->tlf)
	print_method(c->tlf, depth+1);
    else
	print_method(c->method, depth+1);
    printf("%send define method\n", indent(depth));
}

static void
    print_defdomain_constituent(struct defdomain_constituent *c, int depth)
{
    int i;
    struct argument *arg;

    printf("%sdefine sealed domain\n", indent(depth));
    printf("%sname: %s\n", indent(depth+1), c->name->symbol->name);

    if (c->types) {
	for (i = 0, arg = c->types; arg != NULL; i++, arg = arg->next) {
	    printf("%stype %d\n", indent(depth+1), i);
	    print_expr(arg->expr, depth+2);
	}
    }

    if (c->tlf) {
	printf("%stl method\n", indent(depth));
	print_method(c->tlf, depth+1);
    }

    printf("%send define generic\n", indent(depth));
}

static void
    print_defgeneric_constituent(struct defgeneric_constituent *c, int depth)
{
    printf("%sdefine generic\n", indent(depth));
    printf("%sname: %s\n", indent(depth+1), c->name->symbol->name);
    print_param_list(c->params, depth+1);
    print_return_type_list(c->rettypes, depth+1);
    print_plist(c->plist, depth+1);
    if (c->tlf) {
	printf("%ssignature generator\n", indent(depth));
	print_method(c->tlf, depth+1);
    }
    printf("%send define generic\n", indent(depth));
}

static void
    print_defclass_constituent(struct defclass_constituent *c, int depth)
{
    static char *alloc[] = {"instance", "class", "each-subclass",
				"constant", "virtual"};
    struct superclass *super;
    struct slot_spec *slot;
    struct initarg_spec *initarg;
    struct inherited_spec *inherited;

    printf("%sdefine class\n", indent(depth));
    printf("%sname: %s\n", indent(depth+1), c->name->symbol->name);
    if (c->tlf1) {
	printf("%sphase 1:\n", indent(depth+1));
	print_method(c->tlf1, depth+2);
	printf("%sphase 2:\n", indent(depth+1));
	print_method(c->tlf2, depth+2);
    }
    else {
	printf("%ssupers:\n", indent(depth+1));
	for (super = c->supers; super != NULL; super = super->next)
	    print_expr(super->expr, depth+2);
	printf("%sslots:\n", indent(depth+1));
	for (slot = c->slots; slot != NULL; slot = slot->next) {
	    printf("%s%s slot, %s allocation\n", indent(depth+2),
		   slot->name ? (char*)slot->name->symbol->name : "anonymous",
		   alloc[(int)slot->alloc]);
	    if (slot->type) {
		printf("%stype:\n", indent(depth+2));
		print_expr(slot->type, depth+3);
	    }
	    print_plist(slot->plist, depth+2);
	}
        printf("%sinitialization arguments:\n", indent(depth+1));
        for (initarg = c->initargs; initarg != NULL;
	     initarg = initarg->next) {
            printf("%s%s%s initarg\n", indent(depth+2),
                   initarg->keyword->name,
                   initarg->required ? " required " : "");
            print_plist(initarg->plist, depth+2);
        }
	printf("%sinherited slots:\n", indent(depth+1));
	for (inherited = c->inheriteds; inherited != NULL;
	     inherited = inherited->next) {
	    printf("%s%s inherited slot\n", indent(depth+2),
		   inherited->name->symbol->name);
	    print_plist(inherited->plist, depth+2);
	}
    }
    printf("%send define class\n", indent(depth));
}

static void print_expr_constituent(struct expr_constituent *c, int depth)
{
    print_expr(c->expr, depth);
}

static void print_local_constituent(struct local_constituent *c, int depth)
{
    struct method *method;

    printf("%slocal\n", indent(depth));
    for (method = c->methods; method != NULL; method = method->next_local)
	print_method(method, depth+1);
    printf("%sbody\n", indent(depth));
    print_body(c->body, depth+1);
    printf("%send local\n", indent(depth));
}

static void print_handler_constituent(struct handler_constituent *c, int depth)
{
    printf("%shandler\n", indent(depth));
    if (c->type) {
	printf("%stype:\n", indent(depth+1));
	print_expr(c->type, depth+2);
	printf("%sfunction:\n", indent(depth+1));
	print_expr(c->func, depth+2);
	print_plist(c->plist, depth+1);
    }
    printf("%sbody\n", indent(depth));
    print_body(c->body, depth+1);
    printf("%send handler\n", indent(depth));
}

static void print_let_constituent(struct let_constituent *c, int depth)
{
    print_bindings(c->bindings, depth);
    printf("%sbody\n", indent(depth));
    print_body(c->body, depth+1);
    printf("%send bind\n", indent(depth));
}

static void print_tlf_constituent(struct tlf_constituent *c, int depth)
{
    printf("%stop level form\n", indent(depth));
    print_method(c->form, depth+1);
    printf("%send top level form\n", indent(depth));
}

static void print_error_constituent(struct constituent *c, int depth)
{
    printf("%serror constituent\n", indent(depth));
}

static void print_defmodule_constituent(struct constituent *c, int depth)
{
    printf("%sdefmodule constituent\n", indent(depth));
}

static void print_deflibrary_constituent(struct constituent *c, int depth)
{
    printf("%sdeflibrary constituent\n", indent(depth));
}

static void (*ConstituentPrinters[(int)constituent_Kinds])() = {
    print_defconst_constituent, print_defvar_constituent,
    print_defmethod_constituent, print_defdomain_constituent,
    print_defgeneric_constituent, print_defclass_constituent,
    print_expr_constituent, print_local_constituent, print_handler_constituent,
    print_let_constituent, print_tlf_constituent, print_error_constituent,
    print_defmodule_constituent, print_deflibrary_constituent
};

void print_constituent(struct constituent *c, int depth)
{
    (ConstituentPrinters[(int)c->kind])(c, depth);
}

void print_body(struct body *body, int depth)
{
    struct constituent *c;

    for (c = body->head; c != NULL; c = c->next)
	print_constituent(c, depth);
}
