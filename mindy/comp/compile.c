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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/compile.c,v 1.3 1994/03/25 05:03:18 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindycomp.h"
#include "src.h"
#include "dump.h"
#include "lexenv.h"
#include "envanal.h"
#include "sym.h"
#include "literal.h"
#include "compile.h"
#include "byteops.h"
#include "info.h"

#define BLOCK_SIZE (4*1024)

#define TAIL -1
#define FUNC -2
#define make_want(req,restp) (((req)<<1)|((restp)?1:0))
#define want_req(want) ((want)>>1)
#define want_restp(want) ((want)&1)
#define SINGLE make_want(1,FALSE)

static struct component *compile_method(struct method *method);
static void compile_expr(struct expr *expr, struct component *component,
			 int want);
static void compile_body(struct body *body, struct component *component,
			 int want);
static void compile_tl_body(struct body *body);


/* Utilities. */

static void grow_component(struct component *component)
{
    struct block *block = malloc(BLOCK_SIZE);
    struct block *cur = component->cur_block;

    block->next = NULL;

    if (cur) {
	cur->end = component->fill;
	component->bytes += cur->end - cur->bytes;
    }
    else if (component->blocks)
	lose("Attempt to add more stuff to a component we were done with?");
    else
	component->blocks = block;

    component->cur_block = block;
    component->fill = block->bytes;
    component->end = BLOCK_SIZE + (unsigned char *)block;
}

static void emit_byte(struct component *component, int op)
{
    if (component->fill == component->end)
	grow_component(component);

    *component->fill++ = op;
}

#define emit_op emit_byte

static void emit_4bytes(struct component *component, unsigned value)
{
    emit_byte(component, value & 0xff);
    emit_byte(component, (value>>8) & 0xff);
    emit_byte(component, (value>>16) & 0xff);
    emit_byte(component, (value>>24) & 0xff);
}

static void emit_op_and_arg(struct component *component, int op, unsigned arg)
{
    if (arg < 0xf)
	emit_byte(component, op|arg);
    else {
	emit_byte(component, op|0xf);
	if (arg < 0xff)
	    emit_byte(component, arg);
	else {
	    emit_byte(component, 0xff);
	    emit_4bytes(component, arg);
	}
    }
}

static void emit_call_op_and_arg(struct component *component,
				 int op, unsigned arg)
{
    if (arg < 0xf)
	emit_byte(component, op|arg);
    else {
	emit_byte(component, op|0xf);
	if (arg < 0xff)
	    emit_byte(component, arg);
	else {
	    emit_byte(component, 0xff);
	    emit_4bytes(component, arg);
	}
	emit_byte(component, op);
    }
}

static void emit_wants(struct component *component, int want)
{
    if (want == TAIL)
	lose("didn't tail-call when we should?");
    if (want == FUNC)
	lose("calling for multiple values when we want a function?");

    if (want < 0xff)
	emit_byte(component, want);
    else {
	emit_byte(component, 0xff);
	emit_4bytes(component, (unsigned)want);
    }
}

static unsigned char *reserve_space(struct component *component, int count)
{
    unsigned char *res;

    if (component->fill + count > component->end)
	grow_component(component);
    
    res = component->fill;
    component->fill = res + count;

    return res;
}

static int current_position(struct component *component)
{
    return component->bytes + (component->fill - component->cur_block->bytes);
}

static void write_branch_displacement(unsigned char *where, int here,
				      int there)
{
    int disp = there - here;

    where[0] = disp & 0xff;
    where[1] = (disp >> 8) & 0xff;
    where[2] = (disp >> 16) & 0xff;
    where[3] = (disp >> 24) & 0xff;
}

static void canonicalize_value(struct component *component, int want)
{
    if (want == TAIL)
	emit_op(component, op_RETURN_SINGLE);
    else if (want == FUNC)
	emit_op(component, op_CHECK_TYPE_FUNCTION);
    else if (want != SINGLE) {
	emit_op(component, op_CANONICALIZE_VALUE);
	emit_wants(component, want);
    }
}

static int find_literal(struct component *component, struct literal *literal)
{
    int i = 0;
    struct constant *c;

    for (c = component->constants; c != NULL; c = c->next)
	if (c->kind == constant_LITERAL && c->u.literal == literal)
	    return i;
	else
	    i++;

    component->nconstants++;

    c = malloc(sizeof(struct constant));
    c->kind = constant_LITERAL;
    c->next = NULL;
    c->u.literal = literal;

    *component->constants_tail = c;
    component->constants_tail = &c->next;

    return i;
}

static int find_variable(struct component *component, struct id *id,
			 boolean written)
{
    int i = 0;
    struct constant *c;

    for (c = component->constants; c != NULL; c = c->next) {
	if (c->kind == constant_VARREF
	    && c->u.varref.id->symbol == id->symbol
	    && c->u.varref.id->internal == id->internal) {
	    if (written)
		c->u.varref.written = TRUE;
	    return i;
	}
	else
	    i++;
    }

    component->nconstants++;

    c = malloc(sizeof(struct constant));
    c->kind = constant_VARREF;
    c->next = NULL;
    c->u.varref.id = id;
    c->u.varref.written = written;

    *component->constants_tail = c;
    component->constants_tail = &c->next;

    return i;
}

static int find_method_desc(struct component *component, struct method *method)
{
    struct constant *c;

    c = malloc(sizeof(struct constant));
    c->kind = constant_METHODDESC;
    c->next = NULL;
    c->u.method = method;

    *component->constants_tail = c;
    component->constants_tail = &c->next;

    return component->nconstants++;
}



/* Method creation */

static void compile_method_ref(struct method *method,
			       struct component *component,
			       int want)
{
    struct method *home = method->parent;
    struct closes_over *over;

    method->component = compile_method(method);

    for (over = method->closes_over; over != NULL; over = over->next) {
	struct binding *binding = over->binding;

	if (over->over)
	    emit_op_and_arg(component, op_PUSH_ARG, over->over->offset);
	else if (binding->argument)
	    emit_op_and_arg(component, op_PUSH_ARG,
			    home->nargs - binding->offset - 1);
	else
	    emit_op_and_arg(component, op_PUSH_LOCAL, binding->offset);
    }
    
    emit_op_and_arg(component, op_PUSH_CONSTANT,
		      find_method_desc(component, method));
    compile_expr(method->specializers, component, SINGLE);

    if (method->rettypes) {
	compile_expr(method->rettypes->req_types_list, component, SINGLE);
	if (method->rettypes->rest_type)
	    compile_expr(method->rettypes->rest_type, component, SINGLE);
	else
	    emit_op(component, op_PUSH_FALSE);
    }
    else {
	emit_op(component, op_PUSH_NIL);
	emit_op(component, op_PUSH_TRUE);
    }

    emit_op(component, op_MAKE_METHOD);
    canonicalize_value(component, want);
}


/* Expression compilers */

static void compile_varref_expr(struct varref_expr *expr,
				struct component *component,
				int want)
{
    struct binding *binding = expr->binding;

    if (binding) {
	if (want == make_want(0, FALSE))
	    return;
	if (binding->home != expr->home)
	    /* It is a closure var. */
	    emit_op_and_arg(component, op_PUSH_ARG, expr->over->offset);
	else if (binding->argument)
	    emit_op_and_arg(component, op_PUSH_ARG,
			    binding->home->nargs - binding->offset - 1);
	else
	    emit_op_and_arg(component, op_PUSH_LOCAL, binding->offset);
	if (binding->closed_over && binding->set)
	    emit_op(component, op_VALUE_CELL_REF);
	if (!(want == FUNC && binding->function))
	    canonicalize_value(component, want);
    }
    else {
	/* It is a reference to a global variable. */
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, expr->var, FALSE));
	if (want == FUNC)
	    emit_op(component, op_VARIABLE_FUNCTION);
	else {
	    emit_op(component, op_VARIABLE_VALUE);
	    canonicalize_value(component, want);
	}
    }
}

static void compile_literal_expr(struct literal_expr *expr,
				 struct component *component,
				 int want)
{
    struct literal *lit = expr->lit;

    if (want == make_want(0, FALSE))
	return;

    switch (lit->kind) {
      case literal_TRUE:
	emit_op(component, op_PUSH_TRUE);
	break;
      case literal_FALSE:
	emit_op(component, op_PUSH_FALSE);
	break;
      case literal_UNBOUND:
	emit_op(component, op_PUSH_UNBOUND);
	break;
      case literal_INTEGER:
	{
	    struct integer_literal *l = (struct integer_literal *)lit;
	    if (-128 < l->value && l->value < 128) {
		emit_op(component, op_PUSH_BYTE);
		emit_byte(component, l->value & 0xff);
	    }
	    else {
		emit_op(component, op_PUSH_INT);
		emit_4bytes(component, (unsigned)l->value);
	    }
	    break;
	}
	
      case literal_LIST:
	if (((struct list_literal *)lit)->first == NULL) {
	    emit_op(component, op_PUSH_NIL);
	    break;
	}
	/* otherwise, drop though. */

      default:
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			  find_literal(component, lit));
	break;
    }

    canonicalize_value(component, want);
}

static void compile_call(struct call_expr *expr,
			 struct component *component,
			 int want)
{
    struct argument *arg;
    int nargs = 0;

    compile_expr(expr->func, component, FUNC);
    for (arg = expr->args; arg != NULL; arg = arg->next) {
	compile_expr(arg->expr, component, SINGLE);
	nargs++;
    }
    if (want == TAIL)
	emit_op_and_arg(component, op_CALL_TAIL, nargs);
    else if (want == FUNC) {
	emit_call_op_and_arg(component, op_CALL_FOR_SINGLE, nargs);
	emit_op(component, op_CHECK_TYPE_FUNCTION);
    }
    else if (want == SINGLE)
	emit_call_op_and_arg(component, op_CALL_FOR_SINGLE, nargs);
    else {
	emit_call_op_and_arg(component, op_CALL_FOR_MANY, nargs);
	emit_wants(component, want);
    }
}

static void compile_call_expr(struct call_expr *expr,
			      struct component *component,
			      int want)
{
    if (expr->info && expr->info->compile)
	(*expr->info->compile)(expr, component, want);
    else
	compile_call(expr, component, want);
}

static void compile_method_expr(struct method_expr *expr,
				struct component *component,
				int want)
{
    compile_method_ref(expr->method, component, want);
}

static void compile_dot_expr(struct dot_expr *expr,
			     struct component *component,
			     int want)
{
    lose("dot expr made it though expand?\n");
}

static void compile_body_expr(struct body_expr *expr,
			      struct component *component,
			      int want)
{
    compile_body(expr->body, component, want);
}

static void compile_block_expr(struct block_expr *expr,
			       struct component *component,
			       int want)
{
    lose("block expr made it though expand?\n");
}

static void compile_case_expr(struct case_expr *expr,
			      struct component *component,
			      int want)
{
    lose("case expr made it though expand?\n");
}

static void compile_if_expr(struct if_expr *expr,
			    struct component *component,
			    int want)
{
    unsigned char *cond_branch_loc;
    unsigned char *done_branch_loc;
    int concequent_pos;
    int alternate_pos;
    int done_pos;

    compile_expr(expr->cond, component, SINGLE);
    emit_op(component, op_CONDITIONAL_BRANCH);
    cond_branch_loc = reserve_space(component, 4);
    concequent_pos = current_position(component);
    compile_body(expr->consequent, component, want);
    if (want != TAIL) {
	emit_op(component, op_BRANCH);
	done_branch_loc = reserve_space(component, 4);
    }
    alternate_pos = current_position(component);
    write_branch_displacement(cond_branch_loc, concequent_pos, alternate_pos);

    compile_body(expr->alternate, component, want);
    if (want != TAIL) {
	done_pos = current_position(component);
	write_branch_displacement(done_branch_loc, alternate_pos, done_pos);
    }
}

static void compile_for_expr(struct for_expr *expr,
			     struct component *component,
			     int want)
{
    lose("for expr made it though expand?\n");
}

static void compile_select_expr(struct select_expr *expr,
				struct component *component,
				int want)
{
    lose("select expr made it though expand?\n");
}

static void compile_varset_expr(struct varset_expr *expr,
				struct component *component,
				int want)
{
    struct binding *binding = expr->binding;

    compile_expr(expr->value, component, SINGLE);

    if (binding) {
	if (!binding->set)
	    lose("Compiling a varset expr for a binding that isn't set?");
	if (binding->type) {
	    struct varref_expr *varref
		= (struct varref_expr *)make_varref(id(binding->type));
	    compile_varref_expr(varref, component, SINGLE);
	    emit_op(component, op_CHECK_TYPE);
	}
	if (want != make_want(0, FALSE))
	    emit_op(component, op_DUP);
	if (binding->home != expr->home) {
	    /* It is a closure var. */
	    emit_op_and_arg(component, op_PUSH_ARG, expr->over->offset);
	    emit_op(component, op_VALUE_CELL_SET);
	}
	else if (binding->closed_over) {
	    if (binding->argument)
		emit_op_and_arg(component, op_PUSH_ARG,
				binding->home->nargs - binding->offset - 1);
	    else
		emit_op_and_arg(component, op_PUSH_LOCAL, binding->offset);
	    emit_op(component, op_VALUE_CELL_SET);
	}
	else if (binding->argument)
	    emit_op_and_arg(component, op_POP_ARG,
			    binding->home->nargs - binding->offset - 1);
	else
	    emit_op_and_arg(component, op_POP_LOCAL, binding->offset);
    }
    else {
	/* It is a reference to a global variable. */
	if (want != make_want(0, FALSE))
	    emit_op(component, op_DUP);
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, expr->var, TRUE));
	emit_op(component, op_SET_VARIABLE_VALUE);
    }
    if (want != make_want(0, FALSE))
	canonicalize_value(component, want);
}

static void compile_binop_series_expr(struct binop_series_expr *expr,
				      struct component *component,
				      int want)
{
    lose("binop_series expr made it though expand?\n");
}

static void compile_loop_expr(struct loop_expr *expr,
			      struct component *component,
			      int want)
{
    expr->position = current_position(component);
    compile_body(expr->body, component, want);
}

static void compile_repeat_expr(struct repeat_expr *expr,
				struct component *component,
				int want)
{
    emit_op(component, op_BRANCH);
    write_branch_displacement(reserve_space(component, 4),
			      current_position(component),
			      expr->loop->position);
}

static void compile_error_expr(struct expr *expr, struct component *component,
			       int want)
{
    lose("Called compile on a parse tree with errors?");
}

static void (*ExpressionCompilers[])() = {
    compile_varref_expr, compile_literal_expr, compile_call_expr,
    compile_method_expr, compile_dot_expr, compile_body_expr,
    compile_block_expr, compile_case_expr, compile_if_expr, compile_for_expr,
    compile_select_expr, compile_varset_expr, compile_binop_series_expr,
    compile_loop_expr, compile_repeat_expr, compile_error_expr
};

static void compile_expr(struct expr *expr, struct component *component,
			 int want)
{
    if (expr->analized)
	(*ExpressionCompilers[(int)expr->kind])(expr, component, want);
    else
	lose("Compiling an expression that was never analized?");
}


/* Constituent compilers */

static void compile_defconst_constituent(struct defconst_constituent *c,
					 struct component *component,
					 int want)
{
    lose("define constant not at top-level?");
}

static void compile_defvar_constituent(struct defvar_constituent *c,
				       struct component *component,
				       int want)
{
    lose("define variable not at top-level?");
}

static void compile_defmethod_constituent(struct defmethod_constituent *c,
					  struct component *component,
					  int want)
{
    lose("define method not at top-level?");
}

static void compile_defgeneric_constituent(struct defgeneric_constituent *c,
					   struct component *component,
					   int want)
{
    lose("define generic not at top-level?");
}

static void compile_defclass_constituent(struct defclass_constituent *c,
					 struct component *component,
					 int want)
{
    lose("define class not at top-level?");
}

static void compile_expr_constituent(struct expr_constituent *c,
				     struct component *component,
				     int want)
{
    compile_expr(c->expr, component, want);
}

static void compile_local_constituent(struct local_constituent *c,
				      struct component *component,
				      int want)
{
    struct method *method;
    struct binding *binding;

    for (method = c->methods, binding = c->lexenv->bindings;
	 method != NULL;
	 method = method->next_local, binding = binding->next)
	if (binding->closed_over) {
	    emit_op(component, op_PUSH_FALSE);
	    emit_op(component, op_MAKE_VALUE_CELL);
	    emit_op_and_arg(component, op_POP_LOCAL, binding->offset);
	}

    for (method = c->methods, binding = c->lexenv->bindings;
	 method != NULL;
	 method = method->next_local, binding = binding->next) {
	compile_method_ref(method, component, SINGLE);
	if (binding->closed_over) {
	    emit_op_and_arg(component, op_PUSH_LOCAL, binding->offset);
	    emit_op(component, op_VALUE_CELL_SET);
	}
	else
	    emit_op_and_arg(component, op_POP_LOCAL, binding->offset);
    }

    compile_body(c->body, component, want);
}

static void compile_handler_constituent(struct handler_constituent *c,
					struct component *component,
					int want)
{
    if (want == TAIL) {
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, id(symbol("apply")), FALSE));
	emit_op(component, op_VARIABLE_FUNCTION);
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, id(symbol("values")), FALSE));
	emit_op(component, op_VARIABLE_FUNCTION);
	compile_handler_constituent(c, component, make_want(0, TRUE));
	emit_op_and_arg(component, op_CALL_TAIL, 2);
    }
    else {
	compile_body(c->body, component, want);
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, id(symbol("pop-handler")),
				      FALSE));
	emit_op(component, op_VARIABLE_FUNCTION);
	emit_call_op_and_arg(component, op_CALL_FOR_MANY, 0);
	emit_wants(component, make_want(0, FALSE));
    }
}

static void compile_let_constituent(struct let_constituent *c,
				    struct component *component,
				    int want)
{
    struct bindings *bindings = c->bindings;
    struct binding *binding = c->lexenv->bindings;

    compile_expr(bindings->expr, component,
		 make_want(c->required, bindings->params->rest_param));
    while (binding != c->inside) {
	if (binding->set && binding->closed_over)
	    emit_op(component, op_MAKE_VALUE_CELL);
	emit_op_and_arg(component, op_POP_LOCAL, binding->offset);
	binding = binding->next;
    }
    compile_body(c->body, component, want);
}

static void compile_tlf_constituent(struct tlf_constituent *c,
				    struct component *component,
				    int want)
{
    lose("top-level-form not at top-level?");
}

static void compile_error_constituent(struct constituent *c,
				      struct component *component,
				      int want)
{
    lose("called compile on a parse tree with errors?");
}

static void compile_defmodule_constituent(struct defnamespace_constituent *c,
					  struct component *component,
					  int want)
{
    lose("define module not at top-level?");
}

static void compile_deflibrary_constituent(struct defnamespace_constituent *c,
					   struct component *component,
					   int want)
{
    lose("define library not at top-level?");
}

static void (*ConstituentCompilers[])() = {
    compile_defconst_constituent, compile_defvar_constituent,
    compile_defmethod_constituent, compile_defgeneric_constituent,
    compile_defclass_constituent, compile_expr_constituent,
    compile_local_constituent, compile_handler_constituent,
    compile_let_constituent, compile_tlf_constituent,
    compile_error_constituent, compile_defmethod_constituent,
    compile_deflibrary_constituent
};

static void compile_constituent(struct constituent *c,
				struct component *component, int want)
{
    (*ConstituentCompilers[(int)c->kind])(c, component, want);
}


/* Body compiler */

static void compile_body(struct body *body, struct component *component,
			 int want)
{
    struct constituent *c, *next;

    for (c = body->head; (next = c->next) != NULL; c = next)
	compile_constituent(c, component, 0);
    compile_constituent(c, component, want);
}


/* Compile-method */

static struct component *compile_method(struct method *method)
{
    struct component *component = malloc(sizeof(struct component));
    struct binding *binding;

    component->debug_name = method->debug_name;
    component->blocks = NULL;
    component->cur_block = NULL;
    component->fill = NULL;
    component->end = NULL;
    component->bytes = 0;
    component->constants = NULL;
    component->constants_tail = &component->constants;
    component->nconstants = 0;

    emit_4bytes(component, method->frame_size);

    for (binding = method->lexenv->bindings;
	 binding != NULL && binding->home == method;
	 binding = binding->next) {
	if (binding->set && binding->closed_over) {
	    emit_op_and_arg(component, op_PUSH_ARG,
			    method->nargs - binding->offset - 1);
	    emit_op(component, op_MAKE_VALUE_CELL);
	    emit_op_and_arg(component, op_POP_ARG,
			    method->nargs - binding->offset - 1);
	}
    }

    compile_body(method->body, component, TAIL);

    component->cur_block->end = component->fill;
    component->bytes += component->fill - component->cur_block->bytes;
    component->fill = NULL;
    component->end = NULL;
    component->cur_block = NULL;

    return component;
}




/* Top Level Constituent compilers */

static void compile_tl_defconst_constituent(struct defconst_constituent *c)
{
    dump_defconst(c->bindings->params, compile_method(c->tlf));
}

static void compile_tl_defvar_constituent(struct defvar_constituent *c)
{
    dump_defvar(c->bindings->params, compile_method(c->tlf));
}

static void compile_tl_defmethod_constituent(struct defmethod_constituent *c)
{
    dump_defmethod(c->method->name, compile_method(c->tlf));
}

static void compile_tl_defgeneric_constituent(struct defgeneric_constituent *c)
{
    dump_defgeneric(c->name, compile_method(c->tlf));
}

static void compile_tl_defclass_constituent(struct defclass_constituent *c)
{
    dump_defclass(c->name, c->slots, compile_method(c->tlf));
}

static void compile_tl_expr_constituent(struct expr_constituent *c)
{
    struct expr *expr = c->expr;

    if (expr->kind == expr_BODY) {
	struct body_expr *body_expr = (struct body_expr *)expr;
	compile_tl_body(body_expr->body);
    }
    else
	lose("expression constituent at top-level?");
}

static void compile_tl_local_constituent(struct local_constituent *c)
{
    lose("local constituent at top-level?");
}

static void compile_tl_handler_constituent(struct handler_constituent *c)
{
    lose("handler constituent made it through expand?\n");
}

static void compile_tl_let_constituent(struct let_constituent *c)
{
    lose("let constituent at top-level?");
}

static void compile_tl_tlf_constituent(struct tlf_constituent *c)
{
    dump_top_level_form(compile_method(c->form));
}

static void compile_tl_error_constituent(struct constituent *c)
{
    lose("called compile on a parse tree with errors?");
}

static void compile_tl_defmodule_constituent(struct defnamespace_constituent*c)
{
    dump_defmodule(c);
}

static void
    compile_tl_deflibrary_constituent(struct defnamespace_constituent *c)
{
    dump_deflibrary(c);
}


static void (*TLConstituentCompilers[])() = {
    compile_tl_defconst_constituent, compile_tl_defvar_constituent,
    compile_tl_defmethod_constituent, compile_tl_defgeneric_constituent,
    compile_tl_defclass_constituent, compile_tl_expr_constituent,
    compile_tl_local_constituent, compile_tl_handler_constituent,
    compile_tl_let_constituent, compile_tl_tlf_constituent,
    compile_tl_error_constituent, compile_tl_defmodule_constituent,
    compile_tl_deflibrary_constituent
};

static void compile_tl_constituent(struct constituent *c)
{
    (*TLConstituentCompilers[(int)c->kind])(c);
}

static void compile_tl_body(struct body *body)
{
    struct constituent *c;

    for (c = body->head; c != NULL; c = c->next)
	compile_tl_constituent(c);
}


/* Compile */

void compile(struct body *program)
{
    compile_tl_body(program);
}


/* Compilers for various magic functions */

static void compile_check_type_call(struct call_expr *call,
				    struct component *component,
				    int want)
{
    struct argument *args = call->args;

    if (args && args->next && args->next->next == NULL) {
	compile_expr(args->expr, component, SINGLE);
	compile_expr(args->next->expr, component, SINGLE);
	emit_op(component, op_CHECK_TYPE);
	canonicalize_value(component, want);
    }
    else
	compile_call(call, component, want);
}

static void compile_values_call(struct call_expr *call,
				struct component *component,
				int want)
{
    struct argument *args = call->args;

    if (want == TAIL) {
	if (args != NULL && args->next == NULL) {
	    compile_expr(args->expr, component, SINGLE);
	    emit_op(component, op_RETURN_SINGLE);
	}
	else
	    compile_call(call, component, want);
    }
    else if (want == FUNC) {
	if (args) {
	    compile_expr(args->expr, component, FUNC);
	    while ((args = args->next) != NULL)
		compile_expr(args->expr, component, make_want(0, FALSE));
	}
	else {
	    /* This is guarenteed to be an error, but we'll wait til */
	    /* runtime to tell them about it. */
	    emit_op(component, op_PUSH_FALSE);
	    emit_op(component, op_CHECK_TYPE_FUNCTION);
	}
    }
    else if (want_restp(want))
	compile_call(call, component, want);
    else {
	int fixed = want_req(want);
	int i;

	for (i = 0; i < fixed && args != NULL; i++) {
	    compile_expr(args->expr, component, SINGLE);
	    args = args->next;
	}
	if (args == NULL)
	    for (; i < fixed; i++)
		emit_op(component, op_PUSH_FALSE);
	else {
	    while (args != NULL) {
		compile_expr(args->expr, component, make_want(0, FALSE));
		args = args->next;
	    }
	}
    }
}

static void compile_find_variable_call(struct call_expr *call,
				       struct component *component,
				       int want)
{
    struct argument *args = call->args;

    if (args && args->next == NULL) {
	struct varref_expr *expr = (struct varref_expr *)args->expr;

	if (expr->kind != expr_VARREF)
	    lose("find-variable called on something other than a variable?");
	if (expr->binding)
	    lose("find-variable called on a local variable?");
	emit_op_and_arg(component, op_PUSH_CONSTANT,
			find_variable(component, expr->var, FALSE));
	canonicalize_value(component, want);
    }
    else
	lose("find-variable called with the wrong number of arguments?");
}

static void set_compiler(char *name, void compiler(), boolean internal)
{
    struct id *identifier = id(symbol(name));
    struct function_info *info;

    identifier->internal = internal;
    info = lookup_function_info(identifier, TRUE);
    info->compile = compiler;

    free(identifier);
}

void init_compile(void)
{
    set_compiler("check-type", compile_check_type_call, TRUE);
    set_compiler("check-type", compile_check_type_call, FALSE);
    set_compiler("values", compile_values_call, TRUE);
    set_compiler("values", compile_values_call, FALSE);
    set_compiler("find-variable", compile_find_variable_call, TRUE);
}
