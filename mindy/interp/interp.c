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
* $Header: /scm/cvs/src/mindy/interp/interp.c,v 1.5 2003/03/15 22:12:01 gabor Exp $
*
* This file implements the actual byte interpreter.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "driver.h"
#include "func.h"
#include "bool.h"
#include "list.h"
#include "class.h"
#include "obj.h"
#include "module.h"
#include "value.h"
#include "num.h"
#include "vec.h"
#include "sym.h"
#include "error.h"
#include "type.h"
#include "brkpt.h"
#include "interp.h"
#include "../comp/byteops.h"

#define OPS_PER_TIME_SLICE 100

obj_t obj_ComponentClass = 0;

static struct variable *plus_var = NULL;
static struct variable *minus_var = NULL;
static struct variable *lt_var = NULL;
static struct variable *le_var = NULL;
static struct variable *eq_var = NULL;
static struct variable *ne_var = NULL;


/* Various utility routines. */

__inline__ static int decode_byte(struct thread *thread)
{
    return ((unsigned char *)(thread->component))[thread->pc++];
}

__inline__ static int decode_int4(struct thread *thread)
{
    int byte1 = decode_byte(thread);
    int byte2 = decode_byte(thread);
    int byte3 = decode_byte(thread);
    int byte4 = decode_byte(thread);

    return byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24);
}

__inline__ static int decode_arg(struct thread *thread)
{
    int arg = decode_byte(thread);

    if (arg == 0xff)
	return decode_int4(thread);
    else
	return arg;
}

static void canonicalize_values(struct thread *thread, obj_t *old_sp,
				obj_t *vals)
{
    int supplied = thread->sp - vals;
    int wants = decode_arg(thread);
    int fixed;
    boolean restp;
    int i;

    fixed = wants >> 1;
    restp = wants & 1;

    if (supplied <= fixed) {
	if (old_sp != vals)
	    for (i = 0; i < supplied; i++)
		*old_sp++ = *vals++;
	else {
	    i = supplied;
	    old_sp += supplied;
	}
	while (i < fixed) {
	    *old_sp++ = obj_False;
	    i++;
	}
	if (restp)
	    *old_sp++ = make_vector(0, NULL);
    }
    else {
	if (old_sp != vals)
	    for (i = 0; i < fixed; i++)
		*old_sp++ = *vals++;
	else
	    vals += fixed;
	if (restp)
	    *old_sp++ = make_vector(supplied - fixed, vals);
    }

    thread->sp = old_sp;
}



/* Various byte ops. */

static void op_flame(int byte, struct thread *thread)
{
    lose("Bogus byte-op: %d", byte);
}

static void op_breakpoint(int byte, struct thread *thread)
{
    handle_byte_breakpoint(thread);
}

static void op_return_single(int byte, struct thread *thread)
{
    do_return(thread, pop_linkage(thread), thread->sp - 1);
}

static void op_make_value_cell(int byte, struct thread *thread)
{
    thread->sp[-1] = make_value_cell(thread->sp[-1]);
}

static void op_value_cell_ref(int byte, struct thread *thread)
{
    thread->sp[-1] = value_cell_ref(thread->sp[-1]);
}

static void op_value_cell_set(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    value_cell_set(sp[-1], sp[-2]);
    thread->sp = sp - 2;
}

static void op_make_method(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t method_info = sp[-4];
    obj_t specializers = sp[-3];
    obj_t result_types = sp[-2];
    obj_t rest_results_type = sp[-1];
    int n_closure_vars
	= obj_ptr(struct method_info *, method_info)->n_closure_vars;
    obj_t *lexenv = sp - n_closure_vars - 4;
    obj_t method = make_byte_method(method_info, specializers, result_types,
				    rest_results_type, lexenv);

    lexenv[0] = method;
    thread->sp = lexenv+1;
}

static void op_check_type(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t value = sp[-2];
    obj_t type = sp[-1];

    if (!instancep(value, type))
	type_error(value, type);

    thread->sp = sp - 1;
}

static void op_check_type_function(int byte, struct thread *thread)
{
    if (!instancep(thread->sp[-1], obj_FunctionClass))
	type_error(thread->sp[-1], obj_FunctionClass);
}

static void op_canonicalize_value(int byte, struct thread *thread)
{
    obj_t *vals = thread->sp - 1;

    canonicalize_values(thread, vals, vals);
}

static void op_push_byte(int byte, struct thread *thread)
{
    signed char value = decode_byte(thread);
    *thread->sp++ = make_fixnum(value);
}

static void op_push_int(int byte, struct thread *thread)
{
    *thread->sp++ = make_fixnum(decode_int4(thread));
}

static void op_conditional_branch(int byte, struct thread *thread)
{
    obj_t test_obj = *--thread->sp;
    if (test_obj == obj_False) {
	int disp = decode_int4(thread);
	thread->pc += disp;
    } 
#if 0
    else if (test_obj == obj_Nil) {
	fprintf(stderr, "Conditional on EmptyList\n");
        mindy_pause(pause_HitBreakpoint);
	thread->pc += 4;
    }
#endif
    else
	thread->pc += 4;
}

static void op_branch(int byte, struct thread *thread)
{
    int disp = decode_int4(thread);
    thread->pc += disp;
}

static void op_push_nil(int byte, struct thread *thread)
{
    *thread->sp++ = obj_Nil;
}

static void op_push_unbound(int byte, struct thread *thread)
{
    *thread->sp++ = obj_Unbound;
}

static void op_push_true(int byte, struct thread *thread)
{
    *thread->sp++ = obj_True;
}

static void op_push_false(int byte, struct thread *thread)
{
    *thread->sp++ = obj_False;
}

static void op_dup(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t value = sp[-1];

    thread->sp = sp+1;
    sp[0] = value;
}

static void op_dot_tail(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t arg = sp[-2];
    obj_t func = sp[-1];
    obj_t *old_sp = pop_linkage(thread);

    old_sp[0] = func;
    old_sp[1] = arg;
    thread->sp = old_sp + 2;

    invoke(thread, 1);
}

static void op_dot(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t arg = sp[-2];
    obj_t func = sp[-1];

    sp[-2] = func;
    sp[-1] = arg;

    invoke(thread, 1);
}    

static void push_constant(struct thread *thread, int arg)
{
    *thread->sp++
	= COMPONENT(thread->component)->constant[arg];
}

static void op_push_constant_immed(int byte, struct thread *thread)
{
    push_constant(thread, byte & 0x0f);
}

static void op_push_constant(int byte, struct thread *thread)
{
    push_constant(thread, decode_arg(thread));
}

static void push_arg(struct thread *thread, int arg)
{
    *thread->sp++ = thread->fp[-6 - arg];
}

static void op_push_arg_immed(int byte, struct thread *thread)
{
    push_arg(thread, byte & 0x0f);
}

static void op_push_arg(int byte, struct thread *thread)
{
    push_arg(thread, decode_arg(thread));
}

static void pop_arg(struct thread *thread, int arg)
{
    thread->fp[-6 - arg] = *--thread->sp;
}

static void op_pop_arg_immed(int byte, struct thread *thread)
{
    pop_arg(thread, byte & 0x0f);
}

static void op_pop_arg(int byte, struct thread *thread)
{
    pop_arg(thread, decode_arg(thread));
}

static void push_local(struct thread *thread, int arg)
{
    *thread->sp++ = thread->fp[arg];
}

static void op_push_local_immed(int byte, struct thread *thread)
{
    push_local(thread, byte & 0x0f);
}

static void op_push_local(int byte, struct thread *thread)
{
    push_local(thread, decode_arg(thread));
}

static void pop_local(struct thread *thread, int arg)
{
    thread->fp[arg] = *--thread->sp;
}

static void op_pop_local_immed(int byte, struct thread *thread)
{
    pop_local(thread, byte & 0x0f);
}

static void op_pop_local(int byte, struct thread *thread)
{
    pop_local(thread, decode_arg(thread));
}

static void call_tail(struct thread *thread, int arg)
{
    obj_t *sp = thread->sp;
    obj_t *stuff = sp - arg - 1;
    obj_t *old_sp = pop_linkage(thread);

    while (stuff < sp)
	*old_sp++ = *stuff++;

    thread->sp = old_sp;

    invoke(thread, arg);
}

static void op_call_tail_immed(int byte, struct thread *thread)
{
    call_tail(thread, byte & 0x0f);
}

static void op_call_tail(int byte, struct thread *thread)
{
    call_tail(thread, decode_arg(thread));
}

static void op_call_immed(int byte, struct thread *thread)
{
    invoke(thread, byte & 0x0f);
}

static void op_call(int byte, struct thread *thread)
{
    int nargs = decode_arg(thread);
    thread->pc++;
    invoke(thread, nargs);
}

static void push_value(struct thread *thread, int arg)
{
    struct variable *var
	= (struct variable *)COMPONENT(thread->component)->constant[arg];
    obj_t value = var->value;

    if (value != obj_Unbound)
	*thread->sp++ = value;
    else
	error("Unbound variable: %s", var->name);
}

static void op_push_value_immed(int byte, struct thread *thread)
{
    push_value(thread, byte & 0xf);
}

static void op_push_value(int byte, struct thread *thread)
{
    push_value(thread, decode_arg(thread));
}

static void push_function(struct thread *thread, int arg)
{
    struct variable *var
	= (struct variable *)COMPONENT(thread->component)->constant[arg];
    obj_t value = var->value;

    switch (var->function) {
      case func_No:
	type_error(value, obj_FunctionClass);
      case func_Yes:
      case func_Always:
	break;
      case func_Maybe:
	if (instancep(value, obj_FunctionClass)) {
	    var->function = func_Yes;
	    break;
	}
	else if (value == obj_Unbound)
	    error("Unbound variable: %s", var->name);
	else {
	    var->function = func_No;
	    type_error(value, obj_FunctionClass);
	}
    }

    *thread->sp++ = value;
}

static void op_push_function_immed(int byte, struct thread *thread)
{
    push_function(thread, byte & 0xf);
}

static void op_push_function(int byte, struct thread *thread)
{
    push_function(thread, decode_arg(thread));
}

static void pop_value(struct thread *thread, int arg)
{
    struct variable *var
	= (struct variable *)COMPONENT(thread->component)->constant[arg];
    obj_t value = *--thread->sp;

    if (var->type != obj_False && !instancep(value, var->type))
	type_error(value, var->type);
    if (var->function != func_Always)
	var->function = func_Maybe;
    var->value = value;
}

static void op_pop_value_immed(int byte, struct thread *thread)
{
    pop_value(thread, byte & 0xf);
}

static void op_pop_value(int byte, struct thread *thread)
{
    pop_value(thread, decode_arg(thread));
}

static void op_plus(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = make_fixnum(fixnum_value(x) + fixnum_value(y));
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = plus_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_minus(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = make_fixnum(fixnum_value(x) - fixnum_value(y));
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = minus_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_lt(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = ((long)x < (long)y) ? obj_True : obj_False;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = lt_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_le(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = ((long)x <= (long)y) ? obj_True : obj_False;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = le_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_eq(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (x == y) {
	sp[-2] = obj_True;
	thread->sp = sp-1;
    }
    else if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = obj_False;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = eq_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_idp(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (x == y)
	sp[-2] = obj_True;
    else if (obj_is_fixnum(x) || obj_is_fixnum(y))
	sp[-2] = obj_False;
    else if (idp(x, y))
	sp[-2] = obj_True;
    else
	sp[-2] = obj_False;

    thread->sp = sp-1;
}

static void op_ne(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (x == y) {
	sp[-2] = obj_False;
	thread->sp = sp-1;
    }
    else if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = obj_True;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = ne_var->value;
	sp[-1] = x;
	sp[0] = y;
	invoke(thread, 2);
    }
}

static void op_ge(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = ((long)x >= (long)y) ? obj_True : obj_False;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = le_var->value;
	/* sp[-1] already holds y */
	sp[0] = x;
	invoke(thread, 2);
    }
}

static void op_gt(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    obj_t x = sp[-2];
    obj_t y = sp[-1];

    if (obj_is_fixnum(x) && obj_is_fixnum(y)) {
	sp[-2] = ((long)x > (long)y) ? obj_True : obj_False;
	thread->sp = sp-1;
    }
    else {
	thread->sp = sp+1;
	sp[-2] = lt_var->value;
	/* sp[-1] already holds y */
	sp[0] = x;
	invoke(thread, 2);
    }
}

__inline__ void interpret_byte(int byte, struct thread *thread)
{

static void (*const preters[0x100])(int byte, struct thread *thread)
 = {
 	op_flame,
 	op_breakpoint,
 	op_return_single,
 	op_make_value_cell,
 	op_value_cell_ref,
 	op_value_cell_set,
 	op_make_method,
 	op_check_type,
 	op_check_type_function,
 	op_canonicalize_value,
 	op_push_byte,
 	op_push_int,
 	op_conditional_branch,
 	op_branch,
 	op_push_nil,
 	op_push_unbound,
 	op_push_true,
 	op_push_false,
 	op_dup,
 	op_dot_tail,
 	op_dot,  // twice!
 	op_dot, // twice!
 	
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,


#define FIFTEEN_TIMES(op) \
  op,op,op,op,op,op,op,op,op,op,op,op,op,op,op

#define SIXTEEN_TIMES(op) \
  FIFTEEN_TIMES(op ## _immed),op

/* 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant_immed,
 	op_push_constant,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg_immed,
 	op_push_arg,*/
 	SIXTEEN_TIMES(op_push_constant),
 	SIXTEEN_TIMES(op_push_arg),
 	
 	SIXTEEN_TIMES(op_pop_arg),
 	SIXTEEN_TIMES(op_push_local),
 	SIXTEEN_TIMES(op_pop_local),
 	SIXTEEN_TIMES(op_call_tail),
 	SIXTEEN_TIMES(op_call), // twice!
 	SIXTEEN_TIMES(op_call), // twice!
 	SIXTEEN_TIMES(op_push_value),
 	SIXTEEN_TIMES(op_push_function),
 	SIXTEEN_TIMES(op_pop_value),

 	FIFTEEN_TIMES(op_flame), op_flame,
 	FIFTEEN_TIMES(op_flame), op_flame,

 	op_plus,
 	op_minus,
 	op_lt,
 	op_le,
 	op_eq,
 	op_idp,
 	op_ne,
 	op_ge,
 	op_gt,

 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame,
 	op_flame
 	
 };
 
preters[byte](byte, thread);
 
/*    switch (byte) {
      case op_BREAKPOINT:
	op_breakpoint(byte, thread);
	break;
      case op_RETURN_SINGLE:
	op_return_single(byte, thread);
	break;
      case op_MAKE_VALUE_CELL:
	op_make_value_cell(byte, thread);
	break;
      case op_VALUE_CELL_REF:
	op_value_cell_ref(byte, thread);
	break;
      case op_VALUE_CELL_SET:
	op_value_cell_set(byte, thread);
	break;
      case op_MAKE_METHOD:
	op_make_method(byte, thread);
	break;
      case op_CHECK_TYPE:
	op_check_type(byte, thread);
	break;
      case op_CHECK_TYPE_FUNCTION:
	op_check_type_function(byte, thread);
	break;
      case op_CANONICALIZE_VALUE:
	op_canonicalize_value(byte, thread);
	break;
      case op_PUSH_BYTE:
	op_push_byte(byte, thread);
	break;
      case op_PUSH_INT:
	op_push_int(byte, thread);
	break;
      case op_CONDITIONAL_BRANCH:
	op_conditional_branch(byte, thread);
	break;
      case op_BRANCH:
	op_branch(byte, thread);
	break;
      case op_PUSH_NIL:
	op_push_nil(byte, thread);
	break;
      case op_PUSH_UNBOUND:
	op_push_unbound(byte, thread);
	break;
      case op_PUSH_TRUE:
	op_push_true(byte, thread);
	break;
      case op_PUSH_FALSE:
	op_push_false(byte, thread);
	break;
      case op_DUP:
	op_dup(byte, thread);
	break;
      case op_DOT_TAIL:
	op_dot_tail(byte, thread);
	break;
      case op_DOT_FOR_SINGLE:
      case op_DOT_FOR_MANY:
	op_dot(byte, thread);
	break;
      case op_PUSH_CONSTANT|0:
      case op_PUSH_CONSTANT|1:
      case op_PUSH_CONSTANT|2:
      case op_PUSH_CONSTANT|3:
      case op_PUSH_CONSTANT|4:
      case op_PUSH_CONSTANT|5:
      case op_PUSH_CONSTANT|6:
      case op_PUSH_CONSTANT|7:
      case op_PUSH_CONSTANT|8:
      case op_PUSH_CONSTANT|9:
      case op_PUSH_CONSTANT|10:
      case op_PUSH_CONSTANT|11:
      case op_PUSH_CONSTANT|12:
      case op_PUSH_CONSTANT|13:
      case op_PUSH_CONSTANT|14:
	op_push_constant_immed(byte, thread);
	break;
      case op_PUSH_CONSTANT|15:
	op_push_constant(byte, thread);
	break;
      case op_PUSH_ARG|0:
      case op_PUSH_ARG|1:
      case op_PUSH_ARG|2:
      case op_PUSH_ARG|3:
      case op_PUSH_ARG|4:
      case op_PUSH_ARG|5:
      case op_PUSH_ARG|6:
      case op_PUSH_ARG|7:
      case op_PUSH_ARG|8:
      case op_PUSH_ARG|9:
      case op_PUSH_ARG|10:
      case op_PUSH_ARG|11:
      case op_PUSH_ARG|12:
      case op_PUSH_ARG|13:
      case op_PUSH_ARG|14:
	op_push_arg_immed(byte, thread);
	break;
      case op_PUSH_ARG|15:
	op_push_arg(byte, thread);
	break;
      case op_POP_ARG|0:
      case op_POP_ARG|1:
      case op_POP_ARG|2:
      case op_POP_ARG|3:
      case op_POP_ARG|4:
      case op_POP_ARG|5:
      case op_POP_ARG|6:
      case op_POP_ARG|7:
      case op_POP_ARG|8:
      case op_POP_ARG|9:
      case op_POP_ARG|10:
      case op_POP_ARG|11:
      case op_POP_ARG|12:
      case op_POP_ARG|13:
      case op_POP_ARG|14:
	op_pop_arg_immed(byte, thread);
	break;
      case op_POP_ARG|15:
	op_pop_arg(byte, thread);
	break;
      case op_PUSH_LOCAL|0:
      case op_PUSH_LOCAL|1:
      case op_PUSH_LOCAL|2:
      case op_PUSH_LOCAL|3:
      case op_PUSH_LOCAL|4:
      case op_PUSH_LOCAL|5:
      case op_PUSH_LOCAL|6:
      case op_PUSH_LOCAL|7:
      case op_PUSH_LOCAL|8:
      case op_PUSH_LOCAL|9:
      case op_PUSH_LOCAL|10:
      case op_PUSH_LOCAL|11:
      case op_PUSH_LOCAL|12:
      case op_PUSH_LOCAL|13:
      case op_PUSH_LOCAL|14:
	op_push_local_immed(byte, thread);
	break;
      case op_PUSH_LOCAL|15:
	op_push_local(byte, thread);
	break;
      case op_POP_LOCAL|0:
      case op_POP_LOCAL|1:
      case op_POP_LOCAL|2:
      case op_POP_LOCAL|3:
      case op_POP_LOCAL|4:
      case op_POP_LOCAL|5:
      case op_POP_LOCAL|6:
      case op_POP_LOCAL|7:
      case op_POP_LOCAL|8:
      case op_POP_LOCAL|9:
      case op_POP_LOCAL|10:
      case op_POP_LOCAL|11:
      case op_POP_LOCAL|12:
      case op_POP_LOCAL|13:
      case op_POP_LOCAL|14:
	op_pop_local_immed(byte, thread);
	break;
      case op_POP_LOCAL|15:
	op_pop_local(byte, thread);
	break;
      case op_CALL_TAIL|0:
      case op_CALL_TAIL|1:
      case op_CALL_TAIL|2:
      case op_CALL_TAIL|3:
      case op_CALL_TAIL|4:
      case op_CALL_TAIL|5:
      case op_CALL_TAIL|6:
      case op_CALL_TAIL|7:
      case op_CALL_TAIL|8:
      case op_CALL_TAIL|9:
      case op_CALL_TAIL|10:
      case op_CALL_TAIL|11:
      case op_CALL_TAIL|12:
      case op_CALL_TAIL|13:
      case op_CALL_TAIL|14:
	op_call_tail_immed(byte, thread);
	break;
      case op_CALL_TAIL|15:
	op_call_tail(byte, thread);
	break;
      case op_CALL_FOR_MANY|0:
      case op_CALL_FOR_MANY|1:
      case op_CALL_FOR_MANY|2:
      case op_CALL_FOR_MANY|3:
      case op_CALL_FOR_MANY|4:
      case op_CALL_FOR_MANY|5:
      case op_CALL_FOR_MANY|6:
      case op_CALL_FOR_MANY|7:
      case op_CALL_FOR_MANY|8:
      case op_CALL_FOR_MANY|9:
      case op_CALL_FOR_MANY|10:
      case op_CALL_FOR_MANY|11:
      case op_CALL_FOR_MANY|12:
      case op_CALL_FOR_MANY|13:
      case op_CALL_FOR_MANY|14:
      case op_CALL_FOR_SINGLE|0:
      case op_CALL_FOR_SINGLE|1:
      case op_CALL_FOR_SINGLE|2:
      case op_CALL_FOR_SINGLE|3:
      case op_CALL_FOR_SINGLE|4:
      case op_CALL_FOR_SINGLE|5:
      case op_CALL_FOR_SINGLE|6:
      case op_CALL_FOR_SINGLE|7:
      case op_CALL_FOR_SINGLE|8:
      case op_CALL_FOR_SINGLE|9:
      case op_CALL_FOR_SINGLE|10:
      case op_CALL_FOR_SINGLE|11:
      case op_CALL_FOR_SINGLE|12:
      case op_CALL_FOR_SINGLE|13:
      case op_CALL_FOR_SINGLE|14:
	op_call_immed(byte, thread);
	break;
      case op_CALL_FOR_MANY|15:
      case op_CALL_FOR_SINGLE|15:
	op_call(byte, thread);
	break;
      case op_PUSH_VALUE|0:
      case op_PUSH_VALUE|1:
      case op_PUSH_VALUE|2:
      case op_PUSH_VALUE|3:
      case op_PUSH_VALUE|4:
      case op_PUSH_VALUE|5:
      case op_PUSH_VALUE|6:
      case op_PUSH_VALUE|7:
      case op_PUSH_VALUE|8:
      case op_PUSH_VALUE|9:
      case op_PUSH_VALUE|10:
      case op_PUSH_VALUE|11:
      case op_PUSH_VALUE|12:
      case op_PUSH_VALUE|13:
      case op_PUSH_VALUE|14:
	op_push_value_immed(byte, thread);
	break;
      case op_PUSH_VALUE|15:
	op_push_value(byte, thread);
	break;
      case op_PUSH_FUNCTION|0:
      case op_PUSH_FUNCTION|1:
      case op_PUSH_FUNCTION|2:
      case op_PUSH_FUNCTION|3:
      case op_PUSH_FUNCTION|4:
      case op_PUSH_FUNCTION|5:
      case op_PUSH_FUNCTION|6:
      case op_PUSH_FUNCTION|7:
      case op_PUSH_FUNCTION|8:
      case op_PUSH_FUNCTION|9:
      case op_PUSH_FUNCTION|10:
      case op_PUSH_FUNCTION|11:
      case op_PUSH_FUNCTION|12:
      case op_PUSH_FUNCTION|13:
      case op_PUSH_FUNCTION|14:
	op_push_function_immed(byte, thread);
	break;
      case op_PUSH_FUNCTION|15:
	op_push_function(byte, thread);
	break;
      case op_POP_VALUE|0:
      case op_POP_VALUE|1:
      case op_POP_VALUE|2:
      case op_POP_VALUE|3:
      case op_POP_VALUE|4:
      case op_POP_VALUE|5:
      case op_POP_VALUE|6:
      case op_POP_VALUE|7:
      case op_POP_VALUE|8:
      case op_POP_VALUE|9:
      case op_POP_VALUE|10:
      case op_POP_VALUE|11:
      case op_POP_VALUE|12:
      case op_POP_VALUE|13:
      case op_POP_VALUE|14:
	op_pop_value_immed(byte, thread);
	break;
      case op_POP_VALUE|15:
	op_pop_value(byte, thread);
	break;
      case op_PLUS:
	op_plus(byte, thread);
	break;
      case op_MINUS:
	op_minus(byte, thread);
	break;
      case op_LT:
	op_lt(byte, thread);
	break;
      case op_LE:
	op_le(byte, thread);
	break;
      case op_EQ:
	op_eq(byte, thread);
	break;
      case op_IDP:
	op_idp(byte, thread);
	break;
      case op_NE:
	op_ne(byte, thread);
	break;
      case op_GE:
	op_ge(byte, thread);
	break;
      case op_GT:
	op_gt(byte, thread);
	break;
      default:
	op_flame(byte, thread);
    }*/
}

void interpret_next_byte(struct thread *thread)
{
  int timer = OPS_PER_TIME_SLICE ;

  while(timer-- > 0) 
    interpret_byte(decode_byte(thread), thread);
}



/* Entry points into the interpteter. */

void set_byte_continuation(struct thread *thread, obj_t component)
{
    int n_const = COMPONENT(component)->n_constants;
    thread->component = component;
    thread->pc = (char *)(&COMPONENT(component)->constant[n_const])
	- (char *)component;
    thread->sp = thread->fp + COMPONENT(component)->frame_size;
#if SLOW_FUNCTION_POINTERS
    thread->advance = NULL;
#else
    thread->advance = interpret_next_byte;
#endif    
}

void do_byte_return(struct thread *thread, obj_t *old_sp, obj_t *vals)
{
    int opcode = ((unsigned char *)(thread->component))[thread->pc - 1];

    if (opcode == op_BREAKPOINT)
	opcode = original_byte(thread->component, thread->pc - 1);

    if ((opcode&0xf0) == op_CALL_FOR_SINGLE || opcode == op_DOT_FOR_SINGLE
	  || opcode >= op_PLUS) {
	if (vals == thread->sp)
	    *old_sp = obj_False;
	else if (vals != old_sp)
	    *old_sp = vals[0];
	thread->sp = old_sp + 1;
    }
    else if ((opcode&0xf0) == op_CALL_FOR_MANY || opcode == op_DOT_FOR_MANY)
	canonicalize_values(thread, old_sp, vals);
    else
	lose("Strange call opcode: 0x%02x", opcode);

#if SLOW_FUNCTION_POINTERS
    thread->advance = NULL;
#else
    thread->advance = interpret_next_byte;
#endif
}


/* Component allocation. */

obj_t make_component(obj_t debug_name, int frame_size, obj_t mtime,
		     obj_t source_file, obj_t debug_info, int nconst,
		     int nbytes)
{
    int len = sizeof(struct component) + sizeof(obj_t)*(nconst - 1) + nbytes;
    obj_t res = alloc(obj_ComponentClass, len);
    int i;

    COMPONENT(res)->length = len;
    COMPONENT(res)->debug_name = debug_name;
    COMPONENT(res)->frame_size = frame_size;
    COMPONENT(res)->mtime = mtime;
    COMPONENT(res)->source_file = source_file;
    COMPONENT(res)->debug_info = debug_info;
    COMPONENT(res)->n_constants = nconst;

    for (i = 0; i < nconst; i++)
	COMPONENT(res)->constant[i] = obj_Unbound;

    return res;
}


/* GC routines. */

static int scav_component(struct object *ptr)
{
    struct component *component = (struct component *)ptr;
    int i;

    scavenge(&component->debug_name);
    scavenge(&component->mtime);
    scavenge(&component->source_file);
    scavenge(&component->debug_info);
    for (i = 0; i < component->n_constants; i++)
	scavenge(component->constant + i);

    return component->length;
}

static obj_t trans_component(obj_t component)
{
    return transport(component, COMPONENT(component)->length, TRUE);
}


/* Init stuff. */

void make_interp_classes(void)
{
    obj_ComponentClass = make_builtin_class(scav_component, trans_component);
    add_constant_root(&obj_ComponentClass);
}

void init_interp_classes(void)
{
    init_builtin_class(obj_ComponentClass, "<component>",
		       obj_ObjectClass, NULL);
}

void init_interpreter(void)
{
    plus_var = find_variable(module_BuiltinStuff, symbol("+"), FALSE, TRUE);
    minus_var = find_variable(module_BuiltinStuff, symbol("-"), FALSE, TRUE);
    lt_var = find_variable(module_BuiltinStuff, symbol("<"), FALSE, TRUE);
    le_var = find_variable(module_BuiltinStuff, symbol("<="), FALSE, TRUE);
    eq_var = find_variable(module_BuiltinStuff, symbol("="), FALSE, TRUE);
    ne_var = find_variable(module_BuiltinStuff, symbol("~="), FALSE, TRUE);
}
