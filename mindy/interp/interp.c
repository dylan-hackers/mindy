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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/interp.c,v 1.2 1994/03/25 02:34:03 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <setjmp.h>

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "func.h"
#include "bool.h"
#include "list.h"
#include "class.h"
#include "obj.h"
#include "module.h"
#include "value.h"
#include "num.h"
#include "vec.h"
#include "interp.h"
#include "../comp/byteops.h"

static obj_t obj_ComponentClass = 0;
static boolean InInterpreter = FALSE;
static jmp_buf Catcher;
static enum pause_reason PauseReason;

static void (*byte_ops[256])(int byte, struct thread *thread);


/* Various utility routines. */

inline static int decode_byte(struct thread *thread)
{
    return ((unsigned char *)(thread->component))[thread->pc++];
}

inline static int decode_int4(struct thread *thread)
{
    int byte1 = decode_byte(thread);
    int byte2 = decode_byte(thread);
    int byte3 = decode_byte(thread);
    int byte4 = decode_byte(thread);

    return byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24);
}

inline static int decode_arg(struct thread *thread)
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
    error("Bogus byte-op: ~S", byte);
}

static void op_breakpoint(int byte, struct thread *thread)
{
    pause(pause_HitBreakpoint);
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

static void op_variable_value(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    struct variable *var = obj_rawptr(sp[-1]);
    obj_t value = var->value;
    if (value != obj_Unbound)
	sp[-1] = value;
    else
	error("Unbound variable: ~S", var->name);
}

static void op_variable_function(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    struct variable *var = obj_rawptr(sp[-1]);
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
	    error("Unbound variable: ~S", var->name);
	else {
	    var->function = func_No;
	    type_error(value, obj_FunctionClass);
	}
    }
    sp[-1] = value;
}

static void op_set_variable_value(int byte, struct thread *thread)
{
    obj_t *sp = thread->sp;
    struct variable *var = obj_rawptr(sp[-1]);
    obj_t value = sp[-2];

    if (var->type != obj_False && !instancep(value, var->type))
	type_error(value, var->type);
    if (var->function != func_Always)
	var->function = func_Maybe;
    var->value = value;
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
    if (*--thread->sp == obj_False) {
	int disp = decode_int4(thread);
	thread->pc += disp;
    }
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

static void push_constant(struct thread *thread, int arg)
{
    *thread->sp++
	= obj_ptr(struct component *, thread->component)->constant[arg];
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
    *thread->sp++ = thread->fp[-5 - arg];
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
    thread->fp[-5 - arg] = *--thread->sp;
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

static void interpret_byte(struct thread *thread)
{
    int byte = decode_byte(thread);

    (*byte_ops[byte])(byte, thread);
}


/* Entry points into the interpteter. */

enum pause_reason do_stuff(void)
{
    assert (!InInterpreter);

    InInterpreter = TRUE;
    PauseReason = pause_NoReason;
    setjmp(Catcher);
    while (PauseReason == pause_NoReason) {
	struct thread *thread = thread_pick_next();
	if (TimeToGC)
	    collect_garbage();
	thread->advance(thread);
    }
    InInterpreter = FALSE;
    return PauseReason;
}

enum pause_reason single_step(struct thread *thread)
{
    assert(!InInterpreter);
    assert(thread->status == status_Running);
    assert(thread->suspend_count == 0);

    thread_set_current(thread);
    InInterpreter = TRUE;
    PauseReason = pause_NoReason;
    if (setjmp(Catcher) == 0)
	thread->advance(thread);
    InInterpreter = FALSE;
    if (TimeToGC)
	collect_garbage();
    return PauseReason;
}

void go_on(void)
{
    assert(InInterpreter);
    longjmp(Catcher, 1);
}

void pause(enum pause_reason reason)
{
    PauseReason = reason;
    go_on();
}

void set_byte_continuation(struct thread *thread, obj_t component)
{
    thread->component = component;
    thread->pc = (char *)(&obj_ptr(struct component *, component)
			  ->constant[obj_ptr(struct component *, component)
				     ->n_constants])
	- (char *)component;
    thread->sp = thread->fp + decode_int4(thread);
    thread->advance = interpret_byte;
}

void do_byte_return(struct thread *thread, obj_t *old_sp, obj_t *vals)
{
    int opcode = ((unsigned char *)(thread->component))[thread->pc - 1] & 0xf0;

    if (opcode == op_CALL_FOR_SINGLE) {
	if (vals == thread->sp)
	    *old_sp = obj_False;
	else if (vals != old_sp)
	    *old_sp = vals[0];
	thread->sp = old_sp + 1;
    }
    else if (opcode == op_CALL_FOR_MANY)
	canonicalize_values(thread, old_sp, vals);
    else
	lose("Strange call opcode: 0x~02x", opcode);

    thread->advance = interpret_byte;

    go_on();
}


/* Component allocation. */

obj_t make_component(obj_t debug_name, int nconst, int nbytes)
{
    int len = sizeof(struct component) + sizeof(obj_t)*nconst + nbytes;
    obj_t res = alloc(obj_ComponentClass, len);
    int i;

    obj_ptr(struct component *, res)->debug_name = debug_name;
    obj_ptr(struct component *, res)->n_constants = nconst;
    obj_ptr(struct component *, res)->length = len;

    for (i = 0; i < nconst; i++)
	obj_ptr(struct component *, res)->constant[i] = obj_Unbound;

    return res;
}


/* GC routines. */

static int scav_component(struct object *ptr)
{
    struct component *component = (struct component *)ptr;
    int i;

    scavenge(&component->debug_name);
    for (i = 0; i < component->n_constants; i++)
	scavenge(component->constant + i);

    return component->length;
}

static obj_t trans_component(obj_t component)
{
    return transport(component,
		     obj_ptr(struct component *, component)->length);
}

void scavenge_interp_roots(void)
{
    scavenge(&obj_ComponentClass);
}


/* Init stuff. */

void make_interp_classes(void)
{
    obj_ComponentClass = make_builtin_class(scav_component, trans_component);
}

void init_interp_classes(void)
{
    init_builtin_class(obj_ComponentClass, "<component>",
		       obj_ObjectClass, NULL);
}

void init_interpreter(void)
{
    int i;

    for (i = 0; i < 256; i++)
	byte_ops[i] = op_flame;

    byte_ops[op_BREAKPOINT] = op_breakpoint;
    byte_ops[op_RETURN_SINGLE] = op_return_single;
    byte_ops[op_MAKE_VALUE_CELL] = op_make_value_cell;
    byte_ops[op_VALUE_CELL_REF] = op_value_cell_ref;
    byte_ops[op_VALUE_CELL_SET] = op_value_cell_set;
    byte_ops[op_VARIABLE_VALUE] = op_variable_value;
    byte_ops[op_VARIABLE_FUNCTION] = op_variable_function;
    byte_ops[op_SET_VARIABLE_VALUE] = op_set_variable_value;
    byte_ops[op_MAKE_METHOD] = op_make_method;
    byte_ops[op_CHECK_TYPE] = op_check_type;
    byte_ops[op_CHECK_TYPE_FUNCTION] = op_check_type_function;
    byte_ops[op_CANONICALIZE_VALUE] = op_canonicalize_value;
    byte_ops[op_PUSH_BYTE] = op_push_byte;
    byte_ops[op_PUSH_INT] = op_push_int;
    byte_ops[op_CONDITIONAL_BRANCH] = op_conditional_branch;
    byte_ops[op_BRANCH] = op_branch;
    byte_ops[op_PUSH_NIL] = op_push_nil;
    byte_ops[op_PUSH_UNBOUND] = op_push_unbound;
    byte_ops[op_PUSH_TRUE] = op_push_true;
    byte_ops[op_PUSH_FALSE] = op_push_false;
    byte_ops[op_DUP] = op_dup;

    for (i = 0; i < 15; i++) {
	byte_ops[op_PUSH_CONSTANT|i] = op_push_constant_immed;
	byte_ops[op_PUSH_ARG|i] = op_push_arg_immed;
	byte_ops[op_POP_ARG|i] = op_pop_arg_immed;
	byte_ops[op_PUSH_LOCAL|i] = op_push_local_immed;
	byte_ops[op_POP_LOCAL|i] = op_pop_local_immed;
	byte_ops[op_CALL_TAIL|i] = op_call_tail_immed;
	byte_ops[op_CALL_FOR_MANY|i] = op_call_immed;
	byte_ops[op_CALL_FOR_SINGLE|i] = op_call_immed;
    }

    byte_ops[op_PUSH_CONSTANT|15] = op_push_constant;
    byte_ops[op_PUSH_ARG|15] = op_push_arg;
    byte_ops[op_POP_ARG|15] = op_pop_arg;
    byte_ops[op_PUSH_LOCAL|15] = op_push_local;
    byte_ops[op_POP_LOCAL|15] = op_pop_local;
    byte_ops[op_CALL_TAIL|15] = op_call_tail;
    byte_ops[op_CALL_FOR_MANY|15] = op_call;
    byte_ops[op_CALL_FOR_SINGLE|15] = op_call;
}
