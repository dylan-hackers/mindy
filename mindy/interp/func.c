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
* $Header: /scm/cvs/src/mindy/interp/func.c,v 1.1 1998/05/03 19:55:13 andreas Exp $
*
* This file implements functions.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "bool.h"
#include "list.h"
#include "num.h"
#include "class.h"
#include "obj.h"
#include "sym.h"
#include "interp.h"
#include "vec.h"
#include "type.h"
#include "module.h"
#include "print.h"
#include "driver.h"
#include "error.h"
#include "def.h"
#include "extern.h"
#include "coll.h"
#include "func.h"

obj_t obj_FunctionClass = NULL;
static obj_t obj_RawFunctionClass = NULL;
obj_t obj_MethodClass = NULL;
obj_t obj_ByteMethodClass = NULL;
static obj_t obj_RawMethodClass;
static obj_t obj_BuiltinMethodClass = NULL;
static obj_t obj_AccessorMethodClass = NULL;
obj_t obj_CFunctionClass = NULL;
obj_t obj_GFClass = NULL;
obj_t obj_MethodInfoClass = NULL;
static obj_t obj_GFCacheClass = NULL;


/* Tracing support. */

boolean Tracing = FALSE;

static void trace_call(obj_t function, obj_t *args, int nargs)
{
    printf("> 0x%08lx: ", (unsigned long)(args-1));
    prin1(function_debug_name_or_self(function));
    printf("(");
    if (nargs > 0) {
	prin1(*args++);
	while (--nargs > 0) {
	    printf(", ");
	    prin1(*args++);
	}
    }
    printf(")\n");
}

static void trace_return(obj_t *old_sp, obj_t *vals, int nvals)
{
    printf("< 0x%08lx: ", (unsigned long)old_sp);
    if (nvals > 0) {
	prin1(*vals++);
	while (--nvals > 0) {
	    printf(", ");
	    prin1(*vals++);
	}
    }
    printf("\n");
}


/* Functions in general. */

struct gf_cache {
    obj_t class;
    boolean simple;
    obj_t cached_result;
    int size;
    obj_t cached_classes[1];
};

obj_t make_gf_cache(int req_args, obj_t cached_result)
{
    obj_t res = alloc(obj_GFCacheClass, 
		      (sizeof(struct gf_cache) 
		       + sizeof(obj_t)*(req_args - 1)));
    struct gf_cache *gfc = obj_ptr(struct gf_cache *, res);
    int i;

    gfc->simple = TRUE;
    gfc->cached_result = cached_result;
    gfc->size = req_args;
    for (i = 0; i < req_args; i++)
	gfc->cached_classes[i] = obj_Nil;

    return res;
}

struct function {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
};

#define FUNC(o) obj_ptr(struct function *, o)

obj_t make_raw_function(char *debug_name, obj_t specializers,
			boolean restp, obj_t keywords, boolean all_keys,
			obj_t result_types, obj_t more_results_type,
			void (*xep)(struct thread *thread, int nargs))
{
    obj_t res = alloc(obj_RawFunctionClass, sizeof(struct function));

    FUNC(res)->xep = xep;
    FUNC(res)->debug_name = symbol(debug_name);
    FUNC(res)->required_args = length(specializers);
    FUNC(res)->restp = restp;
    FUNC(res)->keywords = keywords;
    FUNC(res)->all_keys = all_keys;
    FUNC(res)->result_types = result_types;
    FUNC(res)->more_results_type = more_results_type;
    FUNC(res)->specializers = specializers;

    return res;
}

obj_t function_debug_name(obj_t function)
{
    return FUNC(function)->debug_name;
}

obj_t function_debug_name_or_self(obj_t function)
{
    if (instancep(function, obj_FunctionClass)) {
	obj_t debug_name = FUNC(function)->debug_name;

	if (debug_name == obj_False)
	    return function;
	else
	    return debug_name;
    }
    else
	return function;
}

obj_t function_keywords(obj_t func)
{
    return FUNC(func)->keywords;
}

boolean function_all_keywords_p(obj_t func)
{
    return FUNC(func)->all_keys;
}

obj_t function_specializers(obj_t fn)
{
    return FUNC(fn)->specializers;
}

void invoke(struct thread *thread, int nargs)
{
    obj_t function = thread->sp[-nargs-1];
    int required = FUNC(function)->required_args;
    obj_t func_type = object_class(function);

    if (func_type != obj_BuiltinMethodClass
	&& func_type != obj_ByteMethodClass
	&& func_type != obj_BuiltinMethodClass
	&& func_type != obj_GFClass
	&& !subtypep(func_type, obj_FunctionClass))
	lose("invoke called on a non-function.");

    if (Tracing)
	trace_call(function, thread->sp - nargs, nargs);

    if (nargs < required) {
	push_linkage(thread, thread->sp - nargs);
	error("Too few arguments for %=: expected %d, got %d",
	      function_debug_name_or_self(function),
	      make_fixnum(required),
	      make_fixnum(nargs));
    }
    
    if (!FUNC(function)->restp && FUNC(function)->keywords == obj_False
	  && nargs > required) {
	push_linkage(thread, thread->sp - nargs);
	error("Too many arguments for %=: expected %d, got %d",
	      function_debug_name_or_self(function),
	      make_fixnum(required),
	      make_fixnum(nargs));
    }

    FUNC(function)->xep(thread, nargs);
#if !SLOW_LONGJMP
    go_on();
#endif
}

obj_t *push_linkage(struct thread *thread, obj_t *args)
{
    obj_t *fp = thread->sp += 5;

    fp[-5] = rawptr_obj(thread->fp);
    fp[-4] = rawptr_obj(args-1);
	fp[-3] = fp[-2] = rawptr_obj(NULL);
    if (obj_is_ptr(thread->component))
		fp[-2] = thread->component;
	else if (thread->component == C_CONTINUATION_MARKER) {
		fp[-3] = save_c_function_hi(thread->c_continuation);
		fp[-2] = save_c_function_lo(thread->c_continuation);
	}
    fp[-1] = make_fixnum(thread->pc);
    thread->fp = fp;
    thread->component = rawptr_obj(NULL);
    thread->pc = 0;

    return fp;
}

obj_t *pop_linkage(struct thread *thread)
{
    obj_t *fp = thread->fp;

    thread->fp = obj_rawptr(fp[-5]);
    if (obj_is_ptr(fp[-2]))
    	thread->component = fp[-2];
	else if (obj_is_saved_c_function(fp[-2])) {
		thread->component = C_CONTINUATION_MARKER;
		thread->c_continuation = restore_c_function(fp[-2], fp[-3]);
	}
	else
		thread->component = rawptr_obj(NULL);
    thread->pc = fixnum_value(fp[-1]);

    return obj_rawptr(fp[-4]);
}

void set_c_continuation(struct thread *thread,
			void (*cont)(struct thread *thread, obj_t *vals))
{
    thread->component = C_CONTINUATION_MARKER;
    thread->c_continuation = cont;
    thread->pc = 0;
}

#if SLOW_LONGJMP
void do_return(struct thread *thread, obj_t *old_sp, obj_t *vals)
#else
void do_return_setup(struct thread *thread, obj_t *old_sp, obj_t *vals)
#endif
{
    if (Tracing)
	trace_return(old_sp, vals, thread->sp - vals);

    if (thread->pc)
	do_byte_return(thread, old_sp, vals);
    else if (thread->component == C_CONTINUATION_MARKER) {
	    thread->component = rawptr_obj(NULL);
	    if (old_sp != vals) {
		obj_t *src = vals, *dst = old_sp, *end = thread->sp;
		while (src < end)
		    *dst++ = *src++;
		thread->sp = dst;
	    }
	    (*thread->c_continuation)(thread, old_sp);
	}
	else
	    lose("Attempt to return, but no continuation established.\n");
}

#if !SLOW_LONGJMP
void do_return(struct thread *thread, obj_t *old_sp, obj_t *vals)
{
    do_return_setup(thread, old_sp, vals);
    go_on();
}
#endif


/* Methods */

struct method {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
};

#define METHOD(o) obj_ptr(struct method *, o)

static obj_t *push_keywords(obj_t *sp, obj_t keywords, obj_t *args, int nargs)
{
    while (keywords != obj_Nil) {
	obj_t key_info = HEAD(keywords);
	obj_t key = HEAD(key_info);
	int i;

	for (i = 0; i < nargs; i += 2) {
	    if (key == args[i]) {
		*sp++ = args[i+1];
		goto next;
	    }
	}
	*sp++ = TAIL(key_info);

      next:
	keywords = TAIL(keywords);
    }
    return sp;
}

static void really_invoke_methods(obj_t method, obj_t next_methods,
				  struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    boolean restp = METHOD(method)->restp;
    obj_t keywords = METHOD(method)->keywords;
    int req_args = METHOD(method)->required_args;
    int rest_count = nargs - req_args;

    /* Change the function on the stack to be the next method so that */
    /* backtraces look better. */
    args[-1] = method;

    if (restp || keywords != obj_False) {
	obj_t *ptr = thread->sp - rest_count;
	obj_t rest = make_vector(rest_count, ptr);

	if (restp)
	    *ptr++ = rest;

	if (keywords != obj_False) {
	    if ((rest_count & 1) != 0) {
		push_linkage(thread, args);
		error("Odd number of keyword/value arguments.");
	    }

	    ptr = push_keywords(ptr, keywords, SOVEC(rest)->contents,
				rest_count);
	}

	thread->sp = ptr;
    }

    /* add next-method info. */
    *thread->sp++ = next_methods;

    METHOD(method)->iep(method, thread, args);
}

void invoke_methods(obj_t method, obj_t next_methods,
		    struct thread *thread, int nargs)
{
    if (method == obj_False) {
	push_linkage(thread, thread->sp - nargs);
	error("It is ambiguous which of these methods to invoke:\n  %=",
	      next_methods);
    }
    else
	really_invoke_methods(method, next_methods, thread, nargs);
}

/* Version of applicable_method_p which does extra work to allow SAM caching 
   for generic function dispatch.  The "cache" argument is carried across
   several calls to gfd_applicable_method_p and may be modified to reflect a
   more restrictive set of types. */
static boolean
    gfd_applicable_method_p(obj_t method, obj_t *args, obj_t cache)
{
    obj_t specializers = METHOD(method)->specializers;
    obj_t *cached_classes = obj_ptr(struct gf_cache *, cache)->cached_classes;

    while (specializers != obj_Nil) {
	obj_t arg = *args++;
	obj_t arg_class = *cached_classes++;
	obj_t specializer = HEAD(specializers);

	/* arg_class may be either a singleton, a limited_int, or a class.
	   This stuff has been worked out on a case by case basis.  It could
	   certainly be made clearer, but this could potentially reduce
	   the efficiency by a large margin. */
	if (!subtypep(arg_class, specializer))
	    if (instancep(arg, specializer)) {
		if (TYPE(specializer)->type_id == id_LimFixnum)
		    *(cached_classes - 1) =
			(TYPE(arg_class)->type_id == id_LimFixnum
			 ? intersect_limited_fixnums(arg_class,specializer)
			 : specializer);
		else if (TYPE(specializer)->type_id == id_LimBignum)
		    *(cached_classes - 1) =
			(TYPE(arg_class)->type_id == id_LimBignum
			 ? intersect_limited_bignums(arg_class,specializer)
			 : specializer);
		else
		    *(cached_classes - 1) = singleton(arg);
		obj_ptr(struct gf_cache *, cache)->simple = FALSE;
	    } else {
		if (overlapp(arg_class, specializer)) {
		    if (TYPE(specializer)->type_id == id_LimFixnum)
			*(cached_classes - 1) =
			    restrict_limited_fixnums(arg, arg_class,
						     specializer);
		    else if (TYPE(specializer)->type_id == id_LimBignum)
			*(cached_classes - 1) =
			    restrict_limited_bignums(arg, arg_class,
						     specializer);
		    else
			*(cached_classes - 1) = restrict_type(specializer,
							      arg_class);
		    obj_ptr(struct gf_cache *, cache)->simple = FALSE;
		}
		return FALSE;
	    }
	specializers = TAIL(specializers);
    }
    return TRUE;
}

static boolean applicable_method_p(obj_t method, obj_t *args)
{
    obj_t cache = METHOD(method)->class_cache;
    int max = METHOD(method)->required_args;
    int i;
    obj_t cache_elem, *cache_class, *arg;
    boolean result;

    if (cache != obj_False) {
	boolean found = TRUE;
	struct gf_cache *c = obj_ptr(struct gf_cache *, cache);
	register boolean simple = c->simple;

	cache_class = c->cached_classes;
	arg = args;

	for (i = 0; i < max; i++, arg++, cache_class++) {
	    boolean simple_arg = simple ||
		TYPE(*cache_class)->type_id == id_Class;
	    if (simple_arg ? *cache_class != object_class(*arg)
		           : !instancep(*arg, *cache_class)) {
		found = FALSE;
		break;
	    }
	}
	if (found)
	    return (c->cached_result == obj_True);
    }

    /* It wasn't in the cache.... */
    cache_elem = (cache == obj_False) ? make_gf_cache(max, obj_False) : cache;
    cache_class = obj_ptr(struct gf_cache *, cache_elem)->cached_classes;
    arg = args;

    for (i = 0; i < max; i++, arg++, cache_class++)
	*cache_class = object_class(*arg);

    result = gfd_applicable_method_p(method, args, cache_elem);

    obj_ptr(struct gf_cache *, cache_elem)->cached_result
	= result ? obj_True : obj_False;
    METHOD(method)->class_cache = cache_elem;

    return result;
}

static boolean method_accepts_keyword(obj_t method, obj_t keyword)
{
    obj_t keywords = METHOD(method)->keywords;

    assert(!METHOD(method)->all_keys);
    assert(keywords != obj_False);

    while (keywords != obj_Nil) {
	if (HEAD(HEAD(keywords)) == keyword)
	    return TRUE;
	keywords = TAIL(keywords);
    }
    return FALSE;
}

static void method_xep(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t method = args[-1];

    if (applicable_method_p(method, args)) {
	if (METHOD(method)->keywords != obj_False
	      && !METHOD(method)->all_keys) {
	    obj_t *ptr = args+METHOD(method)->required_args;
	    while (ptr < thread->sp) {
		if (!method_accepts_keyword(method, *ptr)) {
		    push_linkage(thread, args);
		    error("Method %= does not accept the keyword %=",
			  function_debug_name_or_self(method), *ptr);
		}
		ptr += 2;
	    }
	}
	invoke_methods(method, obj_Nil, thread, nargs);
    }
    else {
	push_linkage(thread, args);
	error("Method %= is not applicable when given the arguments %=",
	      function_debug_name_or_self(method),
	      make_vector(nargs, args));
    }
}

obj_t make_raw_method(char *debug_name, obj_t specializers, boolean restp,
		      obj_t keywords, boolean all_keys, obj_t result_types,
		      obj_t more_results_type,
		      void (*iep)(obj_t self, struct thread *thread, obj_t *args))
{
    obj_t res = alloc(obj_RawMethodClass, sizeof(struct method));

    METHOD(res)->xep = method_xep;
    METHOD(res)->debug_name = symbol(debug_name);
    METHOD(res)->required_args = length(specializers);
    METHOD(res)->restp = restp;
    METHOD(res)->keywords = keywords;
    METHOD(res)->all_keys = all_keys;
    METHOD(res)->result_types = result_types;
    METHOD(res)->more_results_type = more_results_type;
    METHOD(res)->specializers = specializers;
    METHOD(res)->class_cache = obj_False;
    METHOD(res)->iep = iep;

    return res;
}

void set_method_iep(obj_t method, 
		    void (*iep)(obj_t self, struct thread *thread, obj_t *args))
{
    METHOD(method)->iep = iep;
}

static boolean same_specializers(obj_t specializers1, obj_t specializers2)
{
    obj_t scan1 = specializers1;
    obj_t scan2 = specializers2;

    while (scan1 != obj_Nil) {
	obj_t spec1 = HEAD(scan1);
	obj_t spec2 = HEAD(scan2);

	if (!subtypep(spec1, spec2) || !subtypep(spec2, spec1))
	    return FALSE;

	scan1 = TAIL(scan1);
	scan2 = TAIL(scan2);
    }
    return TRUE;
}

enum method_comparison {
    method_MoreSpecific, method_LessSpecific,
    method_Identical, method_Ambiguous
};

static enum method_comparison compare_methods(obj_t meth1, obj_t meth2,
					      obj_t *args)
{
    boolean meth1_first = FALSE;
    boolean meth2_first = FALSE;
    obj_t scan1 = METHOD(meth1)->specializers;
    obj_t scan2 = METHOD(meth2)->specializers;

    while (scan1 != obj_Nil) {
	obj_t spec1 = HEAD(scan1);
	obj_t spec2 = HEAD(scan2);
	boolean spec1_more_specific = subtypep(spec1, spec2);
	boolean spec2_more_specific = subtypep(spec2, spec1);

	if (spec1_more_specific && spec2_more_specific)
	    /* The two specializers are identical. */
	    ;
	else if (spec1_more_specific) {
	    if (meth2_first)
		return method_Ambiguous;
	    meth1_first = TRUE;
	}
	else if (spec2_more_specific) {
	    if (meth1_first)
		return method_Ambiguous;
	    meth2_first = TRUE;
	}
	else if (instancep(spec1, obj_ClassClass)
		 && instancep(spec2, obj_ClassClass)) {
	    obj_t class = object_class(*args);
	    obj_t cpl = obj_ptr(struct class *, class)->cpl;

	    while (cpl != obj_Nil) {
		obj_t super = HEAD(cpl);
		if (super == spec1) {
		    if (meth2_first)
			return method_Ambiguous;
		    meth1_first = TRUE;
		    break;
		}
		if (super == spec2) {
		    if (meth1_first)
			return method_Ambiguous;
		    meth2_first = TRUE;
		    break;
		}
		cpl = TAIL(cpl);
	    }
	    if (cpl == obj_Nil)
		lose("Couldn't find either class in the objects cpl?");
	}
	else
	    return method_Ambiguous;

	scan1 = TAIL(scan1);
	scan2 = TAIL(scan2);
	args++;
    }

    if (meth1_first)
	return method_MoreSpecific;
    else if (meth2_first)
	return method_LessSpecific;
    else
	return method_Identical;
}


/* builtin methods. */

struct builtin_method {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
    obj_t (*func)();
};

#define BUILTIN_METHOD(o) obj_ptr(struct builtin_method *, o)

static void builtin_method_iep_1_arg(obj_t method, struct thread *thread,
				     obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_2_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_3_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_4_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_5_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3], args[4]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_6_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3], args[4], args[5]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_7_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3],
		 args[4], args[5], args[6]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_8_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3],
		 args[4], args[5], args[6], args[7]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_9_args(obj_t method, struct thread *thread,
				      obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3], args[4],
		 args[5], args[6], args[7], args[8]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void builtin_method_iep_10_args(obj_t method, struct thread *thread,
				       obj_t *args)
{
    obj_t (*func)() = BUILTIN_METHOD(method)->func;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    value = func(args[0], args[1], args[2], args[3], args[4],
		 args[5], args[6], args[7], args[8], args[9]);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

static void (*builtin_method_ieps[])(obj_t m, struct thread *t, obj_t *a) = {
    NULL,
    builtin_method_iep_1_arg,
    builtin_method_iep_2_args,
    builtin_method_iep_3_args,
    builtin_method_iep_4_args,
    builtin_method_iep_5_args,
    builtin_method_iep_6_args,
    builtin_method_iep_7_args,
    builtin_method_iep_8_args,
    builtin_method_iep_9_args,
    builtin_method_iep_10_args
};

#define MAX_BUILTIN_METHOD_ARGS (sizeof(builtin_method_ieps)/sizeof(builtin_method_ieps[0]))

obj_t make_builtin_method(char *debug_name, obj_t specializers,
			  boolean restp, obj_t keywords, boolean all_keys,
			  obj_t result_type, obj_t (*func)())
{
    obj_t res = alloc(obj_BuiltinMethodClass, sizeof(struct builtin_method));
    int req_args = length(specializers);
    int num_args = req_args + 1; /* Add one for the next methods */

    if (restp)
	num_args++;
    if (keywords != obj_False)
	num_args += length(keywords);

    if (num_args >= MAX_BUILTIN_METHOD_ARGS)
	lose("Can't make a builtin method that wants %d args -- %d at most.",
	     num_args, MAX_BUILTIN_METHOD_ARGS-1);

    BUILTIN_METHOD(res)->xep = method_xep;
    BUILTIN_METHOD(res)->debug_name = symbol(debug_name);
    BUILTIN_METHOD(res)->required_args = req_args;
    BUILTIN_METHOD(res)->restp = restp;
    BUILTIN_METHOD(res)->keywords = keywords;
    BUILTIN_METHOD(res)->all_keys = all_keys;
    BUILTIN_METHOD(res)->result_types = list1(result_type);
    BUILTIN_METHOD(res)->more_results_type = obj_False;
    BUILTIN_METHOD(res)->specializers = specializers;
    BUILTIN_METHOD(res)->class_cache = obj_False;
    BUILTIN_METHOD(res)->iep = builtin_method_ieps[num_args];
    BUILTIN_METHOD(res)->func = func;

    return res;
}


/* byte methods */

struct byte_method {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
    obj_t component;
    int n_closure_vars;
    obj_t lexenv[1];
};

#define BYTE_METHOD(o) obj_ptr(struct byte_method *, o)

obj_t byte_method_component(obj_t method)
{
    return BYTE_METHOD(method)->component;
}

static void byte_method_iep(obj_t method, struct thread *thread, obj_t *args)
{
    int i, count;
    obj_t *fp;

    /* push the closure vars */
    count = BYTE_METHOD(method)->n_closure_vars;
    for (i = 0; i < count; i++)
	*thread->sp++ = BYTE_METHOD(method)->lexenv[i];

    fp = push_linkage(thread, args);
    set_byte_continuation(thread, BYTE_METHOD(method)->component);
#if !SLOW_LONGJMP
    go_on();
#endif
}

obj_t make_method_info(boolean restp, obj_t keys, boolean all_keys,
		       obj_t component, int n_closure_vars)
{
    obj_t res = alloc(obj_MethodInfoClass, sizeof(struct method_info));

    METHOD_INFO(res)->restp = restp;
    METHOD_INFO(res)->keys = keys;
    METHOD_INFO(res)->all_keys = all_keys;
    METHOD_INFO(res)->component = component;
    METHOD_INFO(res)->n_closure_vars = n_closure_vars;

    return res;
}

obj_t make_byte_method(obj_t method_info, obj_t specializers,
		       obj_t result_types, obj_t more_results_type,
		       obj_t *lexenv)
		       
{
    int n_closure_vars = METHOD_INFO(method_info)->n_closure_vars;
    obj_t res = alloc(obj_ByteMethodClass,
		      sizeof(struct byte_method) + sizeof(obj_t)*(n_closure_vars - 1));
    obj_t component = METHOD_INFO(method_info)->component;
    int i;

    BYTE_METHOD(res)->xep = method_xep;
    BYTE_METHOD(res)->debug_name = COMPONENT(component)->debug_name;
    BYTE_METHOD(res)->required_args = length(specializers);
    BYTE_METHOD(res)->restp = METHOD_INFO(method_info)->restp;
    BYTE_METHOD(res)->keywords = METHOD_INFO(method_info)->keys;
    BYTE_METHOD(res)->all_keys = METHOD_INFO(method_info)->all_keys;
    BYTE_METHOD(res)->result_types = result_types;
    if (more_results_type == obj_True)
	BYTE_METHOD(res)->more_results_type = obj_ObjectClass;
    else
	BYTE_METHOD(res)->more_results_type = more_results_type;
    BYTE_METHOD(res)->specializers = specializers;
    BYTE_METHOD(res)->class_cache = obj_False;
    BYTE_METHOD(res)->iep = byte_method_iep;
    BYTE_METHOD(res)->component = component;
    BYTE_METHOD(res)->n_closure_vars = n_closure_vars;
    for (i = 0; i < n_closure_vars; i++)
	BYTE_METHOD(res)->lexenv[i] = lexenv[i];

    return res;
}


/* Slot accessor methods. */

struct accessor_method {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
    obj_t datum;
};

#define ACCESSOR_METHOD(o) obj_ptr(struct accessor_method *, o)

obj_t make_accessor_method(obj_t debug_name, obj_t class, obj_t type,
			   boolean setter, obj_t datum,
			   void (*iep)(obj_t self, struct thread *thread,
				    obj_t *args))
{
    obj_t res = alloc(obj_AccessorMethodClass, sizeof(struct accessor_method));

    ACCESSOR_METHOD(res)->xep = method_xep;
    ACCESSOR_METHOD(res)->debug_name = debug_name;
    ACCESSOR_METHOD(res)->required_args = setter ? 2 : 1;
    ACCESSOR_METHOD(res)->restp = FALSE;
    ACCESSOR_METHOD(res)->keywords = obj_False;
    ACCESSOR_METHOD(res)->all_keys = FALSE;
    ACCESSOR_METHOD(res)->result_types = list1(type);
    ACCESSOR_METHOD(res)->more_results_type = obj_False;
    ACCESSOR_METHOD(res)->specializers
	= setter ? list2(type, class) : list1(class);
    ACCESSOR_METHOD(res)->class_cache = obj_False;
    ACCESSOR_METHOD(res)->iep = iep;
    ACCESSOR_METHOD(res)->datum = datum;

    return res;
}

obj_t accessor_method_datum(obj_t method)
{
    return ACCESSOR_METHOD(method)->datum;
}

void set_accessor_method_datum(obj_t method, obj_t datum)
{
    ACCESSOR_METHOD(method)->datum = datum;
}


/* C functions. */

struct c_function {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
    void *pointer;
};

#define C_FUNCTION(o) obj_ptr(struct c_function *, o)

static void c_function_xep(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t cf = args[-1];
    int (*fun)() = (int(*) ()) C_FUNCTION(cf)->pointer;
    obj_t res_type = HEAD(C_FUNCTION(cf)->result_types);
    int result;
    obj_t *old_sp;
    obj_t value;

    push_linkage(thread, args);

    switch (nargs) {
    case 0:
	result = fun();
	break;
    case 1:
	result = fun(get_c_object(args[0]));
	break;
    case 2:
	result = fun(get_c_object(args[0]), get_c_object(args[1]));
	break;
    case 3:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]));
	break;
    case 4:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]));
	break;
    case 5:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]));
	break;
    case 6:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]), get_c_object(args[5]));
	break;
    case 7:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]), get_c_object(args[5]),
		     get_c_object(args[6]));
	break;
    case 8:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]), get_c_object(args[5]),
		     get_c_object(args[6]), get_c_object(args[7]));
	break;
    case 9:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]), get_c_object(args[5]),
		     get_c_object(args[6]), get_c_object(args[7]),
		     get_c_object(args[8]));
	break;
    case 10:
	result = fun(get_c_object(args[0]), get_c_object(args[1]),
		     get_c_object(args[2]), get_c_object(args[3]),
		     get_c_object(args[4]), get_c_object(args[5]),
		     get_c_object(args[6]), get_c_object(args[7]),
		     get_c_object(args[8]), get_c_object(args[9]));
	break;
    default:
	result = 0;		/* make compiler happy */
	lose("Can't call a c function with more than 10 args");
    }

    value = convert_c_object(res_type, (void *)result, TRUE);

    old_sp = pop_linkage(thread);
    *old_sp = value;
    thread->sp = old_sp+1;

    do_return(thread, old_sp, old_sp);
}

obj_t make_c_function(obj_t debug_name, void *pointer)
{
    obj_t res = alloc(obj_CFunctionClass, sizeof(struct c_function));

    C_FUNCTION(res)->xep = c_function_xep;
    C_FUNCTION(res)->debug_name = debug_name;
    C_FUNCTION(res)->required_args = 0;
    C_FUNCTION(res)->restp = TRUE;
    C_FUNCTION(res)->keywords = obj_False;
    C_FUNCTION(res)->all_keys = FALSE;
    C_FUNCTION(res)->result_types = obj_ObjectClass;
    C_FUNCTION(res)->more_results_type = obj_False;
    C_FUNCTION(res)->pointer = pointer;
    C_FUNCTION(res)->specializers = obj_Nil;
    C_FUNCTION(res)->class_cache = obj_False;
    C_FUNCTION(res)->iep = NULL;

    return res;
}

obj_t constrain_c_function(obj_t /* <c-function> */ res,
			   obj_t /* <list> */ specializers,
			   obj_t restp,
			   obj_t /* <list> */ result_types)
{
    C_FUNCTION(res)->required_args = length(specializers);
    C_FUNCTION(res)->restp = (restp != obj_False);
    C_FUNCTION(res)->result_types = result_types;
    C_FUNCTION(res)->specializers = specializers;

    return res;
}
    

/* Generic functions. */

struct gf {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    boolean all_keys;
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t methods;
    obj_t methods_clock;
    obj_t cache;
};

#define GF(o) obj_ptr(struct gf *, o)

static obj_t
    slow_sorted_applicable_methods(struct gf *gf, obj_t methods, obj_t *args)
{
    obj_t ordered = obj_Nil;
    obj_t ambiguous = obj_Nil;
    obj_t scan, *prev;
    int i, max = gf->required_args;
    obj_t cache_elem = make_gf_cache(max, obj_False);
    obj_t *cache = obj_ptr(struct gf_cache *, cache_elem)->cached_classes;
    obj_t *arg = args;
    
    for (i = 0; i < max; i++, arg++, cache++)
	*cache = object_class(*arg);

    while (methods != obj_Nil) {
	obj_t method = HEAD(methods);

	if (gfd_applicable_method_p(method, args, cache_elem)) {
	    for (prev=&ordered; (scan=*prev) != obj_Nil; prev=&TAIL(scan)) {
		switch (compare_methods(method, HEAD(scan), args)) {
		  case method_MoreSpecific:
		    *prev = pair(method, scan);
		    goto next;
		  case method_LessSpecific:
		    break;
		  case method_Ambiguous:
		    *prev = obj_Nil;
		    ambiguous = list2(method, HEAD(scan));
		    goto next;
		  case method_Identical:
		    lose("Two identical methods in the same "
			 "generic function?");
		}
	    }
	    {
		obj_t new_ambiguous = obj_Nil;
		boolean more_specific = TRUE;

		for (scan = ambiguous; scan != obj_Nil; scan = TAIL(scan)) {
		    switch (compare_methods(method, HEAD(scan), args)) {
		      case method_MoreSpecific:
			break;
		      case method_Ambiguous:
			new_ambiguous = pair(HEAD(scan), new_ambiguous);
			break;
		      case method_LessSpecific:
			more_specific = FALSE;
			break;
		      case method_Identical:
			lose("Two identical methods in the same "
			     "generic function?");
		    }
		}
		if (new_ambiguous != obj_Nil)
		    ambiguous = new_ambiguous;
		else if (more_specific)
		    *prev = list1(method);
	    }
	}
      next:
	methods = TAIL(methods);
    }

    if (ambiguous != obj_Nil) {
	for (prev = &ordered; (scan = *prev) != obj_Nil; prev = &TAIL(scan))
	    ;
	*prev = pair(obj_False, ambiguous);
    }

    obj_ptr(struct gf_cache *, cache_elem)->cached_result = ordered;
    gf->cache = pair(cache_elem, gf->cache);
    return ordered;
}

static obj_t sorted_applicable_methods(obj_t gf, obj_t *args)
{
    struct gf *true_gf = GF(gf);
    obj_t *prev, cache;
    obj_t methods = true_gf->methods;
    int max = true_gf->required_args;
	
    /* If there are no methods, then nothing is applicable. */
    if (methods == obj_Nil)
	return obj_Nil;

    for (prev = &true_gf->cache, cache = *prev;
	 cache != obj_Nil; prev = &TAIL(cache), cache = *prev) {
	struct gf_cache *cache_elem = obj_ptr(struct gf_cache *, HEAD(cache));
	register boolean simple = cache_elem->simple;
	obj_t *cache_class = cache_elem->cached_classes;
	obj_t *arg = args;
	int i;
	boolean found = TRUE;

	for (i = 0; i < max; i++, arg++, cache_class++) {
	    boolean simple_arg = simple ||
		TYPE(*cache_class)->type_id == id_Class;
	    if (simple_arg ? *cache_class != object_class(*arg)
		           : !instancep(*arg, *cache_class)) {
		found = FALSE;
		break;
	    }
	}

	if (found) {
	    *prev = TAIL(cache);
	    TAIL(cache) = true_gf->cache;
	    true_gf->cache = cache;
	    return cache_elem->cached_result;
	}
    }

    /* We have to do it the slow way */
    return slow_sorted_applicable_methods(true_gf, methods, args);
}

static boolean methods_accept_keyword(obj_t methods, obj_t keyword)
{
    obj_t method;

    while (methods != obj_Nil && (method = HEAD(methods)) != obj_False) {
	if (method_accepts_keyword(method, keyword))
	    return TRUE;
	methods = TAIL(methods);
    }
    return FALSE;
}

static void gf_xep(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t gf = args[-1];
    obj_t methods, primary_method;

    methods = sorted_applicable_methods(gf, args);

    if (methods != obj_Nil) {
	if (GF(gf)->keywords != obj_False && !GF(gf)->all_keys) {
	    obj_t *ptr = args + GF(gf)->required_args;
	    while (ptr < thread->sp) {
		if (!methods_accept_keyword(methods, *ptr)) {
		    push_linkage(thread, args);
		    error("The keyword %= is accepted by none of the "
			  "applicable methods:\n  %=",
			  *ptr, methods);
		}
		ptr += 2;
	    }
	}
	primary_method = HEAD(methods);
	args[-1] = primary_method;
	invoke_methods(primary_method, TAIL(methods), thread, nargs);
    }
    else {
	push_linkage(thread, args);
	if (strcmp(sym_name(function_debug_name_or_self(gf)), "main") == 0) {
	    error("No applicable methods for #\"main\" with arguments %=.\n"
		  "\nYou need to define a method on the generic function\n"
		  "Main with the specializers (<string>, #rest strings).\n"
		  "The generic function Main is exported from the\n"
		  "Extensions module; perhaps you forgot to import the\n"
		  "Extensions module into the module that defines the\n"
		  "method on Main?",
	      make_vector(nargs, args));
	} else {
	    error("No applicable methods for %= with arguments %=",
		  function_debug_name_or_self(gf),
		  make_vector(nargs, args));
	}
    }
}

obj_t make_generic_function(obj_t debug_name, obj_t specializers,
			    boolean restp, obj_t keywords, boolean all_keys,
			    obj_t result_types, obj_t more_results_type)
{
    obj_t res = alloc(obj_GFClass, sizeof(struct gf));
    int req_args = length(specializers);

    GF(res)->xep = gf_xep;
    GF(res)->debug_name = debug_name;
    GF(res)->required_args = req_args;
    GF(res)->restp = restp;
    GF(res)->keywords = keywords;
    GF(res)->all_keys = all_keys;
    GF(res)->result_types = result_types;
    if (more_results_type == obj_True)
	GF(res)->more_results_type = obj_ObjectClass;
    else
	GF(res)->more_results_type = more_results_type;
    GF(res)->specializers = specializers;
    GF(res)->methods = obj_Nil;
    GF(res)->methods_clock = obj_False;
    GF(res)->cache = obj_Nil;

    return res;
}

static obj_t make_nondescript_specializers (int required_args)
{
    obj_t specializers = obj_Nil;
    int i;

    for (i=0; i<required_args; i++)
	specializers = pair(obj_ObjectClass, specializers);

    return specializers;
}

obj_t make_default_generic_function(obj_t debug_name, obj_t method)
{
    boolean restp = METHOD(method)->restp;
    int req_args = METHOD(method)->required_args;
    obj_t keywords = METHOD(method)->keywords;
    boolean all_keys = METHOD(method)->all_keys;
    obj_t specializers = make_nondescript_specializers(req_args);

    if (keywords != obj_False)
	keywords = obj_Nil;

    return make_generic_function(debug_name, specializers, restp, keywords,
				 all_keys, obj_Nil, obj_ObjectClass);
}

void set_gf_signature(obj_t gf, obj_t specializers, boolean restp, obj_t keys,
		      boolean all_keys, obj_t result_types,
		      obj_t more_results_type)
{
    obj_t methods = GF(gf)->methods;

    GF(gf)->required_args = length(specializers);
    GF(gf)->restp = restp;
    GF(gf)->keywords = keys;
    GF(gf)->all_keys = all_keys;
    GF(gf)->result_types = result_types;
    if (more_results_type == obj_True)
	GF(gf)->more_results_type = obj_ObjectClass;
    else
	GF(gf)->more_results_type = more_results_type;
    GF(gf)->specializers = specializers;
    GF(gf)->methods = obj_Nil;

    while (methods != obj_Nil) {
	add_method(gf, HEAD(methods));
	methods = TAIL(methods);
    }
}

obj_t generic_function_methods(obj_t gf)
{
    return GF(gf)->methods;
}

obj_t generic_function_methods_clock(obj_t gf)
{
    obj_t clock = GF(gf)->methods_clock;

    if (clock == obj_False) {
	clock = list1(obj_False);
	GF(gf)->methods_clock = clock;
    }

    return clock;
}

static obj_t really_add_method(obj_t gf, obj_t method)
{
    obj_t methods = GF(gf)->methods;
    obj_t specializers = METHOD(method)->specializers;
    obj_t scan;

    GF(gf)->cache = obj_Nil;
    GF(gf)->methods_clock = obj_False;

    for (scan = methods; scan != obj_Nil; scan = TAIL(scan)) {
	obj_t old = HEAD(scan);
	if (same_specializers(METHOD(old)->specializers, specializers)) {
	    HEAD(scan) = method;
	    return old;
	}
    }
    
    GF(gf)->methods = pair(method, methods);
    return obj_False;
}

obj_t add_method(obj_t gf, obj_t method)
{
    obj_t gfkeys;
    obj_t gfscan, methscan;
    int i;
    boolean warned_about_method = FALSE;

    if (GF(gf)->required_args != METHOD(method)->required_args)
	error("%= has %d required arguments, but %= has %d",
	      method, make_fixnum(METHOD(method)->required_args),
	      gf, make_fixnum(GF(gf)->required_args));

    gfkeys = GF(gf)->keywords;
    if (gfkeys != obj_False) {
	/* The generic function takes keyword arguments. */
	obj_t methkeys = METHOD(method)->keywords;

	if (methkeys == obj_False)
	    error("%= allows keyword arguments, but %= does not.", gf, method);
	while (gfkeys != obj_Nil) {
	    obj_t gfkey = HEAD(gfkeys);
	    obj_t scan;

	    for (scan = methkeys; scan != obj_Nil; scan = TAIL(scan))
		if (HEAD(HEAD(scan)) == gfkey)
		    goto okay;
	    error("The keyword %= is mandatory for %=, "
		  "but %= doesn't accept it.",
		  gfkey, gf, method);
	  okay:
	    gfkeys = TAIL(gfkeys);
	}

	if (METHOD(method)->all_keys && !GF(gf)->all_keys)
	    error("%= accepts all keys, but %= does not.", method, gf);
    }
    else if (METHOD(method)->keywords != obj_False)
	error("%= allows keyword arguments, but %= does not.", method, gf);
    else if (GF(gf)->restp) {
	if (!METHOD(method)->restp)
	    error("%= accepts a variable number of arguments, "
		  "but %= does not.",
		  gf, method);
    }
    else if (METHOD(method)->restp)
	error("%= accepts a variable number of arguments, but %= does not.",
	      method, gf);

    /* Check method specializers against GF */
    gfscan = GF(gf)->specializers;
    methscan = METHOD(method)->specializers;
    i = 1;
    while (gfscan != obj_Nil && methscan != obj_Nil) {
	obj_t gftype = HEAD(gfscan);
	obj_t methtype = HEAD(methscan);

	if (!subtypep(methtype, gftype))
	    error("Specializer %= is %= for %=, but %= for %=",
		  make_fixnum(i), gftype, gf, methtype, method);

	gfscan = TAIL(gfscan);
	methscan = TAIL(methscan);
	i++;
    }

    /* Check method return types against GF */
    gfscan = GF(gf)->result_types;
    methscan = METHOD(method)->result_types;
    i = 1;
    while (gfscan != obj_Nil && methscan != obj_Nil) {
	obj_t gftype = HEAD(gfscan);
	obj_t methtype = HEAD(methscan);

	if (!subtypep(methtype, gftype))
	    error("Result %= is %= for %=, but %= for %=",
		  make_fixnum(i), gftype, gf, methtype, method);

	gfscan = TAIL(gfscan);
	methscan = TAIL(methscan);
	i++;
    }
    i--;   /* Since the counter increments even on the last loop, undo that */

    if (gfscan != obj_Nil) {
	int gf_returns = i;
	while (gfscan != obj_Nil) {
	    gf_returns++;
	    gfscan = TAIL(gfscan);
	}
	if (METHOD(method)->more_results_type == obj_ObjectClass
	    && METHOD(method)->result_types == obj_Nil) {
	    warned_about_method = TRUE;
	    format("WARNING: %= returns #rest <object>, but %= returns %=.\n",
		   method, gf, GF(gf)->result_types);
	} else if (GF(gf)->more_results_type != obj_False) {
	    error("%= returns at least %d results, but %= only returns %d",
		  gf, make_fixnum(gf_returns), method, make_fixnum(i));
	} else {
	    error("%= returns exactly %d results, but %= only returns %d",
		  gf, make_fixnum(gf_returns), method, make_fixnum(i));
	}
    }
    if (methscan != obj_Nil) {
	obj_t gftype = GF(gf)->more_results_type;

	if (gftype == obj_False) {
	    int meth_returns = i;
	    while (methscan != obj_Nil) {
		methscan = TAIL(methscan);
		meth_returns++;
	    }
	    if (METHOD(method)->more_results_type != obj_False) {
		if (!warned_about_method) {
		    warned_about_method = TRUE;
		    error("%= returns exactly %d results, "
			  "but %= returns %d or more",
			  gf, make_fixnum(i), 
			  method, make_fixnum(meth_returns));
		}
	    } else {
		error("%= returns exactly %d results, but %= returns %d",
		      gf, make_fixnum(i), method, make_fixnum(meth_returns));
	    }
	}
	while (methscan != obj_Nil) {
	    obj_t methtype = HEAD(methscan);

	    if (!subtypep(methtype, gftype))
		error("Result %d is %= for %=, but %= for %=",
		      make_fixnum(i), gftype, gf, methtype, method);

	    methscan = TAIL(methscan);
	    i++;
	}
    }

    if (METHOD(method)->more_results_type != obj_False) {
	if (GF(gf)->more_results_type != obj_False) {
	    if (!subtypep(METHOD(method)->more_results_type,
			  GF(gf)->more_results_type)) {
		error("Results %d and on are instances of %= for %=, "
		      "but are instances of %= for %=",
		      make_fixnum(i), GF(gf)->more_results_type, gf,
		      METHOD(method)->more_results_type, method);
	    }
	} else {
	    if (!warned_about_method) {
		warned_about_method = TRUE;
		error("%= returns exactly %d results, "
		      "but %= returns %d or more",
		      gf, make_fixnum(i), method, make_fixnum(i));
	    }
	}
    }

    return really_add_method(gf, method);
}


/* Dylan interface functions. */

static obj_t dylan_make_gf(obj_t debug_name, obj_t required,
			   obj_t restp, obj_t keywords, obj_t all_keys,
			   obj_t res_types, obj_t more_res_type)
{
    obj_t specializers = make_nondescript_specializers(fixnum_value(required));
    return make_generic_function(debug_name, specializers,
				 restp != obj_False, keywords,
				 all_keys != obj_False, res_types,
				 more_res_type);
}

static void dylan_add_method(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *vals = args-1;
    obj_t gf = args[0];
    obj_t method = args[1];
    obj_t old = add_method(gf, method);

    thread->sp = vals + 2;
    vals[0] = method;
    vals[1] = old;

    do_return(thread, vals, vals);
}

static void dylan_function_arguments(obj_t self, struct thread *thread,
				     obj_t *args)
{
    obj_t *vals = args-1;
    obj_t func = *args;
    obj_t keywords = FUNC(func)->keywords;

    thread->sp = vals + 3;
    vals[0] = make_fixnum(FUNC(func)->required_args);
    if (FUNC(func)->restp && keywords == obj_False)
	vals[1] = obj_True;
    else
	vals[1] = obj_False;
    vals[2] = FUNC(func)->all_keys ? symbol("all") : keywords;

    do_return(thread, vals, vals);
}

static void dylan_method_arguments(obj_t self, struct thread *thread,
				   obj_t *args)
{
    obj_t *vals = args-1;
    obj_t meth = *args;
    obj_t keywords = METHOD(meth)->keywords;

    thread->sp = vals + 3;
    vals[0] = make_fixnum(METHOD(meth)->required_args);
    if (METHOD(meth)->restp && keywords == obj_False)
	vals[1] = obj_True;
    else
	vals[1] = obj_False;
    if (METHOD(meth)->all_keys)
	vals[2] = symbol("all");
    else if (keywords != obj_False) {
	obj_t new = obj_Nil;
	while (keywords != obj_Nil) {
	    new = pair(HEAD(HEAD(keywords)), new);
	    keywords = TAIL(keywords);
	}
	vals[2] = new;
    }
    else
	vals[2] = obj_False;

    do_return(thread, vals, vals);
}

static void dylan_function_return_values(obj_t self, struct thread *thread,
					 obj_t *args)
{
    obj_t *vals = args-1;
    obj_t func = *args;

    thread->sp = vals + 2;
    vals[0] = FUNC(func)->result_types;
    vals[1] = FUNC(func)->more_results_type; 
    do_return(thread, vals, vals);
}

static obj_t dylan_sorted_app_meths(obj_t gf, obj_t args)
{
    int nargs = SOVEC(args)->length;

    if (nargs < GF(gf)->required_args)
	return obj_Nil;
    else
	return sorted_applicable_methods(gf, SOVEC(args)->contents);
}

static obj_t dylan_app_meth_p(obj_t method, obj_t args)
{
    int nargs = SOVEC(args)->length;
    
    if (nargs < METHOD(method)->required_args)
	return obj_False;
    else if (applicable_method_p(method, SOVEC(args)->contents))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_find_method(obj_t gf, obj_t specializers)
{
    obj_t scan;

    for (scan = specializers; scan != obj_Nil; scan = TAIL(scan))
	check_type(HEAD(scan), obj_TypeClass);

    for (scan = GF(gf)->methods; scan != obj_Nil; scan = TAIL(scan)) {
	obj_t method = HEAD(scan);
	if (same_specializers(METHOD(method)->specializers, specializers))
	    return method;
    }

    return obj_False;
}

static obj_t dylan_remove_method(obj_t gf, obj_t method)
{
    obj_t scan, *prev;

    GF(gf)->cache = obj_Nil;
    GF(gf)->methods_clock = obj_False;

    prev = &GF(gf)->methods;
    while ((scan = *prev) != obj_Nil) {
	if (method == HEAD(scan)) {
	    *prev = TAIL(scan);
	    return method;
	}
	prev = &TAIL(scan);
    }
    error("%= isn't one of the methods in %=", method, gf);
    return NULL;
}

static void dylan_do_next_method(obj_t self, struct thread *thread,
				 obj_t *args)
{
    obj_t methods = args[0];
    obj_t new_args = args[1];
    int len = SOVEC(new_args)->length;
    int i;

    for (i = 0; i < len; i++)
	args[i] = SOVEC(new_args)->contents[i];
    thread->sp = args + len;

    invoke_methods(HEAD(methods), TAIL(methods), thread, len);
}


/* Printer support. */

static void print_func(obj_t func)
{
    obj_t class = FUNC(func)->class;
    obj_t class_name = obj_ptr(struct class *, class)->debug_name;
    obj_t debug_name = FUNC(func)->debug_name;
    char *class_str;

    if (class_name != NULL && class_name != obj_False)
	class_str = sym_name(class_name);
    else
	class_str = "unknown function";

    if (debug_name != NULL && debug_name != obj_False) {
	printf("{%s ", class_str);
	prin1(debug_name);
	putchar('}');
    }
    else
	printf("{anonymous %s 0x%08lx}", class_str, (unsigned long)func);
}

static void print_method(obj_t method)
{
    obj_t class = METHOD(method)->class;
    obj_t class_name = obj_ptr(struct class *, class)->debug_name;
    obj_t debug_name = METHOD(method)->debug_name;
    char *class_str;

    if (class_name != NULL && class_name != obj_False)
	class_str = sym_name(class_name);
    else
	class_str = "unknown function";

    if (debug_name != NULL && debug_name != obj_False) {
	printf("{%s ", class_str);
	prin1(debug_name);
	putchar(' ');
    }
    else
	printf("{anonymous %s 0x%08lx ", class_str, (unsigned long)method);


    prin1(METHOD(method)->specializers);
    putchar('}');
}    


/* GC stuff. */

static void scav_func(struct function *func)
{
    scavenge(&func->debug_name);
    scavenge(&func->keywords);
    scavenge(&func->result_types);
    scavenge(&func->more_results_type);
    scavenge(&func->specializers);
}

static int scav_raw_func(struct object *ptr)
{
    scav_func((struct function *)ptr);

    return sizeof(struct function);
}

static obj_t trans_raw_func(obj_t func)
{
    return transport(func, sizeof(struct function), TRUE);
}

static int scav_raw_method(struct object *ptr)
{
    scav_func((struct function *)ptr);
    scavenge(&((struct method *)ptr)->class_cache);

    return sizeof(struct method);
}
    
static obj_t trans_raw_method(obj_t method)
{
    return transport(method, sizeof(struct method), FALSE);
}

static int scav_builtin_method(struct object *ptr)
{
    scav_func((struct function *)ptr);
    scavenge(&((struct builtin_method *)ptr)->class_cache);

    return sizeof(struct builtin_method);
}
    
static obj_t trans_builtin_method(obj_t method)
{
    return transport(method, sizeof(struct builtin_method), FALSE);
}

static int scav_byte_method(struct object *ptr)
{
    struct byte_method *method = (struct byte_method *)ptr;
    int i;

    scav_func((struct function *)ptr);
    scavenge(&method->class_cache);
    scavenge(&method->component);

    for (i = 0; i < method->n_closure_vars; i++)
	scavenge(method->lexenv + i);

    return sizeof(struct byte_method) 
	+ sizeof(obj_t)*(method->n_closure_vars - 1);
}

static obj_t trans_byte_method(obj_t method)
{
    int nvars = BYTE_METHOD(method)->n_closure_vars;

    return transport(method,
		     sizeof(struct byte_method) + sizeof(obj_t)*(nvars - 1),
		     FALSE);
}

static int scav_method_info(struct object *ptr)
{
    struct method_info *info = (struct method_info *)ptr;

    scavenge(&info->keys);
    scavenge(&info->component);

    return sizeof(struct method_info);
}

static obj_t trans_method_info(obj_t info)
{
    return transport(info, sizeof(struct method_info), TRUE);
}

static int scav_accessor_method(struct object *ptr)
{
    struct accessor_method *method = (struct accessor_method *)ptr;

    scav_func((struct function *)ptr);
    scavenge(&method->class_cache);
    scavenge(&method->datum);

    return sizeof(struct accessor_method);
}
    
static obj_t trans_accessor_method(obj_t method)
{
    return transport(method, sizeof(struct accessor_method), FALSE);
}

static int scav_c_function(struct object *ptr)
{
    scav_func((struct function *)ptr);

    return sizeof(struct c_function);
}
    
static obj_t trans_c_function(obj_t method)
{
    return transport(method, sizeof(struct c_function), FALSE);
}

static int scav_gf(struct object *ptr)
{
    struct gf *gf = (struct gf *)ptr;

    scav_func((struct function *)gf);
    scavenge(&gf->methods);
    scavenge(&gf->methods_clock);
    scavenge(&gf->cache);

    return sizeof(struct gf);
}

static obj_t trans_gf(obj_t gf)
{
    return transport(gf, sizeof(struct gf), FALSE);
}

static int scav_gf_cache(struct object *ptr)
{
    struct gf_cache *gf_cache = (struct gf_cache *)ptr;
    int i, max = gf_cache->size;

    scavenge(&gf_cache->cached_result);
    for (i = 0; i < max; i++)
	scavenge(&gf_cache->cached_classes[i]);

    return sizeof(struct gf_cache) + sizeof(obj_t)*(max - 1);
}

static obj_t trans_gf_cache(obj_t gf_cache)
{
    return transport(gf_cache, 
		     (sizeof(struct gf_cache) 
		      + sizeof(obj_t) 
		        * (obj_ptr(struct gf_cache *, gf_cache)->size - 1)),
		     FALSE);
}


/* Init stuff. */

void make_func_classes(void)
{
    obj_FunctionClass = make_abstract_class(TRUE);
    obj_RawFunctionClass = make_builtin_class(scav_raw_func, trans_raw_func);
    obj_MethodClass = make_abstract_class(TRUE);
    obj_RawMethodClass
	= make_builtin_class(scav_raw_method, trans_raw_method);
    obj_BuiltinMethodClass
	= make_builtin_class(scav_builtin_method, trans_builtin_method);
    obj_ByteMethodClass
	= make_builtin_class(scav_byte_method, trans_byte_method);
    obj_AccessorMethodClass
	= make_builtin_class(scav_accessor_method, trans_accessor_method);
    obj_CFunctionClass
	= make_builtin_class(scav_c_function, trans_c_function);
    obj_MethodInfoClass
	= make_builtin_class(scav_method_info, trans_method_info);
    obj_GFClass = make_builtin_class(scav_gf, trans_gf);
    obj_GFCacheClass = make_builtin_class(scav_gf_cache, trans_gf_cache);

    add_constant_root(&obj_FunctionClass);
    add_constant_root(&obj_RawFunctionClass);
    add_constant_root(&obj_MethodClass);
    add_constant_root(&obj_RawMethodClass);
    add_constant_root(&obj_BuiltinMethodClass);
    add_constant_root(&obj_ByteMethodClass);
    add_constant_root(&obj_AccessorMethodClass);
    add_constant_root(&obj_CFunctionClass);
    add_constant_root(&obj_MethodInfoClass);
    add_constant_root(&obj_GFClass);
    add_constant_root(&obj_GFCacheClass);
}

void init_func_classes(void)
{
    init_builtin_class(obj_FunctionClass, "<function>", obj_ObjectClass, NULL);
    def_printer(obj_FunctionClass, print_func);
    init_builtin_class(obj_RawFunctionClass, "<builtin-function>",
		       obj_FunctionClass, NULL);
    init_builtin_class(obj_MethodClass, "<method>", obj_FunctionClass, NULL);
    def_printer(obj_MethodClass, print_method);
    init_builtin_class(obj_RawMethodClass, "<raw-method>",
		       obj_MethodClass, NULL);
    init_builtin_class(obj_BuiltinMethodClass, "<builtin-method>",
		       obj_MethodClass, NULL);
    init_builtin_class(obj_ByteMethodClass, "<byte-method>",
		       obj_MethodClass, NULL);
    init_builtin_class(obj_MethodInfoClass, "<method-info>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_AccessorMethodClass, "<slot-accessor-method>",
		       obj_MethodClass, NULL);
    init_builtin_class(obj_CFunctionClass, "<c-function>",
		       obj_FunctionClass, NULL);
    init_builtin_class(obj_GFClass, "<generic-function>",
		       obj_FunctionClass, NULL);
    init_builtin_class(obj_GFCacheClass, "<generic-function-cache>",
		       obj_ObjectClass, NULL);
}

void init_func_functions(void)
{
    obj_t type_or_singleton_false
	= type_union(obj_TypeClass, singleton(obj_False));
    define_method("function-name", list1(obj_FunctionClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, function_debug_name);
    define_function("make-generic-function",
		    listn(7, obj_ObjectClass, obj_FixnumClass,
			  obj_ObjectClass,
			  type_union(object_class(obj_False), obj_ListClass),
			  obj_ObjectClass, obj_ListClass,
			  type_union(object_class(obj_False), obj_TypeClass)),
		    FALSE, obj_False, FALSE,
		    list1(obj_GFClass), dylan_make_gf);
    define_generic_function("add-method", 
			    list2(obj_GFClass, obj_MethodClass), 
			    FALSE, obj_False, FALSE,
			    list2(obj_MethodClass,obj_ObjectClass), obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("add-method"),
			     FALSE, FALSE)->value,
	       make_raw_method("add-method",list2(obj_GFClass,obj_MethodClass),
			       FALSE, obj_False, FALSE,
			       list2(obj_MethodClass, obj_ObjectClass),
			       obj_False, dylan_add_method));
    define_method("generic-function-methods", list1(obj_GFClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, generic_function_methods);
    define_method("generic-function-mandatory-keywords", list1(obj_GFClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass,
		  function_keywords);
    define_method("function-specializers", list1(obj_FunctionClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, function_specializers);
    define_generic_function("function-arguments", list1(obj_FunctionClass), 
			    FALSE, obj_False, FALSE,
			    list3(obj_FixnumClass, obj_BooleanClass,
				  obj_ObjectClass),
			    obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("function-arguments"),
			     FALSE, FALSE)->value,
	       make_raw_method("function-arguments", list1(obj_FunctionClass),
			       FALSE, obj_False, FALSE,
			       list3(obj_FixnumClass, obj_BooleanClass,
				     obj_ObjectClass),
			       obj_False, dylan_function_arguments));
    add_method(find_variable(module_BuiltinStuff, symbol("function-arguments"),
			     FALSE, FALSE)->value,
	       make_raw_method("function-arguments", list1(obj_MethodClass),
			       FALSE, obj_False, FALSE,
			       list3(obj_FixnumClass, obj_BooleanClass,
				     obj_ObjectClass),
			       obj_False, dylan_method_arguments));
    define_generic_function("function-return-values",
			    list1(obj_FunctionClass), 
			    FALSE, obj_False, FALSE,
			    list2(obj_SeqClass, type_or_singleton_false),
			    obj_False);
    add_method(find_variable(module_BuiltinStuff, 
			     symbol("function-return-values"),
			     FALSE, FALSE)->value,
	       make_raw_method("function-return-values", 
			       list1(obj_FunctionClass),
			       FALSE, obj_False, FALSE,
			       list2(obj_SeqClass, type_or_singleton_false),
			       obj_False, dylan_function_return_values));
    define_method("sorted-applicable-methods", list1(obj_GFClass), TRUE,
		  obj_False, FALSE, obj_ObjectClass, dylan_sorted_app_meths);
    define_method("applicable-method?", list1(obj_MethodClass), TRUE,
		  obj_False, FALSE, obj_BooleanClass, dylan_app_meth_p);
    define_method("find-method", list2(obj_GFClass, obj_ListClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_find_method);
    define_method("remove-method", list2(obj_GFClass, obj_MethodClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_remove_method);
    define_constant("do-next-method",
		    make_raw_method("do-next-method",
				    list2(obj_ObjectClass, obj_ObjectClass),
				    FALSE, obj_False, FALSE, obj_Nil,
				    obj_ObjectClass, dylan_do_next_method));
    define_method("constrain-c-function",
		  listn(4, obj_CFunctionClass, obj_ListClass, 
			obj_ObjectClass, obj_ListClass),
		  TRUE, obj_False, FALSE, obj_ObjectClass,
		  constrain_c_function);
}
