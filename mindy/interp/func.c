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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/func.c,v 1.12 1994/04/12 16:01:02 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

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
#include "func.h"

obj_t obj_FunctionClass = NULL;
static obj_t obj_RawFunctionClass = NULL;
obj_t obj_MethodClass = NULL;
obj_t obj_ByteMethodClass = NULL;
static obj_t obj_RawMethodClass;
static obj_t obj_BuiltinMethodClass = NULL;
static obj_t obj_AccessorMethodClass = NULL;
static obj_t obj_GFClass = NULL;
static obj_t obj_MethodInfoClass = NULL;
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
    printf("< 0x%08x: ", (unsigned long)old_sp);
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
    obj_t cached_result;
    int size;
    obj_t cached_classes[0];
};

obj_t make_gf_cache(int req_args, obj_t cached_result)
{
    obj_t res = alloc(obj_GFCacheClass, (sizeof(struct gf_cache) +
					 req_args * sizeof(obj_t)));
    struct gf_cache *gfc = obj_ptr(struct gf_cache *, res);
    int i;

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
    obj_t result_types;
    obj_t more_results_type;
};

#define FUNC(o) obj_ptr(struct function *, o)

obj_t make_raw_function(char *debug_name, int required_args,
			boolean restp, obj_t keywords,
			obj_t result_types, obj_t more_results_type,
			void xep(struct thread *thread, int nargs))
{
    obj_t res = alloc(obj_RawFunctionClass, sizeof(struct function));

    FUNC(res)->xep = xep;
    FUNC(res)->debug_name = symbol(debug_name);
    FUNC(res)->required_args = required_args;
    FUNC(res)->restp = restp;
    FUNC(res)->keywords = keywords;
    FUNC(res)->result_types = result_types;
    FUNC(res)->more_results_type = more_results_type;

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
	error("Too few arguments for ~S: expected ~S, got ~S",
	      function_debug_name_or_self(function),
	      make_fixnum(required),
	      make_fixnum(nargs));
    }
    
    if (!FUNC(function)->restp && FUNC(function)->keywords == obj_False
	  && nargs > required) {
	push_linkage(thread, thread->sp - nargs);
	error("Too many arguments for ~S: expected ~S, got ~S",
	      function_debug_name_or_self(function),
	      make_fixnum(required),
	      make_fixnum(nargs));
    }

    FUNC(function)->xep(thread, nargs);
#ifndef sparc
    go_on();
#endif
}

obj_t *push_linkage(struct thread *thread, obj_t *args)
{
    obj_t *fp = thread->sp += 4;

    fp[-4] = rawptr_obj(thread->fp);
    fp[-3] = rawptr_obj(args-1);
    fp[-2] = thread->component;
    fp[-1] = make_fixnum(thread->pc);
    thread->fp = fp;
    thread->component = rawptr_obj(NULL);
    thread->pc = 0;

    return fp;
}

obj_t *pop_linkage(struct thread *thread)
{
    obj_t *fp = thread->fp;

    thread->fp = obj_rawptr(fp[-4]);
    thread->component = fp[-2];
    thread->pc = fixnum_value(fp[-1]);

    return obj_rawptr(fp[-3]);
}

void set_c_continuation(struct thread *thread,
			void cont(struct thread *thread, obj_t *vals))
{
    thread->component = rawptr_obj(cont);
    thread->pc = 0;
}

void do_return_setup(struct thread *thread, obj_t *old_sp, obj_t *vals)
{
    if (Tracing)
	trace_return(old_sp, vals, thread->sp - vals);

    if (thread->pc)
	do_byte_return(thread, old_sp, vals);
    else {
	void (*cont)(struct thread *thread, obj_t *vals)
	    = (void (*)(struct thread *thread, obj_t *vals))
		obj_rawptr(thread->component);
	if (cont) {
	    thread->component = rawptr_obj(NULL);
	    if (old_sp != vals) {
		obj_t *src = vals, *dst = old_sp, *end = thread->sp;
		while (src < end)
		    *dst++ = *src++;
		thread->sp = dst;
	    }
	    (*cont)(thread, old_sp);
	}
	else
	    lose("Attempt to return, but no continuation established.\n");
    }
}

#ifndef sparc
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
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
};

#define METHOD(o) obj_ptr(struct method *, o)

static obj_t *push_keywords(obj_t *sp, obj_t keywords, obj_t *args, int nargs)
{
    /* ### Should also check to make sure the supplied keywords are okay. */
    /* But doing so requires us to know if we were invoked via a generic */
    /* function or directly.  Also, I'm not sure just what keyword checking */
    /* should be done when we are invoked via a generic function. */

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
	error("It is ambiguous which of these methods to invoke:~%  ~S",
	      next_methods);
    }
    else
	really_invoke_methods(method, next_methods, thread, nargs);
}

/* Version of applicable_method_p which does extra work to allow SAM caching 
   for generic function dispatch.  The "can_cache" argument is carried across
   several calls to gfd_applicable_method_p and will be set to false if the 
   method depends upon anything other than the types of the args. */
static boolean
    gfd_applicable_method_p(obj_t method, obj_t *args, boolean *can_cache)
{
    obj_t specializers = METHOD(method)->specializers;
    boolean l_can_cache = *can_cache;
    boolean all_class_only = l_can_cache;
    boolean any_class_only = FALSE;
    boolean applicable = TRUE;

    while (specializers != obj_Nil) {
	obj_t arg = *args++;
	obj_t arg_class = object_class(arg);
	obj_t specializer = HEAD(specializers);

	if (!subtypep(arg_class, specializer))
	    if (instancep(arg, specializer))
		all_class_only = FALSE;
	    else {
		if (!any_class_only && l_can_cache)
		    any_class_only = !overlapp(arg_class, specializer);
		if (!l_can_cache || any_class_only) {
		    /* no further info can be gained, so return */
		    *can_cache = l_can_cache && any_class_only;
		    return FALSE;
		}
		applicable = FALSE;
	    }
	specializers = TAIL(specializers);
    }
    if (applicable)
	*can_cache = all_class_only;
    else
	*can_cache = l_can_cache && any_class_only;
    return applicable;
}

static boolean applicable_method_p(obj_t method, obj_t *args)
{
    obj_t cache = METHOD(method)->class_cache;
    int max = METHOD(method)->required_args;
    boolean can_cache = TRUE;

    if (cache != obj_False) {
	obj_t *cache_class = obj_ptr(struct gf_cache *, cache)->cached_classes;
	obj_t *arg = args;
	int i;
	boolean found = TRUE;

	for (i = 0; i < max; i++, arg++, cache_class++) {
	    if (*cache_class != object_class(*arg)) {
		found = FALSE;
		break;
	    }
	}
	if (found)
	    return TRUE;
    }

    /* It wasn't in the cache.... */
    if (gfd_applicable_method_p(method, args, &can_cache)) {
	if (can_cache) {
	    int i;
	    obj_t cache_elem =
		(cache == obj_False) ? make_gf_cache(max, obj_False) : cache;
	    obj_t *cache_class = obj_ptr(struct gf_cache *,
					 cache_elem)->cached_classes;
	    obj_t *arg = args;
	    
	    for (i = 0; i < max; i++, arg++, cache_class++)
		*cache_class = object_class(*arg);
	    METHOD(method)->class_cache = cache_elem;
	}
	return TRUE;
    } else
	return FALSE;
}

static boolean method_accepts_keyword(obj_t method, obj_t keyword)
{
    obj_t keywords;

    if (METHOD(method)->restp)
	return TRUE;

    keywords = METHOD(method)->keywords;
    if (keywords == obj_False)
	return FALSE;

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
	if (METHOD(method)->keywords != obj_False && !METHOD(method)->restp) {
	    obj_t *ptr = args+METHOD(method)->required_args;
	    while (ptr < thread->sp) {
		if (!method_accepts_keyword(method, *ptr)) {
		    push_linkage(thread, args);
		    error("Method ~S does not accept the keyword ~S",
			  function_debug_name_or_self(method), *ptr);
		}
		ptr += 2;
	    }
	}
	invoke_methods(method, obj_Nil, thread, nargs);
    }
    else {
	push_linkage(thread, args);
	error("Method ~S is not applicable when given the arguments ~S",
	      function_debug_name_or_self(method),
	      make_vector(nargs, args));
    }
}

obj_t make_raw_method(char *debug_name, obj_t specializers, boolean restp,
		      obj_t keywords, obj_t result_types,
		      obj_t more_results_type,
		      void iep(obj_t self, struct thread *thread, obj_t *args))
{
    obj_t res = alloc(obj_RawMethodClass, sizeof(struct method));

    METHOD(res)->xep = method_xep;
    METHOD(res)->debug_name = symbol(debug_name);
    METHOD(res)->required_args = length(specializers);
    METHOD(res)->restp = restp;
    METHOD(res)->keywords = keywords;
    METHOD(res)->result_types = result_types;
    METHOD(res)->more_results_type = more_results_type;
    METHOD(res)->specializers = specializers;
    METHOD(res)->class_cache = obj_False;
    METHOD(res)->iep = iep;

    return res;
}

void set_method_iep(obj_t method, 
		    void iep(obj_t self, struct thread *thread, obj_t *args))
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
			  boolean restp, obj_t keywords, obj_t result_type,
			  obj_t (*func)())
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
    obj_t result_types;
    obj_t more_results_type;
    obj_t specializers;
    obj_t class_cache;			/* #F or a gf_cache */
    void (*iep)(obj_t self, struct thread *thread, obj_t *args);
    obj_t component;
    int n_closure_vars;
    obj_t lexenv[0];
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
#ifndef sparc
    go_on();
#endif
}

obj_t make_method_info(boolean restp, obj_t keys, obj_t component,
		       int n_closure_vars)
{
    obj_t res = alloc(obj_MethodInfoClass, sizeof(struct method_info));

    METHOD_INFO(res)->restp = restp;
    METHOD_INFO(res)->keys = keys;
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
		      sizeof(struct byte_method)+sizeof(obj_t)*n_closure_vars);
    obj_t component = METHOD_INFO(method_info)->component;
    int i;

    BYTE_METHOD(res)->xep = method_xep;
    BYTE_METHOD(res)->debug_name = obj_ptr(struct component *, component)->debug_name;
    BYTE_METHOD(res)->required_args = length(specializers);
    BYTE_METHOD(res)->restp = METHOD_INFO(method_info)->restp;
    BYTE_METHOD(res)->keywords = METHOD_INFO(method_info)->keys;
    BYTE_METHOD(res)->result_types = result_types;
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
			   void iep(obj_t self, struct thread *thread,
				    obj_t *args))
{
    obj_t res = alloc(obj_AccessorMethodClass, sizeof(struct accessor_method));

    ACCESSOR_METHOD(res)->xep = method_xep;
    ACCESSOR_METHOD(res)->debug_name = debug_name;
    ACCESSOR_METHOD(res)->required_args = setter ? 2 : 1;
    ACCESSOR_METHOD(res)->restp = FALSE;
    ACCESSOR_METHOD(res)->keywords = obj_False;
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


/* Generic functions. */

struct gf {
    obj_t class;
    void (*xep)(struct thread *thread, int nargs);
    obj_t debug_name;
    int required_args;
    boolean restp;
    obj_t keywords;
    obj_t result_types;
    obj_t more_results_type;
    obj_t methods;
    obj_t cache;
};

#define GF(o) obj_ptr(struct gf *, o)

static obj_t
    slow_sorted_applicable_methods(struct gf *gf, obj_t methods, obj_t *args)
{
    obj_t ordered = obj_Nil;
    obj_t ambiguous = obj_Nil;
    obj_t scan, *prev;
    boolean can_cache = TRUE;

    while (methods != obj_Nil) {
	obj_t method = HEAD(methods);

	if (gfd_applicable_method_p(method, args, &can_cache)) {
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

    if (can_cache) {
	int i, max = gf->required_args;
	obj_t cache_elem = make_gf_cache(max, ordered);
	obj_t *cache = obj_ptr(struct gf_cache *, cache_elem)->cached_classes;
	obj_t *arg = args;

	for (i = 0; i < max; i++, arg++, cache++)
	    *cache = object_class(*arg);
	gf->cache = pair(cache_elem, gf->cache);
    }
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
	obj_t *cache_class = cache_elem->cached_classes;
	obj_t *arg = args;
	int i;
	boolean found = TRUE;

	for (i = 0; i < max; i++, arg++, cache_class++) {
	    if (*cache_class != object_class(*arg)) {
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
	if (GF(gf)->keywords != obj_False) {
	    obj_t *ptr = args + GF(gf)->required_args;
	    while (ptr < thread->sp) {
		if (!methods_accept_keyword(methods, *ptr)) {
		    push_linkage(thread, args);
		    error("The keyword ~S is accepted by none of the "
			  "applicable methods:~%  ~S",
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
	error("No applicable methods for ~S with arguments ~S",
	      function_debug_name_or_self(gf),
	      make_vector(nargs, args));
    }
}

obj_t make_generic_function(obj_t debug_name, int req_args, 
			    boolean restp, obj_t keywords,
			    obj_t result_types, obj_t more_results_type)
{
    obj_t res = alloc(obj_GFClass, sizeof(struct gf));

    GF(res)->xep = gf_xep;
    GF(res)->debug_name = debug_name;
    GF(res)->required_args = req_args;
    GF(res)->restp = restp;
    GF(res)->keywords = keywords;
    GF(res)->result_types = result_types;
    GF(res)->more_results_type = more_results_type;
    GF(res)->methods = obj_Nil;
    GF(res)->cache = obj_Nil;

    return res;
}

obj_t make_default_generic_function(obj_t debug_name, obj_t method)
{
    int reqargs = METHOD(method)->required_args;
    boolean restp = METHOD(method)->restp;
    obj_t keywords = METHOD(method)->keywords;

    return make_generic_function(debug_name, reqargs,
				 restp || keywords != obj_False,
				 obj_False, obj_Nil, obj_ObjectClass);
}

void set_gf_signature(obj_t gf, int req_args, boolean restp, obj_t keys,
		      obj_t result_types, obj_t more_results_type)
{
    obj_t methods = GF(gf)->methods;

    GF(gf)->required_args = req_args;
    GF(gf)->restp = restp;
    GF(gf)->keywords = keys;
    GF(gf)->result_types = result_types;
    GF(gf)->more_results_type = more_results_type;
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

static obj_t really_add_method(obj_t gf, obj_t method)
{
    obj_t methods = GF(gf)->methods;
    obj_t specializers = METHOD(method)->specializers;
    obj_t scan;

    GF(gf)->cache = obj_Nil;

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

    if (GF(gf)->required_args != METHOD(method)->required_args)
	error("The method ~S has ~S required arguments, but the generic "
	      "function ~S has ~S",
	      function_debug_name_or_self(method),
	      make_fixnum(METHOD(method)->required_args),
	      function_debug_name_or_self(gf),
	      GF(gf)->required_args);

    gfkeys = GF(gf)->keywords;
    if (gfkeys != obj_False) {
	/* The generic function takes keyword arguments. */
	boolean restp = METHOD(method)->restp;
	if (!restp) {
	    obj_t methkeys = METHOD(method)->keywords;
	    if (methkeys != obj_False)
		error("The generic function ~S allows keyword arguments, "
		      "but the method ~S does not.",
		      function_debug_name_or_self(gf),
		      function_debug_name_or_self(method));
	    /* ### Verify that gfkeys is a subset of methkeys. */
	}
    }
    else if (GF(gf)->restp) {
	/* The generic function has a #rest argument. */
	if (!METHOD(method)->restp && METHOD(method)->keywords == obj_False)
	    /* But the method does not. */
	    error("Generic function ~S allows a variable number of arguments, "
		  "but method ~S does not.",
		  function_debug_name_or_self(gf),
		  function_debug_name_or_self(method));
    }
    else if (METHOD(method)->restp || METHOD(method)->keywords != obj_False)
	error("Method ~S allows a variable number of arguments, but generic "
	      "function ~S does not.",
	      function_debug_name_or_self(method),
	      function_debug_name_or_self(gf));

    /* ### Should also check the result types. */

    return really_add_method(gf, method);
}


/* Dylan interface functions. */

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

static obj_t method_specializers(obj_t method)
{
    return METHOD(method)->specializers;
}

static void dylan_function_arguments(obj_t self, struct thread *thread,
				     obj_t *args)
{
    obj_t *vals = args-1;
    obj_t func = *args;

    thread->sp = vals + 3;
    vals[0] = make_fixnum(FUNC(func)->required_args);
    vals[1] = FUNC(func)->restp ? obj_True : obj_False;
    vals[2] = FUNC(func)->keywords;

    do_return(thread, vals, vals);
}

static void dylan_method_arguments(obj_t self, struct thread *thread,
				   obj_t *args)
{
    obj_t *vals = args-1;
    obj_t meth = *args;
    obj_t scan;

    thread->sp = vals + 3;
    vals[0] = make_fixnum(METHOD(meth)->required_args);
    vals[1] = METHOD(meth)->restp ? obj_True : obj_False;
    scan = METHOD(meth)->keywords;
    if (scan != obj_False) {
	obj_t keys = obj_Nil;
	while (scan != obj_Nil) {
	    keys = pair(HEAD(HEAD(scan)), keys);
	    scan = TAIL(scan);
	}
	vals[2] = keys;
    }
    else
	vals[2] = obj_False;

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

    prev = &GF(gf)->methods;
    while ((scan = *prev) != obj_Nil) {
	if (method == HEAD(scan)) {
	    *prev = TAIL(scan);
	    return method;
	}
	prev = &TAIL(scan);
    }
    error("~S isn't one of the methods in ~S", method, gf);
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


/* GC stuff. */

static void scav_func(struct function *func)
{
    scavenge(&func->debug_name);
    scavenge(&func->keywords);
    scavenge(&func->result_types);
    scavenge(&func->more_results_type);
}

static int scav_raw_func(struct object *ptr)
{
    scav_func((struct function *)ptr);

    return sizeof(struct function);
}

static obj_t trans_raw_func(obj_t func)
{
    return transport(func, sizeof(struct function));
}

static int scav_raw_method(struct object *ptr)
{
    scav_func((struct function *)ptr);
    scavenge(&((struct method *)ptr)->specializers);
    scavenge(&((struct method *)ptr)->class_cache);

    return sizeof(struct method);
}
    
static obj_t trans_raw_method(obj_t method)
{
    return transport(method, sizeof(struct method));
}

static int scav_builtin_method(struct object *ptr)
{
    scav_func((struct function *)ptr);
    scavenge(&((struct builtin_method *)ptr)->specializers);
    scavenge(&((struct builtin_method *)ptr)->class_cache);

    return sizeof(struct builtin_method);
}
    
static obj_t trans_builtin_method(obj_t method)
{
    return transport(method, sizeof(struct builtin_method));
}

static int scav_byte_method(struct object *ptr)
{
    struct byte_method *method = (struct byte_method *)ptr;
    int i;

    scav_func((struct function *)ptr);
    scavenge(&method->specializers);
    scavenge(&method->class_cache);
    scavenge(&method->component);

    for (i = 0; i < method->n_closure_vars; i++)
	scavenge(method->lexenv + i);

    return sizeof(struct byte_method) + sizeof(obj_t)*method->n_closure_vars;
}

static obj_t trans_byte_method(obj_t method)
{
    int nvars = BYTE_METHOD(method)->n_closure_vars;

    return transport(method, sizeof(struct byte_method) + sizeof(obj_t)*nvars);
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
    return transport(info, sizeof(struct method_info));
}

static int scav_accessor_method(struct object *ptr)
{
    struct accessor_method *method = (struct accessor_method *)ptr;

    scav_func((struct function *)ptr);
    scavenge(&method->specializers);
    scavenge(&method->class_cache);
    scavenge(&method->datum);

    return sizeof(struct accessor_method);
}
    
static obj_t trans_accessor_method(obj_t method)
{
    return transport(method, sizeof(struct accessor_method));
}

static int scav_gf(struct object *ptr)
{
    struct gf *gf = (struct gf *)ptr;

    scav_func((struct function *)gf);
    scavenge(&gf->methods);
    scavenge(&gf->cache);

    return sizeof(struct gf);
}

static obj_t trans_gf(obj_t gf)
{
    return transport(gf, sizeof(struct gf));
}

static int scav_gf_cache(struct object *ptr)
{
    struct gf_cache *gf_cache = (struct gf_cache *)ptr;
    int i, max = gf_cache->size;

    scavenge(&gf_cache->cached_result);
    for (i = 0; i < max; i++)
	scavenge(&gf_cache->cached_classes[i]);

    return sizeof(struct gf_cache) + max * sizeof(obj_t);
}

static obj_t trans_gf_cache(obj_t gf_cache)
{
    return transport(gf_cache, (sizeof(struct gf_cache) +
				obj_ptr(struct gf_cache *, gf_cache)->size
				* sizeof(obj_t)));
}

void scavenge_func_roots(void)
{
    scavenge(&obj_FunctionClass);
    scavenge(&obj_MethodClass);
    scavenge(&obj_RawMethodClass);
    scavenge(&obj_BuiltinMethodClass);
    scavenge(&obj_ByteMethodClass);
    scavenge(&obj_MethodInfoClass);
    scavenge(&obj_GFClass);
    scavenge(&obj_GFCacheClass);
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
    obj_MethodInfoClass
	= make_builtin_class(scav_method_info, trans_method_info);
    obj_GFClass = make_builtin_class(scav_gf, trans_gf);
    obj_GFCacheClass = make_builtin_class(scav_gf_cache, trans_gf_cache);
}

void init_func_classes(void)
{
    init_builtin_class(obj_FunctionClass, "<function>", obj_ObjectClass, NULL);
    def_printer(obj_FunctionClass, print_func);
    init_builtin_class(obj_RawFunctionClass, "<builtin-function>",
		       obj_FunctionClass, NULL);
    init_builtin_class(obj_MethodClass, "<method>", obj_FunctionClass, NULL);
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
    init_builtin_class(obj_GFClass, "<generic-function>",
		       obj_FunctionClass, NULL);
    init_builtin_class(obj_GFCacheClass, "<generic-function-cache>",
		       obj_ObjectClass, NULL);
}

void init_func_functions(void)
{
    define_generic_function("add-method", 2, FALSE, obj_False,
			    list2(obj_MethodClass,obj_ObjectClass), obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("add-method"),
			     FALSE, FALSE)->value,
	       make_raw_method("add-method",list2(obj_GFClass,obj_MethodClass),
			       FALSE, obj_False,
			       list2(obj_MethodClass, obj_ObjectClass),
			       obj_False, dylan_add_method));
    define_method("generic-function-methods", list1(obj_GFClass),
		  FALSE, obj_False, obj_ObjectClass, generic_function_methods);
    define_method("method-specializers", list1(obj_MethodClass),
		  FALSE, obj_False, obj_ObjectClass, method_specializers);
    define_generic_function("function-arguments", 1, FALSE, obj_False,
			    list3(obj_IntegerClass, obj_BooleanClass,
				  obj_ObjectClass),
			    obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("function-arguments"),
			     FALSE, FALSE)->value,
	       make_raw_method("function-arguments", list1(obj_FunctionClass),
			       FALSE, obj_False,
			       list3(obj_IntegerClass, obj_BooleanClass,
				     obj_ObjectClass),
			       obj_False, dylan_function_arguments));
    add_method(find_variable(module_BuiltinStuff, symbol("function-arguments"),
			     FALSE, FALSE)->value,
	       make_raw_method("function-arguments", list1(obj_MethodClass),
			       FALSE, obj_False,
			       list3(obj_IntegerClass, obj_BooleanClass,
				     obj_ObjectClass),
			       obj_False, dylan_method_arguments));
    define_method("sorted-applicable-methods", list1(obj_GFClass),
		  TRUE, obj_False, obj_ObjectClass, dylan_sorted_app_meths);
    define_method("applicable-method?", list1(obj_MethodClass),
		  TRUE, obj_False, obj_BooleanClass, dylan_app_meth_p);
    define_method("find-method", list2(obj_GFClass, obj_ListClass),
		  FALSE, obj_False, obj_ObjectClass, dylan_find_method);
    define_method("remove-method", list2(obj_GFClass, obj_MethodClass),
		  FALSE, obj_False, obj_ObjectClass, dylan_remove_method);
    define_constant("do-next-method",
		    make_raw_method("do-next-method",
				    list2(obj_ObjectClass, obj_ObjectClass),
				    FALSE, obj_False, obj_Nil, obj_ObjectClass,
				    dylan_do_next_method));
}
