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
* $Header: /scm/cvs/src/mindy/interp/misc.c,v 1.1 1998/05/03 19:55:16 andreas Exp $
*
* This file implements the stuff we couldn't think of anyplace
* better to put.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

#include "mindy.h"
#include "thread.h"
#include "bool.h"
#include "list.h"
#include "vec.h"
#include "func.h"
#include "obj.h"
#include "module.h"
#include "sym.h"
#include "def.h"
#include "num.h"
#include "error.h"
#include "str.h"
#include "coll.h"

static struct variable *generic_apply_var = NULL;


static obj_t dylan_exit(obj_t exit_value)
{
    exit(fixnum_value(exit_value));
}

static void dylan_values(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    do_return(thread, args-1, args);
}

static void dylan_apply(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t *old_sp = args-1;
    obj_t *src = args;
    obj_t *dst = old_sp;
    obj_t *end = thread->sp - 1;
    obj_t seq = *end;
    obj_t class = object_class(seq);
    boolean vector;

    if (!(vector = (class == obj_SimpleObjectVectorClass))
	&& class != obj_EmptyListClass && class != obj_PairClass) {
	/* It isn't a simple-object-vector nor a list, we have to defer. */
	*dst++ = generic_apply_var->value;
	while (src < end)
	    *dst++ = *src++;
	*dst++ = *src;
    }
    else {
	/* Make sure the function is a function. */
	check_type(*src, obj_FunctionClass);

	/* Copy the function and the first n-1 args down the stack. */
	while (src < end)
	    *dst++ = *src++;

	/* Spread the collection out on the stack. */
	if (vector) {
	    src = obj_ptr(struct sovec *, seq)->contents;
	    end = src + obj_ptr(struct sovec *, seq)->length;
	    while (src < end)
		*dst++ = *src++;
	}
	else {
	    while (seq != obj_Nil) {
		*dst++ = HEAD(seq);
		seq = TAIL(seq);
	    }
	}
    }
    thread->sp = dst;
    invoke(thread, dst - args);
}

static void dylan_apply_curry(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 3;
    obj_t func = args[0];
    obj_t vec1 = args[1];
    obj_t vec2 = args[2];
    int len1 = SOVEC(vec1)->length;
    int len2 = SOVEC(vec2)->length;
    int i;

    assert(nargs == 3);

    args[-1] = func;

    for (i = 0; i < len1; i++)
	*args++ = SOVEC(vec1)->contents[i];
    for (i = 0; i < len2; i++)
	*args++ = SOVEC(vec2)->contents[i];

    thread->sp = args;

    invoke(thread, len1+len2);
}

/* Invoking the debugger. */
static void dylan_invoke_debugger(struct thread *thread, int nargs)
{
    obj_t *args;

    assert(nargs == 1);

    args = thread->sp - 1;
    push_linkage(thread, args);

    thread_debuggered(thread, args[0]);
}

static obj_t dylan_get_time_of_day (void)
{
    return make_bignum(time(NULL));
}

static obj_t dylan_system(obj_t command)
{
    return make_fixnum(system(string_chars(command)));
}

static obj_t dylan_getenv(obj_t name)
{
    char *res = getenv(string_chars(name));

    if (res)
	return make_byte_string(res);
    else
	return obj_False;
}

static obj_t dylan_getcwd()
{
    char buf[MAXPATHLEN + 1];

    if (getcwd(buf, sizeof(buf)) == NULL)
	error("Can't get the current directory: %s",
	      make_byte_string(strerror(errno)));

    return make_byte_string(buf);
}	


/* Init stuff. */

void init_misc_functions(void)
{
#if ! NO_ARGV_0
    define_generic_function("main", list1(obj_ObjectClass),
			    TRUE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
#else
    define_generic_function("main", obj_NIL, TRUE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
#endif
    define_function("raw-exit", list1(obj_FixnumClass), FALSE, obj_False,
		    FALSE, obj_ObjectClass, dylan_exit);
    define_function("get-time-of-day", obj_Nil, FALSE, 
		    obj_False, FALSE, obj_BignumClass, dylan_get_time_of_day);
    define_function("system", list1(obj_ByteStringClass), FALSE, obj_False,
		    FALSE, obj_FixnumClass, dylan_system);
    define_function("getenv", list1(obj_ByteStringClass), FALSE, obj_False,
		    FALSE, obj_ObjectClass, dylan_getenv);
    define_function("getcwd", obj_Nil, FALSE, obj_False,
		    FALSE, obj_ByteStringClass, dylan_getcwd);
    define_constant("invoke-debugger",
		    make_raw_function("invoke-debugger", 
				      list1(obj_ObjectClass),
				      FALSE, obj_False,
				      FALSE, obj_Nil, obj_ObjectClass,
				      dylan_invoke_debugger));
    define_constant("values",
		    make_raw_function("values", obj_Nil,
				      TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      dylan_values));
    define_constant("apply",
		    make_raw_function("apply", 
				      list2(obj_FunctionClass, 
					    obj_ObjectClass), 
				      TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      dylan_apply));
    generic_apply_var = find_variable(module_BuiltinStuff,
				      symbol("generic-apply"),
				      FALSE, TRUE);
    define_constant("apply-curry",
		    make_raw_function("apply-curry", 
				      list3(obj_FunctionClass, obj_SeqClass, 
					    obj_SeqClass), 
				      FALSE, obj_False,
				      FALSE, obj_Nil, obj_ObjectClass,
				      dylan_apply_curry));
}
