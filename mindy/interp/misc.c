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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/misc.c,v 1.1 1994/03/24 21:49:43 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "thread.h"
#include "interp.h"
#include "bool.h"
#include "list.h"
#include "vec.h"
#include "func.h"
#include "obj.h"
#include "module.h"
#include "sym.h"

static struct variable *generic_apply_var = NULL;


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


/* Invoking the debugger. */

static void dylan_invoke_debugger(struct thread *thread, int nargs)
{
    obj_t *args;

    assert(nargs == 1);

    args = thread->sp - 1;
    push_linkage(thread, args);

    thread_debuggered(thread, args[0]);
}


/* Init stuff. */

void init_misc_functions(void)
{
    define_generic_function("main", 0, TRUE, obj_False,
			    obj_Nil, obj_ObjectClass);
    define_constant("invoke-debugger",
		    make_raw_function("invoke-debugger", 1, FALSE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      dylan_invoke_debugger));
    define_constant("values",
		    make_raw_function("values", 0, TRUE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      dylan_values));
    define_constant("apply",
		    make_raw_function("apply", 2, TRUE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      dylan_apply));
    generic_apply_var = find_variable(module_BuiltinStuff,
				      symbol("generic-apply"),
				      FALSE, TRUE);
}
