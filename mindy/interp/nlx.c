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
* $Header: /scm/cvs/src/mindy/interp/nlx.c,v 1.1 1998/05/03 19:55:16 andreas Exp $
*
* This file implements non-local exit.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "func.h"
#include "list.h"
#include "bool.h"
#include "obj.h"
#include "sym.h"
#include "class.h"
#include "error.h"
#include "def.h"

static obj_t obj_CatchBlockClass = 0;


/* Unwind-protect. */

struct uwp {
    obj_t prev_uwp;
    obj_t handlers;
    obj_t cleanup;
};

static void done_with_cleanup(struct thread *thread, obj_t *cleanup_vals)
{
    obj_t *old_sp, *vals;

    /* Get the pointer to the values from the protected form from the stack.
       We pushed it just before calling the cleanup, so it just below the
       cleanup values. */
    vals = obj_rawptr(cleanup_vals[-1]);

    /* Reset the stack to just after the protected form values. */
    thread->sp = cleanup_vals - 1;

    /* Return those values. */
    old_sp = pop_linkage(thread);
    do_return(thread, old_sp, vals);
}

static void do_uwp_cleanup(struct thread *thread, obj_t *vals)
{
    struct uwp *cur_uwp = thread->cur_uwp;

    /* Unlink the unwind protect */
    thread->cur_uwp = obj_rawptr(cur_uwp->prev_uwp);

    /* push a pointer to the values. */
    *thread->sp++ = rawptr_obj(vals);

    /* call the cleanup function. */
    *thread->sp++ = cur_uwp->cleanup;
    set_c_continuation(thread, done_with_cleanup);
    invoke(thread, 0);
}

static void uwp(struct thread *thread, int nargs)
{
    obj_t *args, *fp;
    struct uwp *cur_uwp;

    assert(nargs == 2);

    args = thread->sp - 2;
    fp = push_linkage(thread, args);
    cur_uwp = (struct uwp *)fp;

    /* link in the new unwind protect */
    thread->sp = fp + sizeof(struct uwp)/sizeof(obj_t);
    cur_uwp->prev_uwp = rawptr_obj(thread->cur_uwp);
    thread->cur_uwp = cur_uwp;
    cur_uwp->handlers = thread->handlers;
    cur_uwp->cleanup = args[1];

    /* call the protected form. */
    *thread->sp++ = args[0];
    set_c_continuation(thread, do_uwp_cleanup);
    invoke(thread, 0);
}


/* Catch */

struct catch_block {
    obj_t class;
    struct thread *thread;
    struct uwp *cur_uwp;
    obj_t prev_catch;
    obj_t *fp;
    obj_t handlers;
};

static void unlink_catch(struct thread *thread, obj_t *vals)
{
    obj_t catch_block;
    obj_t *old_sp;

    /* Unlink and invalidate the current catch block. */
    catch_block = thread->cur_catch;
    thread->cur_catch = obj_ptr(struct catch_block *, catch_block)->prev_catch;
    obj_ptr(struct catch_block *, catch_block)->thread = NULL;
    obj_ptr(struct catch_block *, catch_block)->prev_catch = obj_False;

    /* Return the same values we got. */
    old_sp = pop_linkage(thread);
    do_return(thread, old_sp, vals);
}

static void catch(struct thread *thread, int nargs)
{
    obj_t *args, *fp;
    obj_t catch_block;

    assert(nargs == 1);

    args = thread->sp - 1;
    fp = push_linkage(thread, args);

    catch_block = alloc(obj_CatchBlockClass, sizeof(struct catch_block));
    obj_ptr(struct catch_block *, catch_block)->thread = thread;
    obj_ptr(struct catch_block *, catch_block)->cur_uwp = thread->cur_uwp;
    obj_ptr(struct catch_block *, catch_block)->prev_catch = thread->cur_catch;
    obj_ptr(struct catch_block *, catch_block)->fp = fp;
    obj_ptr(struct catch_block *, catch_block)->handlers = thread->handlers;
    thread->cur_catch = catch_block;

    thread->sp = fp + 2;
    fp[0] = args[0];
    fp[1] = catch_block;
    set_c_continuation(thread, unlink_catch);
    invoke(thread, 1);
}


/* Throw */

static void unwind(struct thread *thread, obj_t catch_block, obj_t *vals);

static void throw(struct thread *thread, int nargs)
{
    obj_t *args;
    obj_t catch_block;

    assert (nargs > 0);

    args = thread->sp - nargs;
    catch_block = args[0];
    if (obj_ptr(struct catch_block *, catch_block)->thread != thread) {
	push_linkage(thread, args);
	error("Bogus throw");
    }

    unwind(thread, args[0], args+1);
}

static void continue_unwind(struct thread *thread, obj_t *cleanup_vals)
{
    obj_t catch_block = cleanup_vals[-1];
    obj_t *vals = thread->fp;

    thread->sp = cleanup_vals - 1;

    unwind(thread, catch_block, vals);
}

static void unwind(struct thread *thread, obj_t catch_block, obj_t *vals)
{
    struct uwp *cur_uwp;
    obj_t cur_catch;

    cur_uwp = thread->cur_uwp;

    do {
	cur_catch = thread->cur_catch;
	if (cur_uwp != obj_ptr(struct catch_block *, cur_catch)->cur_uwp) {
	    obj_t cleanup = cur_uwp->cleanup;
	    obj_t *dst = (obj_t *)cur_uwp;
	    obj_t *src = vals;
	    obj_t *end = thread->sp;

	    /* Set the frame pointer to where the values will show up. */
	    thread->fp = dst;

	    /* Restore the handlers, and unlink the uwp. */
	    thread->handlers = cur_uwp->handlers;
	    thread->cur_uwp = obj_rawptr(cur_uwp->prev_uwp);

	    /* Copy the values down the stack, and save the catch block */
	    while (src < end)
		*dst++ = *src++;
	    thread->sp = dst+2;
	    dst[0] = catch_block;

	    /* call the cleanup function. */
	    dst[1] = cleanup;
	    set_c_continuation(thread, continue_unwind);
	    invoke(thread, 0);
	    return;
	}
	/* Unlink the catch block we just unwound past. */
	obj_ptr(struct catch_block *, cur_catch)->thread = NULL;
	thread->handlers = obj_ptr(struct catch_block *, cur_catch)->handlers;
	thread->cur_catch=obj_ptr(struct catch_block *, cur_catch)->prev_catch;
    } while (cur_catch != catch_block);

    /* Restore the frame pointer and the handlers. */
    thread->fp = obj_ptr(struct catch_block *, cur_catch)->fp;
    thread->handlers = obj_ptr(struct catch_block *, cur_catch)->handlers;

    /* And back we go. */
    do_return(thread, pop_linkage(thread), vals);
}


/* GC stuff. */

static int scav_catch(struct object *o)
{
    struct catch_block *catch = (struct catch_block *)o;

    scavenge(&catch->prev_catch);
    scavenge(&catch->handlers);

    return sizeof(struct catch_block);
}

static obj_t trans_catch(obj_t catch)
{
    return transport(catch, sizeof(struct catch_block), FALSE);
}


/* Init stuff. */

void make_nlx_classes(void)
{
    obj_CatchBlockClass = make_builtin_class(scav_catch, trans_catch);
    add_constant_root(&obj_CatchBlockClass);
}

void init_nlx_classes(void)
{
    init_builtin_class(obj_CatchBlockClass, "<catch>", obj_ObjectClass, NULL);
}


void init_nlx_functions(void)
{
    define_constant("uwp",
		    make_raw_function("uwp", 
				      list2(obj_ObjectClass, obj_ObjectClass),
				      FALSE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass, uwp));
    define_constant("catch",
		    make_raw_function("catch", list1(obj_ObjectClass),
				      FALSE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass, catch));
    define_constant("throw",
		    make_raw_function("throw", list1(obj_ObjectClass),
				      TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass, throw));
}
