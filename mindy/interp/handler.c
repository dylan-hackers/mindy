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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/handler.c,v 1.4 1994/06/11 02:23:31 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "class.h"
#include "gc.h"
#include "obj.h"
#include "def.h"
#include "thread.h"
#include "func.h"
#include "list.h"
#include "bool.h"
#include "sym.h"
#include "type.h"
#include "handler.h"


static obj_t obj_HandlerClass = NULL;

static void push_handler(obj_t method, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args-1;
    obj_t type = args[0];
    obj_t func = args[1];
    obj_t test = args[2];
    obj_t descr = args[3];
    obj_t handler = alloc(obj_HandlerClass, sizeof(struct handler));

    HANDLER(handler)->type = type;
    HANDLER(handler)->function = func;
    HANDLER(handler)->test = test;
    HANDLER(handler)->description = descr;
    HANDLER(handler)->next = thread->handlers;
    thread->handlers = handler;

    thread->sp = old_sp;
    do_return(thread, old_sp, old_sp);
}

static void current_handler(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 1;

    assert(nargs == 0);
    *old_sp = thread->handlers;

    do_return(thread, old_sp, old_sp);
}

static obj_t handler_type(obj_t handler)
{
    return HANDLER(handler)->type;
}

static obj_t handler_function(obj_t handler)
{
    return HANDLER(handler)->function;
}

static obj_t handler_test(obj_t handler)
{
    return HANDLER(handler)->test;
}

static obj_t handler_descr(obj_t handler)
{
    return HANDLER(handler)->description;
}

static obj_t handler_next(obj_t handler)
{
    return HANDLER(handler)->next;
}

static void pop_handler(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 1;

    assert(nargs == 0);

    thread->handlers = HANDLER(thread->handlers)->next;

    thread->sp = old_sp;
    do_return(thread, old_sp, old_sp);
}



/* GC stuff. */

static int scav_handler(struct object *obj)
{
    struct handler *handler = (struct handler *)obj;

    scavenge(&handler->type);
    scavenge(&handler->function);
    scavenge(&handler->test);
    scavenge(&handler->description);
    scavenge(&handler->next);

    return sizeof(struct handler);
}

static obj_t trans_handler(obj_t handler)
{
    return transport(handler, sizeof(struct handler));
}

void scavenge_handler_roots(void)
{
    scavenge(&obj_HandlerClass);
}


/* Init stuff. */

void make_handler_classes(void)
{
    obj_HandlerClass = make_builtin_class(scav_handler, trans_handler);
}

void init_handler_classes(void)
{
    init_builtin_class(obj_HandlerClass, "<handler>", obj_ObjectClass, NULL);
}

void init_handler_functions(void)
{
    define_constant("push-handler",
		    make_raw_method("push-handler",
				    list2(obj_TypeClass, obj_FunctionClass),
				    FALSE,
				    list2(pair(symbol("test"), obj_False),
					  pair(symbol("description"),
					       obj_False)),
				    FALSE, obj_Nil, obj_False, push_handler));
    define_constant("current-handler",
		    make_raw_function("current-handler", 0, FALSE, obj_False,
				      FALSE, list1(obj_HandlerClass),
				      obj_False, current_handler));
    define_function("handler-type", list1(obj_HandlerClass), FALSE, obj_False,
		    FALSE, obj_ObjectClass, handler_type);
    define_function("handler-function", list1(obj_HandlerClass), FALSE,
		    obj_False, FALSE, obj_ObjectClass, handler_function);
    define_function("handler-test", list1(obj_HandlerClass), FALSE, obj_False,
		    FALSE, obj_ObjectClass, handler_test);
    define_function("handler-description", list1(obj_HandlerClass), FALSE,
		    obj_False, FALSE, obj_ObjectClass, handler_descr);
    define_function("handler-next", list1(obj_HandlerClass), FALSE, obj_False,
		    FALSE, obj_ObjectClass, handler_next);
    define_constant("pop-handler",
		    make_raw_function("pop-handler", 0, FALSE, obj_False,
				      FALSE, obj_Nil, obj_False, pop_handler));
}
