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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/mindy.c,v 1.2 1994/03/27 02:10:11 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "thread.h"
#include "driver.h"
#include "module.h"
#include "str.h"
#include "bool.h"
#include "list.h"
#include "obj.h"
#include "sym.h"
#include "func.h"
#include "load.h"

static void invoke_main(struct thread *thread, obj_t *vals)
{
    obj_t *fp = thread->fp;
    obj_t *args_end = fp - 4;
    obj_t *old_sp = pop_linkage(thread);
    struct variable *var = find_variable(module_BuiltinStuff, symbol("main"),
					 FALSE, FALSE);

    if (var == NULL)
	lose("main undefined?");

    thread->sp = args_end;
    old_sp[0] = var->value;
    invoke(thread, args_end - old_sp - 1);
}

static void startup(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t *fp = push_linkage(thread, args);

    set_c_continuation(thread, invoke_main);
    load_do_inits(thread);
}

main(int argc, char *argv[])
{
    struct thread *thread;
    enum pause_reason reason;

    init();

    thread = thread_create(symbol("main"));
    *thread->sp++ = make_raw_function("startup", 0, TRUE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      startup);

    while (*++argv != NULL)
	if (strcmp(*argv, "-f") == 0)
	    load(*++argv);
	else
	    *thread->sp++ = make_string(*argv);

    finalize_modules();

    thread_restart(thread);

    reason = do_stuff();
    if (reason != pause_NothingToRun)
	invoke_debugger(reason);
    exit(0);
}
