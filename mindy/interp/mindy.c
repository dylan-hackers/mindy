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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/mindy.c,v 1.8 1994/08/18 19:51:48 wlott Exp $
*
* This file starts everything going.
*
\**********************************************************************/

#include "mindy.h"
#include "init.h"
#include "thread.h"
#include "driver.h"
#include "module.h"
#include "str.h"
#include "bool.h"
#include "list.h"
#include "obj.h"
#include "sym.h"
#include "func.h"
#include "debug.h"
#include "load.h"
#include "num.h"
#include "error.h"

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

    push_linkage(thread, args);
    set_c_continuation(thread, invoke_main);
    load_do_inits(thread);
}

void main(int argc, char *argv[])
{
    struct thread *thread;
    enum pause_reason reason;
    struct variable *var;

    init();

    thread = thread_create(symbol("main"));
    *thread->sp++ = make_raw_function("startup", 0, TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      startup);

    while (*++argv != NULL)
	if (strcmp(*argv, "-f") == 0) {
	    char *file = *++argv;

	    if (file)
		load(file);
	    else
		error("-f must be followed by a file name to load.");
	}
	else
	    *thread->sp++ = make_string(*argv);

    finalize_modules();

    while (1) {
	thread_restart(thread);

	reason = do_stuff();
	if (reason != pause_NothingToRun)
	    invoke_debugger(reason);

	var = find_variable(module_BuiltinStuff, symbol("exit"),
			    FALSE, FALSE);
	if (var == NULL)
	    lose("main undefined?");

	thread = thread_create(symbol("exit"));
	*thread->sp++ = var->value;
    }
}
