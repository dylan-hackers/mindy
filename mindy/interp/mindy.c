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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/mindy.c,v 1.13 1994/11/06 20:00:45 rgs Exp $
*
* This file starts everything going.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

static void missing_arg(char *whose)
{
    fprintf(stderr, "mindy: missing argument to %s option\n", whose);
    exit(1);
}
  
char *exec_file_name;

void main(int argc, char *argv[])
{
    struct thread *thread;
    enum pause_reason reason;
    struct variable *var;
#if ! NO_ARGV_0
    char *argv0 = "mindy";
#endif

    init();

    thread = thread_create(symbol("main"));
    *thread->sp++ = make_raw_function("startup", 0, TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      startup);

    exec_file_name = argv[0];
    while (*++argv != NULL) {
	if (strcmp(*argv, "-f") == 0) {
	    if (*++argv == NULL)
	        missing_arg("-f");
	    load(*argv);
#if ! NO_SHARP_BANG
        } else if (strcmp(*argv, "-x") == 0) {
	    if (*++argv == NULL)
	        missing_arg("-f");
#if ! NO_ARGV_0
	    if (strcmp(*argv, "-") != 0)
	        argv0 = *argv;
#endif
	    load(*argv);
	    argv += 1;
	    break;
#endif
#if ! NO_ARGV_0
	} else if (strcmp(*argv, "-0") == 0) {
	    if (*++argv == NULL)
	        missing_arg("-0");
	    argv0 = *argv;
#endif
        } else {
	    break;
	}
    }

#if ! NO_ARGV_0
        *thread->sp++ = make_byte_string(argv0);    /* pass command name */
#endif
    while (*argv != NULL)
        *thread->sp++ = make_byte_string(*argv++);

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
