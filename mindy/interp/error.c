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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/error.c,v 1.7 1994/08/30 21:55:49 nkramer Exp $
*
* This file implements the stuff to signal errors from C code.
*
\**********************************************************************/

#include <stdio.h>
#include <stdarg.h>

#include "mindy.h"
#include "str.h"
#include "thread.h"
#include "module.h"
#include "sym.h"
#include "list.h"
#include "vec.h"
#include "type.h"
#include "def.h"
#include "bool.h"
#include "obj.h"
#include "print.h"
#include "func.h"
#include "driver.h"

static boolean error_system_enabled = FALSE;

static struct variable *error_var = NULL;
static struct variable *type_error_var = NULL;

void error(char *msg, ...)
{
    int nargs = count_format_args(msg);
    va_list ap;
    int i;
    struct thread *thread = thread_current();
    
    if (error_system_enabled) {
	*thread->sp++ = error_var->value;
	*thread->sp++ = make_byte_string(msg);
	va_start(ap, msg);
	for (i = 0; i < nargs; i++)
	    *thread->sp++ = va_arg(ap, obj_t);
	va_end(ap);

	invoke(thread, nargs+1);
	go_on();
    }
    else if (thread) {
	obj_t cond = make_vector(nargs+1, NULL);

	SOVEC(cond)->contents[0] = make_byte_string(msg);
	va_start(ap, msg);
	for (i = 1; i <= nargs; i++)
	    SOVEC(cond)->contents[i] = va_arg(ap, obj_t);
	va_end(ap);

	thread_debuggered(thread, cond);
    }
    else {
	obj_t cond = make_vector(nargs, NULL);

	va_start(ap, msg);
	for (i = 0; i < nargs; i++)
	    SOVEC(cond)->contents[i] = va_arg(ap, obj_t);
	va_end(ap);
	
	printf("error: ");
	vformat(msg, SOVEC(cond)->contents);
	putchar('\n');
	exit(1);
    }
}

void type_error(obj_t value, obj_t type)
{
    if (error_system_enabled) {
	struct thread *thread = thread_current();
	*thread->sp++ = type_error_var->value;
	*thread->sp++ = value;
	*thread->sp++ = type;
	invoke(thread, 2);
	go_on();
    }
    else
	error("%= is not an instance of type %=", value, type);
}

void check_type(obj_t thing, obj_t type)
{
    if (!instancep(thing, type))
	type_error(thing, type);
}

static obj_t enable_error_system(void)
{
    error_system_enabled = TRUE;
    return obj_True;
}

void init_error_functions(void)
{
    define_function("enable-error-system", obj_Nil, FALSE, obj_False, FALSE,
		    obj_ObjectClass, enable_error_system);
    error_var = find_variable(module_BuiltinStuff, symbol("error"),
			      FALSE, TRUE);
    type_error_var = find_variable(module_BuiltinStuff, symbol("type-error"),
				   FALSE, TRUE);
}

