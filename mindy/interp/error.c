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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/error.c,v 1.2 1994/04/08 17:56:09 wlott Exp $
*
* This file does whatever.
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

static boolean error_system_enabled = FALSE;

static struct variable *error_var = NULL;
static struct variable *type_error_var = NULL;

void error(char *msg, ...)
{
    int nargs = count_format_args(msg);
    va_list ap;
    int i;
    char *ptr;
    struct thread *thread = thread_current();
    
    if (error_system_enabled) {
	*thread->sp++ = error_var->value;
	*thread->sp++ = make_string(msg);
	va_start(ap, msg);
	for (i = 0; i < nargs; i++)
	    *thread->sp++ = va_arg(ap, obj_t);
	va_end(ap);

	invoke(thread, nargs+1);
	go_on();
    }
    else if (thread) {
	obj_t cond = make_vector(nargs+1, NULL);

	SOVEC(cond)->contents[0] = make_string(msg);
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
	error("~S is not an instance of type ~S", value, type);
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
    define_function("enable-error-system", obj_Nil, FALSE, obj_False,
		    obj_ObjectClass, enable_error_system);
    error_var = find_variable(module_BuiltinStuff, symbol("error"),
			      FALSE, TRUE);
    type_error_var = find_variable(module_BuiltinStuff, symbol("type-error"),
				   FALSE, TRUE);
}

