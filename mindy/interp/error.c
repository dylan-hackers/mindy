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
* $Header: /scm/cvs/src/mindy/interp/error.c,v 1.1 1998/05/03 19:55:13 andreas Exp $
*
* This file implements the stuff to signal errors from C code.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

static void verror(char *msg, va_list ap)
{
    int nargs = count_format_args(msg);
    int i;
    struct thread *thread = thread_current();
    
    if (error_system_enabled) {
	*thread->sp++ = error_var->value;
	*thread->sp++ = make_byte_string(msg);
	for (i = 0; i < nargs; i++)
	    *thread->sp++ = va_arg(ap, obj_t);

	invoke(thread, nargs+1);
	go_on();
    }
    else if (thread) {
	obj_t cond = make_vector(nargs+1, NULL);

	SOVEC(cond)->contents[0] = make_byte_string(msg);
	for (i = 1; i <= nargs; i++)
	    SOVEC(cond)->contents[i] = va_arg(ap, obj_t);

	thread_debuggered(thread, cond);
    }
    else {
	obj_t cond = make_vector(nargs, NULL);

	for (i = 0; i < nargs; i++)
	    SOVEC(cond)->contents[i] = va_arg(ap, obj_t);
	
	printf("error: ");
	vformat(msg, SOVEC(cond)->contents, nargs);
	putchar('\n');
	exit(1);
    }
}
#if _USING_PROTOTYPES_
void error(char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    verror(msg, ap);
    va_end(ap);
}
#else
void error(va_alist) va_dcl
{
    va_list ap;
    char *msg;
    
    va_start(ap);
    msg = va_arg(ap, char *);
    verror(msg, ap);
    va_end(ap);
}
#endif

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

obj_t check_type(obj_t thing, obj_t type)
{
    if (!instancep(thing, type)) {
	type_error(thing, type);
	/* Never reached, but keeps the compiler happy. */
	return 0;
    }
    else
	return thing;
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

