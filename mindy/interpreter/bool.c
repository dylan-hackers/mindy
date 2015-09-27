/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* This file implements the booleans, #t and #f.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "print.h"
#include "gc.h"
#include "class.h"
#include "obj.h"
#include "bool.h"
#include "def.h"
#include "list.h"

struct bool {
    obj_t class;
};

obj_t obj_True = 0, obj_False = 0, obj_BooleanClass = 0;


static obj_t dylan_not(obj_t thing)
{
    if (thing == obj_False)
	return obj_True;
    else
	return obj_False;
}


/* Printer support. */

static void print_true(obj_t true)
{
    printf("#t");
}

static void print_false(obj_t false)
{
    printf("#f");
}


/* GC support. */

static int scav_bool(struct object *bool)
{
    return sizeof(struct bool);
}

static obj_t trans_bool(obj_t bool)
{
    return transport(bool, sizeof(struct bool), TRUE);
}


/* Init stuff. */

void make_bool_classes(void)
{
    obj_BooleanClass = make_abstract_class(TRUE);
    obj_True = alloc(make_builtin_class(scav_bool, trans_bool),
		     sizeof(struct bool));
    obj_False = alloc(make_builtin_class(scav_bool, trans_bool),
		      sizeof(struct bool));
    add_constant_root(&obj_BooleanClass);
    add_constant_root(&obj_True);
    add_constant_root(&obj_False);
}

void init_bool_classes(void)
{
    init_builtin_class(obj_BooleanClass, "<boolean>", obj_ObjectClass, NULL);
    init_builtin_class(obj_ptr(struct bool *, obj_True)->class,
		       "<true>", obj_BooleanClass, NULL);
    def_printer(obj_ptr(struct bool *, obj_True)->class, print_true);
    init_builtin_class(obj_ptr(struct bool *, obj_False)->class,
		       "<false>", obj_BooleanClass, NULL);
    def_printer(obj_ptr(struct bool *, obj_False)->class, print_false);
}

void init_bool_functions(void)
{
    define_function("~", list1(obj_ObjectClass), FALSE, obj_False, FALSE,
		    obj_BooleanClass, dylan_not);
}
