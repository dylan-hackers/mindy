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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/obj.c,v 1.2 1994/04/07 18:30:59 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "num.h"
#include "class.h"
#include "bool.h"
#include "obj.h"

obj_t obj_ObjectClass = 0;

#undef object_class		/* Get rid of inline def if it exists */
obj_t object_class(obj_t object)
{
    if (obj_is_ptr(object))
	return obj_ptr(struct object *, object)->class;
    else
	return obj_IntegerClass;
}


/* GC stuff. */

void scavenge_obj_roots(void)
{
    scavenge(&obj_ObjectClass);
}


/* Init stuff. */

void make_obj_classes(void)
{
    obj_ObjectClass = make_abstract_class();
}

void init_obj_classes(void)
{
    init_builtin_class(obj_ObjectClass, "<object>", NULL);
}

void init_obj_functions(void)
{
    define_function("object-class", list1(obj_ObjectClass),
		    FALSE, obj_False, obj_ClassClass, object_class);
}
