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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/obj.c,v 1.6 1994/06/11 02:23:42 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "num.h"
#include "class.h"
#include "bool.h"
#include "list.h"
#include "def.h"
#include "gc.h"
#include "obj.h"

obj_t obj_ObjectClass = 0;

static obj_t dylan_object_class(obj_t object)
{
    return object_class(object);
}


/* GC stuff. */

void scavenge_obj_roots(void)
{
    scavenge(&obj_ObjectClass);
}


/* Init stuff. */

void make_obj_classes(void)
{
    obj_ObjectClass = make_abstract_class(FALSE);
}

void init_obj_classes(void)
{
    init_builtin_class(obj_ObjectClass, "<object>", NULL);
}

void init_obj_functions(void)
{
    define_function("object-class", list1(obj_ObjectClass), FALSE, obj_False,
		    FALSE, obj_ClassClass, dylan_object_class);
}
