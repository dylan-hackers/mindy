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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/obj.c,v 1.7 1994/06/27 16:32:27 wlott Exp $
*
* This file contains <object>.
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
