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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/obj.h,v 1.2 1994/04/07 18:29:36 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_ObjectClass;

extern obj_t object_class(obj_t object);

#ifdef __GNUC__
#define object_class inline_object_class
extern obj_t obj_IntegerClass;
static __inline__ obj_t inline_object_class(obj_t object)
{
    if (((unsigned long)(object))&1)	/* obj_is_ptr(o) -- no mindy.h yet */
	return obj_ptr(struct object *, object)->class;
    else
	return obj_IntegerClass;
}
#endif
