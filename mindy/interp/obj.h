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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/obj.h,v 1.3 1994/04/17 17:43:09 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_ObjectClass;
extern obj_t obj_IntegerClass;

#define object_class(o) \
    (obj_is_ptr(o) ? obj_ptr(struct object *, o)->class : obj_IntegerClass)
