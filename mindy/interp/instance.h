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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/instance.h,v 1.1 1994/03/24 21:49:46 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


enum slot_allocation {
    alloc_INSTANCE, alloc_CLASS, alloc_SUBCLASS, alloc_CONSTANT, alloc_VIRTUAL,
    alloc_Kinds
};

extern obj_t make_defined_class(obj_t debug_name);
extern void init_defined_class(obj_t class, obj_t superclasses,
			       obj_t slots_maker);
