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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/instance.h,v 1.3 1994/04/20 00:22:38 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


enum slot_allocation {
    alloc_INSTANCE, alloc_CLASS, alloc_SUBCLASS, alloc_CONSTANT, alloc_VIRTUAL,
    alloc_Kinds
};

struct library;

extern obj_t make_defined_class(obj_t debug_name, struct library *library);
extern void init_defined_class(obj_t class, obj_t slots);
