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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/weak.h,v 1.1 1994/04/09 13:33:22 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct weak_pointer {
    obj_t class;
    obj_t object;
    boolean broken;
    struct weak_pointer *next;
};

#define WEAK(o) obj_ptr(struct weak_pointer *, o)

extern obj_t obj_WeakPointerClass;
extern obj_t make_weak_pointer(obj_t object);

extern void break_weak_pointers();
