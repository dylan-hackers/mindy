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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/list.h,v 1.1 1994/03/24 21:49:27 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct list {
    obj_t class;
    obj_t head;
    obj_t tail;
};

#define LIST(o) obj_ptr(struct list *, o)

#define HEAD(l) (LIST(l)->head)
#define TAIL(l) (LIST(l)->tail)

extern obj_t obj_ListClass, obj_EmptyListClass, obj_PairClass;
extern obj_t obj_Nil;

extern obj_t pair(obj_t head, obj_t tail);
extern boolean memq(obj_t o, obj_t list);
extern obj_t nreverse(obj_t list);
extern int length(obj_t list);

extern obj_t list1(obj_t x);
extern obj_t list2(obj_t x, obj_t y);
extern obj_t list3(obj_t x, obj_t y, obj_t z);
extern obj_t listn(int n, ...);

