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
* $Header: /scm/cvs/src/mindy/interp/list.h,v 1.1 1998/05/03 19:55:16 andreas Exp $
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
extern obj_t listn _ANSI_ARGS_((int n, ...));

