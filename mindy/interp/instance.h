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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/instance.h,v 1.4 1994/06/27 16:32:01 wlott Exp $
*
\**********************************************************************/


enum slot_allocation {
    alloc_INSTANCE, alloc_CLASS, alloc_SUBCLASS, alloc_CONSTANT, alloc_VIRTUAL,
    alloc_Kinds
};

struct library;

extern obj_t make_defined_class(obj_t debug_name, struct library *library);
extern void init_defined_class(obj_t class, obj_t slots);
