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
* $Header: /scm/cvs/src/mindy/interp/gc.h,v 1.1 1998/05/03 19:55:13 andreas Exp $
*
\**********************************************************************/


extern void add_constant_root(obj_t *addr);
extern void add_variable_root(obj_t *addr);

extern obj_t alloc(obj_t class, int bytes);
extern void shrink(obj_t obj, int old_bytes, int new_bytes);
extern void scavenge(obj_t *addr);
extern obj_t transport(obj_t obj, int bytes, boolean read_only);

extern void collect_garbage(boolean purify);

extern boolean object_collected(obj_t obj);
extern obj_t pointer_hash_state(obj_t pointer);

extern boolean TimeToGC;

#define ForwardingMarker ((obj_t)(0xDEADBEEF))
