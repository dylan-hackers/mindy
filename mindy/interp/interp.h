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
* $Header: /scm/cvs/src/mindy/interp/interp.h,v 1.1 1998/05/03 19:55:15 andreas Exp $
*
\**********************************************************************/


extern obj_t obj_ComponentClass;

struct component {
    obj_t class;
    int length;
    obj_t debug_name;
    int frame_size;
    obj_t mtime;
    obj_t source_file;
    obj_t debug_info;
    int n_constants;
    obj_t constant[1];
};

#define COMPONENT(o) obj_ptr(struct component *, o)

extern void set_byte_continuation(struct thread *thread, obj_t component);
extern void do_byte_return(struct thread *thread, obj_t *old_sp, obj_t *vals);
extern obj_t make_component(obj_t debug_name, int frame_size, obj_t mtime,
			    obj_t src_file, obj_t debug_info, int constants,
			    int bytecode_len);

extern void interpret_byte(int byte, struct thread *thread);
extern void interpret_next_byte(struct thread *thread);
