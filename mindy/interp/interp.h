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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/interp.h,v 1.4 1994/04/12 19:48:31 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_ComponentClass;

struct component {
    obj_t class;
    int length;
    obj_t debug_name;
    int frame_size;
    obj_t source_file;
    obj_t debug_info;
    int n_constants;
    obj_t constant[0];
};

#define COMPONENT(o) obj_ptr(struct component *, o)

extern void set_byte_continuation(struct thread *thread, obj_t component);
extern void do_byte_return(struct thread *thread, obj_t *old_sp, obj_t *vals);
extern obj_t make_component(obj_t debug_name, int frame_size, obj_t src_file,
			    obj_t debug_info, int constants, int bytecode_len);

extern void interpret_byte(int byte, struct thread *thread);
extern void interpret_next_byte(struct thread *thread);
