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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/brkpt.h,v 1.1 1994/04/12 19:51:14 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern int install_byte_breakpoint(obj_t component, int pc);
extern int original_byte(obj_t component, int pc);
extern void handle_byte_breakpoint(struct thread *thread);

extern int install_function_breakpoint(obj_t function);
extern void (*original_xep(struct thread *thread, int nargs))(obj_t function);
extern void (*original_iep(obj_t self, struct thread *thread,
			   obj_t *args))(obj_t method);

extern void remove_breakpoint(int id);

extern void install_return_breakpoint(struct thread *thread, obj_t *fp);
