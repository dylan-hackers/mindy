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
* $Header: /scm/cvs/src/mindy/interp/brkpt.h,v 1.1 1998/05/03 19:55:11 andreas Exp $
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

extern void list_breakpoints(void);

extern void install_return_breakpoint(struct thread *thread, obj_t *fp);
