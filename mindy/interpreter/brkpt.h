/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
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
