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



extern void dump_setup_output(char *source, FILE *file);
extern void dump_top_level_form(struct component *tlf);
extern void dump_defmethod(struct id *name, struct component *tlf);
extern void dump_defgeneric(struct id *name, struct component *tlf);
extern void dump_defclass(struct id *name, struct slot_spec *slots,
			  struct component *tlf1, struct component *tlf2);
extern void dump_defvar(struct param_list *params, struct component *tlf);
extern void dump_defconst(struct param_list *params, struct component *tlf);
extern void dump_defmodule(struct defnamespace_constituent *c);
extern void dump_deflibrary(struct defnamespace_constituent *c);
extern void dump_program(struct body *program);
extern void dump_finalize_output(void);
