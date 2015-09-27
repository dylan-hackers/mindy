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


struct lexenv {
    struct method *method;
    struct binding *bindings;
    int depth;
};

struct binding {
    struct id *id;
    struct symbol *type;
    struct method *home;
    boolean function;
    boolean closed_over;
    boolean set;
    boolean argument;
    int offset;
    struct binding *next;
};

extern struct binding
    *make_binding(struct id *id, struct symbol *type, boolean argument,
		  int offset, struct method *home, struct binding *next);
extern struct lexenv
    *make_lexenv(struct method *method, struct binding *bindings, int depth);

