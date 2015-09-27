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



struct block {
    struct block *next;
    unsigned char *end;
    unsigned char bytes[1];
};

struct debug_info {
    int line;
    int bytes;
    struct scope_info *scope;
    struct debug_info *next;
};

struct var_info {
    struct id *var;
    boolean indirect;
    boolean argument;
    int offset;
    struct var_info *next;
};

struct scope_info {
    int handle;
    int nvars;
    struct var_info *vars;
    struct var_info **vars_tail;
    struct scope_info *outer;
};

struct component {
    struct literal *debug_name;
    int frame_size;
    int cur_line;
    struct scope_info *cur_scope;
    int cur_line_start;
    int ndebug_infos;
    struct debug_info *debug_info;
    struct debug_info **debug_info_tail;
    int nconstants;
    struct constant *constants;
    struct constant **constants_tail;
    int bytes;
    struct block *blocks;
    struct block *cur_block;
    unsigned char *fill;
    unsigned char *end;
};

enum constant_kind {
    constant_LITERAL, constant_METHODDESC, constant_VARREF
};

struct constant {
    enum constant_kind kind;
    struct constant *next;
    union {
	struct literal *literal;
	struct method *method;
	struct {
	    struct id *id;
	    boolean written;
	} varref;
    } u;
};

extern void compile(struct body *program);

extern void init_compile(void);
