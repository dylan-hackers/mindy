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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/compile.h,v 1.2 1994/03/28 11:32:38 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



struct block {
    struct block *next;
    unsigned char *end;
    unsigned char bytes[0];
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

void compile(struct body *program);

