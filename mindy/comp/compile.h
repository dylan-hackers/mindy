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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/compile.h,v 1.1 1994/03/24 21:49:10 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



struct block {
    struct block *next;
    unsigned char *end;
    unsigned char bytes[0];
};

struct component {
    struct literal *debug_name;
    struct block *blocks;
    struct block *cur_block;
    unsigned char *fill;
    unsigned char *end;
    int bytes;
    struct constant *constants;
    struct constant **constants_tail;
    int nconstants;
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

