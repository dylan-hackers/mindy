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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/lexenv.h,v 1.1 1994/03/24 21:49:14 wlott Exp $
*
* This file does whatever.
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

