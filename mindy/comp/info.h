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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/info.h,v 1.3 1994/04/09 14:09:16 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct binop_info {
    int precedence;
    boolean left_assoc;
};

struct function_info {
    boolean (*srctran)();
    void (*compile)();
};

extern struct binop_info *lookup_binop_info(struct id *id);

extern struct function_info
    *lookup_function_info(struct id *id, boolean createp);

extern void init_info(void);
