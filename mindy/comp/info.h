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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/info.h,v 1.1 1994/03/24 21:49:17 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct binop_info {
    int precedence;
    boolean left_assoc;
};

struct function_info {
    void (*srctran)();
    void (*compile)();
};

extern struct binop_info *lookup_binop_info(struct id *id);

extern struct function_info
    *lookup_function_info(struct id *id, boolean createp);
