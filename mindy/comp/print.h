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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/print.h,v 1.1 1994/03/24 21:49:18 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern void print_literal(struct literal *l, int depth);
extern void print_expr(struct expr *e, int depth);
extern void print_constituent(struct constituent *c, int depth);
extern void print_body(struct body *b, int depth);
