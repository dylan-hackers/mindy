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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/print.h,v 1.1 1994/03/24 21:49:49 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

extern void prin1(obj_t object);
extern void print(obj_t object);
extern void format(char *fmt, ...);
extern int count_format_args(char *fmt);
extern void vformat(char *fmt, obj_t *args);
