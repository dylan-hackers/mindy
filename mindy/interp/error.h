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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/error.h,v 1.1 1994/04/09 13:45:30 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

extern void error(char *msg, ...);
extern void type_error(obj_t value, obj_t type);
extern void check_type(obj_t thing, obj_t type);
