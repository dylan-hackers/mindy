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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/load.h,v 1.1 1994/03/24 21:49:47 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern void load(char *name);
extern void load_library(obj_t name);
extern void load_do_inits(struct thread *thread);
