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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/debug.h,v 1.2 1994/04/09 13:35:48 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

extern struct library *CurLibrary;
extern struct module *CurModule;

void invoke_debugger(enum pause_reason reason);
