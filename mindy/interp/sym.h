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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/sym.h,v 1.2 1994/03/31 10:19:10 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_SymbolClass, obj_KeywordClass;

extern obj_t symbol(char *name);
extern char *sym_name(obj_t sym);
extern unsigned sym_hash(obj_t sym);
