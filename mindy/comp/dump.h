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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/dump.h,v 1.1 1994/03/24 21:49:11 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



void dump_prologue(void);
void dump_top_level_form(struct component *tlf);
void dump_defmethod(struct id *name, struct component *tlf);
void dump_defgeneric(struct id *name, struct component *tlf);
void dump_defclass(struct id *name, struct slot_spec *slots,
		   struct component *tlf);
void dump_defvar(struct param_list *params, struct component *tlf);
void dump_defconst(struct param_list *params, struct component *tlf);
void dump_defmodule(struct defnamespace_constituent *c);
void dump_deflibrary(struct defnamespace_constituent *c);
void dump_epilog(void);
