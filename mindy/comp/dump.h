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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/dump.h,v 1.2 1994/04/09 14:09:07 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



extern void dump_setup_output(char *source, FILE *file, char *what);
extern void dump_top_level_form(struct component *tlf);
extern void dump_defmethod(struct id *name, struct component *tlf);
extern void dump_defgeneric(struct id *name, struct component *tlf);
extern void dump_defclass(struct id *name, struct slot_spec *slots,
			  struct component *tlf);
extern void dump_defvar(struct param_list *params, struct component *tlf);
extern void dump_defconst(struct param_list *params, struct component *tlf);
extern void dump_defmodule(struct defnamespace_constituent *c);
extern void dump_deflibrary(struct defnamespace_constituent *c);
extern void dump_program(struct body *program);
extern void dump_finalize_output(void);
