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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/dump.h,v 1.4 1994/04/28 04:03:43 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



extern void dump_setup_output(char *source, FILE *file);
extern void dump_top_level_form(struct component *tlf);
extern void dump_defmethod(struct id *name, struct component *tlf);
extern void dump_defgeneric(struct id *name, struct component *tlf);
extern void dump_defclass(struct id *name, struct slot_spec *slots,
			  struct component *tlf1, struct component *tlf2);
extern void dump_defvar(struct param_list *params, struct component *tlf);
extern void dump_defconst(struct param_list *params, struct component *tlf);
extern void dump_defmodule(struct defnamespace_constituent *c);
extern void dump_deflibrary(struct defnamespace_constituent *c);
extern void dump_program(struct body *program);
extern void dump_finalize_output(void);
