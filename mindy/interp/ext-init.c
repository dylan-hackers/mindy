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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/ext-init.c,v 1.2 1994/11/06 23:03:08 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/

#define _ANSI_ARGS_(a)  () /* Hack to avoid loading "std-c.h" */

#include "mindy.h"
#include "gc.h"
#include "bool.h"
#include "extern.h"

#include "extern1.def"

void add_explicit_symbol(char *name, void *address)
{
    struct foreign_file *syms = FOREIGN_FILE(mindy_explicit_syms);
    if (syms->sym_count >= extern_sym_count)
	error("Number of 'external definitions' does not match the count");
    syms->syms[syms->sym_count].name = name;
    syms->syms[syms->sym_count++].ptr = address;
}

void build_explicit_syms(void)
{
    obj_t foo;
    struct foreign_file *syms;
    int extra_size = (extern_sym_count) * sizeof(struct symtab);

    mindy_explicit_syms = alloc(obj_ForeignFileClass, 
				sizeof(struct foreign_file) + extra_size);
    syms = FOREIGN_FILE(mindy_explicit_syms);
    syms->file_name = obj_False;
    syms->sym_count = 0;
    syms->extra_size = extra_size;

#include "extern2.def"
}
