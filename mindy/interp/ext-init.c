/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/interp/ext-init.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file does whatever.
*
\**********************************************************************/

#define _ANSI_ARGS_(a)  () /* Hack to avoid loading "std-c.h" */

#include "mindy.h"
#include "gc.h"
#include "bool.h"
#include "extern.h"
#include "error.h"

void add_explicit_symbol(char *name, void *address);

/* This file can't use the malloc macro, so be sure to check the 
 * results of malloc yourself. 
 */
#define temp_malloc(sz) malloc(sz)
#undef malloc
#define temp_calloc(sz, cnt) calloc(sz, cnt)
#undef calloc
#include "extern1.def"
#define malloc(sz) temp_malloc(sz)
#define calloc(sz, cnt) temp_calloc(sz, cnt)

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
    struct foreign_file *syms;
    int extra_size = (extern_sym_count - 1) * sizeof(struct symtab);

    mindy_explicit_syms = alloc(obj_ArchivedFileClass, 
				sizeof(struct foreign_file) + extra_size);
    syms = FOREIGN_FILE(mindy_explicit_syms);
    syms->file_name = make_byte_string(exec_file_name);
    syms->sym_count = 0;
    syms->extra_size = extra_size;

#include "extern2.def"
}
