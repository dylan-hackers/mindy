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
* $Header: /scm/cvs/src/mindy/interp/brkpt.c,v 1.1 1998/05/03 19:55:11 andreas Exp $
*
* This file implements breakpoints.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "weak.h"
#include "thread.h"
#include "driver.h"
#include "gc.h"
#include "interp.h"
#include "bool.h"
#include "print.h"
#include "../comp/byteops.h"
#include "brkpt.h"

static int NextBreakpoint = 1;


/* Byte breakpoints. */

struct byte_brkpt_info {
    int id;
    obj_t component;
    int pc;
    int orig_byte;
    struct byte_brkpt_info *next;
};

static struct byte_brkpt_info *ByteBreakpoints = NULL;

static struct byte_brkpt_info *find_byte_breakpoint(obj_t component, int pc)
{
    struct byte_brkpt_info **prev, *info;

    prev = &ByteBreakpoints;

    while ((info = *prev) != NULL) {
	if (WEAK(info->component)->broken) {
	    printf("breakpoint %d garbage collected\n", info->id);
	    *prev = info->next;
	    free(info);
	}
	else if (WEAK(info->component)->object == component && info->pc == pc)
	    return info;
	else
	    prev = &info->next;
    }
    return NULL;
}

int install_byte_breakpoint(obj_t component, int pc)
{
    struct byte_brkpt_info *info = find_byte_breakpoint(component, pc);
    int nconst;
    unsigned char *ptr;

    if (info)
	return info->id;

    ptr = (unsigned char *)component + pc;

    nconst = COMPONENT(component)->n_constants;
    if (ptr < (unsigned char *)(&COMPONENT(component)->constant[nconst]))
	return -1;
    if (ptr > obj_ptr(unsigned char *, component)+COMPONENT(component)->length)
	return -1;

    info = malloc(sizeof(*info));
    info->id = NextBreakpoint++;
    info->component = make_weak_pointer(component);
    info->pc = pc;
    info->orig_byte = *ptr;
    *ptr = op_BREAKPOINT;
    info->next = ByteBreakpoints;
    ByteBreakpoints = info;

    return info->id;
}

int original_byte(obj_t component, int pc)
{
    struct byte_brkpt_info *info = find_byte_breakpoint(component, pc);

    if (info)
	return info->orig_byte;
    else
	return op_TRAP;
}

static void skip_byte_breakpoint(struct thread *thread)
{
    struct byte_brkpt_info *info
	= find_byte_breakpoint(thread->component, thread->pc);

#if SLOW_FUNCTION_POINTERS
    thread->advance = NULL;
#else
    thread->advance = interpret_next_byte;
#endif

    if (info) {
	thread->pc++;
	interpret_byte(info->orig_byte, thread);
    }
    else
	interpret_next_byte(thread);
}

void handle_byte_breakpoint(struct thread *thread)
{
    struct byte_brkpt_info *info
	= find_byte_breakpoint(thread->component, --thread->pc);

    if (info)
	thread->advance = skip_byte_breakpoint;

    mindy_pause(pause_HitBreakpoint);
}



/* Breakpoint removal. */

void remove_breakpoint(int id)
{
    struct byte_brkpt_info **byte_prev, *byte_info;
    boolean removed = FALSE;

    byte_prev = &ByteBreakpoints;
    while ((byte_info = *byte_prev) != NULL) {
	if (WEAK(byte_info->component)->broken) {
	    if (byte_info->id == id)
		removed = TRUE;
	    else
		printf("breakpoint %d garbage collected\n", byte_info->id);
	    *byte_prev = byte_info->next;
	    free(byte_info);
	}
	else if (byte_info->id == id) {
	    unsigned char *ptr
		= (unsigned char *)(WEAK(byte_info->component)->object)
		    + byte_info->pc;
	    *ptr = byte_info->orig_byte;
	    *byte_prev = byte_info->next;
	    free(byte_info);
	    removed = TRUE;
	}
	else
	    byte_prev = &byte_info->next;
    }

    if (!removed)
	printf("No breakpoint %d\n", id);
}

	

/* Breakpoint listing. */

static void list_breakpoints_aux(struct byte_brkpt_info **byte_prev)
{
    struct byte_brkpt_info *byte_info = *byte_prev;

    if (byte_info == NULL) {
    }
    else if (WEAK(byte_info->component)->broken) {
	printf("breakpoint %d garbage collected\n", byte_info->id);
	*byte_prev = byte_info->next;
	free(byte_info);
	list_breakpoints_aux(byte_prev);
    }
    else {
	obj_t component, debug_name;

	list_breakpoints_aux(&byte_info->next);

	printf("%2d  pc %d in ", byte_info->id, byte_info->pc);
	component = WEAK(byte_info->component)->object;
	debug_name = COMPONENT(component)->debug_name;
	if (debug_name != obj_False)
	    print(debug_name);
	else
	    print(component);
    }
}

void list_breakpoints(void)
{
    if (ByteBreakpoints != NULL) {
	printf("id  where\n");
	list_breakpoints_aux(&ByteBreakpoints);
    }
    else
	printf("no breakpoints\n");
}



/* GC routines. */

void scavenge_brkpt_roots(void)
{
    struct byte_brkpt_info **byte_prev, *byte_info;

    byte_prev = &ByteBreakpoints;
    while ((byte_info = *byte_prev) != NULL) {
	if (WEAK(byte_info->component)->broken) {
	    printf("breakpoint %d garbage collected\n", byte_info->id);
	    *byte_prev = byte_info->next;
	    free(byte_info);
	}
	else {
	    scavenge(&byte_info->component);
	    byte_prev = &byte_info->next;
	}
    }
}
