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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/brkpt.c,v 1.1 1994/04/12 19:51:14 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


#include "mindy.h"
#include "weak.h"
#include "thread.h"
#include "driver.h"
#include "gc.h"
#include "interp.h"
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
    char *ptr;

    if (info)
	return info->id;

    ptr = (char *)component + pc;

    nconst = COMPONENT(component)->n_constants;
    if (ptr < (char *)(&COMPONENT(component)->constant[nconst]))
	return -1;
    if (ptr > obj_ptr(char *, component)+COMPONENT(component)->length)
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

    thread->advance = interpret_next_byte;

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

    pause(pause_HitBreakpoint);
}



/* Breakpoint removal. */

void remove_breakpoint(int id)
{
    struct byte_brkpt_info **byte_prev, *byte_info;

    byte_prev = &ByteBreakpoints;
    while ((byte_info = *byte_prev) != NULL) {
	if (WEAK(byte_info->component)->broken) {
	    *byte_prev = byte_info->next;
	    free(byte_info);
	}
	else if (byte_info->id == id) {
	    char *ptr = (char *)byte_info->component + byte_info->pc;
	    *ptr = byte_info->orig_byte;
	    *byte_prev = byte_info->next;
	    free(byte_info);
	}
	else
	    byte_prev = &byte_info->next;
    }
}

	

/* GC routines. */

void scavenge_brkpt_roots(void)
{
    struct byte_brkpt_info **byte_prev, *byte_info;

    byte_prev = &ByteBreakpoints;
    while ((byte_info = *byte_prev) != NULL) {
	if (WEAK(byte_info->component)->broken) {
	    *byte_prev = byte_info->next;
	    free(byte_info);
	}
	else {
	    scavenge(&byte_info->component);
	    byte_prev = &byte_info->next;
	}
    }
}
