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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/mindy.h,v 1.2 1994/04/17 17:43:36 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stddef.h>

extern void *malloc(size_t bytes);
extern void *realloc(void *ptr, size_t bytes);
extern void free(void *ptr);

typedef struct _object *obj_t;

struct object {
    obj_t class;
};

#define obj_is_ptr(o) (((unsigned long)(o))&1)
#define obj_ptr(type, o) ((type)(((unsigned long)(o))-1))
#define ptr_obj(ptr) ((obj_t)(((unsigned long)(ptr))+1))
#define obj_rawptr(o) ((void *)(o))
#define rawptr_obj(p) ((obj_t)(p))

extern void lose(char *fmt, ...);

#define assert(cond) \
    do { \
	if (!(cond)) \
	    lose("assertion failed, line %d file %s", __LINE__, __FILE__); \
    } while (0)

typedef int boolean;
#define TRUE 1
#define FALSE 0

#ifndef NULL
#define NULL ((void*)0)
#endif


#ifdef sparc
#define SLOW_LONGJMP 1
#endif

#ifdef hppa
#define SLOW_FUNCTION_POINTERS 1
#endif


#ifndef SLOW_LONGJMP
#define SLOW_LONGJMP 0
#endif

#ifndef SLOW_FUNCTION_POINTERS
#define SLOW_FUNCTION_POINTERS 0
#endif
