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
* $Header: /scm/cvs/src/mindy/interp/mindy.h,v 1.1 1998/05/03 19:55:18 andreas Exp $
*
\**********************************************************************/

extern char *exec_file_name;

typedef struct _object *obj_t;

struct object {
    obj_t class;
};

#define obj_is_ptr(o) (((unsigned long)(o))&1)
#define obj_ptr(type, o) ((type)(((unsigned long)(o))-1))
#define ptr_obj(ptr) ((obj_t)(((unsigned long)(ptr))+1))
#define obj_rawptr(o) ((void *)(o))
#define rawptr_obj(p) ((obj_t)(p))

extern void lose _ANSI_ARGS_((char *fmt, ...));
extern int mindy_readline(char *prompt, char *buffer, int max_chars);

#define assert(cond) \
    do { \
	if (!(cond)) \
	    lose("assertion failed, line %d file %s", __LINE__, __FILE__); \
    } while (0)

#ifdef WIN32
#   define boolean unsigned char
#   define LIBDIR "c:\dylan\lib\dylan"
#   define BINDIR "c:\dylan\bin"
#   define VERSION "1.6"
#else
#   define boolean int
#endif
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

/* Don't call check_malloc yourself, always use the malloc macro.
 * Has no parameter list so that ext-init.c can read this file 
 * without knowing about a size_t.
 * Also, try to keep this section consistent with comp/mindycomp.h
 */
void *check_malloc();
#ifdef malloc
#undef malloc
#endif
#define malloc(sz) check_malloc(sz, __FILE__, __LINE__)

void *check_calloc();
#ifdef calloc
#undef calloc
#endif
#define calloc(nobj,sz) check_calloc(nobj, sz, __FILE__, __LINE__)

void *check_realloc();
#ifdef realloc
#undef realloc
#endif
#define realloc(ptr,sz) check_realloc(ptr, sz, __FILE__, __LINE__)
