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
* $Header: /scm/cvs/src/mindy/comp/mindycomp.h,v 1.1 1998/05/03 19:55:11 andreas Exp $
*
\**********************************************************************/

#ifdef WIN32
#   define boolean unsigned char
#   define LIBDIR ""
#   define BINDIR ""
#   define VERSION "1.5"
#else
#   define boolean int
#endif
#define TRUE 1
#define FALSE 0

extern char *current_file;
extern boolean GiveWarnings;

extern void error (int line, char *msg, ...);
extern void warn (int line, char *msg, ...);

extern struct symbol *ModuleName;
extern struct symbol *LibraryName;

/* Don't call check_malloc yourself, always use the malloc macro.
 * Has no parameter list so that ext-init.c can read this file 
 * without knowing about a size_t.
 * Also, try to keep this section consistent with interp/mindy.h
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
