/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation.
*
\**********************************************************************/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "shared/portability.h"

extern char *current_file;
extern bool GiveWarnings;

extern void error (int line, char *msg, ...) MINDY_FORMATLIKE(2, 3);
extern void warn (int line, char *msg, ...) MINDY_FORMATLIKE(2, 3);

extern struct symbol *ModuleName;
extern struct symbol *LibraryName;

/* Don't call check_malloc yourself, always use the malloc macro.
 * Also, try to keep this section consistent with interpreter/mindy.h
 */
void *check_malloc(size_t, const char*, int);
#ifdef malloc
#undef malloc
#endif
#define malloc(sz) check_malloc(sz, __FILE__, __LINE__)

void *check_calloc(size_t, size_t, const char*, int);
#ifdef calloc
#undef calloc
#endif
#define calloc(nobj,sz) check_calloc(nobj, sz, __FILE__, __LINE__)

void *check_realloc(void*, size_t, const char*, int);
#ifdef realloc
#undef realloc
#endif
#define realloc(ptr,sz) check_realloc(ptr, sz, __FILE__, __LINE__)
