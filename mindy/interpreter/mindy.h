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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../shared/portability.h"

#include "config.h"

extern char *exec_file_name;

typedef struct _object *obj_t;

struct object {
    obj_t class;
};

#define obj_is_ptr(o) (((uintptr_t)(o))&1)
#define obj_ptr(type, o) ((type)(((uintptr_t)(o))-1))
#define ptr_obj(ptr) ((obj_t)(((uintptr_t)(ptr))+1))
#define obj_rawptr(o) ((void *)(o))
#define rawptr_obj(p) ((obj_t)(p))

enum type_Id {
    id_Singleton, id_Class, id_SubClass, id_LimFixnum, id_LimBignum,
    id_Union, id_NoneOf
};

extern MINDY_NORETURN void lose(char *fmt, ...) MINDY_FORMATLIKE(1, 2);
extern int mindy_readline(char *prompt, char *buffer, int max_chars);

#define assert(cond) \
    do { \
        if (!(cond)) \
            lose("assertion failed, line %d file %s", __LINE__, __FILE__); \
    } while (0)

/* Don't call check_malloc yourself, always use the malloc macro.
 * Also, try to keep this section consistent with compiler/mindycomp.h
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
