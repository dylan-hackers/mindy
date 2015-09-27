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

#include <config.h>

extern void add_constant_root(obj_t *addr);
extern void add_variable_root(obj_t *addr);

extern obj_t alloc(obj_t class, int bytes);
extern void shrink(obj_t obj, int old_bytes, int new_bytes);
extern void scavenge(obj_t *addr);
extern obj_t transport(obj_t obj, int bytes, boolean read_only);

extern void collect_garbage(boolean purify);

extern boolean object_collected(obj_t obj);
extern obj_t pointer_hash_state(obj_t pointer);

extern boolean TimeToGC;

#if (SIZEOF_VOID_P == 4)
#    define ForwardingMarker ((obj_t)(0xdeadbeef))
#elif (SIZEOF_VOID_P == 8)
#    define ForwardingMarker ((obj_t)(0xdeadbeefdeadbeefL))
#else
#    error Neither 32 nor 64-bit architecture.
#endif

#ifdef GD_DEBUG
#    if (SIZEOF_VOID_P == 4)
#        define COLLECTED_COOKIE 0xfacefeed
#        define ALLOC_HEADER_COOKIE 0xbeadbabe
#    elif (SIZEOF_VOID_P == 8)
#        define COLLECTED_COOKIE 0xfacefeedfacefeedL
#        define ALLOC_HEADER_COOKIE 0xbeadbabebeadbabeL
#    else
#        error Running on neither 32-bit nor 64-bit arch.
#    endif
    
#    define ASSERT_VALID_OBJ(o) \
         if ((o) != NULL && obj_is_ptr(o) &&\
	     ((obj_ptr(struct object *, o)->class == ForwardingMarker) \
		 && obj_ptr(unsigned long *, o)[-2] != ALLOC_HEADER_COOKIE)) \
		 lose("Invalid object pointer encountered.")
#else
#    define ASSERT_VALID_OBJ(o)
#endif
