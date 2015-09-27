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
***********************************************************************
*
* This file implements weak pointers.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "obj.h"
#include "bool.h"
#include "list.h"
#include "type.h"
#include "class.h"
#include "def.h"
#include "sym.h"
#include "module.h"
#include "error.h"
#include "thread.h"
#include "func.h"
#include "weak.h"


obj_t obj_WeakPointerClass = NULL;

static struct weak_pointer *WeakPointers = NULL;

obj_t make_weak_pointer(obj_t object)
{
    obj_t res = alloc(obj_WeakPointerClass, sizeof(struct weak_pointer));

    WEAK(res)->object = object;
    WEAK(res)->broken = FALSE;
    WEAK(res)->next = NULL;

    return res;
}


/* Dylan routines. */

obj_t dylan_make_weak_pointer(obj_t class, obj_t object)
{
    if (object == obj_Unbound) {
	error("Must supply the object when making weak pointers.");
	return NULL;
    }
    else
	return make_weak_pointer(object);
}

void dylan_weak_pointer_object(obj_t meth, struct thread *thread, obj_t *args)
{
    obj_t weak = args[0];
    obj_t *old_sp = args-1;

    old_sp[0] = WEAK(weak)->object;
    old_sp[1] = WEAK(weak)->broken ? obj_True : obj_False;

    thread->sp = old_sp + 2;

    do_return(thread, old_sp, old_sp);
}



/* GC routines. */

void weak_pointer_gc_setup(void)
{
    WeakPointers = NULL;
}    

static int scav_weak_pointer(struct object *obj)
{
    struct weak_pointer *weakptr = (struct weak_pointer *)obj;

    if (!weakptr->broken && obj_is_ptr(weakptr->object)) {
	weakptr->next = WeakPointers;
	WeakPointers = weakptr;
    }
    else
	scavenge(&weakptr->object);

    return sizeof(struct weak_pointer);
}

static obj_t trans_weak_pointer(obj_t weakptr)
{
    return transport(weakptr, sizeof(struct weak_pointer), FALSE);
}

void break_weak_pointers(void)
{
    struct weak_pointer *w, *n;

    for (w = WeakPointers; w != NULL; w = n) {
	if (object_collected(w->object)) {
	    w->object = obj_False;
	    w->broken = TRUE;
	}
	else
	    scavenge(&w->object);
	n = w->next;
	w->next = NULL;
    }
}


/* Init stuff. */

void make_weak_classes(void)
{
    obj_WeakPointerClass
	= make_builtin_class(scav_weak_pointer, trans_weak_pointer);
    add_constant_root(&obj_WeakPointerClass);
}

void init_weak_classes(void)
{
    init_builtin_class(obj_WeakPointerClass, "<weak-pointer>", obj_ObjectClass,
		       NULL);
}

void init_weak_functions(void)
{
    define_method("make", list1(singleton(obj_WeakPointerClass)), FALSE,
		  list1(pair(symbol("object"), obj_Unbound)),
		  FALSE, obj_WeakPointerClass, dylan_make_weak_pointer);
    define_generic_function("weak-pointer-object", 
			    list1(obj_WeakPointerClass), 
			    FALSE, obj_False, FALSE,
			    list2(obj_ObjectClass, obj_BooleanClass),
			    obj_False);
    add_method(find_variable(module_BuiltinStuff,symbol("weak-pointer-object"),
			     FALSE, FALSE)->value,
	       make_raw_method("weak-pointer-object",
			       list1(obj_WeakPointerClass), FALSE, obj_False,
			       FALSE, list2(obj_ObjectClass, obj_BooleanClass),
			       obj_False, dylan_weak_pointer_object));
}
