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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/weak.c,v 1.2 1994/04/09 15:39:31 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


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

obj_t dylan_weak_pointer_object(obj_t weakptr)
{
    return WEAK(weakptr)->object;
}

obj_t dylan_weak_pointer_broken(obj_t weakptr)
{
    return WEAK(weakptr)->broken ? obj_True : obj_False;
}


/* GC routines. */

static int scav_weak_pointer(struct object *obj)
{
    struct weak_pointer *weakptr = (struct weak_pointer *)obj;

    if (!weakptr->broken && obj_is_ptr(weakptr->object)) {
	weakptr->next = WeakPointers;
	WeakPointers = weakptr;
    }

    return sizeof(struct weak_pointer);
}

static obj_t trans_weak_pointer(obj_t weakptr)
{
    return transport(weakptr, sizeof(struct weak_pointer));
}

void scavenge_weak_roots(void)
{
    scavenge(&obj_WeakPointerClass);
    WeakPointers = NULL;
}

void break_weak_pointers(void)
{
    struct weak_pointer *w, *n;

    for (w = WeakPointers; w != NULL; w = n) {
	if (obj_ptr(struct object *, w->object)->class == ForwardingMarker)
	    scavenge(&w->object);
	else {
	    w->object = obj_False;
	    w->broken = TRUE;
	}
	n = w->next;
	w->next = NULL;
    }
}


/* Init stuff. */

void make_weak_classes(void)
{
    obj_WeakPointerClass
	= make_builtin_class(scav_weak_pointer, trans_weak_pointer);
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
		  obj_WeakPointerClass, dylan_make_weak_pointer);
    define_method("object", list1(obj_WeakPointerClass), FALSE, obj_False,
		  obj_ObjectClass, dylan_weak_pointer_object);
    define_method("broken?", list1(obj_WeakPointerClass), FALSE, obj_False,
		  obj_BooleanClass, dylan_weak_pointer_broken);
}
