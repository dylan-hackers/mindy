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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/vec.c,v 1.2 1994/03/31 10:19:14 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "gc.h"
#include "coll.h"
#include "class.h"
#include "thread.h"
#include "func.h"
#include "bool.h"
#include "list.h"
#include "num.h"
#include "obj.h"
#include "module.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

obj_t obj_SimpleObjectVectorClass = NULL;

obj_t make_vector(int length, obj_t *contents)
{
    obj_t res = alloc(obj_SimpleObjectVectorClass,
		      sizeof(struct sovec) + sizeof(obj_t)*length);

    obj_ptr(struct sovec *, res)->length = length;

    if (contents)
	bcopy(contents, obj_ptr(struct sovec *, res)->contents,
	      sizeof(obj_t) * length);

    return res;
}

static void dylan_vector(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t res = make_vector(nargs, args);
    obj_t *old_sp = args-1;

    *old_sp = res;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static obj_t dylan_sovec_element(obj_t sovec, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct sovec *, sovec)->length)
	return obj_ptr(struct sovec *, sovec)->contents[i];
    else if (def != obj_Unbound)
	return def;
    else
	error("No element ~S in ~S", index, sovec);
}

static obj_t dylan_sovec_element_setter(obj_t value, obj_t sovec, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct sovec *, sovec)->length)
	obj_ptr(struct sovec *, sovec)->contents[i] = value;
    else
	error("No element ~S in ~S", index, sovec);

    return value;
}

static obj_t dylan_sovec_size(obj_t sovec)
{
    return make_fixnum(obj_ptr(struct sovec *, sovec)->length);
}

static obj_t dylan_vec_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;
    obj_t *ptr;

    if (!instancep(size, obj_IntegerClass))
	error("Bogus size: for make ~S: ~S", class, size);
    len = fixnum_value(size);

    res = make_vector(len, NULL);

    ptr = obj_ptr(struct sovec *, res)->contents;
    while (len-- > 0)
	*ptr++ = fill;

    return res;
}


/* Printing support. */

static void print_sovec(obj_t sovec)
{
    int len = obj_ptr(struct sovec *, sovec)->length;
    int i;

    printf("#[");
    for (i = 0; i < len; i++) {
	if (i)
	    printf(", ");
	prin1(obj_ptr(struct sovec *, sovec)->contents[i]);
    }
    printf("]");
}


/* GC stuff. */

static int scav_sovec(struct object *ptr)
{
    struct sovec *v = (struct sovec *)ptr;
    int len = v->length;
    int i;
    
    for (i = 0; i < len; i++)
	scavenge(v->contents + i);

    return sizeof(struct sovec) + sizeof(obj_t)*len;
}

static obj_t trans_sovec(obj_t v)
{
    int len = obj_ptr(struct sovec *, v)->length;
    return transport(v, sizeof(struct sovec) + sizeof(obj_t)*len);
}

void scavenge_vec_roots(void)
{
    scavenge(&obj_SimpleObjectVectorClass);
}



/* Initialization stuff. */

void make_vec_classes(void)
{
    obj_SimpleObjectVectorClass = make_builtin_class(scav_sovec, trans_sovec);
}

void init_vec_classes(void)
{
    init_builtin_class(obj_SimpleObjectVectorClass, "<simple-object-vector>",
		       obj_VectorClass, NULL);
    def_printer(obj_SimpleObjectVectorClass, print_sovec);
}

void init_vec_functions(void)
{
    define_constant("vector",
		    make_raw_function("vector", 0, TRUE, obj_False,
				      list1(obj_SimpleObjectVectorClass),
				      obj_False, dylan_vector));
    define_method("element",
		    list2(obj_SimpleObjectVectorClass, obj_IntegerClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)),
		    obj_ObjectClass, dylan_sovec_element);
    define_method("element-setter",
		  list3(obj_ObjectClass,
			obj_SimpleObjectVectorClass,
			obj_IntegerClass),
		  FALSE, obj_False,
		  obj_ObjectClass, dylan_sovec_element_setter);
    define_method("size", list1(obj_SimpleObjectVectorClass),
		  FALSE, obj_False, obj_IntegerClass, dylan_sovec_size);
    define_method("make", list1(singleton(obj_VectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  obj_SimpleObjectVectorClass, dylan_vec_make);
    define_method("make", list1(singleton(obj_SimpleObjectVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  obj_SimpleObjectVectorClass, dylan_vec_make);
}
