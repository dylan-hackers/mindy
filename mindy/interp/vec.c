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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/vec.c,v 1.6 1994/06/11 02:23:50 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <string.h>

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
#include "error.h"
#include "print.h"
#include "def.h"
#include "vec.h"


/* Simple object vectors. */

obj_t obj_SimpleObjectVectorClass = NULL;

obj_t make_vector(int length, obj_t *contents)
{
    obj_t res = alloc(obj_SimpleObjectVectorClass,
		      sizeof(struct sovec) + sizeof(obj_t)*length);

    SOVEC(res)->length = length;

    if (contents)
	memcpy(SOVEC(res)->contents, contents,
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

    if (0 <= i && i < SOVEC(sovec)->length)
	return SOVEC(sovec)->contents[i];
    else if (def != obj_Unbound)
	return def;
    else {
	error("No element %= in %=", index, sovec);
	return NULL;
    }
}

static obj_t dylan_sovec_element_setter(obj_t value, obj_t sovec, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < SOVEC(sovec)->length)
	SOVEC(sovec)->contents[i] = value;
    else
	error("No element %= in %=", index, sovec);

    return value;
}

static obj_t dylan_sovec_size(obj_t sovec)
{
    return make_fixnum(SOVEC(sovec)->length);
}

static obj_t dylan_vec_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;
    obj_t *ptr;

    if (!instancep(size, obj_IntegerClass))
	error("Bogus size: for make %=: %=", class, size);
    len = fixnum_value(size);

    res = make_vector(len, NULL);

    ptr = SOVEC(res)->contents;
    while (len-- > 0)
	*ptr++ = fill;

    return res;
}


/* Byte Vector support. */

obj_t obj_ByteVectorClass = NULL;

obj_t make_byte_vector(int length, unsigned char *contents)
{
    obj_t res = alloc(obj_ByteVectorClass,
		      sizeof(struct sovec) + length);

    BYTEVEC(res)->length = length;

    if (contents)
	memcpy(BYTEVEC(res)->contents, contents, length);

    return res;
}

static obj_t dylan_bytevec_element(obj_t bytevec, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < BYTEVEC(bytevec)->length)
	return make_fixnum(BYTEVEC(bytevec)->contents[i]);
    else if (def != obj_Unbound)
	return def;
    else {
	error("No element %= in %=", index, bytevec);
	return NULL;
    }
}

static obj_t dylan_bytevec_element_setter(obj_t value, obj_t bytevec,
					  obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < BYTEVEC(bytevec)->length)
	BYTEVEC(bytevec)->contents[i] = fixnum_value(value);
    else
	error("No element %= in %=", index, bytevec);

    return value;
}

static obj_t dylan_bytevec_size(obj_t bytevec)
{
    return make_fixnum(BYTEVEC(bytevec)->length);
}

static obj_t dylan_byte_vec_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;

    if (!obj_is_fixnum(size) || fixnum_value(size) < 0)
	error("Bogus size: for make %=: %=", class, size);
    len = fixnum_value(size);

    if (!obj_is_fixnum(fill) || fixnum_value(fill) < 0
	  || fixnum_value(fill) > 255)
	error("Bogus fill: for make %=: %=", class, fill);

    res = make_byte_vector(len, NULL);

    memset(BYTEVEC(res)->contents, fixnum_value(fill), len);

    return res;
}



/* Printing support. */

static void print_sovec(obj_t sovec)
{
    int len = SOVEC(sovec)->length;
    int i;

    printf("#[");
    for (i = 0; i < len; i++) {
	if (i)
	    printf(", ");
	prin1(SOVEC(sovec)->contents[i]);
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
    int len = SOVEC(v)->length;
    return transport(v, sizeof(struct sovec) + sizeof(obj_t)*len);
}

static int scav_bytevec(struct object *ptr)
{
    struct bytevec *v = (struct bytevec *)ptr;
    
    return sizeof(struct bytevec) + v->length;
}

static obj_t trans_bytevec(obj_t v)
{
    return transport(v, sizeof(struct bytevec) + BYTEVEC(v)->length);
}

void scavenge_vec_roots(void)
{
    scavenge(&obj_SimpleObjectVectorClass);
    scavenge(&obj_ByteVectorClass);
}



/* Initialization stuff. */

void make_vec_classes(void)
{
    obj_SimpleObjectVectorClass = make_builtin_class(scav_sovec, trans_sovec);
    obj_ByteVectorClass = make_builtin_class(scav_bytevec, trans_bytevec);
}

void init_vec_classes(void)
{
    init_builtin_class(obj_SimpleObjectVectorClass, "<simple-object-vector>",
		       obj_VectorClass, NULL);
    def_printer(obj_SimpleObjectVectorClass, print_sovec);
    init_builtin_class(obj_ByteVectorClass, "<byte-vector>",
		       obj_VectorClass, NULL);
}

void init_vec_functions(void)
{
    define_constant("vector",
		    make_raw_function("vector", 0, TRUE, obj_False, FALSE,
				      list1(obj_SimpleObjectVectorClass),
				      obj_False, dylan_vector));
    define_method("element",
		    list2(obj_SimpleObjectVectorClass, obj_IntegerClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		    obj_ObjectClass, dylan_sovec_element);
    define_method("element-setter",
		  list3(obj_ObjectClass,
			obj_SimpleObjectVectorClass,
			obj_IntegerClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, dylan_sovec_element_setter);
    define_method("size", list1(obj_SimpleObjectVectorClass),
		  FALSE, obj_False, FALSE, obj_IntegerClass, dylan_sovec_size);
    define_method("make", list1(singleton(obj_VectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_vec_make);
    define_method("make", list1(singleton(obj_SimpleObjectVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_vec_make);

    define_method("element",
		  list2(obj_ByteVectorClass, obj_IntegerClass),
		  FALSE, list1(pair(symbol("default"), obj_Unbound)),
		  FALSE, obj_IntegerClass, dylan_bytevec_element);
    define_method("element-setter",
		  list3(obj_IntegerClass,
			obj_ByteVectorClass,
			obj_IntegerClass),
		  FALSE, obj_False, FALSE,
		  obj_IntegerClass, dylan_bytevec_element_setter);
    define_method("size", list1(obj_ByteVectorClass),
		  FALSE, obj_False, FALSE,
		  obj_IntegerClass, dylan_bytevec_size);
    define_method("make", list1(singleton(obj_ByteVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), make_fixnum(0))),
		  FALSE, obj_ByteVectorClass, dylan_byte_vec_make);
}
