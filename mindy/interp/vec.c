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
* $Header: /scm/cvs/src/mindy/interp/vec.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file implements vectors.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

obj_t obj_SimpleVectorClass = NULL;
obj_t obj_SimpleObjectVectorClass = NULL;

obj_t make_vector(int length, obj_t *contents)
{
    obj_t res = alloc(obj_SimpleObjectVectorClass,
		      sizeof(struct sovec) + sizeof(obj_t)*(length-1));

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

    len = fixnum_value(check_type(size, obj_FixnumClass));

    if (len < 0)
	error("Bogus size: for make %=: %=", class, size);

    res = make_vector(len, NULL);

    ptr = SOVEC(res)->contents;
    while (len-- > 0)
	*ptr++ = fill;

    return res;
}

static obj_t dylan_sovec_fill(obj_t /* <simple-object-vector> */ vector,
			      obj_t value, obj_t first, obj_t last)
{
    int start = fixnum_value(check_type(first, obj_FixnumClass));
    int end;
    int size = SOVEC(vector)->length;
    obj_t *ptr;

    if (start < 0)
	error("Bogus start: for fill! %=: %=", vector, first);

    if (last == obj_Unbound)
	end = size;
    else {
	end = fixnum_value(check_type(last, obj_FixnumClass));
	if (end > size)
	    error("Bogus end: for fill! %=: %=", vector, last);
    }

    if (start > end)
	error("Bogus range for fill! %=: %d to %d", vector,
	      make_fixnum(start), make_fixnum(end));
    
    for (ptr = SOVEC(vector)->contents + start; start < end; start++)
	*ptr++ = value;
    return vector;
}

static obj_t dylan_sovec_copy(obj_t /* <simple-object-vector> */ vector,
			      obj_t first, obj_t last)
{
    int start = fixnum_value(check_type(first, obj_FixnumClass));
    int end;
    int size = SOVEC(vector)->length;

    if (start < 0)
	error("Bogus start: for copy-sequence %=: %=", vector, first);

    if (last == obj_Unbound)
	end = size;
    else {
	end = fixnum_value(check_type(last, obj_FixnumClass));
	if (end > size)
	    error("Bogus end: for copy-sequence %=: %=", vector, last);
    }

    if (start > end)
	error("Bogus range for copy-sequence %=: %d to %d", vector,
	      make_fixnum(start), make_fixnum(end));

    return make_vector(end - start, SOVEC(vector)->contents + start);
}


/* Byte Vector support. */

obj_t obj_ByteVectorClass = NULL;

obj_t make_byte_vector(int length, unsigned char *contents)
{
    obj_t res = alloc(obj_ByteVectorClass,
		      sizeof(struct bytevec) + length - 1);

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
    int len, byte;

    len = fixnum_value(check_type(size, obj_FixnumClass));
    if (len < 0)
	error("Bogus size: for make %=: %d", class, size);

    byte = fixnum_value(check_type(fill, obj_FixnumClass));
    if (byte < 0 || byte > 255)
	error("Bogus fill: for make %=: %d", class, fill);

    res = make_byte_vector(len, NULL);

    memset(BYTEVEC(res)->contents, byte, len);

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

    return sizeof(struct sovec) + sizeof(obj_t)*(len-1);
}

static obj_t trans_sovec(obj_t v)
{
    int len = SOVEC(v)->length;
    return transport(v, sizeof(struct sovec) + sizeof(obj_t)*(len-1), FALSE);
}

static int scav_bytevec(struct object *ptr)
{
    struct bytevec *v = (struct bytevec *)ptr;
    
    return sizeof(struct bytevec) + v->length - sizeof(v->contents);
}

static obj_t trans_bytevec(obj_t v)
{
    return transport(v,
		     sizeof(struct bytevec) + BYTEVEC(v)->length
		     - sizeof(((struct bytevec *)v)->contents),
		     TRUE);
}


/* Initialization stuff. */

void make_vec_classes(void)
{
    obj_SimpleVectorClass = make_abstract_class(TRUE);
    obj_SimpleObjectVectorClass = make_builtin_class(scav_sovec, trans_sovec);
    obj_ByteVectorClass = make_builtin_class(scav_bytevec, trans_bytevec);

    add_constant_root(&obj_SimpleObjectVectorClass);
    add_constant_root(&obj_ByteVectorClass);
}

void init_vec_classes(void)
{
    init_builtin_class(obj_SimpleVectorClass, "<simple-vector>",
		       obj_VectorClass, NULL);
    init_builtin_class(obj_SimpleObjectVectorClass, "<simple-object-vector>",
		       obj_SimpleVectorClass, NULL);
    def_printer(obj_SimpleObjectVectorClass, print_sovec);
    init_builtin_class(obj_ByteVectorClass, "<byte-vector>",
		       obj_VectorClass, NULL);
}

void init_vec_functions(void)
{
    define_constant("vector",
		    make_raw_function("vector", obj_Nil,
				      TRUE, obj_False, FALSE,
				      list1(obj_SimpleObjectVectorClass),
				      obj_False, dylan_vector));
    define_method("element",
		    list2(obj_SimpleObjectVectorClass, obj_FixnumClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		    obj_ObjectClass, dylan_sovec_element);
    define_method("element-setter",
		  list3(obj_ObjectClass,
			obj_SimpleObjectVectorClass,
			obj_FixnumClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, dylan_sovec_element_setter);
    define_method("size", list1(obj_SimpleObjectVectorClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass,
		  dylan_sovec_size);
    define_method("make", list1(singleton(obj_VectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_vec_make);
    define_method("make", list1(singleton(obj_SimpleVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_vec_make);
    define_method("make", list1(singleton(obj_SimpleObjectVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), obj_False)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_vec_make);

    define_method("element",
		  list2(obj_ByteVectorClass, obj_FixnumClass),
		  FALSE, list1(pair(symbol("default"), obj_Unbound)),
		  FALSE, obj_FixnumClass, dylan_bytevec_element);
    define_method("element-setter",
		  list3(obj_FixnumClass,
			obj_ByteVectorClass,
			obj_FixnumClass),
		  FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_bytevec_element_setter);
    define_method("size", list1(obj_ByteVectorClass),
		  FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_bytevec_size);
    define_method("fill!", list2(obj_SimpleObjectVectorClass, obj_ObjectClass),
		  FALSE, list2(pair(symbol("start"), make_fixnum(0)),
			       pair(symbol("end"), obj_Unbound)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_sovec_fill);
    define_method("copy-sequence", list1(obj_SimpleObjectVectorClass),
		  FALSE, list2(pair(symbol("start"), make_fixnum(0)),
			       pair(symbol("end"), obj_Unbound)),
		  FALSE, obj_SimpleObjectVectorClass, dylan_sovec_copy);
    define_method("make", list1(singleton(obj_ByteVectorClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), make_fixnum(0))),
		  FALSE, obj_ByteVectorClass, dylan_byte_vec_make);
}
