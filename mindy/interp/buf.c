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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/buf.c,v 1.7 1994/07/26 00:40:44 wlott Exp $
*
* This file implements buffers, a special byte vector used by streams.
*
\**********************************************************************/

#include <string.h>

#include "mindy.h"
#include "gc.h"
#include "coll.h"
#include "class.h"
#include "module.h"
#include "num.h"
#include "bool.h"
#include "obj.h"
#include "error.h"
#include "list.h"
#include "def.h"
#include "sym.h"
#include "type.h"
#include "vec.h"
#include "str.h"
#include "buf.h"

obj_t obj_BufferClass = NULL;

static obj_t dylan_buffer_make(obj_t class, obj_t size)
{
    int len;
    obj_t res;

    if (!instancep(size, obj_IntegerClass))
	error("Bogus size: for make %=: %=", class, size);
    len = fixnum_value(size);

    if (len < 0)
	error("Bogus size: for make %=: %=", class, size);

    res = alloc(obj_BufferClass, sizeof(struct buffer) + len);

    obj_ptr(struct buffer *, res)->length = len;

    return res;
}

static obj_t dylan_buffer_size(obj_t buffer)
{
    return make_fixnum(obj_ptr(struct buffer *, buffer)->length);
}

static obj_t dylan_buffer_element(obj_t buffer, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct buffer *, buffer)->length)
	return make_fixnum(buffer_data(buffer)[i]);
    else if (def != obj_Unbound)
	return def;
    else {
	error("No element %= in %=", index, buffer);
	return NULL;
    }
}

static obj_t dylan_buffer_element_setter(obj_t val, obj_t buffer, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct buffer *, buffer)->length)
	buffer_data(buffer)[i] = fixnum_value(val);
    else
	error("No element %= in %=", index, buffer);

    return val;
}

static obj_t dylan_memcpy(obj_t dst, obj_t dst_off, obj_t src, obj_t src_off,
			  obj_t count)
{
    if (dst == src)
	memmove(buffer_data(dst) + fixnum_value(dst_off),
		buffer_data(src) + fixnum_value(src_off),
		fixnum_value(count));
    else
	memcpy(buffer_data(dst) + fixnum_value(dst_off),
	       buffer_data(src) + fixnum_value(src_off),
	       fixnum_value(count));

    return dst;
}


/* GC stuff. */

static int scav_buffer(struct object *ptr)
{
    struct buffer *buffer = (struct buffer *)ptr;

    return sizeof(struct buffer) + buffer->length;
}

static obj_t trans_buffer(obj_t buffer)
{
    return transport(buffer,
		     sizeof(struct buffer)
		       + obj_ptr(struct buffer *, buffer)->length);
}

void scavenge_buffer_roots(void)
{
    scavenge(&obj_BufferClass);
}



/* Init stuff. */

void make_buffer_classes(void)
{
    obj_BufferClass = make_builtin_class(scav_buffer, trans_buffer);
}

void init_buffer_classes(void)
{
    init_builtin_class(obj_BufferClass, "<buffer>", obj_VectorClass, NULL);
}

void init_buffer_functions(void)
{
    obj_t u;

    define_method("element", list2(obj_BufferClass, obj_IntegerClass),
		  FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		  obj_IntegerClass, dylan_buffer_element);
    define_method("element-setter",
		  list3(obj_IntegerClass, obj_BufferClass, obj_IntegerClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass,
		  dylan_buffer_element_setter);
    define_method("size", list1(obj_BufferClass), FALSE, obj_False, FALSE,
		  obj_IntegerClass, dylan_buffer_size);
    define_method("make", list1(singleton(obj_BufferClass)), FALSE,
		  list1(pair(symbol("size"), make_fixnum(4096))),
		  FALSE, obj_BufferClass, dylan_buffer_make);

    u = type_union(obj_ByteStringClass, obj_ByteVectorClass);
    u = type_union(u, obj_BufferClass);
    define_method("copy-bytes",
		  listn(5, u, obj_IntegerClass, u, obj_IntegerClass,
			obj_IntegerClass),
		  FALSE, obj_False, FALSE, u, dylan_memcpy);
}
