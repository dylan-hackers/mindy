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
* $Header: /scm/cvs/src/mindy/interp/buf.c,v 1.1 1998/05/03 19:55:11 andreas Exp $
*
* This file implements buffers, a special byte vector used by streams.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

#ifndef max
#   define max(a,b) ((a)>(b) ? (a) : (b))
#endif

obj_t obj_BufferClass = NULL;

static obj_t dylan_buffer_make(obj_t class, obj_t size, obj_t next, obj_t end)
{
    int len, start, stop;
    obj_t res;

    len = fixnum_value(check_type(size, obj_FixnumClass));
    start = fixnum_value(next);
    stop = fixnum_value(end);

    if (len < 0)
	error("Bogus size: for make %=: %d", class, size);
    if (start < 0 || start > len)
	error("Bogus buffer-next: for make %=: %d", class, next);
    if (stop < 0 || stop > len)
	error("Bogus buffer-end: for make %=: %d", class, end);

    res = alloc(obj_BufferClass, sizeof(struct buffer) 
	  + max(len - sizeof(((struct buffer *)res)->data),
		sizeof(((struct buffer *)res)->data)));

    BUF(res)->length = len;
    BUF(res)->buffer_next = start;
    BUF(res)->buffer_end = stop;

    return res;
}

static obj_t dylan_buffer_size(obj_t buffer)
{
    return make_fixnum(BUF(buffer)->length);
}

static obj_t dylan_buffer_next(obj_t buffer)
{
    return make_fixnum(BUF(buffer)->buffer_next);
}

static obj_t dylan_buffer_next_setter(obj_t val, obj_t buffer)
{
    long new = fixnum_value(val);

    if (new < 0 || new > BUF(buffer)->length)
	error("Invalid value for buffer-next: %d.  Must be between 0 and %d inclusive.",
	     val, make_fixnum(BUF(buffer)->length));

    BUF(buffer)->buffer_next = new;

    return val;
}

static obj_t dylan_buffer_end(obj_t buffer)
{
    return make_fixnum(BUF(buffer)->buffer_end);
}

static obj_t dylan_buffer_end_setter(obj_t val, obj_t buffer)
{
    long new = fixnum_value(val);

    if (new < 0 || new > BUF(buffer)->length)
	error("Invalid value for buffer-end: %d.  Must be between 0 and %d inclusive.",
	     val, make_fixnum(BUF(buffer)->length));

    BUF(buffer)->buffer_end = new;

    return val;
}

static obj_t dylan_buffer_element(obj_t buffer, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < BUF(buffer)->length)
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

    if (0 <= i && i < BUF(buffer)->length)
	buffer_data(buffer)[i] = fixnum_value(val);
    else
	error("No element %= in %=", index, buffer);

    return val;
}

static obj_t dylan_vec_vec_memcpy(obj_t dst, obj_t dst_off,
				  obj_t src, obj_t src_off,
				  obj_t count)
{
    memmove(vector_data(dst) + fixnum_value(dst_off),
	    vector_data(src) + fixnum_value(src_off),
	    fixnum_value(count));
    return dst;
}

static obj_t dylan_vec_buf_memcpy(obj_t dst, obj_t dst_off,
				  obj_t src, obj_t src_off,
				  obj_t count)
{
    memmove(vector_data(dst) + fixnum_value(dst_off),
	    buffer_data(src) + fixnum_value(src_off),
	    fixnum_value(count));
    return dst;
}

static obj_t dylan_buf_vec_memcpy(obj_t dst, obj_t dst_off,
				  obj_t src, obj_t src_off,
				  obj_t count)
{
    memmove(buffer_data(dst) + fixnum_value(dst_off),
	    vector_data(src) + fixnum_value(src_off),
	    fixnum_value(count));
    return dst;
}

static obj_t dylan_buf_buf_memcpy(obj_t dst, obj_t dst_off,
				  obj_t src, obj_t src_off,
				  obj_t count)
{
    memmove(buffer_data(dst) + fixnum_value(dst_off),
	    buffer_data(src) + fixnum_value(src_off),
	    fixnum_value(count));
    return dst;
}


static obj_t dylan_unicode_memcpy(obj_t dst, obj_t dst_off, obj_t src,
				  obj_t src_off, obj_t count)
{
    memmove(vector_data(dst) + 2*fixnum_value(dst_off),
	    vector_data(src) + 2*fixnum_value(src_off),
	    2*fixnum_value(count));
    return dst;
}


/* GC stuff. */

static int scav_buffer(struct object *ptr)
{
    struct buffer *buffer = (struct buffer *)ptr;

    return sizeof(struct buffer) + max(buffer->length - sizeof(buffer->data),
				       sizeof(buffer->data));
}

static obj_t trans_buffer(obj_t buffer)
{
    return transport(buffer,
		     sizeof(struct buffer)
		     + max(BUF(buffer)->length - sizeof(BUF(buffer)->data),
			   sizeof(BUF(buffer)->data)),
		     TRUE);
}


/* Init stuff. */

void make_buffer_classes(void)
{
    obj_BufferClass = make_builtin_class(scav_buffer, trans_buffer);
    add_constant_root(&obj_BufferClass);
}

void init_buffer_classes(void)
{
    init_builtin_class(obj_BufferClass, "<buffer>", obj_VectorClass, NULL);
}

void init_buffer_functions(void)
{
    obj_t u;

    define_method("element", list2(obj_BufferClass, obj_FixnumClass),
		  FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		  obj_FixnumClass, dylan_buffer_element);
    define_method("element-setter",
		  list3(obj_FixnumClass, obj_BufferClass,
			obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass,
		  dylan_buffer_element_setter);
    define_method("size", list1(obj_BufferClass), FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_buffer_size);
    define_method("buffer-next", list1(obj_BufferClass), FALSE, obj_False,
		  FALSE, obj_FixnumClass, dylan_buffer_next);
    define_method("buffer-next-setter", 
		  list2(obj_FixnumClass, obj_BufferClass), FALSE, obj_False,
		  FALSE, obj_FixnumClass, dylan_buffer_next_setter);
    define_method("buffer-end", list1(obj_BufferClass), FALSE, obj_False,
		  FALSE, obj_FixnumClass, dylan_buffer_end);
    define_method("buffer-end-setter", 
		  list2(obj_FixnumClass, obj_BufferClass), FALSE, obj_False,
		  FALSE, obj_FixnumClass, dylan_buffer_end_setter);
    define_method("make", list1(singleton(obj_BufferClass)), FALSE,
		  list3(pair(symbol("size"), make_fixnum(4096)),
			pair(symbol("next"), make_fixnum(0)),
			pair(symbol("end"), make_fixnum(0))),
		  FALSE, obj_BufferClass, dylan_buffer_make);

    u = type_union(obj_ByteStringClass, obj_ByteVectorClass);

    define_method("copy-bytes",
		  listn(5, u, obj_FixnumClass, u,
			obj_FixnumClass, obj_FixnumClass),
		  FALSE, obj_False, FALSE, u, dylan_vec_vec_memcpy);

    define_method("copy-bytes",
		  listn(5, u, obj_FixnumClass, obj_BufferClass,
			obj_FixnumClass, obj_FixnumClass),
		  FALSE, obj_False, FALSE, u, dylan_vec_buf_memcpy);

    define_method("copy-bytes",
		  listn(5, obj_BufferClass, obj_FixnumClass, u,
			obj_FixnumClass, obj_FixnumClass),
		  FALSE, obj_False, FALSE, u, dylan_buf_vec_memcpy);

    define_method("copy-bytes",
		  listn(5, obj_BufferClass, obj_FixnumClass, obj_BufferClass,
			obj_FixnumClass, obj_FixnumClass),
		  FALSE, obj_False, FALSE, u, dylan_buf_buf_memcpy);

    define_method("copy-bytes",
		  listn(5, obj_UnicodeStringClass, obj_FixnumClass,
			obj_UnicodeStringClass,
			obj_FixnumClass, obj_FixnumClass),
		  FALSE, obj_False, FALSE, u, dylan_unicode_memcpy);

}
