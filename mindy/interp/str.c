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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/str.c,v 1.2 1994/03/31 10:19:09 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "gc.h"
#include "coll.h"
#include "class.h"
#include "char.h"
#include "module.h"
#include "num.h"
#include "bool.h"
#include "obj.h"
#include "str.h"

obj_t obj_ByteStringClass = 0;

obj_t make_string(char *chars)
{
    int len = strlen(chars);
    obj_t res = alloc(obj_ByteStringClass, sizeof(struct string) + len + 1);

    obj_ptr(struct string *, res)->len = len;
    strcpy(obj_ptr(struct string *, res)->chars, chars);

    return res;
}

obj_t alloc_string(int len)
{
    obj_t res = alloc(obj_ByteStringClass, sizeof(struct string) + len + 1);

    obj_ptr(struct string *, res)->len = len;
    obj_ptr(struct string *, res)->chars[len] = '\0';

    return res;
}


/* Dylan routines. */

static obj_t dylan_str_element(obj_t str, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len)
	return int_char(string_chars(str)[i]);
    else if (def != obj_Unbound)
	return def;
    else
	error("No element ~S in ~S", index, str);
}

static obj_t dylan_str_element_setter(obj_t value, obj_t str, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len)
	string_chars(str)[i] = char_int(value);
    else
	error("No element ~S in ~S", index, str);

    return value;
}

static obj_t dylan_str_size(obj_t str)
{
    return make_fixnum(obj_ptr(struct string *, str)->len);
}

static obj_t dylan_str_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;
    unsigned char *ptr;
    int fill_char;

    if (!instancep(size, obj_IntegerClass))
	error("Bogus size: for make ~S: ~S", class, size);
    len = fixnum_value(size);

    if (!instancep(fill, obj_CharacterClass))
	error("Bogus fill: for make ~S: ~S", class, fill);
    fill_char = char_int(fill);

    res = alloc_string(len);

    ptr = string_chars(res);
    while (len-- > 0)
	*ptr++ = fill_char;
    *ptr = '\0';

    return res;
}


/* Printer support. */

static void print_string(obj_t str)
{
    int len = obj_ptr(struct string *, str)->len;
    unsigned char *ptr = string_chars(str);

    putchar('"');
    while (len-- > 0) {
	if (*ptr < ' ' || *ptr > '~')
	    printf("\\%03o", *ptr);
	else if (*ptr == '"')
	    printf("\\\"");
	else
	    putchar(*ptr);
	ptr++;
    }
    putchar('"');
}


/* GC stuff. */

static int scav_string(struct object *ptr)
{
    struct string *str = (struct string *)ptr;

    return sizeof(struct string) + str->len + 1;
}

static obj_t trans_string(obj_t string)
{
    return transport(string,
		     sizeof(struct string)
		       + obj_ptr(struct string *, string)->len
		       + 1);
}

void scavenge_str_roots(void)
{
    scavenge(&obj_ByteStringClass);
}


/* Init stuff. */

void make_str_classes(void)
{
    obj_ByteStringClass = make_builtin_class(scav_string, trans_string);
}

void init_str_classes(void)
{
    init_builtin_class(obj_ByteStringClass, "<byte-string>",
		       obj_VectorClass, obj_StringClass, NULL);
    def_printer(obj_ByteStringClass, print_string);
}

void init_str_functions(void)
{
    define_method("element",
		    list2(obj_ByteStringClass, obj_IntegerClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)),
		    obj_CharacterClass, dylan_str_element);
    define_method("element-setter",
		  list3(obj_CharacterClass,
			obj_ByteStringClass,
			obj_IntegerClass),
		  FALSE, obj_False,
		  obj_ObjectClass, dylan_str_element_setter);
    define_method("size", list1(obj_ByteStringClass),
		  FALSE, obj_False, obj_IntegerClass, dylan_str_size);
    define_method("make", list1(singleton(obj_StringClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), int_char('\0'))),
		  obj_ByteStringClass, dylan_str_make);
    define_method("make", list1(singleton(obj_ByteStringClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), int_char('\0'))),
		  obj_ByteStringClass, dylan_str_make);
}
