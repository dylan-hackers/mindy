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
* $Header: /scm/cvs/src/mindy/interp/str.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file implements strings.
*
\**********************************************************************/

#include "../compat/std-c.h"

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
#include "error.h"
#include "type.h"
#include "print.h"
#include "list.h"
#include "def.h"
#include "sym.h"

obj_t obj_ByteStringClass = 0;
obj_t obj_UnicodeStringClass = 0;

/*
   There is no corresponding make_unicode_string because it is worthless
   for interfacing with C code.
*/
obj_t make_byte_string(char *chars)
{
    int len = strlen(chars);
    obj_t res = alloc(obj_ByteStringClass, sizeof(struct string) 
		      + len + 1 - sizeof(((struct string *)res)->chars));

    obj_ptr(struct string *, res)->len = len;
    strcpy((char *)obj_ptr(struct string *, res)->chars, chars);

    return res;
}


obj_t alloc_byte_string(int len)
{
    obj_t res = alloc(obj_ByteStringClass, sizeof(struct string)
		      + len + 1 - sizeof(((struct string *)res)->chars));

    obj_ptr(struct string *, res)->len = len;
    obj_ptr(struct string *, res)->chars[len] = '\0';

    return res;
}

obj_t alloc_unicode_string(int len)
{
    obj_t res = alloc(obj_UnicodeStringClass, 
		      sizeof(struct string) 
		      + 2*(len+1) - sizeof(((struct string *)res)->chars));

    obj_ptr(struct string *, res)->len = len;
    obj_ptr(struct string *, res)->chars[2*len] = '\0';
    obj_ptr(struct string *, res)->chars[2*len + 1] = '\0';

    return res;
}

/* Dylan routines. */

static obj_t dylan_byte_str_element(obj_t str, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len)
	return int_char(string_chars(str)[i]);
    else if (def != obj_Unbound)
	return def;
    else {
	error("No element %= in %=", index, str);
	return NULL;
    }
}

static obj_t dylan_unicode_str_element(obj_t str, obj_t index, obj_t def)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len)
	return int_char(get_unichar(str, i));
    else if (def != obj_Unbound)
	return def;
    else {
	error("No element %= in %=", index, str);
	return NULL;
    }
}

static obj_t dylan_byte_str_element_setter(obj_t value, obj_t str, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len)
	string_chars(str)[i] = char_int(value);
    else
	error("No element %= in %=", index, str);

    return value;
}

static obj_t dylan_unicode_str_element_setter(obj_t value,
					      obj_t str, obj_t index)
{
    int i = fixnum_value(index);

    if (0 <= i && i < obj_ptr(struct string *, str)->len) {
	string_chars(str)[2*i]     /* High byte */
	    = (obj_ptr(struct character *, value)->unicode_value) >> 8;
	string_chars(str)[2*i + 1]  /* Low byte */
	    = (obj_ptr(struct character *, value)->unicode_value) & 255;
    }
    else
	error("No element %= in %=", index, str);

    return value;
}

static obj_t dylan_str_size(obj_t str)
{
    return make_fixnum(obj_ptr(struct string *, str)->len);
}

static obj_t dylan_byte_str_equal(obj_t /* <byte-string> */ str1,
				  obj_t /* <byte-string> */ str2)
{
    struct string *s1 = obj_ptr(struct string *, str1);
    struct string *s2 = obj_ptr(struct string *, str2);

    if ((s1->len == s2->len) && (strcmp(s1->chars, s2->chars) == 0))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_byte_str_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;
    unsigned char *ptr;
    int fill_char;

    len = fixnum_value(check_type(size, obj_FixnumClass));

    if (len < 0)
	error("Bogus size: for make %=: %=", class, size);

    fill_char = char_int(check_type(fill, obj_ByteCharacterClass));

    res = alloc_byte_string(len);

    ptr = string_chars(res);
    while (len-- > 0)
	*ptr++ = fill_char;
    *ptr = '\0';

    return res;
}

static obj_t dylan_unicode_str_make(obj_t class, obj_t size, obj_t fill)
{
    obj_t res;
    int len;
    int i;
    int fill_char;

    len = fixnum_value(check_type(size, obj_FixnumClass));

    if (len < 0)
	error("Bogus size: for make %=: %=", class, size);

    fill_char = char_int(check_type(fill, obj_CharacterClass));

    res = alloc_unicode_string(len);

    for (i=0; i<len; i++) {
	string_chars(res)[2*i] = (fill_char >> 8);
	string_chars(res)[2*i + 1] = (fill_char & 255);
    }
    string_chars(res)[2*len] = (fill_char >> 8);
    string_chars(res)[2*len + 1] = (fill_char & 255);

    return res;
}


/* Printer support. */

static void print_byte_string(obj_t str)
{
    int len = obj_ptr(struct string *, str)->len;
    unsigned char *ptr = string_chars(str);

    putchar('"');
    while (len-- > 0) {
        if (*ptr == '\n')
	    printf("\\n");
	else if (*ptr < ' ' || *ptr > '~')
	    printf("\\%03o", *ptr);
	else if (*ptr == '"')
	    printf("\\\"");
	else
	    putchar(*ptr);
	ptr++;
    }
    putchar('"');
}

static void print_unicode_string(obj_t str)
{
    int len = obj_ptr(struct string *, str)->len;
    int i = 0;
    int c;

    putchar('"');
    for (i=0; i<len; i++) {
	c = get_unichar(str, i);
	if (c == '\n')
	    printf("\\n");
	else if (c > 255)
	    printf("\\{#x%x}", c);
	else if (c < ' ' || c > '~')
	    printf("\\%03o", c);
	else if (c == '"')
	    printf("\\\"");
	else
	    putchar(c);
    }
    putchar('"');
}


/* GC stuff. */

static int scav_byte_string(struct object *ptr)
{
    struct string *str = (struct string *)ptr;

    return sizeof(struct string) + str->len + 1 - sizeof(str->chars);
}

static int scav_unicode_string(struct object *ptr)
{
    struct string *str = (struct string *)ptr;

    return sizeof(struct string)
	+ 2*(str->len + 1) - sizeof(str->chars);
}

static obj_t trans_byte_string(obj_t string)
{
    return transport(string,
		     sizeof(struct string)
		     + obj_ptr(struct string *, string)->len + 1
		     - sizeof(((struct string *)string)->chars),
		     TRUE);
}

static obj_t trans_unicode_string(obj_t string)
{
    return transport(string,
		     sizeof(struct string) 
		     + 2 * (obj_ptr(struct string *, string)->len + 1)
		     - sizeof(((struct string *)string)->chars),
		     TRUE);
}


/* Init stuff. */

void make_str_classes(void)
{
    obj_ByteStringClass = make_builtin_class(scav_byte_string,
					     trans_byte_string);
    obj_UnicodeStringClass = make_builtin_class(scav_unicode_string, 
						trans_unicode_string);

    add_constant_root(&obj_ByteStringClass);
    add_constant_root(&obj_UnicodeStringClass);
}

void init_str_classes(void)
{
    init_builtin_class(obj_ByteStringClass, "<byte-string>",
		       obj_VectorClass, obj_StringClass, NULL);
    init_builtin_class(obj_UnicodeStringClass, "<unicode-string>",
		       obj_VectorClass, obj_StringClass, NULL);
    def_printer(obj_ByteStringClass, print_byte_string);
    def_printer(obj_UnicodeStringClass, print_unicode_string);
}

void init_str_functions(void)
{
    define_method("element",
		    list2(obj_ByteStringClass, obj_FixnumClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		    obj_ByteCharacterClass, dylan_byte_str_element);
    define_method("element",
		    list2(obj_UnicodeStringClass, obj_FixnumClass),
		    FALSE, list1(pair(symbol("default"), obj_Unbound)), FALSE,
		    obj_CharacterClass, dylan_unicode_str_element);
    define_method("element-setter",
		  list3(obj_ByteCharacterClass,
			obj_ByteStringClass,
			obj_FixnumClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, dylan_byte_str_element_setter);
    define_method("element-setter",
		  list3(obj_CharacterClass,
			obj_UnicodeStringClass,
			obj_FixnumClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, dylan_unicode_str_element_setter);

    /* size is the same for both <byte-string> and <unicode-string>, 
       but not for the general user defined instance of <string>.
    */
    define_method("size", list1(obj_ByteStringClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass, dylan_str_size);
    define_method("size", list1(obj_UnicodeStringClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass, dylan_str_size);

    define_method("=", list2(obj_ByteStringClass, obj_ByteStringClass),
		  FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_byte_str_equal);

    /* make(<string>) returns a <byte-string>, even if fill happens to 
       be a unicode character.
    */
    define_method("make", list1(singleton(obj_StringClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), int_char('\0'))),
		  FALSE, obj_ByteStringClass, dylan_byte_str_make);
    define_method("make", list1(singleton(obj_ByteStringClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), int_char('\0'))),
		  FALSE, obj_ByteStringClass, dylan_byte_str_make);
    define_method("make", list1(singleton(obj_UnicodeStringClass)), FALSE,
		  list2(pair(symbol("size"), make_fixnum(0)),
			pair(symbol("fill"), int_char('\0'))),
		  FALSE, obj_UnicodeStringClass, dylan_unicode_str_make);
}
