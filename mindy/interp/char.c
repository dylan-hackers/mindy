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
* $Header: /scm/cvs/src/mindy/interp/char.c,v 1.1 1998/05/03 19:55:11 andreas Exp $
*
* This file implements characters.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "obj.h"
#include "gc.h"
#include "class.h"
#include "num.h"
#include "bool.h"
#include "error.h"
#include "print.h"
#include "list.h"
#include "type.h"
#include "def.h"
#include "char.h"

#define num_characters 65536

obj_t obj_CharacterClass;
obj_t obj_ByteCharacterClass;
static obj_t obj_Characters[num_characters];

/* C integer to Dylan character.  Does no error checking. */

obj_t int_char(int c)
{
    obj_t res = obj_Characters[c];

    if (res == NULL) {
	if (c < 256)
	    res = alloc(obj_ByteCharacterClass, sizeof(struct character));
	else
	    res = alloc(obj_CharacterClass, sizeof(struct character));
	obj_ptr(struct character *, res)->unicode_value = c;
	obj_Characters[c] = res;
	add_constant_root(obj_Characters + c);
    }

    return res;
}


/* Dylan routines. */

static obj_t fixnum_as_char(obj_t class, obj_t i)
{
    int c = fixnum_value(i);

    if (0 <= c && c < num_characters)
	return int_char(c);
    else {
	error("Can't make a character out of %=", i);
	return NULL;
    }
}

static obj_t fixnum_as_byte_char(obj_t class, obj_t i)
{
    int c = fixnum_value(i);

    if (0 <= c && c < 256)
	return int_char(c);
    else {
	error("Can't make a byte character out of %=", i);
	return NULL;
    }
}


static obj_t char_as_fixnum(obj_t class, obj_t c)
{
    return make_fixnum(char_int(c));
}

static obj_t char_less(obj_t /* <character> */ c1, obj_t /* <character> */ c2)
{
    if (char_int(c1) < char_int(c2))
	return obj_True;
    else
	return obj_False;
}


/* Printing stuff. */

static void print_char(obj_t obj)
{
    int c = char_int(obj);

    if (c > 255)
	printf("'\\{#x%x}'", c);
    else if (c < ' ' || c > '~')
	printf("'\\%03o'", c);
    else if (c == '\'')
	printf("'\\''");
    else
	printf("'%c'", c);
}


/* GC stuff. */

static int scav_char(struct object *ptr)
{
    return sizeof(struct character);
}

static obj_t trans_char(obj_t c)
{
    return transport(c, sizeof(struct character), TRUE);
}


/* Init stuff. */

void make_char_classes()
{
    obj_CharacterClass = make_builtin_class(scav_char, trans_char);
    obj_ByteCharacterClass = make_builtin_class(scav_char, trans_char);
    /* Since characters and byte characters actually have identical 
       C structures, they can use the same functions. */

    add_constant_root(&obj_CharacterClass);
    add_constant_root(&obj_ByteCharacterClass);
}

void init_char_classes()
{
    init_builtin_class(obj_CharacterClass, "<character>",
		       obj_ObjectClass, NULL);
    def_printer(obj_CharacterClass, print_char);
       /* This will also work for byte characters */
    init_builtin_class(obj_ByteCharacterClass, "<byte-character>",
		       obj_CharacterClass, NULL);
}

void init_char_functions()
{
    define_method("as", list2(singleton(obj_CharacterClass), obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_CharacterClass, fixnum_as_char);
    define_method("as",
		  list2(singleton(obj_ByteCharacterClass), obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_ByteCharacterClass, 
		  fixnum_as_byte_char);
    define_method("as", list2(singleton(obj_IntegerClass), obj_CharacterClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass, char_as_fixnum);
    define_method("as", list2(singleton(obj_FixnumClass), obj_CharacterClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass, char_as_fixnum);
    define_method("<", list2(obj_CharacterClass, obj_CharacterClass),
		  FALSE, obj_False, FALSE, obj_BooleanClass, char_less);
}
