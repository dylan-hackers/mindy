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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/char.c,v 1.6 1994/06/27 16:31:29 wlott Exp $
*
* This file implements characters.
*
\**********************************************************************/

#include <stdio.h>

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

obj_t obj_CharacterClass;
static obj_t obj_Characters[256];

obj_t int_char(int c)
{
    obj_t res = obj_Characters[c];

    if (res == NULL) {
	res = alloc(obj_CharacterClass, sizeof(struct character));
	obj_ptr(struct character *, res)->c = c;
	obj_Characters[c] = res;
    }

    return res;
}


/* Dylan routines. */

static obj_t int_as_char(obj_t class, obj_t i)
{
    int c = fixnum_value(i);

    if (0 <= c && c < 256)
	return int_char(c);
    else {
	error("Can't make a character out of %=", i);
	return NULL;
    }
}

static obj_t char_as_int(obj_t class, obj_t c)
{
    return make_fixnum(char_int(c));
}


/* Printing stuff. */

static void print_char(obj_t obj)
{
    int c = char_int(obj);

    if (c < ' ' || c > '~')
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
    return transport(c, sizeof(struct character));
}

void scavenge_char_roots(void)
{
    int i;

    scavenge(&obj_CharacterClass);

    for (i = 0; i < 256; i++)
	if (obj_Characters[i] != NULL)
	    scavenge(obj_Characters + i);
}


/* Init stuff. */

void make_char_classes()
{
    obj_CharacterClass = make_builtin_class(scav_char, trans_char);
}

void init_char_classes()
{
    init_builtin_class(obj_CharacterClass, "<character>",
		       obj_ObjectClass, NULL);
    def_printer(obj_CharacterClass, print_char);
}

void init_char_functions()
{
    define_method("as", list2(singleton(obj_CharacterClass), obj_IntegerClass),
		  FALSE, obj_False, FALSE, obj_CharacterClass, int_as_char);
    define_method("as", list2(singleton(obj_IntegerClass), obj_CharacterClass),
		  FALSE, obj_False, FALSE, obj_IntegerClass, char_as_int);
}
