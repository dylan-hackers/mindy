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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/list.c,v 1.2 1994/04/09 13:35:58 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdarg.h>

#include "mindy.h"
#include "gc.h"
#include "coll.h"
#include "class.h"
#include "obj.h"
#include "bool.h"
#include "num.h"
#include "thread.h"
#include "func.h"
#include "error.h"
#include "print.h"
#include "type.h"
#include "def.h"
#include "list.h"

obj_t obj_Nil = 0;
obj_t obj_ListClass = 0, obj_PairClass = 0, obj_EmptyListClass = 0;

obj_t pair(obj_t head, obj_t tail)
{
    obj_t res = alloc(obj_PairClass, sizeof(struct list));

    HEAD(res) = head;
    TAIL(res) = tail;

    return res;
}

obj_t list1(obj_t x)
{
    return pair(x, obj_Nil);
}

obj_t list2(obj_t x, obj_t y)
{
    return pair(x, list1(y));
}

obj_t list3(obj_t x, obj_t y, obj_t z)
{
    return pair(x, list2(y, z));
}

obj_t listn(int n, ...)
{
    va_list ap;
    obj_t res, *tail = &res;
    int i;

    va_start(ap, n);
    for (i = 0; i < n; i ++) {
	obj_t new = list1(va_arg(ap, obj_t));
	*tail = new;
	tail = &TAIL(new);
    }
    va_end(ap);

    *tail = obj_Nil;

    return res;
}


boolean memq(obj_t o, obj_t list)
{
    while (list != obj_Nil) {
	if (o == HEAD(list))
	    return TRUE;
	list = TAIL(list);
    }
    return FALSE;
}

obj_t nreverse(obj_t list)
{
    obj_t result = obj_Nil;

    while (list != obj_Nil) {
	obj_t t = TAIL(list);
	TAIL(list) = result;
	result = list;
	list = t;
    }
    return result;
}

int length(obj_t list)
{
    int count;

    for (count = 0; list != obj_Nil; list = TAIL(list))
	count++;

    return count;
}


/* Dylan routines. */

static obj_t dylan_head(obj_t list)
{
    return HEAD(list);
}

static obj_t dylan_head_setter(obj_t head, obj_t list)
{
    HEAD(list) = head;
    return head;
}

static obj_t dylan_tail(obj_t list)
{
    return TAIL(list);
}

static obj_t dylan_tail_setter(obj_t tail, obj_t list)
{
    TAIL(list) = tail;
    return tail;
}

static void dylan_list(struct thread *thread, int nargs)
{
    obj_t *ptr = thread->sp;
    obj_t result = obj_Nil;

    while (nargs-- > 0)
	result = pair(*--ptr, result);

    thread->sp = ptr;
    *--ptr = result;

    do_return(thread, ptr, ptr);
}

static obj_t dylan_list_size(obj_t list)
{
    obj_t slow, fast;
    int length;

    if (list == obj_Nil)
	return make_fixnum(0);
    if (object_class(list) != obj_PairClass)
	type_error(list, obj_ListClass);

    slow = list;
    fast = list;
    length = 0;

    do {
	fast = TAIL(fast);
	if (fast == obj_Nil)
	    return make_fixnum(length+1);
	if (object_class(fast) != obj_PairClass)
	    type_error(fast, obj_ListClass);
	fast = TAIL(fast);
	length += 2;
	if (fast == obj_Nil)
	    return make_fixnum(length);
	if (object_class(fast) != obj_PairClass)
	    type_error(fast, obj_ListClass);
	slow = TAIL(slow);
    } while (slow != fast);
    return obj_False;
}


/* Printer support. */

static void print_list(obj_t list)
{
    int len = 0;

    printf("#(");
    if (list != obj_Nil) {
	while (1) {
	    prin1(HEAD(list));
	    list = TAIL(list);
	    if (list == obj_Nil)
		break;
	    if (++len > 20) {
		printf(" ...");
		break;
	    }
	    if (!instancep(list, obj_ListClass)) {
		printf(" . ");
		prin1(list);
		break;
	    }
	    printf(", ");
	}
    }
    putchar(')');
}


/* GC support routines. */

static int scav_list(struct object *o)
{
    struct list *list = (struct list *)o;

    scavenge(&list->head);
    scavenge(&list->tail);

    return sizeof(struct list);
}

static obj_t trans_list(obj_t list)
{
    return transport(list, sizeof(struct list));
}

void scavenge_list_roots(void)
{
    scavenge(&obj_Nil);
    scavenge(&obj_ListClass);
    scavenge(&obj_PairClass);
    scavenge(&obj_EmptyListClass);
}


/* Init stuff. */

void make_list_classes(void)
{
    obj_ListClass = make_abstract_class();
    obj_PairClass = make_builtin_class(scav_list, trans_list);
    obj_EmptyListClass = make_builtin_class(scav_list, trans_list);
}

void init_nil(void)
{
    obj_Nil = alloc(obj_EmptyListClass, sizeof(struct list));
    HEAD(obj_Nil) = obj_Nil;
    TAIL(obj_Nil) = obj_Nil;
}

void init_list_classes(void)
{
    init_builtin_class(obj_ListClass, "<list>", obj_MutSeqClass, NULL);
    def_printer(obj_ListClass, print_list);
    init_builtin_class(obj_PairClass, "<pair>", obj_ListClass, NULL);
    init_builtin_class(obj_EmptyListClass, "<empty-list>",
		       obj_ListClass, NULL);
}

void init_list_functions(void)
{
    define_function("pair", list2(obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, obj_PairClass, pair);
    define_function("head", list1(obj_ListClass),
		    FALSE, obj_False, obj_ObjectClass, dylan_head);
    define_function("head-setter", list2(obj_ObjectClass, obj_ListClass),
		    FALSE, obj_False, obj_ObjectClass, dylan_head_setter);
    define_function("tail", list1(obj_ListClass),
		    FALSE, obj_False, obj_ObjectClass, dylan_tail);
    define_function("tail-setter", list2(obj_ObjectClass, obj_ListClass),
		    FALSE, obj_False, obj_ObjectClass, dylan_tail_setter);
    define_constant("list",
		    make_raw_function("list", 0, TRUE, obj_False,
				      obj_Nil, obj_ObjectClass, dylan_list));
    define_method("size", list1(obj_ListClass), FALSE, obj_False,
		  obj_IntegerClass, dylan_list_size);
}
