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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/table.c,v 1.8 1994/06/11 02:23:47 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <limits.h>

#include "mindy.h"
#include "thread.h"
#include "func.h"
#include "def.h"
#include "list.h"
#include "bool.h"
#include "num.h"
#include "obj.h"
#include "sym.h"
#include "gc.h"
#include "class.h"
#include "print.h"
#include "table.h"

struct hash_state {
    obj_t class;
};

static obj_t obj_HashStateClass = NULL;
static obj_t permanent_state = NULL;
static obj_t valid_state = NULL;

static void dylan_object_hash(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 2;
    obj_t object = old_sp[1];

    assert(nargs == 1);

    old_sp[0] = (obj_t)(((unsigned long)MAX_FIXNUM)&((unsigned long)object));
    if (obj_is_fixnum(object))
	old_sp[1] = permanent_state;
    else {
	if (valid_state == obj_False)
	    valid_state = alloc(obj_HashStateClass, sizeof(struct hash_state));
	old_sp[1] = valid_state;
    }

    do_return(thread, old_sp, old_sp);
}

static obj_t dylan_state_valid_p(obj_t state)
{
    if (state == permanent_state || state == valid_state)
	return obj_True;
    else
	return obj_False;
}

static void dylan_merge_hash_codes(obj_t self, struct thread *thread,
				   obj_t *args)
{
    unsigned long id1 = fixnum_value(args[0]);
    obj_t state1 = args[1];
    unsigned long id2 = fixnum_value(args[2]);
    obj_t state2 = args[3];
    obj_t ordered = args[4];
    obj_t *old_sp = args-1;

    if (ordered != obj_False)
	id2 = (id2 << 5) | (id2 >> (sizeof(long)*CHAR_BIT-5));
    old_sp[0] = make_fixnum(id1 ^ id2);

    if (state1 == permanent_state)
	old_sp[1] = state2;
    else if (state2 == permanent_state)
	old_sp[1] = state1;
    else if (state1 == valid_state)
	old_sp[1] = state2;
    else
	old_sp[1] = state1;

    thread->sp = old_sp + 2;
    do_return(thread, old_sp, old_sp);
}


/* Printing routine. */

static void print_state(obj_t state)
{
    if (state == permanent_state)
	printf("{permanent hash state}");
    else if (state == valid_state)
	printf("{valid hash state}");
    else
	printf("{invalid hash state}");
}


/* GC routines. */

static int scav_state(struct object *o)
{
    return sizeof(struct hash_state);
}

static obj_t trans_state(obj_t state)
{
    return transport(state, sizeof(struct hash_state));
}

void scavenge_table_roots(void)
{
    scavenge(&obj_HashStateClass);
    scavenge(&permanent_state);
    valid_state = NULL;
}    

void table_gc_hook(void)
{
    valid_state = obj_False;
}


/* Init routines. */

void make_table_classes(void)
{
    obj_HashStateClass = make_builtin_class(scav_state, trans_state);
}

void init_table_classes(void)
{
    init_builtin_class(obj_HashStateClass, "<hash-state>",
		       obj_ObjectClass, NULL);
    def_printer(obj_HashStateClass, print_state);
}

void init_table_functions(void)
{
    define_constant("object-hash",
		    make_raw_function("object-hash", 1, FALSE, obj_False,
				      FALSE,
				      list2(obj_IntegerClass,
					    obj_HashStateClass),
				      obj_False, dylan_object_hash));
    define_function("state-valid?", list1(obj_HashStateClass), FALSE,
		    obj_False, FALSE, obj_BooleanClass, dylan_state_valid_p);
    define_constant("merge-hash-codes",
		    make_raw_method("merge-hash-codes",
				    listn(4, obj_IntegerClass,
					  obj_HashStateClass,
					  obj_IntegerClass,
					  obj_HashStateClass),
				    FALSE,
				    list1(pair(symbol("ordered"), obj_False)),
				    FALSE,
				    list2(obj_IntegerClass,
					  obj_HashStateClass),
				    obj_False, dylan_merge_hash_codes));

    permanent_state = alloc(obj_HashStateClass, sizeof(struct hash_state));
    define_constant("$permanent-hash-state", permanent_state);

    valid_state = obj_False;
}
