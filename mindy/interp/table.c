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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/table.c,v 1.1 1994/03/30 17:05:10 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

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

static obj_t valid_state = NULL;

static void dylan_object_hash(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 2;
    obj_t object = old_sp[1];

    assert(nargs == 1);

    old_sp[0] = (obj_t)(((unsigned long)MAX_FIXNUM)&((unsigned long)object));
    if (obj_is_fixnum(object))
	old_sp[1] = obj_False;
    else
	old_sp[1] = valid_state;

    do_return(thread, old_sp, old_sp);
}

static obj_t dylan_state_valid_p(obj_t state)
{
    if (state == valid_state)
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

    if (state1 == obj_False || state1 == valid_state)
	old_sp[1] = state2;
    else
	old_sp[1] = state1;

    thread->sp = old_sp + 2;
    do_return(thread, old_sp, old_sp);
}


/* GC routines. */

void scavenge_table_roots(void)
{
    valid_state = list1(obj_False);
}


/* Init routines. */

void init_table_functions(void)
{
    define_constant("object-hash",
		    make_raw_function("object-hash", 1, FALSE, obj_False,
				      list2(obj_IntegerClass, obj_ObjectClass),
				      obj_Nil, dylan_object_hash));
    define_function("state-valid?", list1(obj_ObjectClass), FALSE, obj_False,
		    obj_BooleanClass, dylan_state_valid_p);
    define_constant("merge-hash-codes",
		    make_raw_method("merge-hash-codes",
				    listn(4, obj_IntegerClass, obj_ObjectClass,
					  obj_IntegerClass, obj_ObjectClass),
				    FALSE,
				    list1(pair(keyword("ordered"), obj_False)),
				    list2(obj_IntegerClass, obj_ObjectClass),
				    obj_Nil, dylan_merge_hash_codes));
    define_constant("$permanent-hash-state", obj_False);

    valid_state = list1(obj_False);
}
