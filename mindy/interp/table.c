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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/table.c,v 1.15 1996/01/03 02:47:18 rgs Exp $
*
* This file implements support for <table>. Specifically, that means
* writing object-hash and merge-hash-codes, and defining
* $permanent-hash-state. As an extension for <equal-table> and 
* <value-table>, float-hash has been included for hashing floating point
* numbers without using object-hash.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

/* The largest fixnum prime */
#define REALLY_BIG_PRIME 1073741789

static void dylan_pointer_hash(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 2;
    obj_t object = old_sp[1];

    assert(nargs == 1);

    old_sp[0] = make_fixnum((unsigned long)object % REALLY_BIG_PRIME);

    if (valid_state == obj_False)
	valid_state = alloc(obj_HashStateClass, 
			    sizeof(struct hash_state));
    old_sp[1] = valid_state;

    do_return(thread, old_sp, old_sp);
}

static void dylan_float_hash(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 2;
    obj_t object = old_sp[1];
    obj_t class = obj_ptr(struct object *, object)->class;
    long double value;

    assert(nargs == 1);

    if (class == obj_SingleFloatClass)
	value = single_value(object);
    else if (class == obj_DoubleFloatClass)
	value = double_value(object);
    else if (class == obj_ExtendedFloatClass)
	value = extended_value(object);

    else 
	lose("I can't float-hash that!");

    old_sp[0] = make_fixnum((*((long *)(&value))) % REALLY_BIG_PRIME);
    old_sp[1] = permanent_state;

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

    if (ordered != obj_False) {
	id2 = (id2 << 5) | (id2 >> (sizeof(long)*CHAR_BIT-5));
	id1 = (id1 >> 2) | (id1 << (sizeof(long)*CHAR_BIT-2));
    }
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
    define_constant("pointer-hash",
		    make_raw_function("float-hash", 1, FALSE, obj_False,
				      FALSE,
				      list2(obj_FixnumClass,
					    obj_HashStateClass),
				      obj_False, dylan_pointer_hash));
    define_constant("float-hash",
		    make_raw_function("float-hash", 1, FALSE, obj_False,
				      FALSE,
				      list2(obj_FixnumClass,
					    obj_HashStateClass),
				      obj_False, dylan_float_hash));
    define_function("state-valid?", list1(obj_HashStateClass), FALSE,
		    obj_False, FALSE, obj_BooleanClass, dylan_state_valid_p);
    define_constant("merge-hash-codes",
		    make_raw_method("merge-hash-codes",
				    listn(4, obj_FixnumClass,
					  obj_HashStateClass,
					  obj_FixnumClass,
					  obj_HashStateClass),
				    FALSE,
				    list1(pair(symbol("ordered"), obj_False)),
				    FALSE, 
				    list2(obj_FixnumClass,
					  obj_HashStateClass),
				    obj_False, dylan_merge_hash_codes));

    permanent_state = alloc(obj_HashStateClass, sizeof(struct hash_state));
    define_constant("$permanent-hash-state", permanent_state);

    valid_state = obj_False;
}
