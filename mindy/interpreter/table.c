/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
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
    boolean valid;
    int volatility;
};

#define STATE(obj) obj_ptr(struct hash_state *, obj)

static obj_t obj_HashStateClass = NULL;
static obj_t permanent_state = NULL;

obj_t make_hash_state(int volatility)
{
    obj_t state = alloc(obj_HashStateClass, sizeof(struct hash_state));

    STATE(state)->valid = TRUE;
    STATE(state)->volatility = volatility;

    return state;
}

void invalidate_hash_state(obj_t state)
{
    STATE(state)->valid = FALSE;
}

/* The largest fixnum prime */
#define REALLY_BIG_PRIME 1073741789

static void dylan_pointer_hash(struct thread *thread, int nargs)
{
    obj_t *old_sp = thread->sp - 2;
    obj_t object = old_sp[1];

    assert(nargs == 1);

    /* We compute the hash state first and *then* the hash id.  If we
       did it in the more natural order, there could be a GC right
       after we compute the hash id, and we'd be returning an outdated
       hash id without even knowing it.
       */
    old_sp[1] = pointer_hash_state(object);
    old_sp[0] = make_fixnum((unsigned long)object % REALLY_BIG_PRIME);

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
    if (STATE(state)->valid)
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_merge_hash_ids(obj_t id1_arg, obj_t id2_arg, obj_t ordered)
{
    unsigned long id1 = fixnum_value(id1_arg);
    unsigned long id2 = fixnum_value(id2_arg);

    if (ordered != obj_False) {
	id2 = (id2 << 5) | (id2 >> (sizeof(long)*CHAR_BIT-5));
	id1 = (id1 >> 2) | (id1 << (sizeof(long)*CHAR_BIT-2));
    }
    return make_fixnum(id1 ^ id2);
}

static obj_t dylan_merge_hash_states(obj_t state1, obj_t state2)
{
    if (STATE(state1)->volatility > STATE(state2)->volatility)
        return state1;
    else
        return state2;
}


/* Printing routine. */

static void print_state(obj_t state)
{
    if (state == permanent_state)
	printf("{permanent hash state}");
    else if (STATE(state)->valid)
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
    return transport(state, sizeof(struct hash_state), TRUE);
}


/* Init routines. */

void make_table_classes(void)
{
    obj_HashStateClass = make_builtin_class(scav_state, trans_state);
    add_constant_root(&obj_HashStateClass);
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
		    make_raw_function("pointer-hash", list1(obj_ObjectClass),
				      FALSE, obj_False,
				      FALSE,
				      list2(obj_FixnumClass,
					    obj_HashStateClass),
				      obj_False, dylan_pointer_hash));
    define_constant("float-hash",
		    make_raw_function("float-hash", list1(obj_FloatClass),
				      FALSE, obj_False,
				      FALSE,
				      list2(obj_FixnumClass,
					    obj_HashStateClass),
				      obj_False, dylan_float_hash));
    define_function("state-valid?", list1(obj_HashStateClass), FALSE,
		    obj_False, FALSE, obj_BooleanClass, dylan_state_valid_p);
    define_function("merge-hash-ids",
		    listn(2, obj_FixnumClass, obj_FixnumClass), FALSE,
		    list1(pair(symbol("ordered"), obj_False)), FALSE, 
		    obj_FixnumClass, dylan_merge_hash_ids);
    define_function("merge-hash-states",
		    listn(2, obj_HashStateClass, obj_HashStateClass), FALSE,
		    obj_False, FALSE, 
		    obj_HashStateClass, dylan_merge_hash_states);
    permanent_state = make_hash_state(0);
    define_constant("$permanent-hash-state", permanent_state);
    add_constant_root(&permanent_state);
}
