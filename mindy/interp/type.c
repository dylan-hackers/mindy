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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/type.c,v 1.11 1994/04/24 21:41:28 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "gc.h"
#include "obj.h"
#include "type.h"
#include "class.h"
#include "bool.h"
#include "list.h"
#include "num.h"
#include "module.h"
#include "sym.h"
#include "error.h"
#include "print.h"
#include "def.h"

obj_t obj_TypeClass = 0;
static obj_t obj_SingletonClass, obj_LimIntClass;
static obj_t obj_SubclassClass, obj_UnionClass;

struct singleton {
    obj_t class;
    enum type_Id type_id;
    obj_t object;
};

struct subclass {
    obj_t class;
    enum type_Id type_id;
    obj_t of;
};

struct lim_int {
    obj_t class;
    enum type_Id type_id;
    obj_t min;
    obj_t max;
};

struct union_type {
    obj_t class;
    enum type_Id type_id;
    obj_t members;
};



/* instancep */

static inline boolean singleton_instancep(obj_t thing, obj_t type)
{
    return idp(thing, obj_ptr(struct singleton *, type)->object);
}

static inline boolean class_instancep(obj_t thing, obj_t class)
{
    obj_t thing_class = object_class(thing);

    static obj_t c1_cache[32], c2_cache[32];
    static boolean result_cache[32];
    register int cacheloc = (((int)thing_class ^ (int)class) >> 4) & 31;

    if (c1_cache[cacheloc] == thing_class && c2_cache[cacheloc] == class)
	return result_cache[cacheloc];

    c1_cache[cacheloc] = thing_class;
    c2_cache[cacheloc] = class;
    return result_cache[cacheloc] = subtypep(thing_class, class);
}

static inline boolean subclass_instancep(obj_t thing, obj_t subclass)
{
    return instancep(thing, obj_ClassClass)
	&& subtypep(thing, obj_ptr(struct subclass *, subclass)->of);
}

static inline boolean lim_int_instancep(obj_t thing, obj_t lim_int)
{
    obj_t min, max;

    if (!obj_is_fixnum(thing))
	return FALSE;

    min = obj_ptr(struct lim_int *, lim_int)->min;
    if (min != obj_False && (long)thing < (long)min)
	return FALSE;

    max = obj_ptr(struct lim_int *, lim_int)->max;
    if (max != obj_False && (long)max < (long)thing)
	return FALSE;

    return TRUE;
}

static inline boolean union_instancep(obj_t thing, obj_t u)
{
    obj_t remaining;

    for (remaining = obj_ptr(struct union_type *, u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(thing, HEAD(remaining)))
	    return TRUE;
    return FALSE;
}

boolean instancep(obj_t thing, obj_t type)
{
    enum type_Id type_id = obj_ptr(struct type *, type)->type_id;

    switch (type_id) {
    case id_Singleton:
	return singleton_instancep(thing, type);
	break;
    case id_Class:
	return class_instancep(thing, type);
	break;
    case id_SubClass:
	return subclass_instancep(thing, type);
	break;
    case id_LimInt:
	return lim_int_instancep(thing, type);
	break;
    case id_Union:
	return union_instancep(thing, type);
	break;
    }
    lose("instancep dispatch didn't do anything.");
    return FALSE;
}


/* subtypep */

static inline boolean sing_sing_subtypep(obj_t sing1, obj_t sing2)
{
    return idp(obj_ptr(struct singleton *, sing1)->object,
	       obj_ptr(struct singleton *, sing2)->object);
}

static inline boolean sing_type_subtypep(obj_t sing, obj_t type)
{
    return instancep(obj_ptr(struct singleton *, sing)->object, type);
}

static inline boolean class_sing_subtypep(obj_t class, obj_t sing)
{
    obj_t o = obj_ptr(struct singleton *, sing)->object;

    if ((o == obj_Nil || o == obj_True || o == obj_False)
	&& object_class(o) == class)
	return TRUE;
    else
	return FALSE;
}

static inline boolean class_class_subtypep(obj_t class1, obj_t class2)
{
    obj_t cpl = obj_ptr(struct class *, class1)->cpl;
    obj_t remaining;

    if (cpl == obj_False)
	error("Attempt to use ~S before it was initialized.", class1);

    for (remaining = cpl; remaining != obj_Nil; remaining = TAIL(remaining))
	if (HEAD(remaining) == class2) {
	    return TRUE;
	}
    return FALSE;
}

static inline boolean class_subclass_subtypep(obj_t class, obj_t subclass)
{
    return class == obj_ClassClass
	    && obj_ptr(struct subclass *, subclass)->of == obj_ClassClass;
}

static inline boolean never_subtypep(obj_t type1, obj_t type2)
{
    return FALSE;
}

static inline boolean subclass_type_subtypep(obj_t sub, obj_t type)
{
    obj_t class = obj_ptr(struct subclass *, sub)->of;

    if (obj_ptr(struct class *, class)->all_subclasses == obj_Nil)
	return instancep(class, type);
    else
	return subtypep(obj_ClassClass, type);
}

static inline boolean subclass_subclass_subtypep(obj_t sub1, obj_t sub2)
{
    return subtypep(obj_ptr(struct subclass *, sub1)->of,
		    obj_ptr(struct subclass *, sub2)->of);
}

static inline boolean lim_sing_subtypep(obj_t lim, obj_t sing)
{
    obj_t min;

    min = obj_ptr(struct lim_int *, lim)->min;

    if (min == obj_False || min != obj_ptr(struct lim_int *, lim)->max
	|| min != obj_ptr(struct singleton *, sing)->object)
	return FALSE;
    else
	return TRUE;
}

static inline boolean lim_lim_subtypep(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = obj_ptr(struct lim_int *, lim1)->min;
    max1 = obj_ptr(struct lim_int *, lim1)->max;
    min2 = obj_ptr(struct lim_int *, lim2)->min;
    max2 = obj_ptr(struct lim_int *, lim2)->max;

    if (min2 != obj_False && (min1 == obj_False || (long)min1 < (long)min2))
	return FALSE;
    if (max2 != obj_False && (max2 == obj_False || (long)max1 > (long)max2))
	return FALSE;
    return TRUE;
}

static inline boolean lim_type_subtypep(obj_t lim, obj_t type)
{
    return subtypep(obj_IntegerClass, type);
}

static inline boolean union_type_subtypep(obj_t u, obj_t type)
{
    obj_t remaining;

    for (remaining = obj_ptr(struct union_type *, u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (!subtypep(HEAD(remaining), type))
	    return FALSE;
    return TRUE;
}

static inline boolean type_union_subtypep(obj_t type, obj_t u)
{
    obj_t remaining;

    for (remaining = obj_ptr(struct union_type *, u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (subtypep(type, HEAD(remaining)))
	    return TRUE;
    return FALSE;
}

boolean subtypep(obj_t type1, obj_t type2)
{
    int type1_id, type2_id;

    if (type1 == type2)
	return TRUE;

    type1_id = obj_ptr(struct type *, type1)->type_id;
    type2_id = obj_ptr(struct type *, type2)->type_id;

    switch (type1_id) {
    case id_Singleton:
	switch (type2_id) {
	    /* singleton x mumble methods */
	case id_Singleton:
	    return sing_sing_subtypep(type1, type2);
	    break;
	case id_Class:
	case id_SubClass:
	case id_LimInt:
	    return sing_type_subtypep(type1, type2);
	    break;
	case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	};
	break;
    case id_Class:
	switch (type2_id) {
	    /* class x mumble methods */
	case id_Singleton:
	    return class_sing_subtypep(type1, type2);
	    break;
	case id_Class:
	    return class_class_subtypep(type1, type2);
	    break;
	case id_SubClass:
	    return class_subclass_subtypep(type1, type2);
	    break;
	case id_LimInt:
	    return never_subtypep(type1, type2);
	    break;
	case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	};
	break;
    case id_SubClass:
	switch (type2_id) {
	    /* subclass x mumble methods */
	case id_Singleton:
	case id_Class:
	    return subclass_type_subtypep(type1, type2);
	    break;
	case id_SubClass:
	    return subclass_subclass_subtypep(type1, type2);
	    break;
	case id_LimInt:
	    return never_subtypep(type1, type2);
	    break;
	case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	}
	break;
    case id_LimInt:
	switch (type2_id) {
	    /* limint x mumble methods */
	case id_Singleton:
	    return lim_sing_subtypep(type1, type2);
	    break;
	case id_Class:
	case id_SubClass:
	    return lim_type_subtypep(type1, type2);
	    break;
	case id_LimInt:
	    return lim_lim_subtypep(type1, type2);
	    break;
	case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	}
	break;
    case id_Union:
	return union_type_subtypep(type1, type2);
	break;
    }
    lose("subtypep dispatch didn't do anything.");
    return FALSE;
}


/* overlapp */

static boolean class_class_overlapp(obj_t class1, obj_t class2)
{
    obj_t remaining;

    if (class1 == class2)
	return TRUE;

    for (remaining = obj_ptr(struct class *, class1)->all_subclasses;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (subtypep(HEAD(remaining), class2))
	    return TRUE;
    return FALSE;
}

static boolean class_type_overlapp(obj_t class, obj_t type)
{
    return overlapp(type, class);
}

static boolean subclass_type_overlapp(obj_t subclass, obj_t type)
{
    obj_t class = obj_ptr(struct subclass *, subclass)->of;
    obj_t remaining;

    if (instancep(class, type))
	return TRUE;

    for (remaining = obj_ptr(struct class *, class)->all_subclasses;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(HEAD(remaining), type))
	    return TRUE;
    return FALSE;
}

static boolean lim_lim_overlapp(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = obj_ptr(struct lim_int *, lim1)->min;
    max1 = obj_ptr(struct lim_int *, lim1)->max;
    min2 = obj_ptr(struct lim_int *, lim2)->min;
    max2 = obj_ptr(struct lim_int *, lim2)->max;

    if (max1 != obj_False && min2 != obj_False && (long)max1 < (long)min2)
	return FALSE;

    if (max2 != obj_False && min1 != obj_False && (long)max2 < (long)min1)
	return FALSE;

    return TRUE;
}

static boolean lim_class_overlapp(obj_t lim, obj_t class)
{
    return class_class_overlapp(class, obj_IntegerClass);
}

static boolean lim_type_overlapp(obj_t lim, obj_t type)
{
    return overlapp(type, lim);
}

static boolean union_type_overlapp(obj_t u, obj_t type)
{
    obj_t remaining;

    for (remaining = obj_ptr(struct union_type *, u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (overlapp(HEAD(remaining), type))
	    return TRUE;
    return FALSE;
}

static boolean (*overlapp_table[5][5])(obj_t t1, obj_t t2) = {
    /* singleton x mumble methods */
    {
	sing_sing_subtypep, sing_type_subtypep, sing_type_subtypep,
	sing_type_subtypep, sing_type_subtypep
    },
    /* class x mumble methods */
    {
	class_type_overlapp, class_class_overlapp, class_type_overlapp,
	class_type_overlapp, class_type_overlapp
    },
    /* subclass x mumble methods */
    {
	subclass_type_overlapp, subclass_type_overlapp, subclass_type_overlapp,
	subclass_type_overlapp, subclass_type_overlapp
    },
    /* limint x mumble methods */
    {
	lim_type_overlapp, lim_class_overlapp, never_subtypep,
	lim_lim_overlapp, lim_type_overlapp,
    },
    /* union x mumble methods */
    {
	union_type_overlapp, union_type_overlapp, union_type_overlapp,
	union_type_overlapp, union_type_overlapp
    }
};

boolean overlapp(obj_t type1, obj_t type2)
{
    int type1_id = obj_ptr(struct type *, type1)->type_id;
    int type2_id = obj_ptr(struct type *, type2)->type_id;

    return (overlapp_table[type1_id][type2_id])(type1, type2);
}


/* singleton construction. */

obj_t singleton(obj_t object)
{
    obj_t res = alloc(obj_SingletonClass, sizeof(struct singleton));

    obj_ptr(struct singleton *, res)->type_id = id_Singleton;
    obj_ptr(struct singleton *, res)->object = object;

    return res;
}


/* class initialization. */

void init_class_type_stuff(obj_t class)
{
    obj_ptr(struct class *, class)->type_id = id_Class;
}


/* subclass construction. */

obj_t subclass(obj_t of)
{
    obj_t res = alloc(obj_SubclassClass, sizeof(struct subclass));

    obj_ptr(struct subclass *, res)->type_id = id_SubClass;
    obj_ptr(struct subclass *, res)->of = of;

    return res;
}


/* limited integer construction. */

obj_t limited_integer(obj_t min, obj_t max)
{
    if (min == MIN_FIXNUM)
	min = obj_False;
    if (max == MAX_FIXNUM)
	max = obj_False;

    if (min != obj_False || max != obj_False) {
	obj_t res = alloc(obj_LimIntClass, sizeof(struct lim_int));

	obj_ptr(struct lim_int *, res)->type_id = id_LimInt;
	obj_ptr(struct lim_int *, res)->min = min;
	obj_ptr(struct lim_int *, res)->max = max;

	return res;
    }
    else
	return obj_IntegerClass;
}

static obj_t merge_limited_integers(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = obj_ptr(struct lim_int *, lim1)->min;
    max1 = obj_ptr(struct lim_int *, lim1)->max;
    min2 = obj_ptr(struct lim_int *, lim2)->min;
    max2 = obj_ptr(struct lim_int *, lim2)->max;

    if (max1 != obj_False && min2 != obj_False
	  && (long)max1 < (long)min2 - (long)make_fixnum(1))
	return obj_False;

    if (max2 != obj_False && min1 != obj_False
	  && (long)max2 < (long)min1 - (long)make_fixnum(1))
	return obj_False;

    if (min1 != obj_False && min2 != obj_False)
	if ((long)min1 < (long)min2)
	    min = min1;
	else
	    min = min2;
    else
	min = obj_False;

    if (max1 != obj_False && max2 != obj_False)
	if ((long)max1 > (long)max2)
	    max = max1;
	else
	    max = max2;
    else
	max = obj_False;

    return limited_integer(min, max);
}

/* returns a new limited integer type which contains all elements common to
   lim1 and lim2.  If there are no common elements, obj_False is returned. */
obj_t intersect_limited_integers(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = obj_ptr(struct lim_int *, lim1)->min;
    max1 = obj_ptr(struct lim_int *, lim1)->max;
    min2 = obj_ptr(struct lim_int *, lim2)->min;
    max2 = obj_ptr(struct lim_int *, lim2)->max;

    if (max1 != obj_False && min2 != obj_False
	  && (long)max1 < (long)min2)
	return obj_False;

    if (max2 != obj_False && min1 != obj_False
	  && (long)max2 < (long)min1)
	return obj_False;

    if (min1 != obj_False || min2 != obj_False)
	if (min1 == obj_False || (long)min1 < (long)min2)
	    min = min2;
        else
	    min = min1;
    else
	min = obj_False;

    if (max1 != obj_False || max2 != obj_False)
	if (max1 == obj_False || (long)max1 > (long)max2)
	    max = max2;
	else
	    max = max1;
    else
	max = obj_False;

    return limited_integer(min, max);
}

/* Return a new limited integer type containing the portion of lim1 which
   contains val but contains no items from lim2.  Lim1 may be "<integer>"
   rather than a limited integer, and we require, as a precondition,
   that lim2 does not contain val. */
obj_t restrict_limited_integers(obj_t val, obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    if (obj_ptr(struct type *, lim1)->type_id == id_LimInt) {
	min1 = obj_ptr(struct lim_int *, lim1)->min;
	max1 = obj_ptr(struct lim_int *, lim1)->max;
    } else
	min1 = max1 = obj_False;
    min2 = obj_ptr(struct lim_int *, lim2)->min;
    max2 = obj_ptr(struct lim_int *, lim2)->max;

    if ((min2 == obj_False || (long)min2 < (long)val) &&
	(min1 == obj_False || (long)min1 < (long)max2))
	min = (obj_t)((long)max2 + (long)make_fixnum(1));
    else
	min = min1;
    
    if ((max2 == obj_False || (long)max2 > (long)val) &&
	(max1 == obj_False || (long)max1 > (long)min2))
	max = (obj_t)((long)min2 - (long)make_fixnum(1));
    else
	max = max1;

    return limited_integer(min, max);
}

/* union construction. */

static obj_t make_union(obj_t members)
{
    obj_t res = alloc(obj_UnionClass, sizeof(struct union_type));

    obj_ptr(struct union_type *, res)->type_id = id_Union;
    obj_ptr(struct union_type *, res)->members = members;

    return res;
}

static obj_t canonicalize_union_member(obj_t type)
{
    if (obj_ptr(struct type *, type)->type_id == id_Singleton) {
	obj_t object = obj_ptr(struct singleton *, type)->object;
	if (obj_is_fixnum(object))
	    return limited_integer(object, object);
	else
	    return type;
    }
    else
	return type;
}

static obj_t merge_members(obj_t type1, obj_t type2)
{
    if (obj_ptr(struct type *, type1)->type_id == id_LimInt
	  && obj_ptr(struct type *, type2)->type_id == id_LimInt)
	return merge_limited_integers(type1, type2);
    else if (subtypep(type1, type2))
	return type2;
    else if (subtypep(type2, type1))
	return type1;
    else
	return obj_False;
}

static obj_t merge_with_union(obj_t type, obj_t members)
{
    obj_t result, *prev;

  again:
    if (members == obj_Nil)
	return type;

    prev = &result;

    do {
	obj_t member = HEAD(members);
	obj_t remaining = TAIL(members);
	obj_t merged = merge_members(type, member);

	if (merged != obj_False) {
	    type = merged;
	    *prev = remaining;
	    members = result;
	    goto again;
	}
	else {
	    obj_t new = list1(member);
	    *prev = new;
	    prev = &TAIL(new);
	    members = remaining;
	}
    } while (members != obj_Nil);

    *prev = obj_Nil;
    return make_union(result);
}

static obj_t merge_unions(obj_t union1, obj_t union2)
{
    obj_t remaining;
    obj_t result = union2;

    for (remaining = obj_ptr(struct union_type *, union1)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	result = type_union(result, HEAD(remaining));

    return result;
}

obj_t type_union(obj_t type1, obj_t type2)
{
    if (obj_ptr(struct type *, type1)->type_id == id_Union) {
	if (obj_ptr(struct type *, type2)->type_id == id_Union)
	    return merge_unions(type1, type2);
	else
	    return merge_with_union(canonicalize_union_member(type2),
				    obj_ptr(struct union_type *, type1)
				      ->members);
    }
    else {
	if (obj_ptr(struct type *, type2)->type_id == id_Union)
	    return merge_with_union(canonicalize_union_member(type1),
				    obj_ptr(struct union_type *, type2)
				      ->members);
	else {
	    obj_t t1 = canonicalize_union_member(type1);
	    obj_t t2 = canonicalize_union_member(type2);
	    obj_t merged = merge_members(t1, t2);
	    if (merged != obj_False)
		return merged;
	    else
		return make_union(pair(t1, pair(t2, obj_Nil)));
	}
    }
}


/* Exported functions. */

static obj_t dylan_instancep(obj_t thing, obj_t type)
{
    if (instancep(thing, type))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_subtypep(obj_t type1, obj_t type2)
{
    if (subtypep(type1, type2))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_limited_integer(obj_t class, obj_t min, obj_t max)
{
    if (min != obj_False)
	check_type(min, obj_IntegerClass);
    if (max != obj_False)
	check_type(max, obj_IntegerClass);
    return limited_integer(min, max);
}

static obj_t dylan_limited_class(obj_t class, obj_t subclass_of)
{
    if (subclass_of == obj_Unbound)
	error("subclass-of: required but missing");
    check_type(subclass_of, obj_ClassClass);
    return subclass(subclass_of);
}

static obj_t dylan_make_singleton(obj_t class, obj_t object)
{
    if (object == obj_Unbound)
	error("object: required but missing");
    return singleton(object);
}


/* Printing stuff. */

static void print_singleton(obj_t singleton)
{
    printf("{singleton ");
    prin1(obj_ptr(struct singleton *, singleton)->object);
    putchar('}');
}

static void print_subclass(obj_t subclass)
{
    printf("{subclass of ");
    prin1(obj_ptr(struct subclass *, subclass)->of);
    putchar('}');
}

static void print_limint(obj_t limint)
{
    printf("{limited integer ");
    if (obj_ptr(struct lim_int *, limint)->min != obj_False)
	printf("%ld<=", fixnum_value(obj_ptr(struct lim_int *, limint)->min));
    putchar('x');
    if (obj_ptr(struct lim_int *, limint)->max != obj_False)
	printf("<=%ld", fixnum_value(obj_ptr(struct lim_int *, limint)->max));
    putchar('}');
}

static void print_union(obj_t union_type)
{
    printf("{union ");
    prin1(obj_ptr(struct union_type *, union_type)->members);
    putchar('}');
}



/* GC stuff. */

static int scav_simp_type(struct object *o)
{
    struct singleton *ptr = (struct singleton *)o;

    scavenge(&ptr->object);

    return sizeof(struct singleton);
}

static obj_t trans_simp_type(obj_t type)
{
    return transport(type, sizeof(struct singleton));
}

static int scav_limint(struct object *ptr)
{
    return sizeof(struct lim_int);
}

static obj_t trans_limint(obj_t limint)
{
    return transport(limint, sizeof(struct lim_int));
}

void scavenge_type_roots(void)
{
    scavenge(&obj_TypeClass);
    scavenge(&obj_SingletonClass);
    scavenge(&obj_SubclassClass);
    scavenge(&obj_LimIntClass);
    scavenge(&obj_UnionClass);
}


/* class initialization. */

void make_type_classes(void)
{
    obj_TypeClass = make_abstract_class(TRUE);
    obj_SingletonClass = make_builtin_class(scav_simp_type, trans_simp_type);
    obj_SubclassClass = make_builtin_class(scav_simp_type, trans_simp_type);
    obj_LimIntClass = make_builtin_class(scav_limint, trans_limint);
    obj_UnionClass = make_builtin_class(scav_simp_type, trans_simp_type);
}

void init_type_classes(void)
{
    init_builtin_class(obj_TypeClass, "<type>", obj_ObjectClass, NULL);
    init_builtin_class(obj_SingletonClass, "<singleton>", obj_TypeClass, NULL);
    def_printer(obj_SingletonClass, print_singleton);
    init_builtin_class(obj_SubclassClass, "<subclass>", obj_TypeClass, NULL);
    def_printer(obj_SubclassClass, print_subclass);
    init_builtin_class(obj_LimIntClass, "<limited-integer>",
		       obj_TypeClass, NULL);
    def_printer(obj_LimIntClass, print_limint);
    init_builtin_class(obj_UnionClass, "<union>", obj_TypeClass, NULL);
    def_printer(obj_UnionClass, print_union);
}

void init_type_functions(void)
{
    define_function("instance?", list2(obj_ObjectClass, obj_TypeClass),
		    FALSE, obj_False, obj_BooleanClass, dylan_instancep);
    define_function("subtype?", list2(obj_TypeClass, obj_TypeClass),
		    FALSE, obj_False, obj_BooleanClass, dylan_subtypep);
    define_function("singleton", list1(obj_ObjectClass),
		    FALSE, obj_False, obj_SingletonClass, singleton);
    define_method("make", list1(singleton(obj_SingletonClass)), FALSE,
		  list1(pair(symbol("object"), obj_Unbound)),
		  obj_TypeClass, dylan_make_singleton);
    define_method("union", list2(obj_TypeClass, obj_TypeClass), FALSE,
		  obj_Nil, obj_TypeClass, type_union);
    define_method("limited", list1(singleton(obj_IntegerClass)), FALSE,
		  list2(pair(symbol("min"), obj_False),
			pair(symbol("max"), obj_False)),
		  obj_TypeClass, dylan_limited_integer);
    define_method("limited", list1(singleton(obj_ClassClass)), FALSE,
		  list1(pair(symbol("subclass-of"), obj_Unbound)),
		  obj_TypeClass, dylan_limited_class);
}
