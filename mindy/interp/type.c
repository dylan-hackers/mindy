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
* $Header: /scm/cvs/src/mindy/interp/type.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file implements the type system.
*
\**********************************************************************/

#include "../compat/std-c.h"

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
static obj_t obj_SubclassClass, obj_UnionClass, obj_NoneOfClass;

struct singleton {
    obj_t class;
    enum type_Id type_id;
    obj_t object;
};

#define SING(x) obj_ptr(struct singleton *, x)

struct subclass {
    obj_t class;
    enum type_Id type_id;
    obj_t of;
};

#define SUBCLASS(x) obj_ptr(struct subclass *, x)

struct lim_int {
    obj_t class;
    enum type_Id type_id;
    obj_t min;
    obj_t max;
};

#define LIMINT(x) obj_ptr(struct lim_int *, x)

struct union_type {
    obj_t class;
    enum type_Id type_id;
    obj_t members;
};

#define UNION(x) obj_ptr(struct union_type *, x)

struct none_of_type {
    obj_t class;
    enum type_Id type_id;
    obj_t base;
    obj_t exclude;
};

#define NONEOF(x) obj_ptr(struct none_of_type *, x)


/* instancep */

static inline boolean singleton_instancep(obj_t thing, obj_t type)
{
    return idp(thing, SING(type)->object);
}

static inline boolean class_instancep(obj_t thing, obj_t class)
{
    obj_t thing_class = object_class(thing);

    static obj_t c1_cache[32], c2_cache[32];
    static boolean result_cache[32];
    register int cacheloc = (((long)thing_class ^ (long)class) >> 4) & 31;

    if (c1_cache[cacheloc] == thing_class && c2_cache[cacheloc] == class)
	return result_cache[cacheloc];

    c1_cache[cacheloc] = thing_class;
    c2_cache[cacheloc] = class;
    return result_cache[cacheloc] = subtypep(thing_class, class);
}

static inline boolean subclass_instancep(obj_t thing, obj_t subclass)
{
    return instancep(thing, obj_ClassClass)
	&& subtypep(thing, SUBCLASS(subclass)->of);
}

static inline boolean limfix_instancep(obj_t thing, obj_t lim_int)
{
    if (!obj_is_fixnum(thing))
	return FALSE;
    if ((long)thing < (long)(LIMINT(lim_int)->min))
	return FALSE;
    if ((long)(LIMINT(lim_int)->max) < (long)thing)
	return FALSE;
    return TRUE;
}

static inline boolean limbig_instancep(obj_t thing, obj_t lim_int)
{
    if (object_class(thing) != obj_BignumClass)
	return FALSE;
    if (compare_bignums(thing, LIMINT(lim_int)->min) < 0)
	return FALSE;
    if (compare_bignums(LIMINT(lim_int)->max, thing) < 0)
	return FALSE;
    return TRUE;
}

static inline boolean union_instancep(obj_t thing, obj_t u)
{
    obj_t remaining;

    for (remaining = UNION(u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(thing, HEAD(remaining)))
	    return TRUE;
    return FALSE;
}

static inline boolean none_of_instancep(obj_t thing, obj_t class)
{
    obj_t remaining;

    if (!instancep(thing, NONEOF(class)->base))
	return FALSE;

    for (remaining = NONEOF(class)->exclude;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(thing, HEAD(remaining)))
	    return FALSE;
    return TRUE;
}

boolean instancep(obj_t thing, obj_t type)
{
    enum type_Id type_id = TYPE(type)->type_id;

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
      case id_LimFixnum:
	return limfix_instancep(thing, type);
	break;
      case id_LimBignum:
	return limbig_instancep(thing, type);
	break;
      case id_Union:
	return union_instancep(thing, type);
	break;
      case id_NoneOf:
	return none_of_instancep(thing, type);
	break;
    }
    lose("instancep dispatch didn't do anything.");
    return FALSE;
}


/* subtypep */

static inline boolean sing_sing_subtypep(obj_t sing1, obj_t sing2)
{
    return idp(SING(sing1)->object, SING(sing2)->object);
}

static inline boolean sing_type_subtypep(obj_t sing, obj_t type)
{
    return instancep(SING(sing)->object, type);
}

static inline boolean class_class_subtypep(obj_t class1, obj_t class2)
{
    obj_t cpl = CLASS(class1)->cpl;
    obj_t remaining;

    if (cpl == obj_False)
	error("Attempt to use %= before it was initialized.", class1);

    for (remaining = cpl; remaining != obj_Nil; remaining = TAIL(remaining))
	if (HEAD(remaining) == class2) {
	    return TRUE;
	}
    return FALSE;
}

static inline boolean never_subtypep(obj_t type1, obj_t type2)
{
    return FALSE;
}

static inline boolean subclass_class_subtypep(obj_t sub, obj_t class)
{
    return subtypep(object_class(SUBCLASS(sub)->of), class);
}

static inline boolean subclass_subclass_subtypep(obj_t sub1, obj_t sub2)
{
    return subtypep(SUBCLASS(sub1)->of, SUBCLASS(sub2)->of);
}

static inline boolean limfix_limfix_subtypep(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if ((long)min1 < (long)min2)
	return FALSE;
    if ((long)max1 > (long)max2)
	return FALSE;
    return TRUE;
}

static inline boolean limbig_limbig_subtypep(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if (min1 == obj_False) {
	if (min2 != obj_False)
	    return FALSE;
    }
    else {
	if (min2 == obj_False || compare_bignums(min2, min1) < 0)
	    return FALSE;
    }

    if (max1 == obj_False) {
	if (max2 != obj_False)
	    return FALSE;
    }
    else {
	if (max2 == obj_False || compare_bignums(max2, max1) > 0)
	    return FALSE;
    }

    return TRUE;
}

static inline boolean limfix_type_subtypep(obj_t lim, obj_t type)
{
    return subtypep(obj_FixnumClass, type);
}

static inline boolean limbig_type_subtypep(obj_t lim, obj_t type)
{
    return subtypep(obj_BignumClass, type);
}

static inline boolean union_type_subtypep(obj_t u, obj_t type)
{
    obj_t remaining;

    for (remaining = UNION(u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (!subtypep(HEAD(remaining), type))
	    return FALSE;
    return TRUE;
}

static inline boolean type_union_subtypep(obj_t type, obj_t u)
{
    obj_t remaining;

    for (remaining = UNION(u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (subtypep(type, HEAD(remaining)))
	    return TRUE;
    return FALSE;
}

static inline boolean noneof_type_subtypep(obj_t n, obj_t type)
{
    return subtypep(NONEOF(n)->base, type);
}

static inline boolean lim_noneof_subtypep(obj_t type, obj_t n)
{
    obj_t remaining;

    if (!subtypep(type, NONEOF(n)->base)) return FALSE;

    for (remaining = NONEOF(n)->exclude;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(SING(HEAD(remaining))->object, type))
	    return FALSE;
    return TRUE;
}

boolean subtypep(obj_t type1, obj_t type2)
{
    int type1_id, type2_id;

    if (type1 == type2)
	return TRUE;

    type1_id = TYPE(type1)->type_id;
    type2_id = TYPE(type2)->type_id;

    switch (type1_id) {
      case id_Singleton:
	switch (type2_id) {
	    /* singleton x mumble methods */
	  case id_Singleton:
	    return sing_sing_subtypep(type1, type2);
	    break;
	  case id_Class:
	  case id_SubClass:
	  case id_LimFixnum:
	  case id_LimBignum:
	  case id_NoneOf:
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
	  case id_Class:
	    return class_class_subtypep(type1, type2);
	    break;
	  case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	  case id_Singleton:
	  case id_SubClass:
	  case id_LimFixnum:
	  case id_LimBignum:
	  case id_NoneOf:
	    return never_subtypep(type1, type2);
	    break;
	};
	break;
      case id_SubClass:
	switch (type2_id) {
	    /* subclass x mumble methods */
	  case id_SubClass:
	    return subclass_subclass_subtypep(type1, type2);
	    break;
	  case id_Class:
	    return subclass_class_subtypep(type1, type2);
	    break;
	  case id_Singleton:
	  case id_LimFixnum:
	  case id_LimBignum:
	  case id_NoneOf:
	    return never_subtypep(type1, type2);
	    break;
	  case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	}
	break;
      case id_LimFixnum:
	switch (type2_id) {
	    /* limfixnum x mumble methods */
	  case id_Singleton:
	    return never_subtypep(type1, type2);
	    break;
	  case id_Class:
	  case id_SubClass:
	    return limfix_type_subtypep(type1, type2);
	    break;
	  case id_LimFixnum:
	    return limfix_limfix_subtypep(type1, type2);
	    break;
	  case id_LimBignum:
	    return never_subtypep(type1, type2);
	    break;
	  case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	  case id_NoneOf:
	    return lim_noneof_subtypep(type1, type2);
	    break;
	}
	break;
      case id_LimBignum:
	switch (type2_id) {
	    /* limint x mumble methods */
	  case id_Singleton:
	    return never_subtypep(type1, type2);
	    break;
	  case id_Class:
	  case id_SubClass:
	    return limbig_type_subtypep(type1, type2);
	    break;
	  case id_LimFixnum:
	    return never_subtypep(type1, type2);
	    break;
	  case id_LimBignum:
	    return limbig_limbig_subtypep(type1, type2);
	    break;
	  case id_Union:
	    return type_union_subtypep(type1, type2);
	    break;
	  case id_NoneOf:
	    return lim_noneof_subtypep(type1, type2);
	    break;
	}
	break;
      case id_Union:
	return union_type_subtypep(type1, type2);
	break;
      case id_NoneOf:
	return noneof_type_subtypep(type1, type2);
	break;
    }
    lose("subtypep dispatch didn't do anything.");
    return FALSE;
}


/* overlapp */

static boolean class_class_overlapp(obj_t class1, obj_t class2)
{
    obj_t remaining;

    if (subtypep(class1, class2))
	return TRUE;

    for (remaining = CLASS(class1)->all_subclasses;
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
    obj_t class = SUBCLASS(subclass)->of;
    obj_t remaining;

    if (instancep(class, type))
	return TRUE;

    for (remaining = CLASS(class)->all_subclasses;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (instancep(HEAD(remaining), type))
	    return TRUE;
    return FALSE;
}

static boolean limfix_limfix_overlapp(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if ((long)max1 < (long)min2)
	return FALSE;

    if ((long)max2 < (long)min1)
	return FALSE;

    return TRUE;
}

static boolean limbig_limbig_overlapp(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if (max1 != obj_False && min2 != obj_False
	  && compare_bignums(max1, min2) <0)
	return FALSE;

    if (max2 != obj_False && min1 != obj_False
	  && compare_bignums(max2, min1) < 0)
	return FALSE;

    return TRUE;
}

static boolean limfix_class_overlapp(obj_t lim, obj_t class)
{
    return class_class_overlapp(class, obj_FixnumClass);
}

static boolean limbig_class_overlapp(obj_t lim, obj_t class)
{
    return class_class_overlapp(class, obj_BignumClass);
}

static boolean lim_type_overlapp(obj_t lim, obj_t type)
{
    return overlapp(type, lim);
}

static boolean union_type_overlapp(obj_t u, obj_t type)
{
    obj_t remaining;

    for (remaining = UNION(u)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	if (overlapp(HEAD(remaining), type))
	    return TRUE;
    return FALSE;
}

static boolean noneof_type_overlapp(obj_t n, obj_t type)
{
    return overlapp(NONEOF(n)->base, type);
}

static boolean (*overlapp_table[7][7])(obj_t t1, obj_t t2) = {
    /* singleton x mumble methods */
    {
	sing_sing_subtypep, sing_type_subtypep, sing_type_subtypep,
	sing_type_subtypep, sing_type_subtypep, sing_type_subtypep,
	sing_type_subtypep
    },
    /* class x mumble methods */
    {
	class_type_overlapp, class_class_overlapp, class_type_overlapp,
	class_type_overlapp, class_type_overlapp, class_type_overlapp,
	class_type_overlapp
    },
    /* subclass x mumble methods */
    {
	subclass_type_overlapp, subclass_type_overlapp, subclass_type_overlapp,
	subclass_type_overlapp, subclass_type_overlapp, subclass_type_overlapp,
	subclass_type_overlapp
    },
    /* limfix x mumble methods */
    {
	lim_type_overlapp, limfix_class_overlapp, never_subtypep,
	limfix_limfix_overlapp, never_subtypep, lim_type_overlapp,
	lim_type_overlapp
    },
    /* limbig x mumble methods */
    {
	lim_type_overlapp, limbig_class_overlapp, never_subtypep,
	never_subtypep, limbig_limbig_overlapp, lim_type_overlapp,
	lim_type_overlapp
    },
    /* union x mumble methods */
    {
	union_type_overlapp, union_type_overlapp, union_type_overlapp,
	union_type_overlapp, union_type_overlapp, union_type_overlapp,
	union_type_overlapp
    },
    /* noneof x mumble methods */
    {
	noneof_type_overlapp, noneof_type_overlapp, noneof_type_overlapp,
	noneof_type_overlapp, noneof_type_overlapp, noneof_type_overlapp,
	noneof_type_overlapp
    }
};

boolean overlapp(obj_t type1, obj_t type2)
{
    int type1_id = TYPE(type1)->type_id;
    int type2_id = TYPE(type2)->type_id;

    return (overlapp_table[type1_id][type2_id])(type1, type2);
}


/* singleton construction. */

obj_t singleton(obj_t object)
{
    obj_t res = alloc(obj_SingletonClass, sizeof(struct singleton));

    SING(res)->type_id = id_Singleton;
    SING(res)->object = object;

    return res;
}


/* class initialization. */

void init_class_type_stuff(obj_t class)
{
    CLASS(class)->type_id = id_Class;
}


/* subclass construction. */

obj_t subclass(obj_t of)
{
    obj_t res = alloc(obj_SubclassClass, sizeof(struct subclass));

    SUBCLASS(res)->type_id = id_SubClass;
    SUBCLASS(res)->of = of;

    return res;
}


/* limited integer construction. */

obj_t limited_fixnum(obj_t min, obj_t max)
{
    if (min == obj_False)
	min = MIN_FIXNUM;
    if (max == obj_False)
	max = MAX_FIXNUM;

    if (min != MIN_FIXNUM || max != MAX_FIXNUM) {
	obj_t res = alloc(obj_LimIntClass, sizeof(struct lim_int));

	LIMINT(res)->type_id = id_LimFixnum;
	LIMINT(res)->min = min;
	LIMINT(res)->max = max;

	return res;
    }
    else
	return obj_FixnumClass;
}

obj_t limited_bignum(obj_t min, obj_t max)
{
    if (min != obj_False || max != obj_False) {
	obj_t res = alloc(obj_LimIntClass, sizeof(struct lim_int));

	LIMINT(res)->type_id = id_LimBignum;
	LIMINT(res)->min = as_bignum(min);
	LIMINT(res)->max = as_bignum(max);

	return res;
    }
    else
	return obj_BignumClass;
}

static obj_t merge_limited_fixnums(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if (fixnum_value(max1) < fixnum_value(min2) - 1)
	return obj_False;

    if (fixnum_value(max2) < fixnum_value(min1) - 1)
	return obj_False;

    if ((long)min1 < (long)min2)
	min = min1;
    else
	min = min2;

    if ((long)max1 > (long)max2)
	max = max1;
    else
	max = max2;

    return limited_fixnum(min, max);
}

static obj_t merge_limited_bignums(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if (max1 != obj_False && min2 != obj_False) {
	obj_t min2_minus_1 = subtract_bignums(min2, make_bignum(1));
	if (compare_bignums(max1, min2_minus_1) < 0)
	    return obj_False;
    }

    if (max2 != obj_False && min1 != obj_False) {
	obj_t min1_minus_1 = subtract_bignums(min1, make_bignum(1));
	if (compare_bignums(max2, min1_minus_1) < 0)
	    return obj_False;
    }

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

    return limited_bignum(min, max);
}

/* returns a new limited integer type which contains all elements common to
   lim1 and lim2.  If there are no common elements, obj_False is returned. */
obj_t intersect_limited_fixnums(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if ((long)max1 < (long)min2)
	return obj_False;

    if ((long)max2 < (long)min1)
	return obj_False;

    if ((long)min1 < (long)min2)
	min = min2;
    else
	min = min1;

    if ((long)max1 > (long)max2)
	max = max2;
    else
	max = max1;

    return limited_fixnum(min, max);
}

/* returns a new limited integer type which contains all elements common to
   lim1 and lim2.  If there are no common elements, obj_False is returned. */
obj_t intersect_limited_bignums(obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    min1 = LIMINT(lim1)->min;
    max1 = LIMINT(lim1)->max;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if (max1 != obj_False && min2 != obj_False
	  && compare_bignums(max1, min2) < 0)
	return obj_False;

    if (max2 != obj_False && min1 != obj_False
	  && compare_bignums(max2, min1) < 0)
	return obj_False;

    if (min1 != obj_False || min2 != obj_False)
	if (min1 == obj_False || compare_bignums(min1, min2) < 0)
	    min = min2;
        else
	    min = min1;
    else
	min = obj_False;

    if (max1 != obj_False || max2 != obj_False)
	if (max1 == obj_False || compare_bignums(max1, max2) > 0)
	    max = max2;
	else
	    max = max1;
    else
	max = obj_False;

    return limited_bignum(min, max);
}

/* Return a new limited integer type containing the portion of lim1 which
   contains val but contains no items from lim2.  Lim1 may be "<integer>"
   rather than a limited integer, and we require, as a precondition,
   that lim2 does not contain val. */
obj_t restrict_limited_fixnums(obj_t val, obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    if (TYPE(lim1)->type_id == id_LimFixnum) {
	min1 = LIMINT(lim1)->min;
	max1 = LIMINT(lim1)->max;
    }
    else {
	min1 = MIN_FIXNUM;
	max1 = MAX_FIXNUM;
    }
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if ((long)min2 < (long)val && (long)min1 < (long)max2)
	min = make_fixnum(fixnum_value(max2) + 1);
    else
	min = min1;
    
    if ((long)max2 > (long)val && (long)max1 > (long)min2)
	max = make_fixnum(fixnum_value(min2) - 1);
    else
	max = max1;

    return limited_fixnum(min, max);
}

obj_t restrict_limited_bignums(obj_t val, obj_t lim1, obj_t lim2)
{
    obj_t min1, min2, max1, max2, min, max;

    if (TYPE(lim1)->type_id == id_LimBignum) {
	min1 = LIMINT(lim1)->min;
	max1 = LIMINT(lim1)->max;
    } else
	min1 = max1 = obj_False;
    min2 = LIMINT(lim2)->min;
    max2 = LIMINT(lim2)->max;

    if ((min2 == obj_False || compare_bignums(min2, val) < 0) &&
	(min1 == obj_False || compare_bignums(min1, max2) < 0))
	min = add_bignums(max2, make_bignum(1));
    else
	min = min1;
    
    if ((max2 == obj_False || compare_bignums(max2, val) > 0) &&
	(max1 == obj_False || compare_bignums(max1, min2) > 0))
	max = subtract_bignums(min2, make_bignum(1));
    else
	max = max1;

    return limited_bignum(min, max);
}



/* none_of construction */

/* return a version of "type" which excludes the given value.  If type
   is already a none_of type, simply extend it, else return a new none_of
   type. */
obj_t restrict_type(obj_t exclude, obj_t type)
{
    if (TYPE(type)->type_id == id_NoneOf) {
	NONEOF(type)->exclude = pair(exclude, NONEOF(type)->exclude);
	return type;
    } else {
	obj_t res = alloc(obj_NoneOfClass, sizeof(struct none_of_type));

	NONEOF(res)->type_id = id_NoneOf;
	NONEOF(res)->base = type;
	NONEOF(res)->exclude =
	    pair(exclude, obj_Nil);

	return res;
    }
}


/* union construction. */

static obj_t make_union(obj_t members)
{
    obj_t res = alloc(obj_UnionClass, sizeof(struct union_type));

    UNION(res)->type_id = id_Union;
    UNION(res)->members = members;

    return res;
}

static obj_t canonicalize_union_member(obj_t type)
{
    if (TYPE(type)->type_id == id_Singleton) {
	obj_t object = SING(type)->object;
	if (obj_is_fixnum(object))
	    return limited_fixnum(object, object);
	else if (object == obj_False || object == obj_True)
	    return object_class(object);
	else if (object_class(object) == obj_BignumClass)
	    return limited_bignum(object, object);
	else
	    return type;
    }
    else
	return type;
}

static obj_t merge_members(obj_t type1, obj_t type2)
{
    enum type_Id id1 = TYPE(type1)->type_id;
    enum type_Id id2 = TYPE(type2)->type_id;

    if (id1 == id_LimFixnum && id2 == id_LimFixnum)
	return merge_limited_fixnums(type1, type2);
    else if (id1 == id_LimBignum && id2 == id_LimBignum)
	return merge_limited_bignums(type1, type2);
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

    *prev = list1(type);

    return make_union(result);
}

static obj_t merge_unions(obj_t union1, obj_t union2)
{
    obj_t remaining;
    obj_t result = union2;

    for (remaining = UNION(union1)->members;
	 remaining != obj_Nil;
	 remaining = TAIL(remaining))
	result = type_union(result, HEAD(remaining));

    return result;
}

obj_t type_union(obj_t type1, obj_t type2)
{
    if (TYPE(type1)->type_id == id_Union) {
	if (TYPE(type2)->type_id == id_Union)
	    return merge_unions(type1, type2);
	else
	    return merge_with_union(canonicalize_union_member(type2),
				    UNION(type1)
				      ->members);
    }
    else {
	if (TYPE(type2)->type_id == id_Union)
	    return merge_with_union(canonicalize_union_member(type1),
				    UNION(type2)
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

static obj_t dylan_limited_fixnum(obj_t class, obj_t min, obj_t max)
{
    if (min != obj_False)
	check_type(min, obj_FixnumClass);
    if (max != obj_False)
	check_type(max, obj_FixnumClass);
    return limited_fixnum(min, max);
}

static obj_t dylan_limited_bignum(obj_t class, obj_t min, obj_t max)
{
    if (min != obj_False)
	check_type(min, obj_IntegerClass);
    if (max != obj_False)
	check_type(max, obj_IntegerClass);
    return limited_bignum(min, max);
}

static obj_t dylan_limited_integer(obj_t class, obj_t min, obj_t max)
{
    obj_t fix_min;
    obj_t fix_max;
    
    if (min != obj_False) {
	if (obj_is_fixnum(min))
	    fix_min = min;
	else {
	    check_type(min, obj_IntegerClass);
	    if (compare_bignums(min, as_bignum(MAX_FIXNUM)) > 0)
		return limited_bignum(min, max);
	    else if (compare_bignums(min, as_bignum(MIN_FIXNUM)) < 0)
		fix_min = MIN_FIXNUM;
	    else
		fix_min = make_fixnum(bignum_value(min));
	}
    }
    else
	fix_min = MIN_FIXNUM;

    if (max != obj_False) {
	if (obj_is_fixnum(max))
	    fix_max = max;
	else {
	    check_type(max, obj_IntegerClass);
	    if (compare_bignums(max, as_bignum(MIN_FIXNUM)) < 0)
		return limited_bignum(min, max);
	    else if (compare_bignums(max, as_bignum(MAX_FIXNUM)) > 0)
		fix_max = MAX_FIXNUM;
	    else
		fix_max = make_fixnum(bignum_value(max));
	}
    }
    else
	fix_max = MAX_FIXNUM;

    return type_union(limited_fixnum(fix_min, fix_max),
		      limited_bignum(min, max));
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


/* Introspection stuff. */

static obj_t dylan_singleton_object(obj_t singleton)
{
    return SING(singleton)->object;
}

static obj_t dylan_subclass_of(obj_t subclass)
{
    return SUBCLASS(subclass)->of;
}

static obj_t dylan_lim_int_class(obj_t lim_int)
{
    switch (LIMINT(lim_int)->type_id) {
      case id_LimFixnum:
	return obj_FixnumClass;
      case id_LimBignum:
	return obj_BignumClass;
      default:
	lose("Strange kind of limited integer.");
	return NULL;
    }
}

static obj_t dylan_lim_int_min(obj_t lim_int)
{
    return LIMINT(lim_int)->min;
}

static obj_t dylan_lim_int_max(obj_t lim_int)
{
    return LIMINT(lim_int)->max;
}

static obj_t dylan_union_members(obj_t u)
{
    return UNION(u)->members;
}


/* Printing stuff. */

static void print_singleton(obj_t singleton)
{
    printf("{singleton ");
    prin1(SING(singleton)->object);
    putchar('}');
}

static void print_subclass(obj_t subclass)
{
    printf("{subclass of ");
    prin1(SUBCLASS(subclass)->of);
    putchar('}');
}

static void print_limint(obj_t limint)
{
    printf("{limited %s integer ",
	   LIMINT(limint)->type_id == id_LimFixnum ? "fixed" : "extended");
    if (LIMINT(limint)->min != obj_False)
	format("%d<=", LIMINT(limint)->min);
    putchar('x');
    if (LIMINT(limint)->max != obj_False)
	format("<=%d", LIMINT(limint)->max);
    putchar('}');
}

static void print_union(obj_t union_type)
{
    printf("{union ");
    prin1(UNION(union_type)->members);
    putchar('}');
}

static void print_none_of(obj_t union_type)
{
    printf("{none of ");
    prin1(NONEOF(union_type)->base);
    printf(": ");
    prin1(NONEOF(union_type)->exclude);
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
    return transport(type, sizeof(struct singleton), TRUE);
}

static int scav_limint(struct object *o)
{
    struct lim_int *ptr = (struct lim_int *)o;

    scavenge(&ptr->min);
    scavenge(&ptr->max);

    return sizeof(struct lim_int);
}

static obj_t trans_limint(obj_t limint)
{
    return transport(limint, sizeof(struct lim_int), TRUE);
}

static int scav_noneof(struct object *obj)
{
    struct none_of_type *ptr = ((struct none_of_type *) obj);

    scavenge(&ptr->base);
    scavenge(&ptr->exclude);

    return sizeof(struct none_of_type);
}

static obj_t trans_noneof(obj_t noneof)
{
    return transport(noneof, sizeof(struct none_of_type), TRUE);
}


/* class initialization. */

void make_type_classes(void)
{
    obj_TypeClass = make_abstract_class(TRUE);
    obj_SingletonClass = make_builtin_class(scav_simp_type, trans_simp_type);
    obj_SubclassClass = make_builtin_class(scav_simp_type, trans_simp_type);
    obj_LimIntClass = make_builtin_class(scav_limint, trans_limint);
    obj_UnionClass = make_builtin_class(scav_simp_type, trans_simp_type);
    obj_NoneOfClass = make_builtin_class(scav_noneof, trans_noneof);

    add_constant_root(&obj_TypeClass);
    add_constant_root(&obj_SingletonClass);
    add_constant_root(&obj_SubclassClass);
    add_constant_root(&obj_LimIntClass);
    add_constant_root(&obj_UnionClass);
    add_constant_root(&obj_NoneOfClass);
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
    init_builtin_class(obj_NoneOfClass, "<none-of>", obj_TypeClass, NULL);
    def_printer(obj_NoneOfClass, print_none_of);
}

void init_type_functions(void)
{
    define_function("instance?", list2(obj_ObjectClass, obj_TypeClass), FALSE,
		    obj_False, FALSE, obj_BooleanClass, dylan_instancep);
    define_function("subtype?", list2(obj_TypeClass, obj_TypeClass), FALSE,
		    obj_False, FALSE, obj_BooleanClass, dylan_subtypep);
    define_function("singleton", list1(obj_ObjectClass), FALSE,
		    obj_False, FALSE, obj_SingletonClass, singleton);
    define_method("make", list1(singleton(obj_SingletonClass)), FALSE,
		  list1(pair(symbol("object"), obj_Unbound)), FALSE,
		  obj_TypeClass, dylan_make_singleton);
    define_function("binary-type-union", list2(obj_TypeClass, obj_TypeClass), 
		    FALSE,
		    obj_Nil, FALSE, obj_TypeClass, type_union);
    define_method("limited", list1(singleton(obj_IntegerClass)), FALSE,
		  list2(pair(symbol("min"), obj_False),
			pair(symbol("max"), obj_False)),
		  FALSE, obj_TypeClass, dylan_limited_integer);
    define_method("limited", list1(singleton(obj_FixnumClass)), FALSE,
		  list2(pair(symbol("min"), obj_False),
			pair(symbol("max"), obj_False)),
		  FALSE, obj_TypeClass, dylan_limited_fixnum);
    define_method("limited", list1(singleton(obj_BignumClass)), FALSE,
		  list2(pair(symbol("min"), obj_False),
			pair(symbol("max"), obj_False)),
		  FALSE, obj_TypeClass, dylan_limited_bignum);
    define_method("limited", list1(singleton(obj_ClassClass)), FALSE,
		  list1(pair(symbol("subclass-of"), obj_Unbound)),
		  FALSE, obj_TypeClass, dylan_limited_class);

    define_function("singleton-object", list1(obj_SingletonClass), FALSE,
		    obj_False, FALSE, obj_ObjectClass, dylan_singleton_object);
    define_function("subclass-of", list1(obj_SubclassClass), FALSE, obj_False,
		    FALSE, obj_ClassClass, dylan_subclass_of);
    define_function("limited-integer-base-class", list1(obj_LimIntClass), 
		    FALSE,
		    obj_False, FALSE, obj_ClassClass, dylan_lim_int_class);
    define_function("limited-integer-minimum", list1(obj_LimIntClass), FALSE,
		    obj_False, FALSE,
		    type_union(object_class(obj_False), obj_IntegerClass),
		    dylan_lim_int_min);
    define_function("limited-integer-maximum", list1(obj_LimIntClass), FALSE,
		    obj_False, FALSE,
		    type_union(object_class(obj_False), obj_IntegerClass),
		    dylan_lim_int_max);
    define_function("union-members", list1(obj_UnionClass), FALSE, obj_False,
		    FALSE, obj_ListClass, dylan_union_members);

    define_constant("<never-returns>", make_union(obj_Nil));
}
