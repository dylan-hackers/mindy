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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/num.c,v 1.3 1994/04/09 13:36:06 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "gc.h"
#include "class.h"
#include "obj.h"
#include "bool.h"
#include "def.h"
#include "list.h"
#include "type.h"
#include "num.h"
#include "thread.h"
#include "func.h"
#include "error.h"
#include "print.h"

obj_t obj_NumberClass = 0;
obj_t obj_ComplexClass = 0;
obj_t obj_RealClass = 0;
obj_t obj_RationalClass = 0;
obj_t obj_IntegerClass = 0;
obj_t obj_FloatClass = 0;
obj_t obj_SingleFloatClass = 0;
obj_t obj_DoubleFloatClass = 0;
obj_t obj_ExtendedFloatClass = 0;

obj_t make_single(float value)
{
    obj_t res = alloc(obj_SingleFloatClass, sizeof(struct single_float));

    obj_ptr(struct single_float *, res)->value = value;

    return res;
}

obj_t make_double(double value)
{
    obj_t res = alloc(obj_DoubleFloatClass, sizeof(struct double_float));

    obj_ptr(struct double_float *, res)->value = value;

    return res;
}

obj_t make_extended(long double value)
{
    obj_t res = alloc(obj_ExtendedFloatClass, sizeof(struct extended_float));

    obj_ptr(struct extended_float *, res)->value = value;

    return res;
}

boolean idp(obj_t x, obj_t y)
{
    obj_t x_class, y_class;

    if (x == y)
	return TRUE;
    if (obj_is_fixnum(x) || obj_is_fixnum(y))
	return FALSE;

    x_class = obj_ptr(struct object *, x)->class;
    y_class = obj_ptr(struct object *, y)->class;

    if (x_class != y_class)
	return FALSE;

    if (x_class == obj_SingleFloatClass)
	return single_value(x) == single_value(y);

    if (x_class == obj_DoubleFloatClass)
	return double_value(x) == double_value(y);

    if (x_class == obj_ExtendedFloatClass)
	return extended_value(x) == extended_value(y);

    return FALSE;
}


/* Printer support. */

static void print_fixnum(obj_t fixnum)
{
    printf("%ld", fixnum_value(fixnum));
}

static void print_sf(obj_t sf)
{
    printf("%#g", single_value(sf));
}

static void print_df(obj_t df)
{
    printf("%#g", double_value(df));
}

static void print_xf(obj_t xf)
{
    printf("%#Lg", extended_value(xf));
}



/* Dylan routines. */

static obj_t dylan_idp(obj_t this, obj_t that)
{
    if (idp(this, that))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_int_negative(obj_t x)
{
    return make_fixnum(-fixnum_value(x));
}

static obj_t dylan_int_int_plus(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) + fixnum_value(y));
}

static obj_t dylan_int_int_minus(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) - fixnum_value(y));
}

static obj_t dylan_int_int_times(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) * fixnum_value(y));
}

static void dylan_int_int_trunc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    int x = fixnum_value(args[0]);
    int y = fixnum_value(args[1]);

    if (y == 0)
	error("Division by zero");
    else {
	int q = x / y;
	int r = x % y;

	/* The remainder is supposed to have the same sign as the dividend. */
	if ((r ^ x) < 0) {
	    r -= y;
	    q++;
	}
		
	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_int_int_floor(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    int x = fixnum_value(args[0]);
    int y = fixnum_value(args[1]);

    if (y == 0)
	error("Division by zero");
    else {
	int q = x / y;
	int r = x % y;

	/* The remainder is supposed to be the same sign as the divisor. */
	if ((r ^ y) < 0) {
	    r += y;
	    q--;
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_int_int_ceil(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    int x = fixnum_value(args[0]);
    int y = fixnum_value(args[1]);

    if (y == 0)
	error("Division by zero");
    else {
	int q = x / y;
	int r = x % y;

	/* The remainder is supposed to be the opposite sign from */
	/* the divisor.  */
	if ((r ^ y) >= 0) {
	    r -= y;
	    q++;
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_int_int_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    int x = fixnum_value(args[0]);
    int y = fixnum_value(args[1]);

    if (y == 0)
	error("Division by zero");
    else {
	int q = x / y;
	int r = x % y;

	/* The remainder should be smaller (i.e. closer to zero) than half */
	/* the divisor. */
	if (y >= 0) {
	    int limit = y >> 1;
	    if (r > limit || (r == limit && (q & 1))) {
		/* r is too large. */
		r -= y;
		q++;
	    }
	    else if (r < -limit || (r == -limit && (q & 1))) {
		/* r is too small */
		r += y;
		q--;
	    }
	}
	else {
	    int limit = -y >> 1;
	    if (r > limit || (r == limit && (q & 1))) {
		/* r is too large. */
		r += y;  /* note: y is negative. */
		q--;
	    }
	    else if (r < -limit || (r == -limit && (q & 1))) {
		/* r is too small */
		r -= y;  /* note: y is negative. */
		q++;
	    }
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static obj_t dylan_int_int_less(obj_t x, obj_t y)
{
    if (fixnum_value(x) < fixnum_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_int_int_equal(obj_t x, obj_t y)
{
    if (fixnum_value(x) == fixnum_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_ash(obj_t x, obj_t shift_obj)
{
    int shift = fixnum_value(shift_obj);

    if (shift < 0)
	return make_fixnum(fixnum_value(x) >> -shift);
    else
	return make_fixnum(fixnum_value(x) << shift);
}

static obj_t dylan_logand(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) & fixnum_value(y));
}

static obj_t dylan_logbitp(obj_t x, obj_t index)
{
    if (fixnum_value(x) & (1 << fixnum_value(index)))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_logior(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) | fixnum_value(y));
}

static obj_t dylan_lognot(obj_t x)
{
    return make_fixnum(~fixnum_value(x));
}

static obj_t dylan_logxor(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) ^ fixnum_value(y));
}

static obj_t dylan_sf_negative(obj_t x)
{
    return make_single(-single_value(x));
}

static obj_t dylan_sf_sf_plus(obj_t x, obj_t y)
{
    return make_single(single_value(x) + single_value(y));
}

static obj_t dylan_sf_sf_minus(obj_t x, obj_t y)
{
    return make_single(single_value(x) - single_value(y));
}

static obj_t dylan_sf_sf_times(obj_t x, obj_t y)
{
    return make_single(single_value(x) * single_value(y));
}

static obj_t dylan_sf_sf_divide(obj_t x, obj_t y)
{
    return make_single(single_value(x) / single_value(y));
}

static obj_t dylan_sf_sf_less(obj_t x, obj_t y)
{
    if (single_value(x) < single_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_sf_sf_less_or_eql(obj_t x, obj_t y)
{
    if (single_value(x) <= single_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_sf_sf_equal(obj_t x, obj_t y)
{
    if (single_value(x) == single_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_sf_sf_not_equal(obj_t x, obj_t y)
{
    if (single_value(x) != single_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_df_negative(obj_t x)
{
    return make_double(-double_value(x));
}

static obj_t dylan_df_df_plus(obj_t x, obj_t y)
{
    return make_double(double_value(x) + double_value(y));
}

static obj_t dylan_df_df_minus(obj_t x, obj_t y)
{
    return make_double(double_value(x) - double_value(y));
}

static obj_t dylan_df_df_times(obj_t x, obj_t y)
{
    return make_double(double_value(x) * double_value(y));
}

static obj_t dylan_df_df_divide(obj_t x, obj_t y)
{
    return make_double(double_value(x) / double_value(y));
}

static obj_t dylan_df_df_less(obj_t x, obj_t y)
{
    if (double_value(x) < double_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_df_df_less_or_eql(obj_t x, obj_t y)
{
    if (double_value(x) <= double_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_df_df_equal(obj_t x, obj_t y)
{
    if (double_value(x) == double_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_df_df_not_equal(obj_t x, obj_t y)
{
    if (double_value(x) != double_value(y))
	return obj_True;
    else
	return obj_False;
}


static obj_t dylan_xf_negative(obj_t x)
{
    return make_extended(-extended_value(x));
}

static obj_t dylan_xf_xf_plus(obj_t x, obj_t y)
{
    return make_extended(extended_value(x) + extended_value(y));
}

static obj_t dylan_xf_xf_minus(obj_t x, obj_t y)
{
    return make_extended(extended_value(x) - extended_value(y));
}

static obj_t dylan_xf_xf_times(obj_t x, obj_t y)
{
    return make_extended(extended_value(x) * extended_value(y));
}

static obj_t dylan_xf_xf_divide(obj_t x, obj_t y)
{
    return make_extended(extended_value(x) / extended_value(y));
}

static obj_t dylan_xf_xf_less(obj_t x, obj_t y)
{
    if (extended_value(x) < extended_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_xf_xf_less_or_eql(obj_t x, obj_t y)
{
    if (extended_value(x) <= extended_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_xf_xf_equal(obj_t x, obj_t y)
{
    if (extended_value(x) == extended_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_xf_xf_not_equal(obj_t x, obj_t y)
{
    if (extended_value(x) != extended_value(y))
	return obj_True;
    else
	return obj_False;
}


static obj_t dylan_identity(obj_t thing)
{
    return thing;
}

static obj_t dylan_int_as_sf(obj_t class, obj_t x)
{
    return make_single((float)fixnum_value(x));
}

static obj_t dylan_int_as_df(obj_t class, obj_t x)
{
    return make_double((double)fixnum_value(x));
}

static obj_t dylan_int_as_xf(obj_t class, obj_t x)
{
    return make_extended((long double)fixnum_value(x));
}

static obj_t dylan_sf_as_df(obj_t class, obj_t x)
{
    return make_double((double)single_value(x));
}

static obj_t dylan_sf_as_xf(obj_t class, obj_t x)
{
    return make_extended((long double)single_value(x));
}

static obj_t dylan_df_as_sf(obj_t class, obj_t x)
{
    return make_single((float)double_value(x));
}

static obj_t dylan_df_as_xf(obj_t class, obj_t x)
{
    return make_extended((long double)double_value(x));
}

static obj_t dylan_xf_as_sf(obj_t class, obj_t x)
{
    return make_single((float)extended_value(x));
}

static obj_t dylan_xf_as_df(obj_t class, obj_t x)
{
    return make_double((double)extended_value(x));
}



/* GC stuff. */

static int scav_sf(struct object *ptr)
{
    return sizeof(struct single_float);
}

static obj_t trans_sf(obj_t sf)
{
    return transport(sf, sizeof(struct single_float));
}

static int scav_df(struct object *ptr)
{
    return sizeof(struct double_float);
}

static obj_t trans_df(obj_t sf)
{
    return transport(sf, sizeof(struct double_float));
}

static int scav_xf(struct object *ptr)
{
    return sizeof(struct extended_float);
}

static obj_t trans_xf(obj_t sf)
{
    return transport(sf, sizeof(struct extended_float));
}

void scavenge_num_roots(void)
{
    scavenge(&obj_NumberClass);
    scavenge(&obj_ComplexClass);
    scavenge(&obj_RealClass);
    scavenge(&obj_RationalClass);
    scavenge(&obj_IntegerClass);
    scavenge(&obj_FloatClass);
    scavenge(&obj_SingleFloatClass);
    scavenge(&obj_DoubleFloatClass);
    scavenge(&obj_ExtendedFloatClass);
}


/* Init stuff. */

void make_num_classes(void)
{
    obj_NumberClass = make_abstract_class();
    obj_ComplexClass = make_abstract_class();
    obj_RealClass = make_abstract_class();
    obj_RationalClass = make_abstract_class();
    /* <integer> isn't really abstract, but there arn't heap instances */
    /* of it either. */
    obj_IntegerClass = make_abstract_class();
    obj_FloatClass = make_abstract_class();
    obj_SingleFloatClass = make_builtin_class(scav_sf, trans_sf);
    obj_DoubleFloatClass = make_builtin_class(scav_df, trans_df);
    obj_ExtendedFloatClass = make_builtin_class(scav_xf, trans_xf);
}

void init_num_classes(void)
{
    init_builtin_class(obj_NumberClass, "<number>", obj_ObjectClass, NULL);
    init_builtin_class(obj_ComplexClass, "<complex>", obj_NumberClass, NULL);
    init_builtin_class(obj_RealClass, "<real>", obj_ComplexClass, NULL);
    init_builtin_class(obj_RationalClass, "<rational>", obj_RealClass, NULL);
    init_builtin_class(obj_IntegerClass, "<integer>",obj_RationalClass, NULL);
    def_printer(obj_IntegerClass, print_fixnum);
    init_builtin_class(obj_FloatClass, "<float>", obj_RealClass, NULL);
    init_builtin_class(obj_SingleFloatClass, "<single-float>",
		       obj_FloatClass, NULL);
    def_printer(obj_SingleFloatClass, print_sf);
    init_builtin_class(obj_DoubleFloatClass, "<double-float>",
		       obj_FloatClass, NULL);
    def_printer(obj_DoubleFloatClass, print_df);
    init_builtin_class(obj_ExtendedFloatClass, "<extended-float>",
		       obj_FloatClass, NULL);
    def_printer(obj_ExtendedFloatClass, print_xf);
}

void init_num_functions(void)
{
    obj_t two_objs = list2(obj_ObjectClass, obj_ObjectClass);
    obj_t two_ints = list2(obj_IntegerClass, obj_IntegerClass);
    obj_t two_sfs = list2(obj_SingleFloatClass, obj_SingleFloatClass);
    obj_t two_dfs = list2(obj_DoubleFloatClass, obj_DoubleFloatClass);
    obj_t two_xfs = list2(obj_ExtendedFloatClass, obj_ExtendedFloatClass);
    obj_t int_sing = singleton(obj_IntegerClass);
    obj_t sf_sing = singleton(obj_SingleFloatClass);
    obj_t df_sing = singleton(obj_DoubleFloatClass);
    obj_t xf_sing = singleton(obj_ExtendedFloatClass);

    define_function("==", two_objs, FALSE, obj_False, obj_BooleanClass,
		    dylan_idp);
    define_method("=", two_objs, FALSE, obj_False, obj_BooleanClass,
		  dylan_idp);

    define_method("negative", list1(obj_IntegerClass), FALSE, obj_False,
		  obj_IntegerClass, dylan_int_negative);
    define_method("+", two_ints, FALSE, obj_False, obj_IntegerClass,
		  dylan_int_int_plus);
    define_method("-", two_ints, FALSE, obj_False, obj_IntegerClass,
		  dylan_int_int_minus);
    define_method("*", two_ints, FALSE, obj_False, obj_IntegerClass,
		  dylan_int_int_times);
    define_constant("truncate/",
		    make_raw_method("truncate/", two_ints, FALSE, obj_False,
				    two_ints, obj_False, dylan_int_int_trunc));
    define_constant("floor/",
		    make_raw_method("floor/", two_ints, FALSE, obj_False,
				    two_ints, obj_False, dylan_int_int_floor));
    define_constant("round/",
		    make_raw_method("round/", two_ints, FALSE, obj_False,
				    two_ints, obj_False, dylan_int_int_round));
    define_constant("ceiling/",
		    make_raw_method("ceiling/", two_ints, FALSE, obj_False,
				    two_ints, obj_False, dylan_int_int_ceil));
    define_method("<", two_ints,
		  FALSE, obj_False, obj_BooleanClass, dylan_int_int_less);
    define_method("=", two_ints,
		  FALSE, obj_False, obj_BooleanClass, dylan_int_int_equal);
    define_function("ash", two_ints,
		    FALSE, obj_False, obj_IntegerClass, dylan_ash);
    define_function("logand", two_ints,
		    FALSE, obj_False, obj_IntegerClass, dylan_logand);
    define_function("logbit?", two_ints,
		    FALSE, obj_False, obj_BooleanClass, dylan_logbitp);
    define_function("logior", two_ints,
		    FALSE, obj_False, obj_IntegerClass, dylan_logior);
    define_function("lognot", list1(obj_IntegerClass), FALSE, obj_False,
		    obj_IntegerClass, dylan_lognot);
    define_function("logxor", two_ints,
		    FALSE, obj_False, obj_IntegerClass, dylan_logxor);

    define_method("negative", list1(obj_SingleFloatClass), FALSE, obj_False,
		  obj_SingleFloatClass, dylan_sf_negative);
    define_method("+", two_sfs, FALSE, obj_False, obj_SingleFloatClass,
		  dylan_sf_sf_plus);
    define_method("-", two_sfs, FALSE, obj_False, obj_SingleFloatClass,
		  dylan_sf_sf_minus);
    define_method("*", two_sfs, FALSE, obj_False, obj_SingleFloatClass,
		  dylan_sf_sf_times);
    define_method("/", two_sfs, FALSE, obj_False, obj_SingleFloatClass,
		  dylan_sf_sf_divide);
    define_method("<", two_sfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_sf_sf_less);
    define_method("<=", two_sfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_sf_sf_less_or_eql);
    define_method("=", two_sfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_sf_sf_equal);
    define_method("~=", two_sfs,FALSE, obj_False, obj_BooleanClass,
		  dylan_sf_sf_not_equal);
    
    define_method("negative", list1(obj_DoubleFloatClass), FALSE, obj_False,
		  obj_DoubleFloatClass, dylan_df_negative);
    define_method("+", two_dfs, FALSE, obj_False, obj_DoubleFloatClass,
		  dylan_df_df_plus);
    define_method("-", two_dfs, FALSE, obj_False, obj_DoubleFloatClass,
		  dylan_df_df_minus);
    define_method("*", two_dfs, FALSE, obj_False, obj_DoubleFloatClass,
		  dylan_df_df_times);
    define_method("/", two_dfs, FALSE, obj_False, obj_DoubleFloatClass,
		  dylan_df_df_divide);
    define_method("<", two_dfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_df_df_less);
    define_method("<=", two_dfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_df_df_less_or_eql);
    define_method("=", two_dfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_df_df_equal);
    define_method("~=", two_dfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_df_df_not_equal);

    define_method("negative", list1(obj_ExtendedFloatClass), FALSE, obj_False,
		  obj_ExtendedFloatClass, dylan_xf_negative);
    define_method("+", two_xfs, FALSE, obj_False, obj_ExtendedFloatClass,
		  dylan_xf_xf_plus);
    define_method("-", two_xfs, FALSE, obj_False, obj_ExtendedFloatClass,
		  dylan_xf_xf_minus);
    define_method("*", two_xfs, FALSE, obj_False, obj_ExtendedFloatClass,
		  dylan_xf_xf_times);
    define_method("/", two_xfs, FALSE, obj_False, obj_ExtendedFloatClass,
		  dylan_xf_xf_divide);
    define_method("<", two_xfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_xf_xf_less);
    define_method("<=", two_xfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_xf_xf_less_or_eql);
    define_method("=", two_xfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_xf_xf_equal);
    define_method("~=", two_xfs, FALSE, obj_False, obj_BooleanClass,
		  dylan_xf_xf_not_equal);
    
    define_method("as", list2(int_sing, obj_IntegerClass),
		  FALSE, obj_False, obj_IntegerClass, dylan_identity);
    define_method("as", list2(sf_sing, obj_IntegerClass),
		  FALSE, obj_False, obj_SingleFloatClass, dylan_int_as_sf);
    define_method("as", list2(df_sing, obj_IntegerClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_int_as_df);
    define_method("as", list2(xf_sing, obj_IntegerClass),
		  FALSE, obj_False, obj_ExtendedFloatClass, dylan_int_as_xf);

    define_method("as", list2(sf_sing, obj_SingleFloatClass),
		  FALSE, obj_False, obj_SingleFloatClass, dylan_identity);
    define_method("as", list2(df_sing, obj_SingleFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_sf_as_df);
    define_method("as", list2(xf_sing, obj_SingleFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_sf_as_xf);

    define_method("as", list2(sf_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, obj_SingleFloatClass, dylan_df_as_sf);
    define_method("as", list2(df_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_identity);
    define_method("as", list2(xf_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_df_as_xf);

    define_method("as", list2(sf_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, obj_SingleFloatClass, dylan_xf_as_sf);
    define_method("as", list2(df_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_xf_as_df);
    define_method("as", list2(xf_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, obj_DoubleFloatClass, dylan_identity);
}
