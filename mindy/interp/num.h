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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/num.h,v 1.2 1994/06/11 17:51:01 hallgren Exp $
*
* This file does whatever.
*
\**********************************************************************/

#define obj_is_fixnum(o) (!obj_is_ptr(o))

#define fixnum_value(o) (((long)(o))>>1)
#define make_fixnum(i) ((obj_t)(((long)(i))<<1))
#define MAX_FIXNUM ((obj_t)((((unsigned long)~0)<<2)>>1))
#define MIN_FIXNUM ((obj_t)~(((unsigned long)~0)>>1))

extern obj_t obj_IntegerClass;

struct single_float {
    obj_t class;
    float value;
};

extern obj_t make_single(float value);
#define single_value(x) (obj_ptr(struct single_float *, x)->value)

struct double_float {
    obj_t class;
    double value;
};

extern obj_t make_double(double value);
#define double_value(x) (obj_ptr(struct double_float *, x)->value)

struct extended_float {
    obj_t class;
    long double value;
};

extern obj_t make_extended(long double value);
#define extended_value(x) (obj_ptr(struct extended_float *, x)->value)

extern boolean idp(obj_t x, obj_t y);

