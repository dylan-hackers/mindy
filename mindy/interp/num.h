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
* $Header: /scm/cvs/src/mindy/interp/num.h,v 1.1 1998/05/03 19:55:16 andreas Exp $
*
\**********************************************************************/

#define obj_is_fixnum(o) (!obj_is_ptr(o))
#define fixnum_value(o) (((long)(o))>>1)
#define make_fixnum(i) ((obj_t)(((long)(i))<<1))
#define MAX_FIXNUM ((obj_t)((((unsigned long)~0)<<2)>>1))
#define MIN_FIXNUM ((obj_t)~(((unsigned long)~0)>>1))

typedef unsigned char digit_t;

struct bignum {
    obj_t class;
    int length;
    digit_t digits[1];
};

#define BIGNUM(o) obj_ptr(struct bignum *, o)

extern obj_t make_bignum(long value);
#define as_bignum(i) (obj_is_fixnum(i)?make_bignum(fixnum_value(i)):(i))
extern long bignum_value(obj_t x);
extern int compare_bignums(obj_t x, obj_t y);
extern obj_t add_bignums(obj_t x, obj_t y);
extern obj_t subtract_bignums(obj_t x, obj_t y);
extern obj_t negate_bignum(obj_t x);
extern obj_t multiple_bignums(obj_t x, obj_t y);

extern void print_bignum(obj_t bignum, int radix);


struct ratio {
    obj_t class;
    obj_t numerator;
    obj_t denominator;
};

#define RATIO(o) obj_ptr(struct ratio *, o)

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

extern obj_t obj_IntegerClass;
extern obj_t obj_FixnumClass;
extern obj_t obj_BignumClass;
extern obj_t obj_FloatClass;     /* table.c needs the floats */
extern obj_t obj_SingleFloatClass;
extern obj_t obj_DoubleFloatClass;
extern obj_t obj_ExtendedFloatClass;

extern boolean idp(obj_t x, obj_t y);
