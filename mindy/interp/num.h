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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/num.h,v 1.4 1994/06/27 16:32:26 wlott Exp $
*
\**********************************************************************/

#define obj_is_fixnum(o) (!obj_is_ptr(o))

#define fixnum_value(o) (((long)(o))>>1)
#define make_fixnum(i) ((obj_t)(((long)(i))<<1))
#define MAX_FIXNUM ((obj_t)((((unsigned long)~0)<<2)>>1))
#define MIN_FIXNUM ((obj_t)~(((unsigned long)~0)>>1))

extern obj_t obj_IntegerClass;
extern obj_t obj_SingleFloatClass;     /* table.c needs the floats */
extern obj_t obj_DoubleFloatClass;
extern obj_t obj_ExtendedFloatClass;

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

