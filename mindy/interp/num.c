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
* $Header: /scm/cvs/src/mindy/interp/num.c,v 1.1 1998/05/03 19:55:16 andreas Exp $
*
* This file implements numbers.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include <math.h>

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
#include "module.h"
#include "sym.h"

obj_t obj_NumberClass = 0;
obj_t obj_ComplexClass = 0;
obj_t obj_RealClass = 0;
obj_t obj_RationalClass = 0;
obj_t obj_IntegerClass = 0;
obj_t obj_FixnumClass = 0;
obj_t obj_BignumClass = 0;
obj_t obj_RatioClass = 0;
obj_t obj_FloatClass = 0;
obj_t obj_SingleFloatClass = 0;
obj_t obj_DoubleFloatClass = 0;
obj_t obj_ExtendedFloatClass = 0;

#ifndef MAX
#define MAX(m, n) (((m) > (n)) ? (m) : (n))
#endif

#ifdef WIN32
#   define M_PI 3.14159265358979323846
#   define M_E  2.7182818284590452354
#endif

/* Simple constructors */

obj_t make_ratio(obj_t numerator, obj_t denominator)
{
    obj_t res = alloc(obj_RatioClass, sizeof(struct ratio));

    RATIO(res)->numerator = numerator;
    RATIO(res)->denominator = denominator;

    return res;
}

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


/* Extended Integer Support */

#define SIGN_MASK 0x80
#define DIGIT_MASK 0xff
#define DIGIT_BITS 8
#define SIGN(e) (BIGNUM(e)->digits[BIGNUM(e)->length-1] & SIGN_MASK)
#define ZEROP(e) (BIGNUM(e)->length == 1 && BIGNUM(e)->digits[0] == 0)
#define PAD(e) ((SIGN(e)) ? (DIGIT_MASK) : (0))

static obj_t alloc_bignum(int length)
{
    obj_t res = alloc(obj_BignumClass,
		      sizeof(struct bignum) + (length-1) * sizeof(digit_t));

    BIGNUM(res)->length = length;

    return res;
}

static void shrink_bignum(obj_t num, int length)
{
    shrink(num,
	   sizeof(struct bignum) + (BIGNUM(num)->length - 1) * sizeof(digit_t),
	   sizeof(struct bignum) + (length - 1) * sizeof(digit_t));
    BIGNUM(num)->length = length;
}

obj_t make_bignum(long value)
{
    obj_t res = alloc_bignum(sizeof(long) / sizeof(digit_t));
    digit_t *ptr = BIGNUM(res)->digits;
    boolean sign;

    if (value < 0)
	do {
	    sign = value & SIGN_MASK;
	    *ptr++ = value & DIGIT_MASK;
	    value >>= DIGIT_BITS;
	} while (value != -1 || !sign);
    else
	do {
	    sign = value & SIGN_MASK;
	    *ptr++ = value & DIGIT_MASK;
	    value >>= DIGIT_BITS;
	} while (value != 0 || sign);

    shrink_bignum(res, ptr - BIGNUM(res)->digits);

    return res;
}

long bignum_value(obj_t bignum)
{
    int length = BIGNUM(bignum)->length;
    digit_t *digits = BIGNUM(bignum)->digits;
    int i;
    long res = 0;

    if (digits[length-1] & SIGN_MASK)
	res = -1;

    for (i = length - 1; i >= 0; i--)
	res = (res << DIGIT_BITS) | digits[i];

    return res;
}

static void dump_bignum(obj_t bignum, int length)
{
    digit_t *digits = BIGNUM(bignum)->digits;
    digit_t *ptr = digits + length;

    while (ptr-- > digits) {
	printf("%02x ", *ptr);
    }
    printf("(%d)", length);
}

static obj_t extend_bignum(obj_t bignum, int length)
{
    obj_t res;
    int extend;
    int i;

    if (SIGN(bignum))
	extend = DIGIT_MASK;
    else
	extend = 0;

    if (BIGNUM(bignum)->length < length)
	res = alloc_bignum(length);
    else
        res = alloc_bignum(BIGNUM(bignum)->length);

    memcpy(BIGNUM(res)->digits, BIGNUM(bignum)->digits,
	   BIGNUM(bignum)->length * sizeof(digit_t));

    for (i = BIGNUM(bignum)->length; i < length; i++)
	BIGNUM(res)->digits[i] = extend;

    return res;
}

static void normalize_bignum(obj_t bignum, int length)
{
    digit_t *digits = BIGNUM(bignum)->digits;
    digit_t *ptr = digits + length - 1;
    int useless = (*ptr & SIGN_MASK) ? DIGIT_MASK : 0;
/*
    printf("normalizing "); dump_bignum(bignum, length);
*/
    while (ptr > digits && *ptr == useless)
	ptr--;

    if ((*ptr & SIGN_MASK) == (useless & SIGN_MASK))
	shrink_bignum(bignum, ptr - digits + 1);
    else
	shrink_bignum(bignum, ptr - digits + 2);
/*
    printf(" is "); dump_bignum(bignum, BIGNUM(bignum)->length); printf("\n");
*/
}

int compare_bignums(obj_t x, obj_t y)
{
    digit_t *x_digits = BIGNUM(x)->digits;
    digit_t *y_digits = BIGNUM(y)->digits;
    int x_length = BIGNUM(x)->length;
    int y_length = BIGNUM(y)->length;
    int i;

    if (x_digits[x_length-1] & SIGN_MASK) {
	if (y_digits[y_length-1] & SIGN_MASK) {
	    if (x_length > y_length)
		return -1;
	    else if (x_length < y_length)
		return 1;
	    /* else fall though */
	}
	else
	    return -1;
    }
    else {
	if (y_digits[y_length-1] & SIGN_MASK)
	    return 1;
	else {
	    if (x_length > y_length)
		return 1;
	    else if (x_length < y_length)
		return -1;
	    /* else fall though */
	}
    }

    for (i = x_length-1; i >= 0; i--) {
	digit_t x_digit = x_digits[i];
	digit_t y_digit = y_digits[i];

	if (x_digit != y_digit)
	    return x_digit - y_digit;
    }
    return 0;
}

obj_t add_bignums(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = MAX(len1, len2) + 1;
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i, carry = 0;

    if (len1 < len2) {
	for (i = 0; i < len1; i++) {
	    int sum = digits1[i] + digits2[i] + carry;
	    result[i] = sum & DIGIT_MASK;
	    carry = sum >> DIGIT_BITS;
	}
	for (i = len1; i < len2; i++) {
	    int sum = pad1 + digits2[i] + carry;
	    result[i] = sum & DIGIT_MASK;
	    carry = sum >> DIGIT_BITS;
	}
    }
    else {
	for (i = 0; i < len2; i++) {
	    int sum = digits1[i] + digits2[i] + carry;
	    result[i] = sum & DIGIT_MASK;
	    carry = sum >> DIGIT_BITS;
	}
	for (i = len2; i < len1; i++) {
	    int sum = digits1[i] + pad2 + carry;
	    result[i] = sum & DIGIT_MASK;
	    carry = sum >> DIGIT_BITS;
	}
    }
    result[length - 1] = (pad1 + pad2 + carry) & DIGIT_MASK;
    normalize_bignum(res, length);
/*
    printf("adding "); dump_bignum(x, BIGNUM(x)->length);
    printf(" and "); dump_bignum(y, BIGNUM(y)->length);
    printf(" is "); dump_bignum(res, BIGNUM(res)->length); printf("\n");
*/
    return res;

}

obj_t subtract_bignums(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = MAX(len1, len2) + 1;
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i, borrow = 0;

    if (len1 < len2) {
	for (i = 0; i < len1; i++) {
	    int sum = digits1[i] - digits2[i] - borrow;
	    result[i] = sum & DIGIT_MASK;
	    borrow = (sum >> DIGIT_BITS) & 1;
	}
	for (i = len1; i < len2; i++) {
	    int sum = pad1 - digits2[i] - borrow;
	    result[i] = sum & DIGIT_MASK;
	    borrow = (sum >> DIGIT_BITS) & 1;
	}
    }
    else {
	for (i = 0; i < len2; i++) {
	    int sum = digits1[i] - digits2[i] - borrow;
	    result[i] = sum & DIGIT_MASK;
	    borrow = (sum >> DIGIT_BITS) & 1;
	}
	for (i = len2; i < len1; i++) {
	    int sum = digits1[i] - pad2 - borrow;
	    result[i] = sum & DIGIT_MASK;
	    borrow = (sum >> DIGIT_BITS) & 1;
	}
    }
    result[length - 1] = (pad1 - pad2 - borrow) & DIGIT_MASK;
    normalize_bignum(res, length);
/*
    printf("subracting "); dump_bignum(x, BIGNUM(x)->length);
    printf(" and "); dump_bignum(y, BIGNUM(y)->length);
    printf(" is "); dump_bignum(res, BIGNUM(res)->length); printf("\n");
*/
    return res;
}

obj_t negate_bignum(obj_t x)
{
    int len = BIGNUM(x)->length;
    int length = len + 1;
    obj_t res = alloc_bignum(length);
    digit_t *digits = BIGNUM(x)->digits;
    digit_t *result = BIGNUM(res)->digits;
    int pad = PAD(x);
    int i;
    int borrow = 0;

    for (i = 0; i < len; i++) {
	int sum = 0 - digits[i] - borrow;
	result[i] = sum & DIGIT_MASK;
	borrow = (sum >> DIGIT_BITS) & 1;
    }
    result[length - 1] = (0 - pad - borrow) & DIGIT_MASK;
    normalize_bignum(res, length);
/*
    printf("negating "); dump_bignum(x, BIGNUM(x)->length);
    printf(" is "); dump_bignum(res, BIGNUM(res)->length); printf("\n");
*/
    return res;
}

obj_t multiply_bignums(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = len1 + len2;
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i, j;

    for (i = 0; i < length; i++)
        result[i] = 0;
    for (i = 0; i < len2; i++) {
	int carry = 0;

	for (j = 0; (j < len1) && (j < length - i); j++) {
	    int product = digits1[j] * digits2[i] + result[i+j] + carry;
	    result[i+j] = product & DIGIT_MASK;
	    carry = product >> DIGIT_BITS;
	}
	for (j = len1; j < length - i; j++) {
	    int product = pad1 * digits2[i] + result[i+j] + carry;
	    result[i+j] = product & DIGIT_MASK;
	    carry = product >> DIGIT_BITS;
	}
    }
    for (i = len2; i < length; i++) {
	int carry = 0;

	for (j = 0; (j < len1) && (j < length - i); j++) {
	    int product = digits1[j] * pad2 + result[i+j] + carry;
	    result[i+j] = product & DIGIT_MASK;
	    carry = product >> DIGIT_BITS;
	}
	for (j = len1; j < length - i; j++) {
	    int product = pad1 * pad2 + result[i+j] + carry;
	    result[i+j] = product & DIGIT_MASK;
	    carry = product >> DIGIT_BITS;
	}
    }
    normalize_bignum(res, length);
/*
    printf("multiplying "); dump_bignum(x, BIGNUM(x)->length);
    printf(" and "); dump_bignum(y, BIGNUM(y)->length);
    printf(" is "); dump_bignum(res, BIGNUM(res)->length); printf("\n");
*/
    return res;
}

static obj_t bignum_shift_left(obj_t bignum, int shift)
{
    int ndigits = shift / DIGIT_BITS;
    int nbits = shift % DIGIT_BITS;
    int len = BIGNUM(bignum)->length;
    int length = len + ndigits + 1;
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits = BIGNUM(bignum)->digits;
    int pad = PAD(bignum);
    int high_mask = (~0 << nbits) & DIGIT_MASK;
    int low_mask = ~high_mask & DIGIT_MASK;
    int i;

    for (i = 0; i < ndigits; i++)
        result[i] = 0;

    if (nbits == 0) {
	for (i = ndigits; i < length - 1; i++)
	    result[i] = digits[i - ndigits];
	result[length - 1] = pad;
    }
    else {
	result[ndigits] = (digits[0] << nbits) & high_mask;
	for (i = ndigits + 1; i < length - 1; i++)
	    result[i] =
	      ((digits[i-ndigits] << nbits) & high_mask)
		| ((digits[i-ndigits-1] >> (DIGIT_BITS - nbits)) & low_mask);
	result[length - 1] =
	  (pad & high_mask)
	    | ((digits[len - 1] >> (DIGIT_BITS - nbits)) & low_mask);
    }
    normalize_bignum(res, length);

    return res;
}

static obj_t bignum_shift_right(obj_t bignum, int shift)
{
    int ndigits = shift / DIGIT_BITS;
    int nbits = shift % DIGIT_BITS;
    int len = BIGNUM(bignum)->length;
    int length = len - ndigits;
    int pad = PAD(bignum);

    if (length < 1) {
	obj_t res = alloc_bignum(1);
	BIGNUM(res)->digits[0] = pad;
	return res;
    }
    else {
	obj_t res = alloc_bignum(length < 1 ? 1 : length);
	digit_t *result = BIGNUM(res)->digits;
	digit_t *digits = BIGNUM(bignum)->digits;
	int high_mask = (~0 << (DIGIT_BITS - nbits)) & DIGIT_MASK;
	int low_mask = ~high_mask & DIGIT_MASK;
	int i;

	if (nbits == 0) {
	    for (i = 0; i < length; i++)
		result[i] = digits[i + ndigits];
	}
	else {
	    for (i = 0; i < length - 1; i++)
		result[i] =
		    ((digits[i+ndigits] >> nbits) & low_mask)
			| ((digits[i+ndigits+1] << (DIGIT_BITS - nbits))
			   & high_mask);
	    result[length - 1] =
		((digits[len - 1] >> nbits) & low_mask)
		    | (pad & high_mask);
	}
	normalize_bignum(res, length);
	/*
	   printf("shifting "); dump_bignum(bignum, BIGNUM(bignum)->length);
	   printf(" by (%d, %d) is ", ndigits, nbits);
	   dump_bignum(res, BIGNUM(res)->length); printf("\n");
	   */
	return res;
    }
}

static void divide_by_digit(obj_t *quotient, int *remainder,
			    obj_t dividend, digit_t divisor)
{
    int length = BIGNUM(dividend)->length;
    digit_t *qptr, *dptr;
    int i;
    int d, q, r;

    *quotient = alloc_bignum(length);
    qptr = BIGNUM(*quotient)->digits + length;
    dptr = BIGNUM(dividend)->digits + length;

    r = 0;
    for (i = 0; i < length; i++) {
	d = (r << DIGIT_BITS) + *--dptr;
	q = d / divisor;
	r = d % divisor;
	*--qptr = q;
    }
    normalize_bignum(*quotient, length);
    *remainder = r;
}

static int division_shift(obj_t divisor)
{
    int y1 = BIGNUM(divisor)->digits[BIGNUM(divisor)->length - 1];
    int shift = 0;

    while (y1 > 0) {
	y1 = y1 >> 1;
	shift++;
    }

    return (DIGIT_BITS - shift - 1);
}

static int division_guess(int x1, int x2, int x3, int y1, int y2)
{
    int guess;
    int x12 = (x1 << DIGIT_BITS) | x2;
    int x123 = (x12 << DIGIT_BITS) | x3;
/*
    printf("starting guess with %02x %02x %02x / %02x %02x\n",
	   x1, x2, x3, y1, y2);
*/
    if (x1 == y1)
        guess = DIGIT_MASK;
    else
        guess = x12 / y1;
/*
    printf("initial guess is %x\n", guess);
*/
    while (TRUE) {
/*
	printf("x is %x\n", x123);
	printf("guess * y1 is %x\n", guess * y1 << DIGIT_BITS);
	printf("guess * y2 is %x\n", guess * y2);
	printf("x - guess * y1 is %x\n", x123 - ((guess * y1) << DIGIT_BITS));
*/
	if (x123 - ((guess * y1) << DIGIT_BITS) < guess * y2)
	    guess--;
	else
	    return guess;
/*
	printf("new guess is %x\n", guess);
*/
    }
}

static void divide(obj_t *quotient, obj_t *remainder,
		   obj_t dividend, obj_t divisor)
{
    obj_t x, y, q;
    digit_t *result, *digits1, *digits2;
    int len1, len2, length;
    int shift = division_shift(divisor);
    int i, j;
/*
    x = dividend; y = divisor;
    printf("dividing "); dump_bignum(x, BIGNUM(x)->length);
    printf(" by "); dump_bignum(y, BIGNUM(y)->length); printf("\n");
*/
    x = bignum_shift_left(dividend, shift);
    y = bignum_shift_left(divisor, shift);
    x = extend_bignum(x, BIGNUM(x)->length + 1);

    len1 = BIGNUM(x)->length;
    len2 = BIGNUM(y)->length;
    length = len1 - len2;
    digits1 = BIGNUM(x)->digits;
    digits2 = BIGNUM(y)->digits;

    q = alloc_bignum(length);
    result = BIGNUM(q)->digits;

    for (i = length - 1; i >= 0; i--) {
	int x1 = digits1[i + len2];
	int x2 = digits1[i + len2 - 1];
	int x3 = digits1[i + len2 - 2];
	int y1 = digits2[len2 - 1];
	int y2 = digits2[len2 - 2];
	int guess = division_guess(x1, x2, x3, y1, y2);
	int value, carry, borrow;
/*
	printf("doing digit %d of quotient\n", i);
	printf("guess is %d\n", guess);
*/
	carry = borrow = 0;
	for (j = 0; j < len2; j++) {
	    value = digits2[j] * guess + carry;
	    carry = value >> DIGIT_BITS;
	    value = digits1[i + j] - (value & DIGIT_MASK) - borrow;
	    digits1[i + j] = value & DIGIT_MASK;
	    borrow = (value >> DIGIT_BITS) & 1;
	}
	value = digits1[i + len2] - carry - borrow;
	digits1[i + len2] = value & DIGIT_MASK;

	if (value & SIGN_MASK) {
	    guess--;
	    carry = 0;
	    for (j = 0; j < len2; j++) {
		value = digits1[i + j] + digits2[j] + carry;
		digits1[i + j] = value & DIGIT_MASK;
		carry = value >> DIGIT_BITS;
	    }
	    value = digits1[i + len2] + carry;
	    digits1[i + len2] = value & DIGIT_MASK;
	}
/*
	printf("remainder is "); dump_bignum(x, BIGNUM(x)->length); printf("\n");
*/
	result[i] = guess;
    }
    normalize_bignum(x, len1);
    normalize_bignum(q, length);
    *remainder = bignum_shift_right(x, shift);
    *quotient = q;
}

static void bignum_divide(obj_t *q, obj_t *r, obj_t x, obj_t y)
{
    int len1, len2;
    digit_t *digits1, *digits2;

    if (ZEROP(y))
        error("Division by zero");

    len1 = BIGNUM(x)->length;
    len2 = BIGNUM(y)->length;
    digits1 = BIGNUM(x)->digits;
    digits2 = BIGNUM(y)->digits;

    if (len1 < len2
	|| (len1 == len2 && digits1[len1 - 1] < digits2[len2 - 1])) {
	*q = make_bignum(0);
	*r = x;
    }
    else if (len2 == 1) {
	int r_value;
	divide_by_digit(q, &r_value, x, digits2[0]);
	*r = make_bignum(r_value);
    }
    else
	divide(q, r, x, y);
}

static void print_bignum_aux(obj_t bignum, int radix)
{
    int remainder;
    obj_t quotient;

    divide_by_digit(&quotient, &remainder, bignum, radix);
    if (!ZEROP(quotient))
	print_bignum_aux(quotient, radix);
    if (remainder < 10)
	putchar('0' + remainder);
    else
	putchar('a' + remainder - 10);
}

void print_bignum(obj_t bignum, int radix)
{
    if (SIGN(bignum)) {
	putchar('-');
	bignum = negate_bignum(bignum);
    }

    print_bignum_aux(bignum, radix);
}


/* id? */

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

    if (x_class == obj_BignumClass)
        return (compare_bignums(x, y) == 0);

    if (x_class == obj_RatioClass)
	return (idp(RATIO(x)->numerator, RATIO(y)->numerator)
		&& idp(RATIO(x)->denominator, RATIO(y)->denominator));

    if (x_class == obj_SingleFloatClass)
	return single_value(x) == single_value(y);

    if (x_class == obj_DoubleFloatClass)
	return double_value(x) == double_value(y);

    if (x_class == obj_ExtendedFloatClass)
	return extended_value(x) == extended_value(y);

    return FALSE;
}

static obj_t dylan_idp(obj_t this, obj_t that)
{
    if (idp(this, that))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_not_idp(obj_t this, obj_t that)
{
    if (idp(this, that))
	return obj_False;
    else
	return obj_True;
}


/* Printer support. */

static void print_fixnum(obj_t fixnum)
{
    printf("%ld", fixnum_value(fixnum));
}

static void print_bignum_object(obj_t bignum)
{
    printf("#e");
    print_bignum(bignum, 10);
}

static void print_ratio(obj_t ratio)
{
    format("%d/%d", RATIO(ratio)->numerator, RATIO(ratio)->denominator);
}

static void print_sf(obj_t sf)
{
    printf("%#g", single_value(sf));
}

static void change_exponent_marker(char *ptr, int marker)
{
    while (*ptr != '\0' && *ptr != 'e' && *ptr != 'E')
	ptr++;
    if (*ptr == '\0') {
	ptr[0] = marker;
	ptr[1] = '0';
	ptr[2] = '\0';
    }
    else
	ptr[0] = marker;
}

static void print_df(obj_t df)
{
    char buffer[64];

    sprintf(buffer, "%#g", double_value(df));
    change_exponent_marker(buffer, 'd');
    printf("%s", buffer);
}

static void print_xf(obj_t xf)
{
    char buffer[64];

    sprintf(buffer, "%#g", (double)extended_value(xf));
    change_exponent_marker(buffer, 'x');
    printf("%s", buffer);
}


/* Dylan Routines -- Fixed Integers */

static obj_t dylan_fi_negative(obj_t x)
{
    return make_fixnum(-fixnum_value(x));
}

static obj_t dylan_fi_fi_plus(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) + fixnum_value(y));
}

static obj_t dylan_fi_fi_minus(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) - fixnum_value(y));
}

static obj_t dylan_fi_fi_times(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) * fixnum_value(y));
}

static void dylan_fi_fi_trunc(obj_t self, struct thread *thread, obj_t *args)
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
	if (r != 0 && (r ^ x) < 0) {
	    r -= y;
	    q++;
	}
		
	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_fi_fi_floor(obj_t self, struct thread *thread, obj_t *args)
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
	if (r != 0 && (r ^ y) < 0) {
	    r += y;
	    q--;
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_fi_fi_ceil(obj_t self, struct thread *thread, obj_t *args)
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
	if (r != 0 && (r ^ y) >= 0) {
	    r -= y;
	    q++;
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static void dylan_fi_fi_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    int x = fixnum_value(args[0]);
    int y = fixnum_value(args[1]);

    if (y == 0)
	error("Division by zero");
    else {
	int q = x / y;
	int r = x % y;

	if (r != 0) {
	    /* The remainder should be smaller (i.e. closer to zero) than */
	    /* half the divisor. */
	    if (y > 0) {
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
	}

	thread->sp = old_sp + 2;

	old_sp[0] = make_fixnum(q);
	old_sp[1] = make_fixnum(r);
	
	do_return(thread, old_sp, old_sp);
    }
}

static obj_t dylan_fi_fi_less(obj_t x, obj_t y)
{
    if (fixnum_value(x) < fixnum_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_fi_fi_equal(obj_t x, obj_t y)
{
    if (fixnum_value(x) == fixnum_value(y))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_fi_ash(obj_t x, obj_t shift_obj)
{
    int shift = fixnum_value(shift_obj);

    if (shift < 0)
	return make_fixnum(fixnum_value(x) >> -shift);
    else
	return make_fixnum(fixnum_value(x) << shift);
}

static obj_t dylan_fi_fi_logand(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) & fixnum_value(y));
}

static obj_t dylan_fi_logbitp(obj_t index, obj_t x)
{
    if (fixnum_value(x) & (1 << fixnum_value(index)))
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_fi_fi_logior(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) | fixnum_value(y));
}

static obj_t dylan_fi_lognot(obj_t x)
{
    return make_fixnum(~fixnum_value(x));
}

static obj_t dylan_fi_fi_logxor(obj_t x, obj_t y)
{
    return make_fixnum(fixnum_value(x) ^ fixnum_value(y));
}


/* Dylan Routines -- Extended Integers */

static obj_t dylan_ei_ei_equal(obj_t x, obj_t y)
{
    if (compare_bignums(x, y) == 0)
	return obj_True;
    else
	return obj_False;
}

static obj_t dylan_ei_ei_less(obj_t x, obj_t y)
{
    if (compare_bignums(x, y) < 0)
        return obj_True;
    else
        return obj_False;
}

static void dylan_ei_ei_floor(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    obj_t x = args[0];
    obj_t y = args[1];
    obj_t xabs, yabs, q, r;
    boolean xneg, yneg;

    if ((xneg = SIGN(x)))
	xabs = negate_bignum(x);
    else
	xabs = x;

    if ((yneg = SIGN(y)))
	yabs = negate_bignum(y);
    else
	yabs = y;

    bignum_divide(&q, &r, xabs, yabs);

    if (yneg)
	r = negate_bignum(r);
    if (xneg != yneg) {
	q = negate_bignum(q);
	if (!ZEROP(r)) {
	    q = subtract_bignums(q, make_bignum(1));
	    r = subtract_bignums(y, r);
	}
    }

    thread->sp = old_sp + 2;
    old_sp[0] = q;
    old_sp[1] = r;

    do_return(thread, old_sp, old_sp);
}

static void dylan_ei_ei_ceil(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    obj_t x = args[0];
    obj_t y = args[1];
    obj_t xabs, yabs, q, r;
    boolean xneg, yneg;

    if ((xneg = SIGN(x)))
	xabs = negate_bignum(x);
    else
	xabs = x;

    if ((yneg = SIGN(y)))
	yabs = negate_bignum(y);
    else
	yabs = y;

    bignum_divide(&q, &r, xabs, yabs);

    if (xneg)
	r = negate_bignum(r);

    if (xneg != yneg)
	q = negate_bignum(q);
    else if (!ZEROP(r)) {
	q = add_bignums(q, make_bignum(1));
	r = subtract_bignums(r, y);
    }

    thread->sp = old_sp + 2;
    old_sp[0] = q;
    old_sp[1] = r;

    do_return(thread, old_sp, old_sp);
}

static void dylan_ei_ei_trunc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    obj_t x = args[0];
    obj_t y = args[1];
    obj_t xabs, yabs, q, r;
    boolean xneg, yneg;

    if ((xneg = SIGN(x)))
	xabs = negate_bignum(x);
    else
	xabs = x;

    if ((yneg = SIGN(y)))
	yabs = negate_bignum(y);
    else
	yabs = y;

    bignum_divide(&q, &r, xabs, yabs);

    if (xneg != yneg)
	q = negate_bignum(q);
    if (xneg)
	r = negate_bignum(r);

    thread->sp = old_sp + 2;
    old_sp[0] = q;
    old_sp[1] = r;

    do_return(thread, old_sp, old_sp);
}

static void dylan_ei_ei_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    obj_t x = args[0];
    obj_t y = args[1];
    obj_t xabs, yabs, q, r, twice_r;
    boolean xneg, yneg;
    int cmp;

    if ((xneg = SIGN(x)))
	xabs = negate_bignum(x);
    else
	xabs = x;

    if ((yneg = SIGN(y)))
	yabs = negate_bignum(y);
    else
	yabs = y;

    bignum_divide(&q, &r, xabs, yabs);

    twice_r = add_bignums(r, r);
    cmp = compare_bignums(twice_r, yabs);

    if (cmp > 0 || (cmp == 0 && (BIGNUM(q)->digits[0] & 1) != 0)) {
	q = add_bignums(q, make_bignum(1));
	r = subtract_bignums(r, yabs);
    }

    if (xneg != yneg)
	q = negate_bignum(q);
    if (xneg)
	r = negate_bignum(r);

    thread->sp = old_sp + 2;
    old_sp[0] = q;
    old_sp[1] = r;

    do_return(thread, old_sp, old_sp);
}

static obj_t dylan_ei_ei_logior(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = MAX(len1, len2);
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i;

    if (len1 < len2) {
	for (i = 0; i < len1; i++)
	    result[i] = digits1[i] | digits2[i];
	for (i = len1; i < length; i++)
	    result[i] = pad1 | digits2[i];
    }
    else {
	for (i = 0; i < len2; i++)
	    result[i] = digits1[i] | digits2[i];
	for (i = len2; i < length; i++)
	    result[i] = digits1[i] | pad2;
    }
    normalize_bignum(res, length);

    return res;
}

static obj_t dylan_ei_ei_logxor(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = MAX(len1, len2);
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i;

    if (len1 < len2) {
	for (i = 0; i < len1; i++)
	    result[i] = digits1[i] ^ digits2[i];
	for (i = len1; i < length; i++)
	    result[i] = pad1 ^ digits2[i];
    }
    else {
	for (i = 0; i < len2; i++)
	    result[i] = digits1[i] ^ digits2[i];
	for (i = len2; i < length; i++)
	    result[i] = digits1[i] ^ pad2;
    }
    normalize_bignum(res, length);

    return res;
}

static obj_t dylan_ei_ei_logand(obj_t x, obj_t y)
{
    int len1 = BIGNUM(x)->length;
    int len2 = BIGNUM(y)->length;
    int length = MAX(len1, len2);
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits1 = BIGNUM(x)->digits;
    digit_t *digits2 = BIGNUM(y)->digits;
    int pad1 = PAD(x);
    int pad2 = PAD(y);
    int i;

    if (len1 < len2) {
	for (i = 0; i < len1; i++)
	    result[i] = digits1[i] & digits2[i];
	for (i = len1; i < length; i++)
	    result[i] = pad1 & digits2[i];
    }
    else {
	for (i = 0; i < len2; i++)
	    result[i] = digits1[i] & digits2[i];
	for (i = len2; i < length; i++)
	    result[i] = digits1[i] & pad2;
    }
    normalize_bignum(res, length);

    return res;
}

static obj_t dylan_ei_lognot(obj_t x)
{
    int length = BIGNUM(x)->length;
    obj_t res = alloc_bignum(length);
    digit_t *result = BIGNUM(res)->digits;
    digit_t *digits = BIGNUM(x)->digits;
    int i;

    for (i = 0; i < length; i++)
	result[i] = ~digits[i];
    normalize_bignum(res, length);

    return res;
}

static obj_t dylan_ei_logbitp(obj_t i, obj_t x)
{
    int index = fixnum_value(i);
    int digit = index / DIGIT_BITS;
    int bit = index % DIGIT_BITS;

    if (index < 0)
	return obj_False;

    if (digit >= BIGNUM(x)->length) {
	if (SIGN(x))
	    return obj_True;
	else
	    return obj_False;
    }

    if (BIGNUM(x)->digits[digit] & (1 << bit))
        return obj_True;
    else
        return obj_False;
}

static obj_t dylan_ei_ash(obj_t x, obj_t shift_count)
{
    int shift = fixnum_value(shift_count);

    if (shift > 0)
        return bignum_shift_left(x, shift);
    else
        return bignum_shift_right(x, -shift);
}


/* Dylan Routines -- Ratios */

static obj_t dylan_numerator(obj_t ratio)
{
    return (RATIO(ratio)->numerator);
}

static obj_t dylan_denominator(obj_t ratio)
{
    return (RATIO(ratio)->denominator);
}

static obj_t dylan_numerator_setter(obj_t value, obj_t ratio)
{
    return (RATIO(ratio)->numerator = value);
}

static obj_t dylan_denominator_setter(obj_t value, obj_t ratio)
{
    return (RATIO(ratio)->denominator = value);
}


/* Dylan Routines -- Single Float */

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

static void dylan_sf_trunc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    float x = single_value(args[0]);
    int res = x < 0 ? ceil(x) : floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_single(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_sf_floor(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    float x = single_value(args[0]);
    int res = floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_single(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_sf_ceil(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    float x = single_value(args[0]);
    int res = ceil(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_single(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_sf_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    float x = single_value(args[0]);
    int res = rint(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_single(x - res);

    do_return(thread, old_sp, old_sp);
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


/* Dylan Routines -- Double Float */

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

static void dylan_df_trunc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    double x = double_value(args[0]);
    int res = x < 0 ? ceil(x) : floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_double(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_df_floor(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    double x = double_value(args[0]);
    int res = floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_double(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_df_ceil(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    double x = double_value(args[0]);
    int res = ceil(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_double(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_df_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    double x = double_value(args[0]);
    int res = rint(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_double(x - res);

    do_return(thread, old_sp, old_sp);
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


/* Dylan Routines -- Extended Float */

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

static void dylan_xf_trunc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    long double x = extended_value(args[0]);
    int res = x < 0 ? ceil(x) : floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_extended(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_xf_floor(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    long double x = extended_value(args[0]);
    int res = floor(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_extended(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_xf_ceil(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    long double x = extended_value(args[0]);
    int res = ceil(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_extended(x - res);

    do_return(thread, old_sp, old_sp);
}

static void dylan_xf_round(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    long double x = extended_value(args[0]);
    int res = rint(x);

    thread->sp = old_sp + 2;

    old_sp[0] = make_fixnum(res);
    old_sp[1] = make_extended(x - res);

    do_return(thread, old_sp, old_sp);
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


/* Coercions */

static obj_t dylan_as_identity(obj_t class, obj_t thing)
{
    return thing;
}

static obj_t dylan_fi_as_ei(obj_t class, obj_t x)
{
    return make_bignum(fixnum_value(x));
}

static obj_t dylan_fi_as_sf(obj_t class, obj_t x)
{
    return make_single((float)fixnum_value(x));
}

static obj_t dylan_fi_as_df(obj_t class, obj_t x)
{
    return make_double((double)fixnum_value(x));
}

static obj_t dylan_fi_as_xf(obj_t class, obj_t x)
{
    return make_extended((long double)fixnum_value(x));
}

static obj_t dylan_ei_as_fi(obj_t class, obj_t x)
{
    int length = BIGNUM(x)->length;
    digit_t *digits = BIGNUM(x)->digits;
    int i;
    long res = 0;

    if (digits[length-1] & SIGN_MASK) {
	/* It is negative, make sure it is not too negative. */
	if (compare_bignums(x, as_bignum(MIN_FIXNUM)) < 0)
	    error("Can't convert %= to <integer>", x);
	res = -1;
    }
    else {
	/* It is positive, make sure it is not too positive. */
	if (compare_bignums(x, as_bignum(MAX_FIXNUM)) > 0)
	    error("Can't convert %= to <integer>", x);
    }

    for (i = length - 1; i >= 0; i--)
	res = (res << DIGIT_BITS) | digits[i];
    return make_fixnum(res);
}

static obj_t dylan_ei_as_sf(obj_t class, obj_t x)
{
    int length = BIGNUM(x)->length;
    digit_t *digits = BIGNUM(x)->digits;
    digit_t digit = 0;
    float res = 0;
    float base = (float) (1 << DIGIT_BITS);
    float place = 1;
    int i;

    for (i = 0; i < length; i++) {
	digit = digits[i];
	res += ((float) digit) * place;
	place *= base;
    }

    if (digit & SIGN_MASK)
	return make_single(res - base);
    else
	return make_single(res);
}

static obj_t dylan_ei_as_df(obj_t class, obj_t x)
{
    int length = BIGNUM(x)->length;
    digit_t *digits = BIGNUM(x)->digits;
    digit_t digit = 0;
    double res = 0;
    double base = (double) (1 << DIGIT_BITS);
    double place = 1;
    int i;

    for (i = 0; i < length; i++) {
	digit = digits[i];
	res += ((double) digit) * place;
	place *= base;
    }

    if (digit & SIGN_MASK)
	return make_double(res - base);
    else
	return make_double(res);
}

static obj_t dylan_ei_as_xf(obj_t class, obj_t x)
{
    int length = BIGNUM(x)->length;
    digit_t *digits = BIGNUM(x)->digits;
    digit_t digit = 0;
    long double res = 0;
    long double base = (long double) (1 << DIGIT_BITS);
    long double place = 1;
    int i;

    for (i = 0; i < length; i++) {
	digit = digits[i];
	res += ((long double) digit) * place;
	place *= base;
    }
    if (digit & SIGN_MASK)
	return make_extended(res - base);
    else
	return make_extended(res);
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



/* Transcendental support */

#define define_transcendental_function(function)                        \
    static obj_t dylan_sf_##function (obj_t sf)                         \
    {                                                                   \
	return make_single((float) function(single_value(sf)));         \
    }                                                                   \
    static obj_t dylan_df_##function (obj_t df)                         \
    {                                                                   \
	return make_double((long double) function(double_value(df)));   \
    }

define_transcendental_function(sin)
define_transcendental_function(cos)
define_transcendental_function(tan)
define_transcendental_function(asin)
define_transcendental_function(acos)
define_transcendental_function(atan)
define_transcendental_function(sinh)
define_transcendental_function(cosh)
define_transcendental_function(tanh)
define_transcendental_function(exp)
define_transcendental_function(log)
define_transcendental_function(sqrt)

static obj_t dylan_sf_atan2 (obj_t sf1, obj_t sf2)                 
{                                                                   
    return make_single((float) atan2(single_value(sf1),          
				    single_value(sf2)));        
}                                                                   

static obj_t dylan_df_atan2 (obj_t df1, obj_t df2)             
{                                                                   
    return make_double((long double) atan2(double_value(df1),    
					   double_value(df2)));  
}

/* The Common Lisp expt() function is called pow() in C
 */
static obj_t dylan_sf_expt (obj_t sf1, obj_t sf2)                 
{                                                                   
    return make_single((float) pow(single_value(sf1),          
				   single_value(sf2)));        
}                                                                   

static obj_t dylan_df_expt (obj_t df1, obj_t df2)             
{                                                                   
    return make_double((long double) pow(double_value(df1),    
					 double_value(df2)));  
}


/* GC stuff. */

static int scav_bignum(struct object *ptr)
{
    int length = ((struct bignum *)ptr)->length;
    return (sizeof(struct bignum) + (length - 1) * sizeof(digit_t));
}

static obj_t trans_bignum(obj_t sf)
{
    int length = BIGNUM(sf)->length;
    return transport(sf,
		     (sizeof(struct bignum) + (length - 1) * sizeof(digit_t)),
		     TRUE);
}

static int scav_ratio(struct object *ptr)
{
    struct ratio *ratio = (struct ratio *) ptr;

    scavenge(&ratio->numerator);
    scavenge(&ratio->denominator);

    return sizeof(struct ratio);
}

static obj_t trans_ratio(obj_t ratio)
{
    return transport(ratio, sizeof(struct ratio), TRUE);
}

static int scav_sf(struct object *ptr)
{
    return sizeof(struct single_float);
}

static obj_t trans_sf(obj_t sf)
{
    return transport(sf, sizeof(struct single_float), TRUE);
}

static int scav_df(struct object *ptr)
{
    return sizeof(struct double_float);
}

static obj_t trans_df(obj_t sf)
{
    return transport(sf, sizeof(struct double_float), TRUE);
}

static int scav_xf(struct object *ptr)
{
    return sizeof(struct extended_float);
}

static obj_t trans_xf(obj_t sf)
{
    return transport(sf, sizeof(struct extended_float), TRUE);
}


/* Init stuff. */

void make_num_classes(void)
{
    obj_NumberClass = make_abstract_class(FALSE);
    obj_ComplexClass = make_abstract_class(FALSE);
    obj_RealClass = make_abstract_class(TRUE);
    obj_RationalClass = make_abstract_class(TRUE);
    obj_IntegerClass = make_abstract_class(TRUE);

    /* isn't really abstract, but there arn't heap instances either */

    obj_FixnumClass = make_abstract_class(TRUE);
    obj_BignumClass = make_builtin_class(scav_bignum, trans_bignum);
    obj_RatioClass = make_builtin_class(scav_ratio, trans_ratio);
    obj_FloatClass = make_abstract_class(TRUE);
    obj_SingleFloatClass = make_builtin_class(scav_sf, trans_sf);
    obj_DoubleFloatClass = make_builtin_class(scav_df, trans_df);
    obj_ExtendedFloatClass = make_builtin_class(scav_xf, trans_xf);

    add_constant_root(&obj_NumberClass);
    add_constant_root(&obj_ComplexClass);
    add_constant_root(&obj_RealClass);
    add_constant_root(&obj_RationalClass);
    add_constant_root(&obj_IntegerClass);
    add_constant_root(&obj_FixnumClass);
    add_constant_root(&obj_BignumClass);
    add_constant_root(&obj_RatioClass);
    add_constant_root(&obj_FloatClass);
    add_constant_root(&obj_SingleFloatClass);
    add_constant_root(&obj_DoubleFloatClass);
    add_constant_root(&obj_ExtendedFloatClass);
}

void init_num_classes(void)
{
    init_builtin_class(obj_NumberClass, "<number>", obj_ObjectClass, NULL);
    init_builtin_class(obj_ComplexClass, "<complex>", obj_NumberClass, NULL);
    init_builtin_class(obj_RealClass, "<real>", obj_ComplexClass, NULL);
    init_builtin_class(obj_RationalClass, "<rational>", obj_RealClass, NULL);
    init_builtin_class(obj_IntegerClass, "<general-integer>",
		       obj_RationalClass, NULL);
    init_builtin_class(obj_FixnumClass, "<integer>", obj_IntegerClass,
		       NULL);
    def_printer(obj_FixnumClass, print_fixnum);
    init_builtin_class(obj_BignumClass, "<extended-integer>", obj_IntegerClass,
		       NULL);
    def_printer(obj_BignumClass, print_bignum_object);
    init_builtin_class(obj_RatioClass, "<ratio>", obj_RationalClass, NULL);
    def_printer(obj_RatioClass, print_ratio);
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

#define add_transcendental_function(function)                              \
    define_generic_function(#function, any_float, FALSE, obj_False, FALSE, \
			    any_float, obj_False);                         \
    define_method(#function, sf, FALSE, obj_False, FALSE,                  \
		  obj_SingleFloatClass, dylan_sf_##function);              \
    define_method(#function, df, FALSE, obj_False, FALSE,                  \
		  obj_DoubleFloatClass, dylan_df_##function);

void init_num_functions(void)
{
    obj_t fi = list1(obj_FixnumClass);
    obj_t ei = list1(obj_BignumClass);
    obj_t ratio = list1(obj_RatioClass);
    obj_t sf = list1(obj_SingleFloatClass);
    obj_t df = list1(obj_DoubleFloatClass);
    obj_t xf = list1(obj_ExtendedFloatClass);
    obj_t any_float = list1(obj_FloatClass);
    obj_t any_real = list1(obj_RealClass);
    obj_t two_objs = list2(obj_ObjectClass, obj_ObjectClass);
    obj_t two_ints = list2(obj_IntegerClass, obj_IntegerClass);
    obj_t two_fis = list2(obj_FixnumClass, obj_FixnumClass);
    obj_t two_eis = list2(obj_BignumClass, obj_BignumClass);
    obj_t two_sfs = list2(obj_SingleFloatClass, obj_SingleFloatClass);
    obj_t two_dfs = list2(obj_DoubleFloatClass, obj_DoubleFloatClass);
    obj_t two_xfs = list2(obj_ExtendedFloatClass, obj_ExtendedFloatClass);
    obj_t two_reals = list2(obj_RealClass, obj_RealClass);
    obj_t two_floats = list2(obj_FloatClass, obj_FloatClass);
    obj_t int_and_real = list2(obj_IntegerClass, obj_RealClass);
    obj_t int_and_sf = list2(obj_IntegerClass, obj_SingleFloatClass);
    obj_t int_and_df = list2(obj_IntegerClass, obj_DoubleFloatClass);
    obj_t int_and_xf = list2(obj_IntegerClass, obj_ExtendedFloatClass);
    obj_t fi_sing = singleton(obj_FixnumClass);
    obj_t ei_sing = singleton(obj_BignumClass);
    obj_t float_sing = singleton(obj_FloatClass);
    obj_t sf_sing = singleton(obj_SingleFloatClass);
    obj_t df_sing = singleton(obj_DoubleFloatClass);
    obj_t xf_sing = singleton(obj_ExtendedFloatClass);

    define_function("==", two_objs, FALSE, obj_False, FALSE, obj_BooleanClass,
		    dylan_idp);
    define_function("~==", two_objs, FALSE, obj_False, FALSE, obj_BooleanClass,
		    dylan_not_idp);
    define_generic_function("=", two_objs, FALSE, obj_False, FALSE,
			    list1(obj_BooleanClass), obj_False);
    define_method("=", two_objs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_idp);

    define_generic_function("truncate/", two_reals, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("truncate", any_real, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("floor/", two_reals, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("floor", any_real, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("ceiling/", two_reals, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("ceiling", any_real, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("round/", two_reals, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);
    define_generic_function("round", any_real, FALSE, obj_False, FALSE,
			    int_and_real, obj_False);

    define_generic_function("negative", list1(obj_ObjectClass), 
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
    define_generic_function("+", two_objs,
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
    define_generic_function("-", two_objs,
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
    define_generic_function("*", two_objs,
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);
    define_generic_function("/", two_objs,
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_ObjectClass);

    define_generic_function("<", two_objs,
			    FALSE, obj_False, FALSE,
			    list1(obj_BooleanClass), obj_False);

    define_method("negative", fi, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_negative);
    define_method("+", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_plus);
    define_method("-", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_minus);
    define_method("*", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_times);
    add_method(find_variable(module_BuiltinStuff, symbol("truncate/"),
			     FALSE, FALSE)->value,
	       make_raw_method("truncate/", two_fis, FALSE, obj_False, FALSE,
			       two_fis, obj_False, dylan_fi_fi_trunc));
    add_method(find_variable(module_BuiltinStuff, symbol("floor/"),
			     FALSE, FALSE)->value,
	       make_raw_method("floor/", two_fis, FALSE, obj_False, FALSE,
			       two_fis, obj_False, dylan_fi_fi_floor));
    add_method(find_variable(module_BuiltinStuff, symbol("ceiling/"),
			     FALSE, FALSE)->value,
	       make_raw_method("ceiling/", two_fis, FALSE, obj_False, FALSE,
			       two_fis, obj_False, dylan_fi_fi_ceil));
    add_method(find_variable(module_BuiltinStuff, symbol("round/"),
			     FALSE, FALSE)->value,
	       make_raw_method("round/", two_fis, FALSE, obj_False, FALSE,
			       two_fis, obj_False, dylan_fi_fi_round));
    define_method("<", two_fis, FALSE, obj_False, FALSE,
		  obj_BooleanClass, dylan_fi_fi_less);
    define_method("=", two_fis, FALSE, obj_False, FALSE,
		  obj_BooleanClass, dylan_fi_fi_equal);
    define_method("ash", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_ash);
    define_method("binary-logand", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_logand);
    define_method("logbit?", two_fis, FALSE, obj_False, FALSE,
		  obj_BooleanClass, dylan_fi_logbitp);
    define_method("binary-logior", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_logior);
    define_method("lognot", fi, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_lognot);
    define_method("binary-logxor", two_fis, FALSE, obj_False, FALSE,
		  obj_FixnumClass, dylan_fi_fi_logxor);

    define_method("=", two_eis, FALSE, obj_False, FALSE,
		  obj_BooleanClass, dylan_ei_ei_equal);
    define_method("<", two_eis, FALSE, obj_False, FALSE,
		  obj_BooleanClass, dylan_ei_ei_less);
    define_method("negative", ei, FALSE, obj_False, FALSE,
		  obj_BignumClass, negate_bignum);
    define_method("+", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, add_bignums);
    define_method("-", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, subtract_bignums);
    define_method("*", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, multiply_bignums);
    add_method(find_variable(module_BuiltinStuff, symbol("floor/"),
			     FALSE, FALSE)->value,
	       make_raw_method("floor/", two_eis, FALSE, obj_False, FALSE,
			       two_eis, obj_False, dylan_ei_ei_floor));
    add_method(find_variable(module_BuiltinStuff, symbol("ceiling/"),
			     FALSE, FALSE)->value,
	       make_raw_method("ceiling/", two_eis, FALSE, obj_False, FALSE,
			       two_eis, obj_False, dylan_ei_ei_ceil));
    add_method(find_variable(module_BuiltinStuff, symbol("round/"),
			     FALSE, FALSE)->value,
	       make_raw_method("round/", two_eis, FALSE, obj_False, FALSE,
			       two_eis, obj_False, dylan_ei_ei_round));
    add_method(find_variable(module_BuiltinStuff, symbol("truncate/"),
			     FALSE, FALSE)->value,
	       make_raw_method("truncate/", two_eis, FALSE, obj_False, FALSE,
			       two_eis, obj_False, dylan_ei_ei_trunc));
    define_method("binary-logior", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, dylan_ei_ei_logior);
    define_method("binary-logand", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, dylan_ei_ei_logand);
    define_method("binary-logxor", two_eis, FALSE, obj_False, FALSE,
		  obj_BignumClass, dylan_ei_ei_logxor);
    define_method("lognot", ei, FALSE, obj_False, FALSE,
		  obj_BignumClass, dylan_ei_lognot);
    define_method("logbit?", list2(obj_FixnumClass, obj_BignumClass), FALSE,
		  obj_False, FALSE, obj_BooleanClass, dylan_ei_logbitp);
    define_method("ash", list2(obj_BignumClass, obj_FixnumClass), FALSE,
		  obj_False, FALSE, obj_BignumClass, dylan_ei_ash);

    define_method("make-ratio", two_ints, FALSE, obj_False,
		  FALSE, obj_RatioClass, make_ratio);
    define_method("numerator", ratio, FALSE, obj_False, FALSE,
		  obj_IntegerClass, dylan_numerator);
    define_method("denominator", ratio, FALSE, obj_False, FALSE,
		  obj_IntegerClass, dylan_denominator);
    define_method("numerator-setter", list2(obj_ObjectClass, obj_RatioClass),
		  FALSE, obj_False, FALSE, obj_IntegerClass,
		  dylan_numerator_setter);
    define_method("denominator-setter", list2(obj_ObjectClass, obj_RatioClass),
		  FALSE, obj_False, FALSE, obj_IntegerClass,
		  dylan_denominator_setter);

    define_method("negative", sf, FALSE, obj_False, FALSE,
		  obj_SingleFloatClass, dylan_sf_negative);
    define_method("+", two_sfs, FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_sf_sf_plus);
    define_method("-", two_sfs, FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_sf_sf_minus);
    define_method("*", two_sfs, FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_sf_sf_times);
    define_method("/", two_sfs, FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_sf_sf_divide);
    add_method(find_variable(module_BuiltinStuff, symbol("truncate"),
			     FALSE, FALSE)->value,
	       make_raw_method("truncate", sf, FALSE, obj_False, FALSE,
			       int_and_sf, obj_False, dylan_sf_trunc));
    add_method(find_variable(module_BuiltinStuff, symbol("floor"),
			     FALSE, FALSE)->value,
	       make_raw_method("floor", sf, FALSE, obj_False, FALSE,
			       int_and_sf, obj_False, dylan_sf_floor));
    add_method(find_variable(module_BuiltinStuff, symbol("ceiling"),
			     FALSE, FALSE)->value,
	       make_raw_method("ceiling", sf, FALSE, obj_False, FALSE,
			       int_and_sf, obj_False, dylan_sf_ceil));
    add_method(find_variable(module_BuiltinStuff, symbol("round"),
			     FALSE, FALSE)->value,
	       make_raw_method("round", sf, FALSE, obj_False, FALSE,
			       int_and_sf, obj_False, dylan_sf_round));
    define_method("<", two_sfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_sf_sf_less);
    define_method("<=", two_sfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_sf_sf_less_or_eql);
    define_method("=", two_sfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_sf_sf_equal);
    define_method("~=", two_sfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_sf_sf_not_equal);
    
    define_method("negative", df, FALSE, obj_False, FALSE,
		  obj_DoubleFloatClass, dylan_df_negative);
    define_method("+", two_dfs, FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_df_df_plus);
    define_method("-", two_dfs, FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_df_df_minus);
    define_method("*", two_dfs, FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_df_df_times);
    define_method("/", two_dfs, FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_df_df_divide);
    add_method(find_variable(module_BuiltinStuff, symbol("truncate"),
			     FALSE, FALSE)->value,
	       make_raw_method("truncate", df, FALSE, obj_False, FALSE,
			       int_and_df, obj_False, dylan_df_trunc));
    add_method(find_variable(module_BuiltinStuff, symbol("floor"),
			     FALSE, FALSE)->value,
	       make_raw_method("floor", df, FALSE, obj_False, FALSE,
			       int_and_df, obj_False, dylan_df_floor));
    add_method(find_variable(module_BuiltinStuff, symbol("ceiling"),
			     FALSE, FALSE)->value,
	       make_raw_method("ceiling", df, FALSE, obj_False, FALSE,
			       int_and_df, obj_False, dylan_df_ceil));
    add_method(find_variable(module_BuiltinStuff, symbol("round"),
			     FALSE, FALSE)->value,
	       make_raw_method("round", df, FALSE, obj_False, FALSE,
			       int_and_df, obj_False, dylan_df_round));
    define_method("<", two_dfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_df_df_less);
    define_method("<=", two_dfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_df_df_less_or_eql);
    define_method("=", two_dfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_df_df_equal);
    define_method("~=", two_dfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_df_df_not_equal);

    define_method("negative", list1(obj_ExtendedFloatClass), FALSE, obj_False,
		  FALSE, obj_ExtendedFloatClass, dylan_xf_negative);
    define_method("+", two_xfs, FALSE, obj_False, FALSE,
		  obj_ExtendedFloatClass, dylan_xf_xf_plus);
    define_method("-", two_xfs, FALSE, obj_False, FALSE,
		  obj_ExtendedFloatClass, dylan_xf_xf_minus);
    define_method("*", two_xfs, FALSE, obj_False, FALSE,
		  obj_ExtendedFloatClass, dylan_xf_xf_times);
    define_method("/", two_xfs, FALSE, obj_False, FALSE,
		  obj_ExtendedFloatClass, dylan_xf_xf_divide);
    add_method(find_variable(module_BuiltinStuff, symbol("truncate"),
			     FALSE, FALSE)->value,
	       make_raw_method("truncate", xf, FALSE, obj_False, FALSE,
			       int_and_xf, obj_False, dylan_xf_trunc));
    add_method(find_variable(module_BuiltinStuff, symbol("floor"),
			     FALSE, FALSE)->value,
	       make_raw_method("floor", xf, FALSE, obj_False, FALSE,
			       int_and_xf, obj_False, dylan_xf_floor));
    add_method(find_variable(module_BuiltinStuff, symbol("ceiling"),
			     FALSE, FALSE)->value,
	       make_raw_method("ceiling", xf, FALSE, obj_False, FALSE,
			       int_and_xf, obj_False, dylan_xf_ceil));
    add_method(find_variable(module_BuiltinStuff, symbol("round"),
			     FALSE, FALSE)->value,
	       make_raw_method("round", xf, FALSE, obj_False, FALSE,
			       int_and_xf, obj_False, dylan_xf_round));
    define_method("<", two_xfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_xf_xf_less);
    define_method("<=", two_xfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_xf_xf_less_or_eql);
    define_method("=", two_xfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_xf_xf_equal);
    define_method("~=", two_xfs, FALSE, obj_False, FALSE, obj_BooleanClass,
		  dylan_xf_xf_not_equal);
    
    define_method("as", list2(fi_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass,
		  dylan_as_identity);
    define_method("as", list2(ei_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_BignumClass,
		  dylan_fi_as_ei);
    define_method("as", list2(float_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_fi_as_sf);
    define_method("as", list2(sf_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_fi_as_sf);
    define_method("as", list2(df_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_fi_as_df);
    define_method("as", list2(xf_sing, obj_FixnumClass),
		  FALSE, obj_False, FALSE, obj_ExtendedFloatClass,
		  dylan_fi_as_xf);

    define_method("as", list2(ei_sing, obj_BignumClass),
		  FALSE, obj_False, FALSE, obj_BignumClass,
		  dylan_as_identity);
    define_method("as", list2(fi_sing, obj_BignumClass),
		  FALSE, obj_False, FALSE, obj_FixnumClass,
		  dylan_ei_as_fi);
    define_method("as", list2(sf_sing, obj_BignumClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_ei_as_sf);
    define_method("as", list2(df_sing, obj_BignumClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_ei_as_df);
    define_method("as", list2(xf_sing, obj_BignumClass),
		  FALSE, obj_False, FALSE, obj_ExtendedFloatClass,
		  dylan_ei_as_xf);

    define_method("as", list2(float_sing, obj_FloatClass),
		  FALSE, obj_False, FALSE, obj_FloatClass,
		  dylan_as_identity);

    define_method("as", list2(sf_sing, obj_SingleFloatClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_as_identity);
    define_method("as", list2(df_sing, obj_SingleFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_sf_as_df);
    define_method("as", list2(xf_sing, obj_SingleFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_sf_as_xf);

    define_method("as", list2(sf_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_df_as_sf);
    define_method("as", list2(df_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_as_identity);
    define_method("as", list2(xf_sing, obj_DoubleFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_df_as_xf);

    define_method("as", list2(sf_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, FALSE, obj_SingleFloatClass,
		  dylan_xf_as_sf);
    define_method("as", list2(df_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_xf_as_df);
    define_method("as", list2(xf_sing, obj_ExtendedFloatClass),
		  FALSE, obj_False, FALSE, obj_DoubleFloatClass,
		  dylan_as_identity);

    define_constant("$maximum-integer", MAX_FIXNUM);
    define_constant("$minimum-integer", MIN_FIXNUM);

    define_constant("$single-pi", make_single((float) M_PI));
    define_constant("$single-e", make_single((float) M_E));
    define_constant("$double-pi", make_double(M_PI));
    define_constant("$double-e", make_double(M_E));

    add_transcendental_function(sin);
    add_transcendental_function(cos);
    add_transcendental_function(tan);
    add_transcendental_function(asin);
    add_transcendental_function(acos);
    add_transcendental_function(atan);
    add_transcendental_function(sinh);
    add_transcendental_function(cosh);
    add_transcendental_function(tanh);
    add_transcendental_function(exp);
    add_transcendental_function(log);
    add_transcendental_function(sqrt);

    define_generic_function("atan2", two_floats, FALSE, obj_False, FALSE,      
			    any_float, obj_False);                             
    define_method("atan2", two_sfs, FALSE, obj_False, FALSE,  
		  obj_SingleFloatClass, dylan_sf_atan2);     
    define_method("atan2", two_dfs, FALSE, obj_False, FALSE,  
		  obj_DoubleFloatClass, dylan_df_atan2);     

    define_method("^", two_sfs, FALSE, obj_False, FALSE,  
		  obj_SingleFloatClass, dylan_sf_expt);     
    define_method("^", two_dfs, FALSE, obj_False, FALSE,  
		  obj_DoubleFloatClass, dylan_df_expt);     
}
