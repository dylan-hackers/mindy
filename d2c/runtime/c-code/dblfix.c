#include "runtime.h"

#ifndef GD_HAVE_LONG_LONG

int double_integer_eq(gd_long_long a, gd_long_long b)
{
  return a.hi == b.hi && a.lo == b.lo;
}

int double_integer_lt(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_add(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_mul(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_sub(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_neg(gd_long_long a)
{
  abort();
}

gd_long_long double_integer_div(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_mod(gd_long_long a, gd_long_long b)
{
  abort();
}

gd_long_long double_integer_logior(gd_long_long a, gd_long_long b)
{
  gd_long_long temp;
  temp.hi = a.hi | b.hi;
  temp.hi = a.lo | b.lo;
  return temp;
}

gd_long_long double_integer_logxor(gd_long_long a, gd_long_long b)
{
  gd_long_long temp;
  temp.hi = a.hi ^ b.hi;
  temp.hi = a.lo ^ b.lo;
  return temp;
}

gd_long_long double_integer_logand(gd_long_long a, gd_long_long b)
{
  gd_long_long temp;
  temp.hi = a.hi & b.hi;
  temp.hi = a.lo & b.lo;
  return temp;
}

gd_long_long double_integer_lognot(gd_long_long a)
{
  gd_long_long temp;
  temp.hi = ~a.hi;
  return temp;
}

gd_long_long double_integer_shl(gd_long_long a, long b)
{
  abort();
}

gd_long_long double_integer_shr(gd_long_long a, long b)
{
  abort();
}

gd_long_long integer_to_double_integer(long b)
{
  gd_long_long temp;
  temp.hi = (b < 0) ? -1 : 0;
  temp.lo = b;
  return temp;
}

long double_integer_to_integer(gd_long_long b)
{
  return b.lo;
}

#endif


