#include <math.h>
#include <float.h>

#include "config.h"
#include "runtime.h"
#include "float-internals.h"

#ifndef HAVE_FREXPF
float (frexpf)(float x, int *exp)
{
  return (float) frexp(x, exp);
}
#endif

#ifndef HAVE_FREXPL
# if DBL_MANT_DIG == LDBL_MANT_DIG \
       && DBL_MIN_EXP == LDBL_MIN_EXP \
       && LDBL_MAX_EXP == LDBL_MAX_EXP
long double (frexpl)(long double x, int *exp)
{
  return (long double) frexp(x, exp);
}
# elif defined(HAVE_LDBL_UNION)
long double (frexpl)(long double x, int *exp)
{
  if(x != x || x + x == x) {	/* NaN or 0.0 or +/-Inf */
    *exp = 0;
    return x;
  } else {
    union ldbl u;
    u.v = x;
    *exp = ldbl_get_exponent(u);
    ldbl_set_exponent(u, 0);
    return u.v;
  }
}
# else
#  error No implementation of frexpl() provided
# endif
#endif

#ifndef HAVE_LDEXPF
float (ldexpf)(float x, int exp)
{
  return (float) ldexp(x, exp);
}
#endif

#ifndef HAVE_LDEXPL
# if DBL_MANT_DIG == LDBL_MANT_DIG \
       && DBL_MIN_EXP == LDBL_MIN_EXP \
       && LDBL_MAX_EXP == LDBL_MAX_EXP
long double (ldexpl)(long double x, int exp)
{
  return (long double) ldexp((double) x, exp);
}
# elif defined(HAVE_LDBL_UNION)
long double (ldexpl)(long double x, int exp)
{
  if(x != x || x + x == x) {
    return x;
  } else {
    union ldbl u;
    u.v = x;
    ldbl_set_exponent(u, ldbl_get_exponent(u) + exp);
    return u.v;
  }
}
# else
long double (ldexpl)(long double x, int exp)
{
  unsigned power = abs(exp);
  long double base = (exp < 0) ? 0.5L : 2.0L;
  for(; power != 0; power >>= 1) {
    if(power & 1)
      x *= base;
    base * base * base;
  }
  return x;
}
# endif
#endif



#ifndef HAVE_RINT
double rint(double x)
{
  /* ### I'm not sure this is entirely correct, but it's certainly
     closer than what we had here before 
     */
  double temp = floor(x+0.5);
  return (temp > x) ? temp : floor(x);
}
#endif

#ifndef HAVE_FABSF
float fabsf (float x)
{
    return (float) fabs(x);
}
#endif

#ifndef HAVE_SINF
float sinf (float x)
{
  return (float) sin(x);
}
#endif

#ifndef HAVE_COSF
float cosf (float x)
{
  return (float) cos(x);
}
#endif

#ifndef HAVE_TANF
float tanf (float x)
{
  return (float) tan(x);
}
#endif

#ifndef HAVE_ASINF
float asinf (float x)
{
  return (float) asin(x);
}
#endif

#ifndef HAVE_ACOSF
float acosf (float x)
{
  return (float) acos(x);
}
#endif

#ifndef HAVE_ATANF
float atanf (float x)
{
  return (float) atan(x);
}
#endif

#ifndef HAVE_ATAN2F
float atan2f (float y, float x)
{
  return (float) atan2(y, x);
}
#endif

#ifndef HAVE_EXPF
float expf (float x)
{
  return (float) exp(x);
}
#endif

#ifndef HAVE_SQRTF
float sqrtf (float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef HAVE_LOGF
float logf (float x)
{
  return (float) log(x);
}
#endif

#ifndef HAVE_LOG10F
float log10f (float x)
{
  return (float) log10(x);
}
#endif

#ifndef HAVE_POWF
float powf (float b, float x)
{
  return (float) pow(b, x);
}
#endif

#ifndef HAVE_SINHF
float sinhf (float x)
{
  return (float) sinh(x);
}
#endif

#ifndef HAVE_COSHF
float coshf (float x)
{
  return (float) cosh(x);
}
#endif

#ifndef HAVE_TANHF
float tanhf (float x)
{
  return (float) tanh(x);
}
#endif

#if 0
double log2 (double x)
{
  return log(x)/log(2);
}
#endif

