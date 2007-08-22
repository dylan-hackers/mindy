#include <stddef.h>
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



#ifndef HAVE_LOGL
# if DBL_MANT_DIG == LDBL_MANT_DIG \
       && DBL_MIN_EXP == LDBL_MIN_EXP \
       && LDBL_MAX_EXP == LDBL_MAX_EXP
long double (logl)(long double x)
{
  return (long double) log(x);
}
# else
# define CEPHESL
/*
 * Cephes Math Library Release 2.7:  May, 1998
 * Copyright 1984, 1990, 1998 by Stephen L. Moshier
 */

#define SQRTHL	0.70710678118654752440L

static long double polevll(long double, long double *, size_t);
static long double p1evll(long double, long double *, size_t);

long double (logl)(long double x)
{
  /* Coefficients for log(1+x) = x - x**2/2 + x**3 P(x)/Q(x) 1/sqrt(2) <= x <
   * sqrt(2) Theoretical peak relative error = 2.32e-20
   */
  static long double P[] = {
    4.5270000862445199635215E-5L,
    4.9854102823193375972212E-1L,
    6.5787325942061044846969E0L,
    2.9911919328553073277375E1L,
    6.0949667980987787057556E1L,
    5.7112963590585538103336E1L,
    2.0039553499201281259648E1L,
  };
  static long double Q[] = {
    /* 1.0000000000000000000000E0, */
    1.5062909083469192043167E1L,
    8.3047565967967209469434E1L,
    2.2176239823732856465394E2L,
    3.0909872225312059774938E2L,
    2.1642788614495947685003E2L,
    6.0118660497603843919306E1L,
  };
  
  /* Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2), where z = 2(x-1)/(x+1)
   * 1/sqrt(2) <= x < sqrt(2) Theoretical peak relative error = 6.16e-22
   */
  static long double R[4] = {
    1.9757429581415468984296E-3L,
    -7.1990767473014147232598E-1L,
    1.0777257190312272158094E1L,
    -3.5717684488096787370998E1L,
  };
  static long double S[4] = {
    /* 1.00000000000000000000E0L, */
    -2.6201045551331104417768E1L,
    1.9361891836232102174846E2L,
    -4.2861221385716144629696E2L,
  };
  static long double C1 = 6.9314575195312500000000E-1L;
  static long double C2 = 1.4286068203094172321215E-6L;

  long double y, z;
  int e;

  if (x != x)
    return (x);
  if (x == INFINITY)
    return (x);

  /* Test for domain */
  if (x <= 0.0L) {
    if (x == 0.0L) {
      return (-INFINITY);
    } else {
      return ((x - x) / (x - x));
    }
  }

  /* separate mantissa from exponent
   * Note, frexp is used so that denormal numbers will be handled properly.
   */
  x = frexpl(x, &e);

  /* logarithm using log(x) = z + z**3 P(z)/Q(z), where z = 2(x-1)/x+1) */
  if ((e > 2) || (e < -2)) {
    if (x < SQRTHL) {		/* 2( 2x-1 )/( 2x+1 ) */
      e -= 1;
      z = x - 0.5L;
      y = 0.5L * z + 0.5L;
    } else {			/* 2 (x-1)/(x+1)   */
      z = x - 0.5L;
      z -= 0.5L;
      y = 0.5L * x + 0.5L;
    }
    x = z / y;
    z = x * x;
    z = x * (z * polevll(z, R, 3) / p1evll(z, S, 3));
    z = z + e * C2;
    z = z + x;
    z = z + e * C1;
    return (z);
  }

  /* logarithm using log(1+x) = x - .5x**2 + x**3 P(x)/Q(x) */
  if (x < SQRTHL) {
    e -= 1;
    x = ldexpl(x, 1) - 1.0L;	/* 2x - 1  */
  } else {
    x = x - 1.0L;
  }
  z = x * x;
  y = x * (z * polevll(x, P, 6) / p1evll(x, Q, 6));
  y = y + e * C2;
  z = y - ldexpl(z, -1);	/* y - 0.5 * z  */

  /*
   * Note, the sum of above terms does not exceed x/4, so it contributes at
   * most about 1/4 lsb to the error.
   */
  z = z + x;
  z = z + e * C1;		/* This sum has an error of 1/2 lsb. */
  return (z);
}
# endif
#endif

#ifndef HAVE_LOG2L
# if DBL_MANT_DIG == LDBL_MANT_DIG \
       && DBL_MIN_EXP == LDBL_MIN_EXP \
       && LDBL_MAX_EXP == LDBL_MAX_EXP
long double (log2l)(long double x)
{
  return (long double) log2(x);
}
# else
# define CEPHESL
/*
 * Cephes Math Library Release 2.8:  May, 1998
 * Copyright 1984, 1991, 1998 by Stephen L. Moshier
 */

#define LOG2EAL	4.4269504088896340735992e-1L
#define SQRTHL	0.70710678118654752440L

static long double polevll(long double, long double *, size_t);
static long double p1evll(long double, long double *, size_t);

long double (log2l)(long double x)
{
  static long double P[] = {
    4.9962495940332550844739E-1L,
    1.0767376367209449010438E1L,
    7.7671073698359539859595E1L,
    2.5620629828144409632571E2L,
    4.2401812743503691187826E2L,
    3.4258224542413922935104E2L,
    1.0747524399916215149070E2L,
  };
  static long double Q[] = {
    /* 1.0000000000000000000000E0, */
    2.3479774160285863271658E1L,
    1.9444210022760132894510E2L,
    7.7952888181207260646090E2L,
    1.6911722418503949084863E3L,
    2.0307734695595183428202E3L,
    1.2695660352705325274404E3L,
    3.2242573199748645407652E2L,
  };

  /* Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2), where z = 2(x-1)/(x+1)
   * 1/sqrt(2) <= x < sqrt(2) Theoretical peak relative error = 6.16e-22
   */
  static long double R[4] = {
    1.9757429581415468984296E-3L,
    -7.1990767473014147232598E-1L,
    1.0777257190312272158094E1L,
    -3.5717684488096787370998E1L,
  };
  static long double S[4] = {
    /* 1.00000000000000000000E0L, */
    -2.6201045551331104417768E1L,
    1.9361891836232102174846E2L,
    -4.2861221385716144629696E2L,
  };
  
  volatile long double z;
  long double y;
  int e;

  if (x != x)
    return (x);
  if (x == INFINITY)
    return (x);
  /* Test for domain */
  if (x <= 0.0L) {
    if (x == 0.0L) {
      return (-INFINITY);
    } else {
      return ((x - x) / (x - x));
    }
  }
  /* separate mantissa from exponent
   * Note, frexp is used so that denormal numbers will be handled properly.
   */
  x = frexpl(x, &e);

  if ((e > 2) || (e < -2)) {
    /* logarithm using log(x) = z + z**3 P(z)/Q(z), where z = 2(x-1)/x+1) */
    if (x < SQRTHL) {		/* 2( 2x-1 )/( 2x+1 ) */
      e -= 1;
      z = x - 0.5L;
      y = 0.5L * z + 0.5L;
    } else {			/* 2 (x-1)/(x+1)   */
      z = x - 0.5L;
      z -= 0.5L;
      y = 0.5L * x + 0.5L;
    }
    x = z / y;
    z = x * x;
    y = x * (z * polevll(z, R, 3) / p1evll(z, S, 3));
  } else {
    /* logarithm using log(1+x) = x - .5x**2 + x**3 P(x)/Q(x) */

    if (x < SQRTHL) {
      e -= 1;
      x = ldexpl(x, 1) - 1.0L;	/* 2x - 1  */
    } else {
      x = x - 1.0L;
    }
    z = x * x;
    y = x * (z * polevll(x, P, 6) / p1evll(x, Q, 7));
    y = y - ldexpl(z, -1);	/* -0.5x^2 + ... */
  }

  /*
   * Multiply log of fraction by log2(e) and base 2 exponent by 1
   * 
   * ***CAUTION***
   * 
   * This sequence of operations is critical and it may be horribly defeated by
   * some compiler optimizers.
   */
  z = y * LOG2EAL;
  z += x * LOG2EAL;
  z += y;
  z += x;
  z += e;
  return (z);
}
# endif
#endif

#ifndef HAVE_LOG10L
# if DBL_MANT_DIG == LDBL_MANT_DIG \
       && DBL_MIN_EXP == LDBL_MIN_EXP \
       && LDBL_MAX_EXP == LDBL_MAX_EXP
long double (log10l)(long double x)
{
  return (long double) log10(x);
}
# else
# define CEPHESL
/*
 * Cephes Math Library Release 2.2:  January, 1991
 * Copyright 1984, 1991 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */

/* log10(2) */
#define L102A	0.3125L
#define L102B	-1.1470004336018804786261e-2L
/* log10(e) */
#define L10EA	0.5L
#define L10EB	-6.5705518096748172348871e-2L

#define SQRTHL	0.70710678118654752440L

static long double polevll(long double, long double *, size_t);
static long double p1evll(long double, long double *, size_t);

long double (log10l)(long double x)
{
  /* Coefficients for log(1+x) = x - x**2/2 + x**3 P(x)/Q(x) 1/sqrt(2) <= x <
   * sqrt(2) Theoretical peak relative error = 6.2e-22
   */
  static long double P[] = {
    4.9962495940332550844739E-1L,
    1.0767376367209449010438E1L,
    7.7671073698359539859595E1L,
    2.5620629828144409632571E2L,
    4.2401812743503691187826E2L,
    3.4258224542413922935104E2L,
    1.0747524399916215149070E2L,
  };
  static long double Q[] = {
    /* 1.0000000000000000000000E0, */
    2.3479774160285863271658E1L,
    1.9444210022760132894510E2L,
    7.7952888181207260646090E2L,
    1.6911722418503949084863E3L,
    2.0307734695595183428202E3L,
    1.2695660352705325274404E3L,
    3.2242573199748645407652E2L,
  };

  /* Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2), where z = 2(x-1)/(x+1)
   * 1/sqrt(2) <= x < sqrt(2) Theoretical peak relative error = 6.16e-22
   */
  static long double R[4] = {
    1.9757429581415468984296E-3L,
    -7.1990767473014147232598E-1L,
    1.0777257190312272158094E1L,
    -3.5717684488096787370998E1L,
  };
  static long double S[4] = {
    /* 1.00000000000000000000E0L, */
    -2.6201045551331104417768E1L,
    1.9361891836232102174846E2L,
    -4.2861221385716144629696E2L,
  };

  long double y;
  volatile long double z;
  int e;

  if (x != x)
    return (x);
  if (x == INFINITY)
    return (INFINITY);
  
  /* Test for domain */
  if (x <= 0.0L) {
    if (x == 0.0L) {
      return (-INFINITY);
    } else {
      return ((x - x) / (x - x));
    }
  }

  /* separate mantissa from exponent
   * Note, frexp is used so that denormal numbers will be handled properly.
   */
  x = frexpl(x, &e);

  if ((e > 2) || (e < -2)) {
    /* logarithm using log(x) = z + z**3 P(z)/Q(z), where z = 2(x-1)/x+1) */
    if (x < SQRTHL) {		/* 2( 2x-1 )/( 2x+1 ) */
      e -= 1;
      z = x - 0.5L;
      y = 0.5L * z + 0.5L;
    } else {			/* 2 (x-1)/(x+1)   */
      z = x - 0.5L;
      z -= 0.5L;
      y = 0.5L * x + 0.5L;
    }
    x = z / y;
    z = x * x;
    y = x * (z * polevll(z, R, 3) / p1evll(z, S, 3));
  } else {
    /* logarithm using log(1+x) = x - .5x**2 + x**3 P(x)/Q(x) */
    if (x < SQRTHL) {
      e -= 1;
      x = ldexpl(x, 1) - 1.0L;	/* 2x - 1  */
    } else {
      x = x - 1.0L;
    }
    z = x * x;
    y = x * (z * polevll(x, P, 6) / p1evll(x, Q, 7));
    y = y - ldexpl(z, -1);	/* -0.5x^2 + ... */
  }

  /*
   * Multiply log of fraction by log10(e) and base 2 exponent by log10(2).
   * 
   ***CAUTION***
   * 
   * This sequence of operations is critical and it may be horribly defeated by
   * some compiler optimizers.
   */
  z = y * (L10EB);
  z += x * (L10EB);
  z += e * (L102B);
  z += y * (L10EA);
  z += x * (L10EA);
  z += e * (L102A);

  return (z);
}
# endif
#endif

#ifndef HAVE_RINT
double (rint)(double x)
{
  /* ### I'm not sure this is entirely correct, but it's certainly
     closer than what we had here before 
     */
  double temp = floor(x+0.5);
  return (temp > x) ? temp : floor(x);
}
#endif

#ifndef HAVE_LOG2
# define CEPHES
/*
 * Cephes Math Library Release 2.8:  June, 2000
 * Copyright 1984, 1995, 2000 by Stephen L. Moshier
 */

#define LOG2EA	0.44269504088896340735992	/* log2(e) - 1 */
#define SQRTH	0.70710678118654752440

static double polevl(double, double *, size_t);
static double p1evl(double, double *, size_t);

double (log2)(double x)
{
  /* Coefficients for log(1+x) = x - x**2/2 + x**3 P(x)/Q(x) 1/sqrt(2) <= x <
   * sqrt(2)
   */
  static double   P[] = {
    1.01875663804580931796e-4,
    4.97494994976747001425e-1,
    4.70579119878881725854e0,
    1.44989225341610930846e1,
    1.79368678507819816313e1,
    7.70838733755885391666e0,
  };
  static double   Q[] = {
    /* 1.00000000000000000000e0, */
    1.12873587189167450590e1,
    4.52279145837532221105e1,
    8.29875266912776603211e1,
    7.11544750618563894466e1,
    2.31251620126765340583e1,
  };
  
  /* Coefficients for log(x) = z + z**3 P(z)/Q(z), where z = 2(x-1)/(x+1)
   * 1/sqrt(2) <= x < sqrt(2)
   */
  static double   R[3] = {
    -7.89580278884799154124e-1,
    1.63866645699558079767e1,
    -6.41409952958715622951e1,
  };
  static double   S[3] = {
    /* 1.00000000000000000000e0, */
    -3.56722798256324312549e1,
    3.12093766372244180303e2,
    -7.69691943550460008604e2,
  };
  
  int e;
  double y;
  volatile double z;

  if (x != x)
    return x;
  if (x == INFINITY)
    return x;

  /* Test for domain */
  if (x <= 0.0) {
    if (x == 0.0) {
      return (-INFINITY);
    } else {
      return ((x - x) / (x - x)); /* NaN */
    }
  }
  /* Separate mantissa from exponent
   * Note, frexp is used so that denormal numbers will be handled properly.
   */
  x = frexp(x, &e);

  if ((e > 2) || (e < -2)) {
    /*
     * logarithm using log(x) = z + z**3 P(z)/Q(z), where z = 2(x-1)/x+1)
     */
    if (x < SQRTH) {
      /* 2( 2x-1 )/( 2x+1 ) */
      e -= 1;
      z = x - 0.5;
      y = 0.5 * z + 0.5;
    } else {
      /* 2 (x-1)/(x+1)   */
      z = x - 0.5;
      z -= 0.5;
      y = 0.5 * x + 0.5;
    }

    x = z / y;
    z = x * x;
    y = x * (z * polevl(z, R, 2) / p1evl(z, S, 3));
  } else {
    /* logarithm using log(1+x) = x - .5x**2 + x**3 P(x)/Q(x) */
    if (x < SQRTH) {
      e -= 1;
      x = ldexp(x, 1) - 1.0;	/* 2x - 1  */
    } else {
      x = x - 1.0;
    }

    z = x * x;
    y = x * (z * polevl(x, P, 5) / p1evl(x, Q, 5)) - ldexp(z, -1);
  }

  /*
   * Multiply log of fraction by log2(e) and base 2 exponent by 1
   * 
   * **CAUTION***
   * 
   * This sequence of operations is critical and it may be horribly defeated by
   * some compiler optimizers.
   */
  z = y * LOG2EA;
  z += x * LOG2EA;
  z += y;
  z += x;
  z += e;
  return (z);
}
#endif

#ifndef HAVE_FABSF
float (fabsf)(float x)
{
    return (float) fabs(x);
}
#endif

#ifndef HAVE_SINF
float (sinf)(float x)
{
  return (float) sin(x);
}
#endif

#ifndef HAVE_COSF
float (cosf)(float x)
{
  return (float) cos(x);
}
#endif

#ifndef HAVE_TANF
float (tanf)(float x)
{
  return (float) tan(x);
}
#endif

#ifndef HAVE_ASINF
float (asinf)(float x)
{
  return (float) asin(x);
}
#endif

#ifndef HAVE_ACOSF
float (acosf)(float x)
{
  return (float) acos(x);
}
#endif

#ifndef HAVE_ATANF
float (atanf)(float x)
{
  return (float) atan(x);
}
#endif

#ifndef HAVE_ATAN2F
float (atan2f)(float y, float x)
{
  return (float) atan2(y, x);
}
#endif

#ifndef HAVE_EXPF
float (expf)(float x)
{
  return (float) exp(x);
}
#endif

#ifndef HAVE_SQRTF
float (sqrtf)(float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef HAVE_LOGF
float (logf)(float x)
{
  return (float) log(x);
}
#endif

#ifndef HAVE_LOG2F
float (log2f)(float x)
{
  return (float) log2(x);
}
#endif

#ifndef HAVE_LOG10F
float (log10f)(float x)
{
  return (float) log10(x);
}
#endif

#ifndef HAVE_POWF
float (powf)(float b, float x)
{
  return (float) pow(b, x);
}
#endif

#ifndef HAVE_SINHF
float (sinhf)(float x)
{
  return (float) sinh(x);
}
#endif

#ifndef HAVE_COSHF
float (coshf)(float x)
{
  return (float) cosh(x);
}
#endif

#ifndef HAVE_TANHF
float (tanhf)(float x)
{
  return (float) tanh(x);
}
#endif

#ifdef CEPHESL
/*
 * Cephes Math Library Release 2.2:  July, 1992
 * Copyright 1984, 1987, 1988, 1992 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */

/* Polynomial evaluator:
 *  P[0] x^n  +  P[1] x^(n-1)  +  ...  +  P[n]
 */
static long double polevll(long double x, long double *p, size_t n)
{
  long double y = *p++;

  do {
    y = y * x + *p++;
  } while(--n);

  return(y);
}


/* Polynomial evaluator:
 *  x^n  +  P[0] x^(n-1)  +  P[1] x^(n-2)  +  ...  +  P[n]
 */
static long double p1evll(long double x, long double *p, size_t n)
{
  long double y = x + *p++;;

  n -= 1;

  do {
    y = y * x + *p++;
  } while( --n );

  return(y);
}
#endif

#ifdef CEPHES
/*
 * Cephes Math Library Release 2.1:  December, 1988
 * Copyright 1984, 1987, 1988 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */

static double polevl(double x, double coef[], size_t N)
{
  double *p = coef;
  double ans = *p++;
  size_t i = N;

  do
    ans = ans * x + *p++;
  while (--i);

  return (ans);
}

/* p1evl()
 * Evaluate polynomial when coefficient of x is 1.0. Otherwise same as
 * polevl.
 */

static double p1evl(double x, double coef[], size_t N)
{
  double *p = coef;
  double ans = x + *p++;
  int i = N - 1;;

  do
    ans = ans * x + *p++;
  while (--i);

  return (ans);
}
#endif
