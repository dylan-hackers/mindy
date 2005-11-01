#include <stdio.h>
#include <stddef.h>
#include <limits.h>
#include <float.h>
#include <math.h>

#define TYPE(name, type) \
	{ \
	  struct z { char c; type x; } z; \
	  printf(name "-size: %u\n", (unsigned) sizeof(type)); \
	  printf(name "-alignment: %u\n", (unsigned) offsetof(struct z, x)); \
	}

int main(int argc, char *argv[]) {
  double d;
  
  /* Attention: what d2c calls an "integer-length" is really the word
   * size, represented as a "long" */
  printf("integer-length: %u\n", (unsigned) (CHAR_BIT * sizeof(long)));
  TYPE("pointer", void *);
  TYPE("integer", int);
  TYPE("short", short);
  TYPE("long", long);
#ifndef WITHOUT_LONG_LONG
  TYPE("long-long", long long);
#endif
  TYPE("single", float);
  TYPE("double", double);
  TYPE("long-double", long double);

  printf("single-mantissa-digits: %d\n", FLT_MANT_DIG);
  printf("double-mantissa-digits: %d\n", DBL_MANT_DIG);
  printf("long-double-mantissa-digits: %d\n", LDBL_MANT_DIG);

  printf("minimum-single-float-exponent: %d\n", FLT_MIN_EXP);
  printf("maximum-single-float-exponent: %d\n", FLT_MAX_EXP);
  printf("minimum-double-float-exponent: %d\n", DBL_MIN_EXP);
  printf("maximum-double-float-exponent: %d\n", DBL_MAX_EXP);
  printf("minimum-long-double-float-exponent: %d\n", LDBL_MIN_EXP);
  printf("maximum-long-double-float-exponent: %d\n", LDBL_MAX_EXP);

  if((d = ldexp(1.0, -FLT_MANT_DIG + 1)) != FLT_EPSILON)
    fprintf(stderr, "platformer: FLT_EPSILON mismatch (%g != %g)\n",
	    d, FLT_EPSILON);
  if((d = ldexp(1.0, -DBL_MANT_DIG + 1)) != DBL_EPSILON)
    fprintf(stderr, "platformer: DBL_EPSILON mismatch (%g != %g)\n",
	    d, DBL_EPSILON);

  if(FLT_RADIX != 2)
    fprintf(stderr, "platformer: FLT_RADIX is not 2\n");

  return 0;
}
