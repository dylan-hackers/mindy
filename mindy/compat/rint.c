#include <math.h>

double rint(x) double x;
{
  /* uh, this is wrong unless rounding to -infinity */
  return floor(x+0.5);				
}
