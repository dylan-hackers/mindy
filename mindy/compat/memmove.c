#include "std-c.h"

VOID *memmove(t, f, n) VOID *t; CONST VOID *f; size_t n;
{
  if (t < f) {
    char *pt = t;
    CONST char *pf = f;
    for (; n>0; n -= 1)
      *pt++ = *pf++;
  } else if (t > f) {
    char *pt = t+n;
    CONST char *pf = f+n;
    for (; n>0; n -= 1)
      *--pt = *--pf;
  }
  return t;
}

      
