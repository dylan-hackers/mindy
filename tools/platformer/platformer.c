#include <stdio.h>
#include <stddef.h>
#include <limits.h>

#define TYPE(name, type) \
	{ \
	  struct z { char c; type x; } z; \
	  printf(name "-size: %u\n", (unsigned) sizeof(type)); \
	  printf(name "-alignment: %u\n", (unsigned) offsetof(struct z, x)); \
	}

int main() {
  printf("integer-length: %u\n", (unsigned) (CHAR_BIT * sizeof(int)));
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

  return 0;
}
