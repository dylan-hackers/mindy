#include <stdio.h>

int main() {
  printf("pointer-size: %d\n", sizeof(void *));
  printf("integer-size: %d\n", sizeof(int));
  printf("short-size: %d\n", sizeof(short));
  printf("long-size: %d\n", sizeof(long));
  printf("single-size: %d\n", sizeof(float));
  printf("double-size: %d\n", sizeof(double));
  printf("long-double-size: %d\n", sizeof(long double));

  return 0;
}
