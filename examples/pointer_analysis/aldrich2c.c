#include <stdio.h>

void foo (int n) {
  int y = 0;
  int z = 1;
  int* p;
  if (n > 10) {
    p = &y;
  } else {
    p = &z;
  }
  *p = 14;

  printf("z=%d, y=%d, p=%p\n", z, y, (void*)p);
}