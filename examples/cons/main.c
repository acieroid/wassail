#define WASM_EXPORT __attribute__((visibility("default")))

#include <stdio.h>
#include <stdlib.h>

struct Cons {
  int car;
  struct Cons *cdr;
};
typedef struct Cons Cons;

Cons mklist_simple() {
  Cons l1, l2;
  l1.car = 1;
  l1.cdr = &l2;
  l2.car = 2;
  l2.cdr = NULL;

  return l1;
}

WASM_EXPORT
Cons mklist(int init, int size) {
  Cons l;
  Cons l2;
  l.car = init;
  if (size == 1) {
    l2 = mklist(init, size-1);
    l.cdr = &l2;
  } else {
    l.cdr = 0;
  }
  return l;
}

int length(Cons *l) {
  if (l == NULL) {
    return 0;
  } else {
    return 1 + length(l->cdr);
  }
}

