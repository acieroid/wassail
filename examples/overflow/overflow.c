// Compile this program into a .wasm/.wat with make (see Makefile)


#define WASM_EXPORT __attribute__((visibility("default")))

typedef struct Foo {
  char msg[16];
  int len;
} Foo;

int strlen(const char *data) {
  int i, n;
  n = 0;
  for (i = 0; data[i] != '\0'; i++) {
    n++;
  }
  return n;
}

char *memcpy(char *dst, const char *src, int len) {
  int i;
  for (i = 0; i < len; i++) {
    dst[i] = src[i];
  }
  return dst;
}

WASM_EXPORT
int foo(const char *data) {
  Foo f;
  f.len = strlen(data);
  memcpy(f.msg, data, f.len);
  return f.len;
}

WASM_EXPORT
int main() {
  /* The output is supposed to be the argument's length, but due to overflow it is 2 */
  return foo("0123456789ABCDEF\x02\x00\x00\x00");
}
