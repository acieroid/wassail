#define WASM_EXPORT __attribute__((visibility("default")))

WASM_EXPORT
int strlen(const int *data) {
  int i;
  for (i = 0; data[i] != 0; i++) { }
  return i;
}

WASM_EXPORT
int use_strlen(const int *data) {
  if (strlen(data) == 5) {
    return 1;
  } else {
    return 0;
  }
}

WASM_EXPORT
void memcpy(int *dst, const int *src, int len) {
  int i;
  for (i = 0; i < len; i++) {
    dst[i] = src[i];
  }
}

WASM_EXPORT
char* unsafe(int *dst, const int *data) {
  memcpy(dst, data, 3);
  return dst;
}

WASM_EXPORT
int unsafe_int(int n) {
  return n;
}

WASM_EXPORT
char* safe(int *dst, const int *data) {
  memcpy(dst, "foo", 3);
  return dst;
}
