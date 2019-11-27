#define WASM_EXPORT __attribute__((visibility("default")))

// Annotated as sanitizer
int hash(int password) {
  return 1;
}

// Annotated as sink
void call_db(int password) {
  return;
}

WASM_EXPORT
int safe(int password) {
  return hash(password);
}

WASM_EXPORT
int unsafe1(int password) {
  return password;
}

WASM_EXPORT
void unsafe2(int password) {
  return call_db(password);
}

WASM_EXPORT
int unsafe3(int password) {
  if (password % 2 == 0) {
    return 1;
  } else {
    return 0;
  }
}
