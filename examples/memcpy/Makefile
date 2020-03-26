all:
	clang -c -Wall --target=wasm32 -O0 -flto -fno-inline-functions -nostdlib -fvisibility=hidden -ffunction-sections -fdata-sections -o memcpy.o memcpy.c
	wasm-ld -o memcpy.wasm --no-entry --strip-all --export-dynamic --initial-memory=131072 --error-limit=0 -O0 --gc-sections memcpy.o
	wasm2wat memcpy.wasm -o memcpy.wat
