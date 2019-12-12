all:
	clang -c -Wall --target=wasm32 -O0 -flto -nostdlib -fvisibility=hidden -ffunction-sections -fdata-sections -o overflow.o overflow.c
	wasm-ld -o overflow.wasm --no-entry --strip-all --export-dynamic --initial-memory=131072 --error-limit=0 -O0 --gc-sections overflow.o
	wasm2wat overflow.wasm -o overflow.wat
