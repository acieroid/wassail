all:
	clang -c -Wall --target=wasm32 -O0 -flto -nostdlib -fvisibility=hidden -ffunction-sections -fdata-sections -o simple-taint.o simple-taint.c
	wasm-ld -o simple-taint.wasm --no-entry --strip-all --export-dynamic --initial-memory=131072 --error-limit=0 -O0 --gc-sections simple-taint.o
	wasm2wat simple-taint.wasm -o simple-taint.wat
