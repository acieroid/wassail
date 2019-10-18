all:
	clang -c -Wall --target=wasm32 -Os -flto -nostdlib -fvisibility=hidden -ffunction-sections -fdata-sections -o overflow.o overflow.c
	wasm-ld -o overflow.wasm --no-entry --strip-all --export-dynamic --initial-memory=131072 --error-limit=0 --lto-O3 -O3 --gc-sections overflow.o
	wasm2wat overflow.wasm -o overflow.wat
