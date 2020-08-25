all:
	clang -c -Wall --target=wasm32 -O0 -flto -fno-inline-functions -nostdlib -fvisibility=hidden -ffunction-sections -fdata-sections -o console.o console.c
	wasm-ld -o console.wasm --no-entry --strip-all --export-dynamic --import-memory--initial-memory=131072 --error-limit=0 -O0 --gc-sections -allow-undefined-file wasm.syms console.o
	wasm2wat console.wasm -o console.wat
