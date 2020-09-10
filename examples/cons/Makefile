WASM2WAT=wasm2wat

all:
	clang -emit-llvm --target=wasm32-unknown-wasi --sysroot /home/quentin/p/wasi -Oz main.c -c -o main.o
	wasm-ld --no-entry --strip-all --export-dynamic --initial-memory=131072 -error-limit=0 --gc-sections main.o -o main.wasm
	${WASM2WAT} main.wasm -o main.wat
