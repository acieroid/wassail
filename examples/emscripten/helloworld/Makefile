EMCC=/usr/lib/emscripten/emcc
EMCCFLAGS= -g2 -Oz # full size optimizations, keep debug information

WASM2WAT=wasm2wat

all:
	${EMCC} ${EMCCFLAGS} main.c -o main.wasm
	${WASM2WAT} main.wasm -o main.wat
