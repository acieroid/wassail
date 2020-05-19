EMCC=/usr/lib/emscripten/emcc
EMCCFLAGS= -Oz # full size optimizations

WASM2WAT=wasm2wat

all:
	${EMCC} ${EMCCFLAGS} main.c -o main.wasm
	${WASM2WAT} main.wasm -o main.wat
