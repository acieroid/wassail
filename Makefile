TEST_EXECUTABLE=./_build/default/lib/.wassail.inline-tests/inline_test_runner_wassail.exe

all: bin

bin:
	opam exec -- dune build ./bin/main.exe

js:
	opam exec -- dune build ./js/jsbridge.bc.js
	cp -f _build/default/js/jsbridge.bc.js interface/jsbridge.bc.js

test:
	opam exec -- dune runtest

clean:
	opam exec -- dune clean

install:
	opam exec -- dune build @install
	opam exec -- dune install

.PHONY: bin js test clean install all
