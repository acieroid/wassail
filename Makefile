TEST_EXECUTABLE=./_build/default/lib/.wassail.inline-tests/inline_test_runner_wassail.exe

all: bin

bin:
	opam exec -- dune build main.exe

test: bin
	opam exec -- dune runtest

clean:
	opam exec -- dune clean

install:
	opam exec -- dune build @install
	opam exec -- dune install

coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	echo 'Coverage produced in _coverage/index.html'

js:
	opam exec -- dune build jsbridge.bc.js
	cp _build/default/jsbridge.bc.js ./js

.PHONY: bin js test clean install all
