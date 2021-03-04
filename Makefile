TEST_EXECUTABLE=./_build/default/lib/.wassail.inline-tests/inline_test_runner_wassail.exe

all: bin

bin:
	opam exec -- dune build

test: bin
	opam exec -- dune runtest

clean:
	opam exec -- dune clean

install:
	opam exec -- dune build @install
	opam exec -- dune install

.PHONY: bin js test clean install all
