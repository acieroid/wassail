TEST_EXECUTABLE=./_build/default/lib/.wassail.inline-tests/inline_test_runner_wassail.exe

all: bin test

bin:
	dune build ./bin/main.exe
	ln -sf _build/default/bin/main.exe main.exe

js:
	dune build ./js/jsbridge.bc.js
	cp -f _build/default/js/jsbridge.bc.js interface/jsbridge.bc.js

test:
	dune runtest

clean:
	dune clean


.PHONY: bin js test clean all
