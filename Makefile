.PHONY: build test install

build:
	dune build @install --profile release

format:
	dune build @fmt --auto-promote

test: build
	dune runtest -f --profile release

clean:
	dune clean

utop:
	dune utop --profile release

install: build
	dune install