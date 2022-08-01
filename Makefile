.PHONY: utop
utop:
	@dune utop src

.PHONY: dev
dev:
	@dune build @check -w

.PHONY: all
all:
	@dune clean; dune build @check

.PHONY: build
build:
	@dune build @check
