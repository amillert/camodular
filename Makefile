.PHONY: utop
utop:
	@dune utop src

.PHONY: dev
dev:
	@dune build @check -w

.PHONY: build
build:
	@dune build @check
