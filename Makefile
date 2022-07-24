.PHONY: all build run
all:
	build run

.PHONY: dev
dev:
	@dune build @check -w

.PHONY: build
build:
	@dune build @check

.PHONY: run
run:
	@dune exec ./main.exe
