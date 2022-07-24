.PHONY: all build run
all:
	build run

.PHONY: build
build:
	@dune build @check

.PHONY: run
run:
	@dune exec ./main.exe
