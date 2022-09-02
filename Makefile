.PHONY: all format build test lint run clean

all: format build test

format:
	@stylish-haskell -i src/**/*.hs
	@stylish-haskell -i app/*.hs
	@stylish-haskell -i test/*.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/**/*.hs app/*.hs

run:
	@stack run

clean:
	@stack purge

watch:
	ghcid -T :main

docker:
	@docker build -t carp-sushi/user-service .
