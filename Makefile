.PHONY: all format build test lint run clean watch docker

all: format build test

format:
	@fourmolu -q -i src/**/*.hs app/*.hs test/*.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/**/*.hs app/*.hs test/*.hs

run:
	@stack run

clean:
	@stack purge

watch:
	ghcid -T :main

docker:
	@docker build -t carp-sushi/user-service .
