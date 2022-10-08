.PHONY: all format build test lint run clean build-docker run-docker

all: format build lint

format:
	fourmolu -q -i src/**/*.hs app/*.hs test/*.hs

build:
	nix-build

lint:
	hlint src/**/*.hs app/*.hs test/*.hs

clean:
	rm -rf result image

build-docker:
	nix-build --attr docker-image release.nix -o image
	docker load -i ./image

run-docker:
	docker run -it --rm -v $(CURDIR)/data:/data -v $(CURDIR)/user-service.cfg:/user-service.cfg -p 8080:8080 carp-sushi/user-service
