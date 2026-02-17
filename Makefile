build:
	gerbil build

test:
	gerbil test ./...

clean:
	gerbil clean

install:
	gerbil pkg install

.PHONY: build test clean install
