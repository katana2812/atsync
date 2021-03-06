NEW_ERL_LIBS=$(shell pwd)/..

compile:
	ERL_LIBS=$(NEW_ERL_LIBS) erl -make

all: src test

.PHONY: src
src:
	@$(MAKE) --directory=src

.PHONY: test
test:
	@$(MAKE) --directory=test

test-verbose:
	@$(MAKE) --directory=test test-verbose

clean:
	rm -f ebin/*.beam
