CABAL := cabal
HLINT := hlint

all: \
  lint \
  build \
  test

.PHONY: lint
lint:
	$(HLINT) czz/src czz-jvm/{exe,src,test} czz-llvm/{exe,src,test} czz-llvm-tui/{exe,src}

.PHONY: entr-lint
entr-lint:
	find . -name "*.hs" -or -name "Makefile" -or -name "*.cabal" | entr -c -s "$(MAKE) lint"

.PHONY: build
build:
	$(CABAL) build all

.PHONY: test
test:
	$(CABAL) test all
