CABAL := cabal
HLINT := hlint
PROF_TARGET := exe:czz-llvm
PROF_ARGS := 
PROF_FLAGS := \
  --builddir=dist-prof \
  --enable-library-profiling \
  --enable-executable-profiling

all: \
  doc \
  lint \
  build \
  test

.PHONY: doc
doc:
	$(MAKE) -C doc html

.PHONY: lint
lint:
	$(HLINT) \
	  czz/src \
	  czz-jvm/{exe,src,test} \
	  czz-llvm/{exe,src,test} \
	  czz-llvm-tui/{exe,src} \
	  husk/

.PHONY: entr-lint
entr-lint:
	find . -name "*.hs" -or -name "Makefile" -or -name "*.cabal" | entr -c -s "$(MAKE) lint"

.PHONY: build
build:
	$(CABAL) build all

.PHONY: prof
prof:
	$(CABAL) run $(PROF_TARGET) $(PROF_FLAGS) -- $(PROF_ARGS)

.PHONY: test
test:
	$(CABAL) test all
