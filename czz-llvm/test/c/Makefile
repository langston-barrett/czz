CLANG := clang
CLANG_FORMAT := clang-format
PROGS := $(wildcard *.c)

# These are intentionally raised in some programs
NO_WARN := -Wno-nonnull

.PHONY: all
all: fmt $(PROGS:.c=.out) $(PROGS:.c=.bc)

.PHONY: fmt
fmt:
	$(CLANG_FORMAT) -i *.c

%.bc: %.c
	$(CLANG) $(NO_WARN) -fno-discard-value-names -emit-llvm -g -c $< -o $@

%.elf: %.c
	$(CLANG) $(NO_WARN) -g $< -o $@

%.out: %.elf
	./$< > $@ || true

.PHONY: clean
clean:
	rm -f *.elf *.ll *.bc *.out
