CLANG := clang
CLANG_FORMAT := clang-format
PROGS := $(wildcard *.c)

.PHONY: all
all: fmt $(PROGS:.c=.out) $(PROGS:.c=.bc)

.PHONY: fmt
fmt:
	$(CLANG_FORMAT) -i *.c

%.bc: %.c
	$(CLANG) -fno-discard-value-names -emit-llvm -g -c $< -o $@

%.elf: %.c
	$(CLANG) -g $< -o $@

%.out: %.elf
	./$< > $@ || true

.PHONY: clean
clean:
	rm -f *.elf *.ll *.bc *.out
