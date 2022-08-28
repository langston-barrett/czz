CLANG := clang
PROGS := $(wildcard *.c)

.PHONY: all
all: $(PROGS:.c=.out) $(PROGS:.c=.bc)

%.bc: %.c
	$(CLANG) -fno-discard-value-names -emit-llvm -g -c $< -o $@

%.elf: %.c
	$(CLANG) -g $< -o $@

%.out: %.elf
	./$< > $@ || true

.PHONY: clean
clean:
	rm -f *.elf *.ll *.bc *.out
