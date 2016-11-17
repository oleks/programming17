.PHONY: all clean

SRC=$(wildcard *.c)
BIN=$(patsubst %.c,%.bin,$(SRC))

CC=gcc
CFLAGS=-Wall -Werror -Wextra -pedantic -std=c11 -g

all: $(BIN)

%.bin: %.c Makefile
	$(CC) $(CFLAGS) $< -o $@

%.trace: %.bin Makefile
	strace -f -o $@ ./$<

gdb-%.bin: %.bin Makefile
	gdb -q -x run.gdb $<

dump-%.bin: %.bin Makefile
	objdump --source -M no-aliases $<

clean:
	rm -f *.bin
	rm -f *.trace
