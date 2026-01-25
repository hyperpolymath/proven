# SPDX-License-Identifier: PMPL-1.0
# Makefile for Ephapax ↔ Proven FFI bindings
#
# Pure Zig implementation - no C files needed

ZIG = zig
ZIGFLAGS = -O ReleaseFast

# Targets
LIB = libephapax_proven.so
TEST = test_ffi

all: $(LIB)

# Build shared library from pure Zig
$(LIB): src/*.zig
	$(ZIG) build-lib src/ephapax_proven.zig \
		-dynamic \
		-fallow-shlib-undefined \
		$(ZIGFLAGS) \
		-femit-bin=$(LIB)

# Simple C test program
test: $(LIB) test_simple.c
	gcc -std=c11 -Wall -I. -L. -o $(TEST) test_simple.c -lephapax_proven
	LD_LIBRARY_PATH=. ./$(TEST)

# Clean
clean:
	rm -f $(LIB) $(TEST) *.o

.PHONY: all test clean
