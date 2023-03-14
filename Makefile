CC = arm-linux-gnueabi-gcc
CFLAGS = -mcpu=arm1176jzf-s -mtune=arm1176jzf-s

RUNTIME_SOURCES = waccyruntime/array.c waccyruntime/gc.o waccyruntime/hashmap.o waccyruntime/linkedlist.c waccyruntime/runtime.c
RUNTIME_OBJECTS = $(RUNTIME_SOURCES:%.c=%.o)

LIBRARY_SOURCES = libraries/string.c libraries/math.c libraries/array.c
LIBRARY_OBJECTS = $(LIBRARY_SOURCES:%.c=%.o)

runtime: $(RUNTIME_OBJECTS)

library: runtime $(LIBRARY_OBJECTS)

all:
	sbt compile assembly

ARMASSEMBLY = $(shell find . -type f -name \*.s)
clean:
	-rm -f $(RUNTIME_OBJECTS) $(LIBRARY_OBJECTS) $(ARMASSEMBLY) $(ARMASSEMBLY:%.s=%)
	sbt clean && rm -rf wacc-42-compiler.jar

.PHONY: all clean runtime library
