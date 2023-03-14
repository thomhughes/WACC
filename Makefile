CC = arm-linux-gnueabi-gcc
CFLAGS = -mcpu=arm1176jzf-s -mtune=arm1176jzf-s

SOURCES = waccyruntime/array.c waccyruntime/gc.o waccyruntime/hashmap.o waccyruntime/linkedlist.c waccyruntime/runtime.c waccyruntime/string.c waccyruntime/math.c waccyruntime/array.c
OBJECTS = $(SOURCES:%.c=%.o)

runtime: $(OBJECTS)

all:
	sbt compile assembly

clean:
	rm $(runtime_objects)
	sbt clean && rm -rf wacc-42-compiler.jar

.PHONY: all clean runtime
