#ifndef _GC_H
#define _GC_H

enum heap_type {
  Array,
  Pair
};

void *heap_create(int size, enum heap_type type);
void heap_destroy(void *address);
struct heapnode *heap_lookup(void *address);
void heap_add_child(void *parent_address, void *child_address);
void heap_mark(void *address);

#endif

