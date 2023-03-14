#ifndef _GC_H
#define _GC_H

enum HeapType {
  Array,
  Pair
};

void *heap_create(int size, enum HeapType type);
void heap_destroy(void *address);
struct HeapNode *heap_lookup(void *address);
void heap_add_child(void *parent_address, void *child_address);
void heap_mark(void *address);

#endif

