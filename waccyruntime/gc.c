#include "gc.h"
#include "runtime.h"
#include "linkedlist.h"
#include "hashmap.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct LinkedList __thread call_stack;
struct hashmap __thread *heap;

bool __thread mark;

struct CallInfo {
  unsigned scope;
  struct hashmap *roots;
};

struct HeapNode {
  void *address;
  enum HeapType type;
  bool marked;
};

int heap_compare(const void *a_, const void *b_, void *udata) {
  const struct HeapNode *a = a_;
  const struct HeapNode *b = b_;
  return a->address - b->address;
}

uint64_t heap_hash(const void *item, uint64_t seed0, uint64_t seed1) {
  const struct HeapNode *heap_node = item;
  return hashmap_sip(&heap_node->address, sizeof(heap_node->address), seed0, seed1);
}

struct RootNode {
  unsigned scope;
  char *name;
  void *reference_address;
};

struct CallInfo *callinfo_get() {
  assert(list_peek(&call_stack));
  return list_peek(&call_stack)->data;
}

int root_compare(const void *a_, const void *b_, void *udata) {
  const struct RootNode *a = a_;
  const struct RootNode *b = b_;
  int cmp = a->scope - b->scope;
  return !cmp ? strcmp(a->name, b->name) : cmp;
}

uint64_t root_hash(const void *item, uint64_t seed0, uint64_t seed1)
{
  const struct RootNode *root_node = item;
  return hashmap_sip(&root_node->scope, sizeof(root_node->scope), seed0,
                     seed1) ^
         hashmap_sip(root_node->name, strlen(root_node->name), seed0, seed1);
}

void *heap_create(int size, enum HeapType type) {
  // TODO: create when sweeping?
  void *address = calloc(size, 1);
  hashmap_set(heap, &(struct HeapNode) { .address = address, .marked = !mark, .type = type });
  return address;
}

void heap_destroy(void *address) {
  struct HeapNode *destroy_node = (struct HeapNode *)hashmap_get(heap, &(struct HeapNode){ .address=address });
  assert(destroy_node);
  free(destroy_node->address);
  hashmap_delete(heap, destroy_node);
}

void heap_mark(void *address) {
  struct HeapNode *node = (struct HeapNode *)hashmap_get(heap, &(struct HeapNode){ .address=address });
  if (!node || node->marked == mark) return;
  node->marked = mark;
  if (node->type == Array) {
    array_mark(node->address);
  } else if (node->type == Pair) {
    pair_mark(node->address);
  }
}

struct HeapNode *heap_lookup(void *address) {
  return hashmap_get(heap, &(struct HeapNode){ .address = address });
}

extern void root_assignment(int scope, char *name, void *address) {
  struct RootNode *root = (struct RootNode *)hashmap_get(callinfo_get()->roots, &(struct RootNode){ .scope = scope, .name = name });
  if (root) {
    root->reference_address = address;
  } else {
    hashmap_set(callinfo_get()->roots, &(struct RootNode){ .scope = scope, .name = name, .reference_address = address });
  }
}

unsigned *get_scope() {
  struct CallInfo *call_info = list_peek(&call_stack)->data;
  return &call_info->scope;
}

extern void func_call();
extern void gc_init();

void enter_scope(unsigned new) {
  *get_scope() = new;
}

extern void exit_scope(unsigned new) {
  unsigned *current_scope = get_scope();

  // Currently we iterate through whole dictionary to do scoping cleanup 
  // We could sacrifice some space to store a list for each scope
  size_t i = 0;
  struct RootNode *node = NULL;
  while (hashmap_iter(callinfo_get()->roots, &i, (void**)&node)) {
    if (node->scope == *current_scope) {
      hashmap_delete(callinfo_get()->roots, node);
      i = 0;
    }
  }

  *current_scope = new;
}

extern void func_call() {
  if (list_peek(&call_stack) == NULL) {
    gc_init();
  }

  struct CallInfo *new_frame = (struct CallInfo*)calloc(1, sizeof(struct CallInfo));
  new_frame->scope = 0;
  new_frame->roots = hashmap_new(sizeof(struct RootNode), 0, 0, 0, root_hash, root_compare, NULL, NULL);
  list_push(&call_stack, new_frame);
}


void gc_mark();
void gc_sweep();

extern void func_return(void *returnvalue) {
  assert(callinfo_get());
  hashmap_free(callinfo_get()->roots);
  list_pop(&call_stack);

  // // TODO: Maybe move elsewhere :3
  if (returnvalue) {
    heap_mark(returnvalue);
  }
  gc_mark();
  gc_sweep();
}

bool root_mark(const void *root_, void *udata) {
  const struct RootNode *root = root_;
  heap_mark(root->reference_address);
  return true;
}

void gc_mark() {
  struct LinkedList *curr = list_peek(&call_stack);
  while (curr != NULL) {
    struct CallInfo *current_frame = current_frame = curr->data;
    hashmap_scan(current_frame->roots, root_mark, NULL);
    curr = curr->next;
  }
}

void gc_sweep() {
  size_t i = 0;
  struct HeapNode *node = NULL;
  while (hashmap_iter(heap, &i, (void **)&node)) {
    if (node->marked != mark) {
      free(node->address);
      hashmap_delete(heap, node);
      i = 0;
    }
  }
  mark = !mark;
}

void heapnode_free(void *node_) {
  struct HeapNode *node = (struct HeapNode *)node_;
  fprintf(stderr, "[!] Late freeing: %p\n", node->address);
  free(node->address);
}

extern void gc_init() {
  heap = hashmap_new(sizeof(struct HeapNode), 0, 0, 0, heap_hash, heap_compare, heapnode_free, NULL);
  assert(heap);
}

extern void gc_free() {
  hashmap_free(heap);
}
