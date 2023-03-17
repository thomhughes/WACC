#include "gc.h"
#include "runtime.h"
#include "linkedlist.h"
#include "hashmap.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define GC_ENABLED true

struct linked_list __thread call_stack;
struct hashmap __thread *heap;

bool __thread mark = true;

struct call_info {
  unsigned scope;
  struct hashmap *roots;
};

struct heapnode {
  void *address;
  enum heap_type type;
  bool marked;
};

int heap_compare(const void *a_, const void *b_, void *udata) {
  const struct heapnode *a = a_;
  const struct heapnode *b = b_;
  return a->address - b->address;
}

uint64_t heap_hash(const void *item, uint64_t seed0, uint64_t seed1) {
  const struct heapnode *heapnode = item;
  return hashmap_sip(&heapnode->address, sizeof(heapnode->address), seed0, seed1);
}

struct rootnode {
  unsigned scope;
  char *name;
  void *reference_address;
};

struct call_info *callinfo_get() {
  assert(list_peek(&call_stack));
  return list_peek(&call_stack)->data;
}

int root_compare(const void *a_, const void *b_, void *udata) {
  const struct rootnode *a = a_;
  const struct rootnode *b = b_;
  int cmp = a->scope - b->scope;
  return !cmp ? strcmp(a->name, b->name) : cmp;
}

uint64_t root_hash(const void *item, uint64_t seed0, uint64_t seed1)
{
  const struct rootnode *root_node = item;
  return hashmap_sip(&root_node->scope, sizeof(root_node->scope), seed0,
                     seed1) ^
         hashmap_sip(root_node->name, strlen(root_node->name), seed0, seed1);
}

void *heap_create(int size, enum heap_type type) {
  void *address = calloc(size, 1);
  hashmap_set(heap, &(struct heapnode) { .address = address, .marked = !mark, .type = type });
  return address;
}

void heap_destroy(void *address) {
  struct heapnode *destroy_node = (struct heapnode *)hashmap_get(heap, &(struct heapnode){ .address=address });
  assert(destroy_node);
  if (GC_ENABLED)
    free(destroy_node->address);
  hashmap_delete(heap, destroy_node);
}

void heap_mark(void *address) {
  struct heapnode *node = (struct heapnode *)hashmap_get(heap, &(struct heapnode){ .address=address });
  if (!node || node->marked == mark) return;
  node->marked = mark;
  if (node->type == Array) {
    array_mark(node->address);
  } else if (node->type == Pair) {
    pair_mark(node->address);
  }
}

struct heapnode *heap_lookup(void *address) {
  return hashmap_get(heap, &(struct heapnode){ .address = address });
}

extern void root_assignment(int scope, char *name, void *address) {
  struct rootnode *root = (struct rootnode *)hashmap_get(callinfo_get()->roots, &(struct rootnode){ .scope = scope, .name = name });
  if (root) {
    root->reference_address = address;
  } else {
    hashmap_set(callinfo_get()->roots, &(struct rootnode){ .scope = scope, .name = name, .reference_address = address });
  }
}

unsigned *get_scope() {
  struct call_info *call_info = list_peek(&call_stack)->data;
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
  struct rootnode *node = NULL;
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

  struct call_info *new_frame = (struct call_info*)calloc(1, sizeof(struct call_info));
  new_frame->scope = 0;
  new_frame->roots = hashmap_new(sizeof(struct rootnode), 0, 0, 0, root_hash, root_compare, NULL, NULL);
  list_push(&call_stack, new_frame);
}

void gc_mark();
void gc_sweep();

extern void func_return(void *returnvalue) {
  assert(callinfo_get());
  hashmap_free(callinfo_get()->roots);
  if (GC_ENABLED)
    free(list_peek(&call_stack)->data);
  list_pop(&call_stack);

  if (returnvalue) {
    heap_mark(returnvalue);
  }
  gc_mark();
  gc_sweep();
}

bool root_mark(const void *root_, void *udata) {
  const struct rootnode *root = root_;
  heap_mark(root->reference_address);
  return true;
}

void gc_mark() {
  struct linked_list *curr = list_peek(&call_stack);
  while (curr != NULL) {
    struct call_info *current_frame = current_frame = curr->data;
    hashmap_scan(current_frame->roots, root_mark, NULL);
    curr = curr->next;
  }
}

void gc_sweep() {
  size_t i = 0;
  struct heapnode *node = NULL;
  while (hashmap_iter(heap, &i, (void **)&node)) {
    if (node->marked != mark) {
      if (GC_ENABLED)
        free(node->address);
      hashmap_delete(heap, node);
      i = 0;
    }
  }
  mark = !mark;
}

void heapnode_free(void *node_) {
  struct heapnode *node = (struct heapnode *)node_;
  fprintf(stderr, "[!] Late freeing: %p\n", node->address);
  if (GC_ENABLED)
    free(node->address);
}

extern void gc_init() {
  heap = hashmap_new(sizeof(struct heapnode), 0, 0, 0, heap_hash, heap_compare, heapnode_free, NULL);
  assert(heap);
}

extern void gc_free() {
  hashmap_free(heap);
}
