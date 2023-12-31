#include <stdlib.h>

#include "linkedlist.h"

void list_push(struct linked_list *list_top, void *data) {
  struct linked_list *new_node = calloc(1, sizeof(*list_top));
  new_node->data = data;
  if (list_top->next != NULL) {
    list_top->next->prev = new_node;
  }
  new_node->next = list_top->next;
  list_top->next = new_node;
  new_node->prev = list_top;
}

void list_pop(struct linked_list *list_top) {
  struct linked_list *new_node = list_top->next->next;
  if (new_node) {
    new_node->prev = list_top;
  }
  free(list_top->next);
  list_top->next = new_node;
}

struct linked_list *list_peek(struct linked_list *list_top) {
  return list_top->next;
}

