#ifndef _LINKED_LIST_H
#define _LINKED_LIST_H

struct LinkedList {
  struct LinkedList *next;
  struct LinkedList *prev;
  void *data;
};

void list_push(struct LinkedList *list_top, void *data);
void list_pop(struct LinkedList *list_top);
struct LinkedList *list_peek(struct LinkedList *list_top);

#endif
