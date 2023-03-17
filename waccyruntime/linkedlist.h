#ifndef _LINKED_LIST_H
#define _LINKED_LIST_H

struct linked_list {
  struct linked_list *next;
  struct linked_list *prev;
  void *data;
};

void list_push(struct linked_list *list_top, void *data);
void list_pop(struct linked_list *list_top);
struct linked_list *list_peek(struct linked_list *list_top);

#endif
