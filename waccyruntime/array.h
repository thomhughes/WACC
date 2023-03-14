#ifndef _ARRAY_H
#define _ARRAY_H

struct Array {
  int size;
  void **data;
};

struct Array *array_init(int reserved, int elemsize);
void *array_get(struct Array *array, int index, int elemsize);
void array_cleanup(struct Array *array);

#endif

