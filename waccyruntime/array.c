#include "array.h"

#include <stdlib.h>

struct Array *array_init(int reserved, int elemsize) {
  struct Array *array = malloc(sizeof(struct Array) + elemsize * reserved);
  array->size = reserved;
  array->data = (void**)(&array->data) + 1;
  return array;
}

void *array_get(struct Array *array, int index, int elemsize) {
  if (index >= array->size) {
    array = realloc(array, sizeof(struct Array) + elemsize * (elemsize * 2 * (index + 1)));
    array->data = (void**)(&array->data) + 1;
  }

  return array->data[index];
}

void array_cleanup(struct Array *array) {
  free(array);
}
