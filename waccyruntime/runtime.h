#ifndef _RUNTIME_H
#define _RUNTIME_H

struct array
{
  void **data;
  unsigned elemsize;
  unsigned size;
  int dimensions;
};

void array_mark(void *array);
void pair_mark(void *pair);
unsigned array_size(struct array *in);

#endif
