#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// Array
struct array
{
  unsigned size;
  void **data;
};

void array_literal_create_bytes(unsigned char *data, unsigned size,
                                va_list elemlist)
{
  for (int i = 0; i < size; ++i)
  {
    data[i] = va_arg(elemlist, long);
  }
}

void array_literal_create_longs(long *data, unsigned size, va_list elemlist)
{
  for (int i = 0; i < size; ++i)
  {
    data[i] = va_arg(elemlist, long);
    printf("%ld ", data[i]);
  }
  printf("\n");
}

extern struct array *array_literal_create(unsigned elemsize, unsigned size, void *_R2, void *_R3, ...)
{
  va_list args;
  va_start(args, _R3);
  struct array *out;
  out = (struct array *)malloc(sizeof(struct array) + elemsize * size);
  printf("Creating array literal with elemntsize: %d, %d!\n", elemsize, size);
  out->size = elemsize;
  out->data = (void *)&out->data + sizeof(void *);
  if (elemsize == 1)
  {
    array_literal_create_bytes((unsigned char *)out->data, size, args);
  }
  else if (elemsize == 4)
  {
    array_literal_create_longs((long *)out->data, size, args);
  }
  else
  {
    fprintf(stderr,
            "Runtime error: Unknown runtime error, array of element size: %d\n",
            elemsize);
  }
  va_end(args);
  return out;
}

void check_array_access(struct array *in, unsigned index)
{
  if (in == NULL || in->data == NULL)
  {
    fprintf(stderr, "Runtime error: invalid array %p", in);
    exit(-1);
  }
  else if (index >= in->size)
  {
    fprintf(stderr,
            "Runtime error: array of size %d invalid access of index %d\n",
            in->size, index);
    exit(-1);
  }
}

unsigned array_size(struct array *in) { return in->size; }

extern void *array_access(struct array *in, unsigned index)
{
  check_array_access(in, index);
  printf("Accessing: %d from %p (%d) -> %d\n", index, in, in->size, ((int *)in->data)[index]);
  return &in->data[index];
}

// Pair
struct pair
{
  void *fst;
  void *snd;
};

extern struct pair *pair_create(void *_R0, void *_R1, void *_R2, void *_R3, void *fst, void *snd)
{
  struct pair *out;
  out = (struct pair *)malloc(sizeof(struct pair));
  out->fst = fst;
  out->snd = snd;
  return out;
}

extern void *pair_fst(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  return &in->fst;
}

extern void *pair_snd(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  return &in->snd;
}
// Print functions? (Optional)
