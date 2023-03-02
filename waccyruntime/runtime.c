#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// Array
struct array
{
  void **data;
  unsigned elemsize;
  unsigned size;
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
  }
}

extern struct array *array_literal_create(unsigned elemsize, unsigned size, void *_R2, void *_R3, ...)
{
  va_list args;
  va_start(args, _R3);
  struct array *out;
  out = (struct array *)calloc(1, sizeof(struct array) + elemsize * size);
  out->elemsize = elemsize;
  out->size = size;
  out->data = (void **)&out->size + 1;
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
    fprintf(stdout,
            "Runtime error: Unknown runtime error, array of element size: %d\n",
            elemsize);
  }
  va_end(args);
  return out;
}

extern void array_free(struct array *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: attempting to free NULL array.\n");
    exit(-1);
  }
  free(in);
  in = NULL;
}

void check_array_access(struct array *in, unsigned index)
{
  if (in == NULL || in->data == NULL)
  {
    fprintf(stdout, "Runtime error: invalid array %p\n", in);
    exit(-1);
  }
  else if (index >= in->size)
  {
    fprintf(stdout,
            "Runtime error: array of size %d invalid access of index %d\n",
            in->size, index);
    exit(-1);
  }
}

unsigned array_size(struct array *in) { return in->size; }

extern void *array_access(struct array *in, unsigned index)
{
  check_array_access(in, index);
  if (in->elemsize == 1)
  {
    return &((unsigned char *)in->data)[index];
  }

  return &in->data[index];
}

// Pair
struct pair
{
  void *fst;
  void *snd;
};

void check_pair(struct pair *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: invalid pair %p\n", in);
    exit(-1);
  }
}

extern struct pair *pair_create(void *_R0, void *_R1, void *_R2, void *_R3, void *fst, void *snd)
{
  struct pair *out;
  out = (struct pair *)calloc(1, sizeof(struct pair));
  // printf("(%p) fst: %p, snd: %p\n", out, fst, snd);
  out->fst = fst;
  out->snd = snd;
  return out;
}

extern void pair_free(struct pair *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: attempting to free NULL pair.\n");
    exit(-1);
  }
  free(in);
  in = NULL;
}

extern void *pair_fst(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  check_pair(in);
  return &in->fst;
}

extern void *pair_snd(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  check_pair(in);
  return &in->snd;
}

// Print functions? (Optional)

// Read functions?
void read(void *out, int length)
{
  if (out == NULL)
  {
    fprintf(stdout, "Runtime error: reading into NULL value.\n");
    exit(-1);
  }
  switch (length)
  {
  case 4:
    scanf("%d", (int *)out);
    break;
  case 1:
    scanf(" %c", (char *)out);
    break;
  }
}

// Arithmetic runtime errors
extern void error_arithmetic_overflow()
{
  fprintf(stdout, "Runtime error: arithmetic overflow\n");
  exit(-1);
}

// https://developer.arm.com/documentation/dui0773/e/Coding-Considerations/Integer-division-by-zero-errors-in-C-code?lang=en
extern int __aeabi_idiv0(void)
{
  fprintf(stdout, "Runtime error: division by zero\n");
  exit(-1);
}