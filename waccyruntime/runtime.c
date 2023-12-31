#include "runtime.h"
#include "gc.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Array

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

struct array *array_literal_create(unsigned elemsize, unsigned size, int dimensions, void *_R3, ...)
{
  // printf("Creating array literal!\n");
  va_list args;
  va_start(args, _R3);
  struct array *out;
  out = (struct array *)heap_create(sizeof(struct array) + elemsize * size, Array);
  out->elemsize = elemsize;
  out->size = size;
  out->data = (void **)&out->size + 1;
  out->dimensions = dimensions;
  if (elemsize == 1)
  {
    array_literal_create_bytes((unsigned char *)out->data, size, args);
  }
  else if (elemsize == 4)
  {
    array_literal_create_longs((long*)out->data, size, args);
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

void array_free(struct array *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: attempting to free NULL array.\n");
    exit(-1);
  }
  heap_destroy(in);
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

void *array_access(struct array *in, unsigned index)
{
  check_array_access(in, index);
  if (in->elemsize == 1)
  {
    return &((unsigned char *)in->data)[index];
  }

  void *ptr = &in->data[index];
  return ptr;
}

void array_mark(void *array_) {
  struct array *array = (struct array *)array_;
  if (array->dimensions > 1) {
    for (int i = 0; i < array->size; ++i) {
     heap_mark(*(void**)array_access(array, i));
    }
  }
}

// Pair
struct pair
{
  void *fst;
  void *snd;
  bool fst_ref;
  bool snd_ref;
};

void check_pair(struct pair *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: invalid pair %p\n", in);
    exit(-1);
  }
}

struct pair *pair_create(bool fst_ref, bool snd_ref, void *_R2, void *_R3, void *fst, void *snd)
{
  struct pair *out;
  out = (struct pair *)heap_create(sizeof(struct pair), Pair); 
  out->fst = fst;
  out->snd = snd;
  out->fst_ref = fst_ref;
  out->snd_ref = snd_ref;
  return out;
}

void pair_free(struct pair *in)
{
  if (in == NULL)
  {
    fprintf(stdout, "Runtime error: attempting to free NULL pair.\n");
    exit(-1);
  }
  heap_destroy(in);
  in = NULL;
}

void *pair_fst(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  check_pair(in);
  return &in->fst;
}

void *pair_snd(struct pair *in, void *_R1, void *_R2, void *_R3, void *_R4)
{
  check_pair(in);
  return &in->snd;
}

void pair_mark(void *pair_) {
  struct pair *pair = (struct pair *)pair_;
  if (pair->fst_ref) {
    heap_mark(pair->fst);
  }
  if (pair->snd_ref) {
    heap_mark(pair->snd);
  }
}

// Print functions
void _printi(int i)
{
  printf("%d", i);
}

void _prints(char *string)
{
  printf("%s", string);
}

void _printb(char b)
{
  printf("%s", b ? "true" : "false");
}

void _printc(char c)
{
  printf("%c", c);
}

void _printp(void *p)
{
  printf("%p", p);
}

void _println(void)
{
  putchar('\n');
}

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
void error_arithmetic_overflow()
{
  fprintf(stdout, "Runtime error: arithmetic overflow\n");
  exit(-1);
}

// https://developer.arm.com/documentation/dui0773/e/Coding-Considerations/Integer-division-by-zero-errors-in-C-code?lang=en
int __aeabi_idiv0(void)
{
  fprintf(stdout, "Runtime error: division by zero\n");
  exit(-1);
}

