#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// Array
struct array {
  unsigned size;
  void *data;
};

inline void array_literal_create_bytes(unsigned char *data, unsigned size,
                                       va_list elemlist) {
  for (int i = 0; i < size; ++i) {
    data[i] = va_arg(elemlist, long);
  }
}

inline void array_literal_create_longs(long *data, unsigned size, va_list elemlist) {
  for (int i = 0; i < size; ++i) {
    data[i] = va_arg(elemlist, long);
  }
}

struct array *array_literal_create(unsigned elemsize, unsigned size, void *_R2, void *_R3, ...) {
  va_list args;
  va_start(args, _R3);
  struct array *out;
  out = (struct array *)malloc(sizeof(struct array) + elemsize * size);
  out->size = elemsize;
  out->data = &out->data + sizeof(void *);
  if (elemsize == 1) {
    array_literal_create_bytes((unsigned char *)out->data, size, args);
  } else if (elemsize == 4) {
    array_literal_create_longs((long *)out->data, size, args);
  } else {
    fprintf(stderr,
            "Runtime error: Unknown runtime error, array of element size: %d\n",
            elemsize);
  }
  va_end(args);
  return out;
}

inline void check_array_access(struct array *in, unsigned index) {
  if (in == NULL || in->data == NULL) {
    fprintf(stderr, "Runtime error: invalid array %p", in);
    exit(-1);
  } else if (index >= in->size) {
    fprintf(stderr,
            "Runtime error: array of size %d invalid access of index %d\n",
            in->size, index);
    exit(-1);
  }
}

unsigned array_size(struct array *in) { return in->size; }

void *array_read(struct array *in, unsigned index) {
  check_array_access(in, index);
  return ((void **)in->data)[index];
}

void array_write(struct array *in, void *value, unsigned index) {
  check_array_access(in, index);
  ((void **)in->data)[index] = value;
}

// Pair
struct pair {
  void *fst;
  void *snd;
};

struct pair *pair_create(void* _R0, void* _R1, void* _R2, void* _R3, void *fst, void *snd) {
  struct pair *out;
  out = (struct pair*)malloc(sizeof(struct pair));
  out->fst = fst;
  out->snd = snd;
  return out;
}

void *pair_fst(struct pair* in, void* _R1, void* _R2, void* _R3, void *value) {
  if (value != NULL) in->fst = value;
  return in->fst;
}

void *pair_snd(struct pair* in, void* _R1, void* _R2, void* _R3, void *value) {
  if (value != NULL) in->snd = value;
  return in->snd;
}
// Print functions? (Optional)

