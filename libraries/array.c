#include <stdlib.h>
#include "wacc.h"
#include "../waccyruntime/runtime.h"

int cmpInts(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

int WACC(qsort)(struct array *array) {
    qsort(array->data, array->size, array->elemsize, cmpInts);
    return 0;
}