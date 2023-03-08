#include <stdlib.h>
#include <stdbool.h>
#include "wacc.h"
#include "../waccyruntime/runtime.h"

int cmpInts(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

int WACC(qsort)(struct array *array) {
    qsort(array->data, array->size, array->elemsize, cmpInts);
    return 0;
}

int WACC(max)(struct array *array) {
    int max = (int) array->data[0];
    for (int i = 1; i < array->size; i++) {
        if ((int) array->data[i] > max) {
            max = (int) array->data[i];
        }
    }
    return max;
}

int WACC(min)(struct array *array) {
    int min = (int) array->data[0];
    for (int i = 1; i < array->size; i++) {
        if ((int) array->data[i] < min) {
            min = (int) array->data[i];
        }
    }
    return min;
}

int WACC(sum)(struct array *array) {
    int sum = 0;
    for (int i = 0; i < array->size; i++) {
        sum += (int) array->data[i];
    }
    return sum;
}

int WACC(reverse)(struct array *array) {
    int temp;
    for (int i = 0; i < array->size / 2; i++) {
        temp = (int) array->data[i];
        array->data[i] = array->data[array->size - i - 1];
        array->data[array->size - i - 1] = (void *) temp;
    }
    return 0;
}

bool WACC(contains)(struct array *array, int elem) {
    for (int i = 0; i < array->size; i++) {
        if ((int) array->data[i] == elem) {
            return true;
        }
    }
    return false;
}