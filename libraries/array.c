#include <stdlib.h>
#include <stdbool.h>
#include "wacc.h"
#include "../waccyruntime/runtime.h"

int cmpInts(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

char cmpChars(const void *a, const void *b) {
    return *(char *)a - *(char *)b;
}

int WACC(qsort, 0)(struct array *array) {
    qsort(array->data, array->size, array->elemsize, cmpInts);
    return 0;
}

int WACC(max, 0)(struct array *array) {
    int max = (int) array->data[0];
    for (int i = 1; i < array->size; i++) {
        if ((int) array->data[i] > max) {
            max = (int) array->data[i];
        }
    }
    return max;
}

int WACC(min, 0)(struct array *array) {
    int min = (int) array->data[0];
    for (int i = 1; i < array->size; i++) {
        if ((int) array->data[i] < min) {
            min = (int) array->data[i];
        }
    }
    return min;
}

int WACC(sum, 0)(struct array *array) {
    int sum = 0;
    for (int i = 0; i < array->size; i++) {
        sum += (int) array->data[i];
    }
    return sum;
}

int WACC(reverse, 0)(struct array *array) {
    int temp;
    for (int i = 0; i < array->size / 2; i++) {
        temp = (int) array->data[i];
        array->data[i] = array->data[array->size - i - 1];
        array->data[array->size - i - 1] = (void *) temp;
    }
    return 0;
}

bool WACC(contains, 0)(struct array *array, int elem) {
    for (int i = 0; i < array->size; i++) {
        if ((int) array->data[i] == elem) {
            return true;
        }
    }
    return false;
}

char WACC(max, 1)(struct array *array) {
    char max = ((unsigned char *)array->data)[0];
    for (int i = 1; i < array->size; i++) {
        if (((unsigned char *)array->data)[i] > max) {
            max = ((unsigned char *)array->data)[i];
        }
    }
    return max;
}

char WACC(min, 1)(struct array *array) {
    char min = ((unsigned char *)array->data)[0];
    for (int i = 1; i < array->size; i++) {
        if (((unsigned char *)array->data)[i] < min) {
            min = ((unsigned char *)array->data)[i];
        }
    }
    return min;
}

int WACC(reverse, 1)(struct array *array) {
    char temp;
    for (int i = 0; i < array->size / 2; i++) {
        temp = ((unsigned char *)array->data)[i];
        ((unsigned char *)array->data)[i] = ((unsigned char *)array->data)[array->size - i - 1];
        ((unsigned char *)array->data)[array->size - i - 1] = temp;
    }
    return 0;
}

bool WACC(contains, 1)(struct array *array, char elem) {
    for (int i = 0; i < array->size; i++) {
        if (((unsigned char *)array->data)[i] == elem) {
            return true;
        }
    }
    return false;
}