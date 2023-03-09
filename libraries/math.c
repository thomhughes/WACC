#include <math.h>
#include <stdlib.h>
#include "wacc.h"

int WACC(pow, 0)(int n, int index) {
    return pow(n, index);
}

int WACC(srand, 0)(unsigned int seed) {
    srand(seed);
    return 0;
}

int WACC(rand, 0)(void) {
    return rand();
}

int WACC(abs, 0)(int n) {
    return abs(n);
}