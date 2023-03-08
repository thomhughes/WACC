#include <string.h>

#define WACC(name) \
    wacc_##name

int WACC(strlen)(char *str)
{
    return strlen(str);
}