#include <stdio.h>
#include <stdlib.h>

void printInt(const int i) {
    printf("%d\n", i);
}

void printString(const char *s) {
    printf("%s\n", s);
}

void error() {
    exit(1);
}

int readInt() {
    int i;
    if (scanf("%d", &i));
    return i;
}

//TODO readString
