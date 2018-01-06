#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(const int i) {
    printf("%d\n", i);
}

void printString(const char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error");
    exit(1);
}

int readInt() {
    int i;
    char* buff = 0;
    size_t n = 0;
    if (getline(&buff, &n, stdin));
    if (sscanf(buff, "%d", &i));
    free(buff);
    return i;
}

char* readString() {
    char* buff = 0;
    size_t n = 0;
    ssize_t newline;
    newline = getline(&buff, &n, stdin);
    if (newline > 0)
        buff[newline-1] = 0;
    return buff;
}

char* __CONCAT_STRINGS__(char* s1, char* s2) {
    char* buff = malloc(strlen(s1) + strlen(s2) + 1 * sizeof(char));
    strcpy(buff, s1);
    strcat(buff, s2);
    return buff;
}
