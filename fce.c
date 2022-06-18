#include <stdio.h>

int writeln(int x) {
    printf("%d\n", x);
    return 0;
}

int write(int x) {
    printf("%d", x);
    return 0;
}

int readln(int *x) {
    scanf("%d", x);
    return 0;
}

int writefln(double x) {
    printf("%lf\n", x);
    return 0;
}

int writef(double x) {
    printf("%lf", x);
    return 0;
}

int readfln(double *x) {
    scanf("%lf", x);
    return 0;
}

int writec(int a) {
    char c = a;
    printf("%c", c);
    return 0;
}

int writecln(int a) {
    char c = a;
    printf("%c\n", c);
    return 0;
}

int double_to_int(double a) {
    return a;
}

double int_to_double(int a) {
    return a;
}