#include <stdio.h>

int print_int(int a) {
    printf("%d\n", a);
    return 0;
}

int print_float(double a) {
    printf("%f\n", a);
    return 0;
}

int one() {
    return 1;
}

double one_float() {
    return 1.0;
}

int many_args(int x, double y, int z) {
    return x + (int) y + z;
}

int print_add(double a, double b) {
    printf("Adding %f + %f\n", a, b);
    return 0;
}

int increment() {
    static int count = 0;
    count++;
    return count;
}

double int_to_float(int x) {
    return 0.0 + x;
}