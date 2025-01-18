#include <stdio.h>
extern int add(int lhs, int rhs);

int main(void) {
    printf("output of add(2,4): %i\n", add(2,4));
}