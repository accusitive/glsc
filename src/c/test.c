extern int printf(char* fmt, int *num, int *num2, int num);
int add(int lhs, int rhs)
{
    // stack slot containgin 5
    int a = 5;
    // stack slot containing address of a
    int *b = &a;
    // store 10 to ??
    *b = 10;
    printf("abc   %p   %p   %i", &a, &a, a);
    return a;
}
