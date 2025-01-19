extern int printf(char *fmt, int num1, int num2);
int add(int lhs, int rhs)
{
    struct s
    {
        int a;
        int b;
    } instance;

    instance.a = 5;
    instance.b = 100;

    int *instancea = &instance.a;
    *instancea = 50;

    printf("test: %i %i", instance.a, instance.b);
    return instance.a + instance.b;
}

