extern int printf(char *fmt, int num0, int num1);
int add(int a, int b)
{
    for (int i = 0; i < 5; i = i + 1)
    {
        for (int j = 0; j < 5; j = j + 1)
        {
            printf("i: %i j: %i    ", i, j);
        }
    }
    return 0;
}
