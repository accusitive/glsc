#include <stdio.h>
int main(void)
{
    {
        int a = 5;
        {
            int i = 0;
        _internal_0:;
            if (i < 10)
            {
                {
                    {
                        {
                            int j = 0;
                        _internal_2:;
                            if (j < 10)
                            {
                                {
                                    {
                                        a += 10;
                                    }
                                    j++;
                                    goto _internal_2;
                                }
                            }
                            else
                            {
                                goto _internal_3;
                            }
                        _internal_3:;
                        }
                    }
                    i++;
                    goto _internal_0;
                }
            }
            else
            {
                goto _internal_1;
            }
        _internal_1:;
        }
        printf("%i\n", a);
        return 0;
    }
}