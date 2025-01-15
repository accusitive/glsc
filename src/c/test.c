int figure_me_out(void)
{
    int accumulator = 5;
    for (int i = 0; i < 10; i++)
    {
        for (int j = 0; j < 10; j++)
        {
            for (int k = 0; k < 10; k++)
            {
                for (int l = 0; l < 10; l++)
                {
                    accumulator += 1;
                }
            }
        }
    }
    return accumulator;
}
