#include <stdio.h>

int main(int a, int b)
{

    int i = 0;

// label statement, create block and jump to it
    loop_start: 
    if(i < 5) {
        i = i + 1;
        // unconditionally jump to loop_start, thats where this branch ends
        goto loop_start;
    } else {
        goto loop_end;
    }
    loop_end:;

    // for(int i = 0; i < 5; i = i+1) {

    // }
    return i;
}
