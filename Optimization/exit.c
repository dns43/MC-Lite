
#include <stdio.h>
#include <stdlib.h>


extern int bbCounter;

void exitfunc()
{
    printf("NUMBER OF BASIC BLOCKS EXECUTED := %d\n", bbCounter);
}

void setupAtExit() 
{
    atexit(exitfunc);
}
