/* $Header: /home/housel/work/rcs/gd/src/d2c/runtime/c-code/main.c,v 1.2 1995/12/18 05:12:08 wlott Exp $ */

#include <stdlib.h>

#include <runtime.h>

void not_reached(void)
{
    fprintf(stderr, "entered a branch that supposedly never could be.\n");
    abort();
}

void main(int argc, char *argv[])
{
    descriptor_t *sp = malloc(64*1024);

    /* Run all the top level initializations. */
    inits(sp);
}
