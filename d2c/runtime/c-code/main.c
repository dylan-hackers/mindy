/* $Header: /home/housel/work/rcs/gd/src/d2c/runtime/c-code/main.c,v 1.3 1996/02/06 15:56:04 wlott Exp $ */

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
    inits(sp, argc, argv);
}
