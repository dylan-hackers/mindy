/* $Header: /home/housel/work/rcs/gd/src/d2c/runtime/c-code/main.c,v 1.4 1996/02/09 17:15:18 wlott Exp $ */

#include <stdlib.h>

#include <runtime.h>

#include "../gc/gc.h"

void not_reached(void)
{
    fprintf(stderr, "entered a branch that supposedly never could be.\n");
    abort();
}

void main(int argc, char *argv[])
{
    descriptor_t *sp = GC_malloc(64*1024);

    /* Run all the top level initializations. */
    inits(sp, argc, argv);
}
