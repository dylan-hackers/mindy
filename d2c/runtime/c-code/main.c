/* $Header: /home/housel/work/rcs/gd/src/d2c/runtime/c-code/main.c,v 1.1 1995/11/06 17:22:35 wlott Exp $ */

#include <stdlib.h>

#include <runtime.h>

void main(int argc, char *argv[])
{
    descriptor_t *sp = malloc(64*1024);

    /* Run all the top level initializations. */
    inits(sp);
}
