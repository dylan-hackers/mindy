#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <runtime.h>

static char *freeptr = NULL;
static char *freeend = NULL;

heapptr_t allocate(int bytes)
{
    heapptr_t res;

    if (freeptr + bytes > freeend) {
	freeptr = sbrk(1024*1024);
	freeend = freeptr + 1024*1024;
    }

    res = (heapptr_t)freeptr;
    freeptr += (bytes + 7) & ~7;

    return res;
}
