#include <stdlib.h>
#include <stdio.h>

#include "runtime.h"

#include <gc.h>

heapptr_t allocate(int bytes)
{
    return (heapptr_t)GC_malloc_ignore_off_page(bytes);
}
