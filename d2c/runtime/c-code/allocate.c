#include <stdlib.h>
#include <stdio.h>

#include <runtime.h>

#include "../gc/gc.h"

heapptr_t allocate(int bytes)
{
    return GC_malloc(bytes);
}
