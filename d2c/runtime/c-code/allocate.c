#include <stdlib.h>

#include <runtime.h>

heapptr_t allocate(int bytes)
{
    return malloc(bytes);
}
