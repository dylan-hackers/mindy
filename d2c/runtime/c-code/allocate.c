#include <stdlib.h>
#include <stdio.h>

#include "config.h"
#include "runtime.h"

#ifdef HAVE_GC_H
#include <gc.h>
#endif

#ifdef HAVE_GC_GC_H
#include <gc/gc.h>
#endif

heapptr_t allocate(int bytes)
{
    return (heapptr_t)GC_malloc_ignore_off_page(bytes);
}

void destroy(void* ptr)
{
  GC_free(ptr);
}
