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

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define STACK_SIZE (96 * 1024)

heapptr_t allocate(int bytes)
{
  return (heapptr_t)GC_malloc_ignore_off_page(bytes);
}

void destroy(void* ptr)
{
  GC_free(ptr);
}

#if defined(HAVE_MPROTECT)
void finalize_stack(void *stack, void *boundary)
{
  mprotect(boundary, getpagesize(), PROT_READ | PROT_WRITE | PROT_EXEC);
}

descriptor_t *allocate_stack(void)
{
  void *stack = GC_malloc_ignore_off_page(STACK_SIZE);
  int pagesize = getpagesize();
  GC_word boundary;
  void *boundaryp;
  
  if(pagesize & -pagesize != pagesize) /* power of 2? */
    abort();
  
  boundary = ((GC_word)stack + STACK_SIZE - pagesize) & -(GC_word)pagesize;
  boundaryp = (void *)boundary;
  
  if(mprotect(boundaryp, pagesize, PROT_READ) == 0) {
    GC_register_finalizer(stack, finalize_stack, boundaryp, NULL, NULL);
  }
  
  return (descriptor_t *)stack;
}
#else
descriptor_t *allocate_stack(void)
{
  return (descriptor_t *) allocate(STACK_SIZE);
}
#endif

