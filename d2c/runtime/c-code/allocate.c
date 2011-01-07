#include <stdlib.h>
#include <stdio.h>

#ifndef __MWERKS__
#include "config.h"
#endif
#include "runtime.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined(HAVE_GC_H)
#include <gc.h>
#elif defined(HAVE_GC_GC_H)
#include <gc/gc.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#define STACK_SIZE (96 * 1024)

void dylan_gc_init(void)
{
  GC_all_interior_pointers = 0;
  GC_free_space_divisor = 2;
  GC_use_entire_heap = 1;
  GC_INIT();
}

long dylan_gc_get_total_bytes(void)
{
  return GC_get_total_bytes();
}

heapptr_t allocate(int bytes)
{
  return (heapptr_t)GC_malloc(bytes);
}

void destroy(void* ptr)
{
  GC_free(ptr);
}

#if defined(HAVE_MPROTECT) && defined(HAVE_GETPAGESIZE) && !defined(__CYGWIN__)
void finalize_stack(void *stack, void *boundary)
{
  mprotect(boundary, getpagesize(), PROT_READ | PROT_WRITE | PROT_EXEC);
}

descriptor_t *allocate_stack(void)
{
  void *stack = GC_malloc(STACK_SIZE);
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

