/* $Header: /scm/cvs/src/d2c/runtime/c-code/main.c,v 1.28 2003/12/05 08:14:32 brent Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#ifndef __MWERKS__
#include "config.h"
#endif
#include "runtime.h"

#if defined(HAVE_GC_H)
#include <gc.h>
#elif defined(HAVE_GC_GC_H)
#include <gc/gc.h>
#endif

int application_argc;
char **application_argv;

void GD_NORETURN not_reached(void)
{
    fprintf(stderr, "entered a branch that supposedly never could be.\n");
    abort();
}

/* Microsoft Visual C++ refuses to allow main() to come from a
   library.  Thus, we need to put main() in inits.c and have it simply
   call this function.  
   */
void real_main(int argc, char *argv[])
{
    descriptor_t *sp;

    GC_INIT();

    sp = allocate_stack();

    /* Remember our arguments so we can support Harlequin-style
       application-name and application-arguments functions. Once we
       make these copies, we are no longer allowed to destructively
       modify argv. But this is Dylan--you should know better than
       to destructively modify things without express permission anyway. */
    application_argc = argc;
    application_argv = argv;

    /* Run all the top level initializations. */
    inits(sp, argc, argv);
}


/* GDB support routines */
/*   The following routines are included to allow easier debugging via the */
/*   GDB debugger.  There has been very little attempt to make them pretty */
/*   or even terribly stable.  However, they have sufficient utility at    */
/*   present to (hopefully) justify a bit of hackishness.                  */

#define MAX_GDB_RECURSION 20 
#define MAX_RESULTS_COUNT 20
#define GDB_STACK_SIZE 100000

descriptor_t *gdb_stack_stack[MAX_GDB_RECURSION];
descriptor_t gdb_result_stack[MAX_RESULTS_COUNT];
int gdb_stack_stack_index = 0;
int gdb_stack_index = 0;

/* extern descriptor_t dylan_apply_safely_value; not used? */ 
extern descriptor_t dylanZdylan_visceraZgdb_integer_value;
extern struct heapobj dylanZdylan_visceraZCLS_byte_string_HEAP;
extern void dylanZdylan_visceraZgdb_print_object_METH();

void string_arg (char *arg) {
  descriptor_t tmp;
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];
  void **dylan_str;
  size_t len = 0;

  if (stack == 0)
    stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);
  len = strlen(arg);
  if (*(arg + len) != 0) {
	  /* provide space for a null-terminator */
	  len++;
  }

  dylan_str = (void **)
    GC_malloc(sizeof(struct heapobj *) + sizeof(long) + len);
  dylan_str[0] = (void *)(&dylanZdylan_visceraZCLS_byte_string_HEAP);
  dylan_str[1] = (void *)len;
  strncpy((char *)(&dylan_str[2]), arg, len);
  *((char*)(&dylan_str[2]) + len) = 0;

  tmp.heapptr = (struct heapobj *)dylan_str;
  tmp.dataword.l = 0;
  stack[gdb_stack_index++] = tmp;
}

void int_arg (int arg) {
  descriptor_t tmp;
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];

  if (stack == 0)
   stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);

  tmp.heapptr = dylanZdylan_visceraZgdb_integer_value.heapptr;
  tmp.dataword.l = arg;
  stack[gdb_stack_index++] = tmp;
}

void heap_arg (struct heapobj *arg) {
  descriptor_t tmp;
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];

  if (stack == 0)
   stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);

  tmp.heapptr = arg;
  tmp.dataword.l = 0;
  stack[gdb_stack_index++] = tmp;
}

void desc_arg (descriptor_t arg) {
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];

  if (stack == 0)
   stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);

  stack[gdb_stack_index++] = arg;
}

#define GENERAL_ENTRY(func) \
    ((entry_t)SLOT(func, void *, 8))

int gdb_invoke_function (descriptor_t fun, int count)
{
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];
  descriptor_t *result, *newstack;
  int i, result_count;

  if (stack == 0)
    stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);
  gdb_stack_stack_index++;
  gdb_stack_index = 0;

  result = GENERAL_ENTRY(fun.heapptr)(stack + count, fun.heapptr, count);
  newstack = gdb_stack_stack[gdb_stack_stack_index];
  if (newstack == 0)
    newstack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);
  gdb_stack_stack_index++;
  gdb_stack_index = 0;
  result_count = result - stack;
  for (i = 0 ; i < result_count; i++)
    gdb_result_stack[i] = stack[i];

  --gdb_stack_stack_index;
  return result_count;
}

void gdb_print_heapobj (struct heapobj *obj)
{
  descriptor_t tmp;
  tmp.heapptr = obj;
  tmp.dataword.l = 0;
  dylanZdylan_visceraZgdb_print_object_METH((descriptor_t *)GC_malloc(10000), tmp);
}

void gdb_print_genobj (descriptor_t obj)
{
  dylanZdylan_visceraZgdb_print_object_METH((descriptor_t *)GC_malloc(10000), obj);
}



#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if HAVE_GETRLIMIT
void no_core_dumps(void)
{
  struct rlimit lim;
  getrlimit(RLIMIT_CORE, &lim);
  lim.rlim_cur = 0;
  setrlimit(RLIMIT_CORE, &lim);
  lim.rlim_cur = RLIM_INFINITY;
  lim.rlim_max = RLIM_INFINITY;
  setrlimit(RLIMIT_STACK, &lim);
}
#else
void no_core_dumps(void)
{
  /* other platforms don't core nicely, if at all */
}
#endif

#ifdef HAVE_GETRUSAGE
long *cpu_time(void)
{
  long *retval = (long *) allocate(2 * sizeof(long));
  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru) == 0) {
    retval[0]
      = ru.ru_utime.tv_sec + ru.ru_stime.tv_sec
      + (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) / 1000000L;
    retval[1] = (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) % 1000000L;
  } else {
    retval[0] = retval[1] = 0;
  }
  return retval;
}
#else
long *cpu_time(void)
{
  long *retval = (long *) allocate(2 * sizeof(long));
  clock_t runtime = clock();
  if (runtime >= 0) {
    retval[0] = (runtime / CLOCKS_PER_SEC);
#ifdef __MWERKS__
    retval[1] = (runtime - retval[0] * CLOCKS_PER_SEC) * 1000000L / CLOCKS_PER_SEC;
#else
    retval[1] = (runtime % CLOCKS_PER_SEC) * 1000000L / CLOCKS_PER_SEC;
#endif
  } else {
    retval[0] = retval[1] = 0;
  }
  return retval;
}
#endif
