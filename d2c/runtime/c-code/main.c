/* $Header: /scm/cvs/src/d2c/runtime/c-code/main.c,v 1.1 1998/05/03 19:55:41 andreas Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <runtime.h>
#include <math.h>

#include "../gc/gc.h"

void not_reached(void)
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
    descriptor_t *sp = GC_malloc(64*1024);

    /* Run all the top level initializations. */
    inits(sp, argc, argv);

    /* exit(0) -- I would have thought this to be the default, but it
       seems to be neccessary, at least on win32 */
    exit(0);
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
extern descriptor_t dylanZdylan_visceraZCLS_byte_string;
extern void dylanZdylan_visceraZgdb_print_object_METH();

void string_arg (char *arg) {
  descriptor_t tmp;
  descriptor_t *stack = gdb_stack_stack[gdb_stack_stack_index];
  void **dylan_str;

  if (stack == 0)
    stack = gdb_stack_stack[gdb_stack_stack_index]
      = (descriptor_t *) malloc(GDB_STACK_SIZE);

  dylan_str = (void **)
    GC_malloc(sizeof(struct heapobj *) + sizeof(long) + strlen(arg));
  dylan_str[0] = (void *)(dylanZdylan_visceraZCLS_byte_string.heapptr);
  dylan_str[1] = (void *)strlen(arg);
  strcpy((char *)(&dylan_str[2]), arg);

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

gdb_print_heapobj (struct heapobj *obj)
{
  descriptor_t tmp;
  tmp.heapptr = obj;
  tmp.dataword.l = 0;
  dylanZdylan_visceraZgdb_print_object_METH((descriptor_t *)GC_malloc(10000), tmp);
}

gdb_print_genobj (descriptor_t obj)
{
  dylanZdylan_visceraZgdb_print_object_METH((descriptor_t *)GC_malloc(10000), obj);
}


/* win32 specific stuff which patches over deficiencies in the Visual
   C++ runtime.
   */

#if (defined(WIN32) && !defined(WIN32_GCC))

double rint(double x)
{
  /* ### I'm not sure this is entirely correct, but it's certainly
     closer than what we had here before 
     */
  double temp = floor(x+0.5);
  return (temp > x) ? temp : floor(x);
}

float fabsf (float x)
{
    return (float) fabs(x);
}

float sinf (float x)
{
  return (float) sin(x);
}

float cosf (float x)
{
  return (float) cos(x);
}

float tanf (float x)
{
  return (float) tan(x);
}

float asinf (float x)
{
  return (float) asin(x);
}

float acosf (float x)
{
  return (float) acos(x);
}

float atanf (float x)
{
  return (float) atan(x);
}

float atan2f (float x, float y)
{
  return (float) atan2(x, y);
}

float expf (float x)
{
  return (float) exp(x);
}

float sqrtf (float x)
{
  return (float) sqrt(x);
}

/* Man, is this broken...  */
double log2 (double x)
{
  return 0;
}

#endif
