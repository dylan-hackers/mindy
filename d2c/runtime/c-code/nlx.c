#include <stdlib.h>
#include <runtime.h>
#include <setjmp.h>

/*
$header$

Changes:

- portable version using setjmp/jongjmp (andreas)

*/

descriptor_t *catch(descriptor_t *(*fn)(descriptor_t *sp, void *state,
					heapptr_t body_func),
		    descriptor_t *sp, heapptr_t body_func)
{
    volatile jmp_buf state;
    volatile int rc;

    if(rc = setjmp(state)) { /* This _is_ an assignment */
      /* longjmp was called, return stack_top */
      return (descriptor_t *)rc;    
    } else {
      /* first pass */
      return (descriptor_t *)(fn(sp, state, body_func));
    }
}

void throw(void *state, descriptor_t *stack_top)
{
    longjmp(state, (long)stack_top);
    /*    restore_state(state, (long)stack_top); */
}
