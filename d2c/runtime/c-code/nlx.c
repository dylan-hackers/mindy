#include <stdlib.h>

#include <runtime.h>

#include "internals.h"

struct catch_info {
    descriptor_t *(*fn)(descriptor_t *sp, void *state, heapptr_t body_func);
    descriptor_t *sp;
    heapptr_t body_func;
};

static long catch_aux(struct machine_state *state, void *arg)
{
    struct catch_info *info = arg;

    return (long)(info->fn(info->sp, state, info->body_func));
}

descriptor_t *catch(descriptor_t *(*fn)(descriptor_t *sp, void *state,
					heapptr_t body_func),
		    descriptor_t *sp, heapptr_t body_func)
{
    struct catch_info info;

    info.fn = fn;
    info.sp = sp;
    info.body_func = body_func;

    return (descriptor_t *)save_state(catch_aux, &info);
}

void throw(void *state, descriptor_t *stack_top)
{
    restore_state(state, (long)stack_top);
}
