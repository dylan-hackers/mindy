/* Author: Jim Studt (jim@federated.com)
 */

/*
** This module approximates the HP style 'shl_' calls for shared library
** handling for ELF based machines.
*/
#include "shl.h"
#include <dlfcn.h>
#include <stdlib.h>

shl_t shl_load (const char *path, int flags)
{
    int flag;

    if ((flags & BIND_IMMEDIATE)) flag = RTLD_NOW;
    else if ((flags & BIND_DEFERRED)) flag = RTLD_NOW;
    else abort();

    if (flags & BIND_FIRST) abort();

    return dlopen(path, flag);
}

int shl_findsym (shl_t *handle, const char *sym, void **value)
{
    static void *self_handle = 0;

    if (*handle == 0) abort();
    if (*handle == PROG_HANDLE) {
        if (self_handle == 0) {
            self_handle = dlopen(0, RTLD_NOW);
        }
        handle = &self_handle;
    }

    *value = dlsym(*handle, (char *)sym);

    return dlerror() == 0 ? 0 : -1;
}
