/* Author: Jim Studt (jim@federated.com)
 */

/*
** This module provides a simple shared library loading interface
** for ELF based machines.
*/
#include "shl.h"
#include <dlfcn.h>
#include <stdlib.h>

shl_t shl_load (const char *path)
{
    int flag = RTLD_NOW;

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
