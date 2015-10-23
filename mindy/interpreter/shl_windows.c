/*
** This module provides a simple shared library loading interface
** for Windows based machines.
*/
#include "shl.h"
#include <Windows.h>

shl_t shl_load (const char *path)
{
	return LoadLibraryA(path);
}

int shl_findsym (shl_t *handle, const char *sym, void **value)
{
    static void *self_handle = 0;

    if (*handle == 0) abort();
    if (*handle == PROG_HANDLE) {
        if (self_handle == 0) {
            self_handle = GetModuleHandle(NULL);
        }
        handle = &self_handle;
    }

    *value = GetProcAddress(*handle, (char *)sym);

    return *value == NULL;
}