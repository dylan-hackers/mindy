/*
** This module provides a simple shared library loading interface
** for Windows based machines.
*/
#include "shl.h"
#include <Windows.h>
#include <Psapi.h>

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

    *value = GetProcAddress(*handle, sym);

    if (*value == NULL && handle == &self_handle)
    {
        // On Windows, GetProcAddress doesn't recurse into loaded libraries like it
        // does on other operating systems. We emulate this behaviour for our own
        // process.
        DWORD    modules_size = sizeof(HMODULE) * 512, required_size = 0;
        HMODULE* modules = malloc(modules_size);
        BOOL     result = EnumProcessModulesEx(GetCurrentProcess(), modules, modules_size, &required_size,
                                               LIST_MODULES_DEFAULT);
        if (required_size > modules_size)
        { // modules array not big enough, resize and try again
            free(modules);
            modules = malloc(required_size);
            result = EnumProcessModulesEx(GetCurrentProcess(), modules, required_size, &required_size,
                                          LIST_MODULES_DEFAULT);
        }
        modules_size = required_size;

        if (result == 0)
        {
            free(modules);
            return -1; // Abort on error.
        }

        for (DWORD i = 0; i < modules_size / sizeof(HMODULE); i++)
        {
            *value = GetProcAddress(modules[i], sym);
            if (*value != NULL) break;
        }

        free(modules);
    }
    
    return *value == NULL;
}
