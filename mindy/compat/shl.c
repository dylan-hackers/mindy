/* Author: Jim Studt (jim@federated.com)
 */

/*
** This module approximates the HP style 'shl_' calls for shared library
** handling for ELF based machines.
*/
#include "shl.h"
#include <dlfcn.h>
#include <stdlib.h>

shl_t shl_load ( const char *path, int flags, long address)
{
    int flag;

    if ( address != 0) abort();
    
    if ( (flags & BIND_IMMEDIATE)) flag = RTLD_NOW;
    else if ( (flags & BIND_DEFERRED)) flag = RTLD_NOW;
    else abort();
    
    if ( flags & BIND_FIRST) abort();
    if ( flags & BIND_NONFATAL) abort();
    if ( flags & BIND_NOSTART) abort();

    return dlopen( path, flag);
}

int shl_findsym ( shl_t *handle, const char *sym, short type,
		 void *value)
{
    const void *v;
    static void *self_handle = 0;

    if ( *handle == 0) abort();
    if ( *handle == PROG_HANDLE) {
	if ( self_handle == 0) {
	    self_handle = dlopen( 0, RTLD_NOW);
	}
	handle = &self_handle;
    }

    v = dlsym( *handle, (char *)sym);
    *(void **)value = (void *)v;
    
    return dlerror() == 0 ? 0 : -1;
}
