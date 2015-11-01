/* Author: Jim Studt (jim@federated.com)
 */

#ifndef shl_IS_IN
#define shl_IS_IN

/*
** This module provides a simple shared library loading interface.
*/
typedef void *shl_t;

#define PROG_HANDLE ((void *)-1)

extern shl_t shl_load(const char *path);
extern int shl_findsym(shl_t *handle, const char *sym, void **value);
#endif
