/* Author: Jim Studt (jim@federated.com)
 */

#ifndef shl_IS_IN
#define shl_IS_IN

/*
** This module approximates the HP shl_* functions for ELF based systems.
*/
typedef void *shl_t;

#define BIND_IMMEDIATE  0x0
#define BIND_DEFERRED   0x1

#define BIND_FIRST      0x4

#define  PROG_HANDLE    ((void *)-1)

extern shl_t shl_load ( const char *path, int flags);
extern int shl_findsym ( shl_t *handle, const char *sym, void **value);
#endif
