/* Author: Jim Studt (jim@federated.com)
 */

#ifndef shl_IS_IN
#define shl_IS_IN

/*
** This module approximates the HP shl_* functions for ELF based systems.
*/
typedef void *shl_t; 

#define         NO_INITIALIZER  ((void *)(-1))

#define BIND_IMMEDIATE  0x0
#define BIND_DEFERRED   0x1
#define BIND_REFERENCE  0x2

#define BIND_FIRST      0x4
#define BIND_NONFATAL   0x8
#define BIND_NOSTART    0x10
#define BIND_VERBOSE    0x20
#define BIND_RESTRICTED 0x40  

#define TYPE_UNDEFINED  0
#define TYPE_PROCEDURE  3
#define TYPE_DATA       2
#define TYPE_STORAGE    7

#define  PROG_HANDLE    ((void *)-1)

extern shl_t shl_load ( const char *path, int flags, long address);
extern int shl_findsym ( shl_t *handle, const char *sym, short type,
                          void *value);

/*
** The following are not implemented because mindy didn't need them.
** They will fail to link.
*/
#if 0
extern int shl_unload ( shl_t handle);
extern int shl_get ( int index, struct shl_descriptor **desc);
extern int shl_gethandle ( shl_t handle, struct shl_descriptor **desc);
extern int shl_definesym ( const char *sym, short type, long value, int flags);
extern int shl_getsymbols ( shl_t handle, short type, int flags, 
                            void *(*memory)(), struct shl_symbol **symbols); 
#endif



#endif
