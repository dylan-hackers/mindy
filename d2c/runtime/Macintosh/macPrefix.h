/* Mac prefix header for d2c code */

#include <string.h> /* For memcpy */ 

/* For the mac gc */

#define true 1
#define false 0

//#define SHARED_LIBRARY_BUILD

/* A couple of dylan library functions use this */
#include <time.h>

//extern void * GC_malloc( unsigned long size );

#include <fcntl.mac.h>

/* Make sure the boehm GC compiles properly */
//#define ATOMIC_UNCOLLECTABLE

#define __D2C

/* Boehm GC Configuration*/

#ifdef __MWERKS__

// for CodeWarrior Pro with Metrowerks Standard Library (MSL).
// #define MSL_USE_PRECOMPILED_HEADERS 0
#include <ansi_prefix.mac.h>
#ifndef __STDC__
#define __STDC__ 0
#endif

#endif /* __MWERKS__ */

//#define ATOMIC_UNCOLLECTABLE
#define NO_SIGNALS		// signals aren't real on the Macintosh.
#define ALL_INTERIOR_POINTERS	// follows interior pointers.
#define NO_EXECUTE_PERMISSION
#define SILENT			// no collection messages.
//#define LARGE_CONFIG
#define DONT_ADD_BYTE_AT_END	// no padding.
//#define SMALL_CONFIG		// whether to use a smaller heap.
//#define USE_TEMPORARY_MEMORY	// use Macintosh temporary memory.


#include <sys/errno.h>

#define write( _____a, _____b, _____c )	MacWrite( _____a, _____b, _____c )