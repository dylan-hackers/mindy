/* Mac prefix header for d2c code */


#define GD_PLATFORM_MACOS

#include <string.h> /* For memcpy */ 

/* For the mac gc */

//#define SHARED_LIBRARY_BUILD

/* A couple of dylan library functions use this */
#include <time.h>

//extern void * GC_malloc( unsigned long size );

#include <fcntl.h>

/* Make sure the boehm GC compiles properly */
//#define ATOMIC_UNCOLLECTABLE

// mindy files use the following #define
//#define __D2C

/* Boehm GC Configuration*/

// #### MrC ####
#ifdef __MRC__
// for CodeWarrior Pro with Metrowerks Standard Library (MSL).
// Using MrC plugin compiler
#include <ansi_prefix.mac.h>
#ifndef __STDC__
#define __STDC__ 1
#endif
#endif /* __MRC__ */



// #### MetroWerks ####
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


#define write( _____a, _____b, _____c )	MacWrite( _____a, _____b, _____c )