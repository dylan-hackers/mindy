/* Mac prefix header for d2c code */

#include <string.h> /* For memcpy */ 

/* For the mac gc */

#define true 1
#define false 0

#define SHARED_LIBRARY_BUILD

/* A couple of dylan library functions use this */
#include <time.h>

/* Keep the Boehm GC quiet */

#define SILENT

/* Same as macmindyPrefix.h */

// Errnos
// Synchronized with CW

	#define EPERM				1
	#define EACCES				2
	#define EBADF				3
	#define EDEADLOCK			4
	#define EMFILE				5
	#define ENOENT				6
	#define ENFILE				7
	#define ENOSPC				8
	#define EINVAL				9
	#define EIO				    10
	#define ENOMEM			    11
	#define ENOSYS			    12
// Made up
	#define EINTR			    13
	#define EEXIST			    14
	#define EFBIG				15
    #define EPIPE				16
    #define ENAMETOOLONG		17
    #define EROFS				18
    #define EISDIR				19
    #define ENOTDIR				20
    #define EFAULT				21
    #define ENXIO				22
	
// File Modes
// Likewise

	#define O_RDWR				0x0		
	#define O_RDONLY			0x1		
	#define O_WRONLY			0x2			
	#define O_APPEND			0x0100		
	#define O_CREAT				0x0200		
	#define O_EXCL				0x0400		
	#define O_TRUNC				0x0800	
	#define O_NRESOLVE			0x1000		
	#define O_ALIAS				0x2000		
	#define O_RSRC 				0x4000		
	#define O_BINARY			0x8000		
