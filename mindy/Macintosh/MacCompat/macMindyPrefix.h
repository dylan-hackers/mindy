/*
	This is a combination of all the defines that comp. and interp. need.
*/

	#define MACOS				1		/* System 7 & 8, not OS-X. */
	#define SHLB				1		/* Define if building as a shared library */
	
	#define HAVE_LIBDLD			1		/* We can load PowerPC shared libraries only */
	#define HAVE_LIBDLDELF		1
	
	#define VERSION				"2.3.2"
	#define TARGET				"MACOS" /* Change to MACOSX for X? */

	#define _USING_PROTOTYPES_	1
	
	#define NO_LIMITS_H 1
	#define NO_BSTRING_H 1

	#define M_PI				3.14159265358979323846
	#define M_E					2.7182818284590452354

	#undef NO_LIMITS_H
	
	#define SIZEOF_VOID_P		4					/* 32-bit architecture */
	
	#define	bzero(a, b)			memset(a, 0, b)
	
	#define CHAR_BIT			8
	

	#define NO_PWD_H 			1
	#define NO_SYS_PARAM_H 		1
	#define NO_DIRENT_H 		1
	#define NO_SYS_FILE_H 		1
	#define NO_SYS_WAIT_H 		1
	#define NO_FSYNC			1
	#define NO_SHARP_BANG		1
	#define NO_SIGACTION		1		
	#define NSIG				6 					/* __signal_max is 6 for CW */		

	#define kLIBDIR 				"\t:libraries"	/* : for relative path */
	#define BINDIR 				""
	
	extern char * LIBDIR;
	
	#define WNOHANG				0					/* Dummy Value since we can't use it anyway */
	
	#define YY_ALWAYS_INTERACTIVE 		0
	#define YY_NEVER_INTERACTIVE 		1


	#include<strcasecmp.h>							/* Should just extern it here */
	
	#define TRUE 1
	#define FALSE 0

	
// File Modes
// Likewise

	/*#define O_RDWR				0x0		
	#define O_RDONLY			0x1		
	#define O_WRONLY			0x2			
	#define O_APPEND			0x0100		
	#define O_CREAT				0x0200		
	#define O_EXCL				0x0400		
	#define O_TRUNC				0x0800	
	#define O_NRESOLVE			0x1000		
	#define O_ALIAS				0x2000		
	#define O_RSRC 				0x4000		
	#define O_BINARY			0x8000	*/	
