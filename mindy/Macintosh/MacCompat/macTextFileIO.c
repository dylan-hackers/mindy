// MactextFileIO.c
// Replaces MSL's read/write routines with ones that know about text/binary files
// Hack 'open' or your code to set the flags correctly, binary is default( unlike ansi stdio )
// Link in before MSL


// includes

#include <console.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include <Errors.h>
#include <Files.h>
#include <LowMem.h>
#include <Processes.h>
#include <Types.h>
#include <Script.h>
#include <MacMemory.h>

#include <abort_exit.h>
#include <fcntl.h>
#include <path2fss.h>

char * LIBDIR;


// defines

#define MINDY_DEBUGGER			1

#define WRITE_BUFFER_SIZE		4096		// Size of the write translation buffer
#define MAX_FILES				64

#ifndef O_RDWR
#	define O_RDWR		0x0
#	define O_RDONLY		0x1
#	define O_WRONLY		0x2
#	define O_APPEND		0x0100
#	define O_CREAT		0x0200
#	define O_EXCL		0x0400
#	define O_TRUNC		0x0800
#	define O_NRESOLVE	0x1000
#	define O_ALIAS		0x2000
#	define O_RSRC 		0x4000
#	define O_BINARY		0x8000
#endif

#ifndef TRUE
#	define TRUE		1
#	define FALSE	0
#endif

#ifdef MINDY_DEBUGGER
	#include "std-c.h"
	#include "mindy.h"
	#include "gc.h"
	#include "bool.h"
	#include "class.h"
	#include "str.h"
	#include "thread.h"
	#include "vec.h"
#endif

// prototypes

int MacOpen( const char *path, int oflag, int mode );
int MacClose( int fd );
int MacCreat(const char *path, mode_t mode);
int MacRead(int fd, char *buf, int count);
int MacWrite(int fd, const char *buf, int count);
OSErr ReallyWrite( int fd, Ptr buffer, long * amount );
int FindUnusedFileStatus( void );
long OpenApplicationAsFile( void );
Boolean IsDUFile( Str255 name );
void FileError( int err );


// static prototypes

static Boolean FileIsText( int fd );
static void n2r( char * buffer, int count);
static void r2n( char * buffer, int count);

// public globals

// File status indicator array
// Must be same size as maximum number of open fds on Mac

// 0 is text as \r must be \n . 1 and 2 are binary as they expect \n

typedef struct
{
	long 	fileNum;
	Boolean	isText;
} FileStatus;

FileStatus __fileIsText[ MAX_FILES ] =
{
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, 
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
	{ -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE }, { -1, FALSE },
};


// static globals

static char * conversionBuffer = NULL;

static Boolean gD2c = FALSE;


// functions

// open
// Opens a file and sets the binary flag
// Ignores the UN*X mode param

int MacOpen( const char *path, int flags, int mode )
{
	OSErr err;
	FSSpec fileSpec;
	int fileStatusRecord;
	short fd;
	char permission;
	
	// Open data fork as file?
	if( strcmp( path, "The Application File            " ) == 0 )	// 0 = equality
	{
		return OpenApplicationAsFile();
	}
	
	// Make FSSpec
	err = __path2fss( path, &fileSpec );
	
	// Set flags
	if( flags & O_RDONLY )		
		permission = fsRdPerm; 		
	else if( (flags & O_WRONLY) || (flags & O_RDWR ) )		
		permission = fsRdWrPerm; 
	else
		permission = fsCurPerm;	

	// open data fork
	err = FSpOpenDF( &fileSpec, permission, &fd);
	
	// If there's an error
	if (err) 
	{
		// File not found, but might be creatable
		if (err == fnfErr && flags & O_CREAT) 
		{
			err = FSpCreate( &fileSpec, 'MPS ', 'TEXT', smSystemScript );
			//if (!err)	//-
			//	err = FSpTouchFolder( &fileSpec );
			if (!err || err == dupFNErr )
				err = FSpOpenDF( &fileSpec, permission, &fd );
		}
		else
		{
			fd =  -1;
			goto finally;
		}
	}
	else if (flags & O_EXCL) 
	{
		FSClose(fd);
		fd = -1;
		goto finally;
	}
		
	// Truncate?	
	if( flags & O_TRUNC )
		SetEOF( fd, 0 );
	
	
	// finally
	
	finally:
		
	if( fd > 0 )									// If opened ok
	{
		Boolean b; //-
		fileStatusRecord = FindUnusedFileStatus();
		__fileIsText[ fileStatusRecord ].fileNum = fd;
		// hack for .du files
		if( gD2c && IsDUFile( fileSpec.name ) )
			__fileIsText[ fileStatusRecord ].isText = FALSE;
		else
			__fileIsText[ fileStatusRecord ].isText = !(flags & O_BINARY );	// Set flag
		
		b = __fileIsText[ fileStatusRecord ].isText;
	}
	else
	{	
		fd = -1;
	}
	

	FileError( err );
	return fd;										// Return the fd
}

// MacClose
// Closes the file and deallocates the file status

int MacClose( int fd )
{
	int 	fileNum = 0;
	short 	vRefNum;
	long 	filePos;
	OSErr 	err;
	
	// Reset the data structure
	while( fileNum < MAX_FILES )
	{
		if( __fileIsText[ fileNum ].fileNum == fd )
		{
			__fileIsText[ fileNum ].fileNum = -1;
		}
		fileNum++;
	}
	
	// If it's the console, return OK
	if ( fd < 3 && fd >= 0 )
	{
		return 0;
	}
	
	// Truncate file to current position
	/*err = GetFPos( fd, &filePos );
	if( err == noErr )
		err = SetEOF( fd, filePos );*/
		
	// Close the file and flush the volume to make sure EOF gets written	
	GetVRefNum( fd, &vRefNum );
	err = FSClose( fd );
	FlushVol( NULL, vRefNum );
	
	// Return some sort of error code
	FileError( err );
	return (err == noErr ? 0 : -1 );
}

// creat
// Creates and opens a file
// Sets the binary flag

int MacCreat(const char *path, mode_t mode)
{
	int		openMode = O_WRONLY | O_CREAT | O_TRUNC;
	int 	fd;
	
	if (mode & O_BINARY)
		openMode |= O_BINARY;

	fd = MacOpen( path, openMode, 0);
	
	return fd;
}


// read
// Translates newlines if file is text

int MacRead(int fd, char *buf, int count)
{
	IOParam			param;
	OSErr			theError;
	int				readCount;
	
	if( conversionBuffer == NULL )
	{
		conversionBuffer = NewPtr( WRITE_BUFFER_SIZE );
		if( conversionBuffer == NULL || MemError() )
		{
			return -1;
		}
	}

	if ((fd == 0)) 												// If it's 0/stdin
	{
		if (InstallConsole(fd) == 0)							// Read from the console 
		{
			__console_exit = RemoveConsole;
			fflush(stdout);
			readCount =  ReadCharsFromConsole((char *)buf, count);
			r2n( buf, readCount );			// It's text, so convert
			return readCount;
		} 
		else
			return -1;

	}

	readCount = count;
	
	theError = FSRead( fd, &readCount, buf );

	if (theError != noErr && theError != eofErr)				// Unexpected theErrorors?
	{
		FileError( theError );										// Set the errno
		readCount = -1;											// Return the error or count
	}	
	else														// Otherwise
	{
		if( FileIsText( fd ) )									// If the file's text
			r2n( buf, readCount );								// Convert it
	}
	
	return readCount;											// Return the count
}

// write
// Converts newlines if file is text

int MacWrite(int fd, const char *buf, int count)
{
	ParamBlockRec	param;
	OSErr			theError;
	int 			totalWritten = 0;							// How much of the buffer is left
	long			blockSize;
	short			vRef;

	if( conversionBuffer == NULL )
	{
		conversionBuffer = NewPtr( WRITE_BUFFER_SIZE );
		if( conversionBuffer == NULL || MemError() )
		{
			return -1;
		}
	}

	if ((fd == 1) || (fd == 2)) 						// If it's 1/stdout or 2/stdtheError
	{
		if (InstallConsole(fd) == 0) 
		{
			__console_exit = RemoveConsole;
			fflush(stdin);
			
			while( totalWritten < count )							// While there's more to write
			{
				if( totalWritten + WRITE_BUFFER_SIZE < count )		// If there's a whole buffer's worth
					blockSize = WRITE_BUFFER_SIZE;					// Set the block size to the buffer size
				else												// Otherwise
					blockSize = count - totalWritten;				// Set the exact size
				BlockMove( &( buf[ totalWritten ]), conversionBuffer, blockSize ); // Copy the block to the buffer
				n2r( conversionBuffer, blockSize );
				WriteCharsToConsole((char *)conversionBuffer, blockSize);
				
				totalWritten += blockSize;
			}
			
		} 
		else
			count =  -1;
		
		return count;											// Naughty but what WriteChars.. does anyway
	}
	
	if( FileIsText( fd ) )										// If it's a text file it needs translation
	{
		while( totalWritten < count )							// While there's more to write
		{
			if( count - totalWritten > WRITE_BUFFER_SIZE )		// If there's a whole buffer's worth
				blockSize = WRITE_BUFFER_SIZE;					// Set the block size to the buffer size
			else												// Otherwise
				blockSize = count - totalWritten;				// Set the exact size
			BlockMove( &( buf[ totalWritten ]), conversionBuffer, blockSize ); // Copy the block to the buffer
			n2r( conversionBuffer, blockSize );					// Translate newlines
			
			theError = ReallyWrite( fd, conversionBuffer, &blockSize );

			if (theError != noErr)								// If there's an error
			{
				FileError( fd );
				fd = -1;								// Set the errno
				break;											// And don't try again!
			}
			
			totalWritten += blockSize;					// Move along
		}
	}
	else														// Otherwise, it's a binary file. Don't translate
	{
		totalWritten = count;
		theError = ReallyWrite( fd, buf, &totalWritten );
	}
	finally:
	return (theError == noErr ? totalWritten : -1);			// Return the result
}

// ReallyWrite

OSErr ReallyWrite( int fd, Ptr buffer, long * amount )
{
	short vRef;
	OSErr theError;
	
	theError = FSWrite( fd, amount, buffer );
	if (theError != noErr)									// If there's an error
	{
		return theError;
	}
	
	GetVRefNum( fd, &vRef );			// get file vRefNum
	theError = FlushVol( NULL, vRef );
	if (theError != noErr)									// If there's an error
	{
		return theError;
	}

	return theError;
}

// select
// Always returns 1
// GUSI always returns true for readable or writable, so I hope this is OK

int select( int nfds, struct fd_set *readfds, struct fd_set *write_fds, struct fd_set *except_fds, struct timeval *timeout)
{
	return 1;
}

// FindUnusedFileStatus
// Get an unused file status record

int FindUnusedFileStatus( void )
{
	int fileNum = 0;
	
	while( fileNum < MAX_FILES )
	{
		if( __fileIsText[ fileNum ].fileNum == -1 )
			break;
		fileNum++;
	}
	
	return fileNum;
}

// FileIsText
// Checks whether a file is text

Boolean FileIsText( int fd )
{
	int fileNum = 0;
	
	while( fileNum < MAX_FILES )
	{
		if( __fileIsText[ fileNum ].fileNum == fd )
			return __fileIsText[ fileNum ].isText;
		fileNum++;
	}
	
	return TRUE;
}


// n2r
// Convert newlines to returns

void n2r( char * buffer, int count)
{                                                                                    
   	while (count--)                                    
	{      
		switch( *buffer )
		{                                              
			case '\r':
				*buffer = '\n';  
				break;                  
			case '\n':
				*buffer = '\r'; 
		}                    
		buffer++;                                           
	}                                     
}

// r2n
// Convert returns to newlines

void r2n( char * buffer, int count)  
{                                                      
	while (count--)                                    
	{      
		switch( *buffer )
		{                                              
			case '\n':
				*buffer = '\r';  
				break;                  
			case '\r':
				*buffer = '\n'; 
		}                    
		buffer++;                                           
	}                                                       
}

// OpenApplicationAsFile
// Opens the data fork of the app.
// On PowerMac, the app MUST have its code in the resource fork!

long OpenApplicationAsFile( void )
{
	OSErr 				err;
	short 				fd;
	FSSpec				spec;
	ProcessSerialNumber	serial;
	ProcessInfoRec 		info;
	
	serial.highLongOfPSN 	= 0;
	serial.lowLongOfPSN 	= kCurrentProcess;					
	
	info.processInfoLength 	= sizeof( ProcessInfoRec );
	info.processName		= NULL;
	info.processAppSpec 	= &spec;
	
	// Get information from the system about this program
	err = GetProcessInformation( &serial, &info );
	if( err != noErr )
	{
		return -1;
	}

	err = FSpOpenDF(	&spec, fsCurPerm, &fd);
	if( err != noErr )
	{
		return -1;
	}
	
	if( (spec.name[0] == 3) && (spec.name[1] == 'd') && (spec.name[2] == '2') && (spec.name[3] == 'c') )
		gD2c = TRUE;
	else
		gD2c = FALSE;
	
	return (short)fd;
}

// IsDUFile

Boolean IsDUFile( Str255 name )
{
	int end = name[ 0 ];
	
	if( end < 4 )
		return FALSE;

	if(	name[ end-2 ] != '.' )
		return FALSE;	
	if(	name[ end-1 ] != 'd' )
		return FALSE;
	if(	name[ end ] != 'u' )
		return FALSE;

	return TRUE;
}

// Error translation
// From GUSI

void FileError( int err )
{
	switch (err) {
	case noErr:
		errno = 0;
		break;
	case bdNamErr:
		errno = (ENAMETOOLONG);
		break;
	case afpObjectTypeErr:
		errno = (ENOTDIR);
		break;
	case fnfErr:
	case dirNFErr:
		errno = (ENOENT);
		break;
	case dupFNErr:
		errno = (EEXIST);
		break;
	case dirFulErr:
	case dskFulErr:
		errno = (ENOSPC);
		break;
	/*case fBsyErr:
		errno = (EBUSY);
		break;*/
	case tmfoErr:
		errno =  (ENFILE);
		break;
	case fLckdErr:
	case permErr:
	case afpAccessDenied:
		errno =  (EACCES);
		break;
	case wPrErr:
	case vLckdErr:
		errno =  (EROFS);
		break;
	case badMovErr:
		errno =  (EINVAL);
		break;
	/*case diffVolErr:
		errno =  (EXDEV);
		break;*/
	default:
		errno =  (EINVAL);
	}
}
