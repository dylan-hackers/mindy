// MactextFileIO.c
// Replaces MSL's read/write routines with ones that know about text/binary files
// Hack 'open' or your code to set the flags correctly, binary is default( unlike ansi stdio )
// Link in before MSL


// includes

#include <console.h>
#include <sys/errno.h>
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

#include <fcntl.h>
#include <stat.h>

#include <errno.h>

#include <assert.h>

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

#undef write

//#ifdef __D2C
//	#define ALLOCATE_CONVERSION_BUFFER(a)	(char *)GC_malloc_uncollectable(a)
//#else
//	#define ALLOCATE_CONVERSION_BUFFER(a) 	NewPtr(a)
//#endif

// prototypes

int MacOpen( const char *path, int oflag, int mode );
int MacClose( int fd );
int MacCreat(const char *path, mode_t mode);
int MacRead(int fd, char *buf, int count);
int MacWrite(int fd, const char *buf, int count);
short OpenApplicationAsFile( void );
Boolean IsDUFile( const char * name );
Boolean IsDBCFile( const char * name );
void SetFileText( const char * path, int fd, int flags );
void FileError( int err );

// static prototypes

static Boolean FileIsText( int fd );
static void n2r( char * buffer, int count);
static void r2n( char * buffer, int count);

// public globals

// File status indicator array
// Must be same size as maximum number of open fds on Mac

// 0 is text as \r must be \n . 1 and 2 are binary as they expect \n

int __fileIsBinary[ MAX_FILES ] = {}; // Init to 0's


// static globals

static char * conversionBuffer = NULL;

static Boolean gD2c =
						#ifdef __D2C
							TRUE;
						#else
							FALSE;
						#endif


// functions

// open
// Opens a file and sets the binary flag
// Ignores the UN*X mode param

int MacOpen( const char *path, int flags, int mode )
{
	int fd;
	
	if( strcmp( path, "The Application File            " ) )	// 0 == match, so check for fail
	{
		if( IsDUFile( path ) )
		{
			flags |= O_BINARY;
		}
		
		fd = open( path, flags );
	}
	else
	{
		fd = (int)OpenApplicationAsFile();
	}
	
	FileError( fd );
	if( fd >= 0 )
		SetFileText( path, fd, flags );
	
	return fd;
}

// MacClose
// Closes the file and deallocates the file status

int MacClose( int fd )
{
	int fileNum = 0;
	
	assert( fd > 2 );	// Closing the std streams would be bad
	
	for( fileNum = 0; fileNum < MAX_FILES; fileNum++ )
	{
		if( __fileIsBinary[ fileNum ] == fd )
		{
			__fileIsBinary[ fileNum ] = 0;
		}
	}

	return close( fd );
}

// creat
// Creates and opens a file
// Sets the binary flag

int MacCreat(const char *path, mode_t mode)
{
    int fd = creat( path, mode );
    FileError( fd );
    if( fd >= 0 )
		SetFileText( path, fd, mode );
	
	return fd;
}

static int RedirectedFileDescriptor(int fd)
{
	switch (fd)
	{
		case 0:	return stdin->handle;
		case 1:	return stdout->handle;
		case 2:	return stderr->handle;
		default: return fd;
	}
	
}


// read
// Translates newlines if file is text

int MacRead(int fd, char *buf, int count)
{
	int result;
	Boolean isText;
	
	assert( fd >= 0 );
	assert( buf != NULL );
	assert(  count > 0 );
	
	result = read( RedirectedFileDescriptor(fd), buf, count );
	FileError( result );
	isText = FileIsText(fd);
	
	assert( (isText == FALSE) || (isText == TRUE) );
	
	if( isText && (result > 0) )
	{
		r2n( buf, count );
		return result;
	}
	
	return result;
}

// write
// Converts newlines if file is text

int MacWrite(int fd, const char *buf, int count)
{
	int result;
	Boolean isText;
	
	assert( fd >= 0 );
	assert( buf != NULL );
	assert(  count > 0 );
	
	isText = FileIsText(fd);
	
	assert( (isText == FALSE) || (isText == TRUE) );
	
	if( isText )
	{
		n2r( (char*)buf, count );
		result = write( RedirectedFileDescriptor(fd), buf, count );
		r2n( (char*)buf, count );	// Must be a better way
	}
	else
	{
		result = write( RedirectedFileDescriptor(fd), buf, count );
	}

	FileError( result );
	
	return result;
}

// select
// Always returns 1
// GUSI always returns true for readable or writable, so I hope this is OK

int select( int nfds, struct fd_set *readfds, struct fd_set *write_fds, struct fd_set *except_fds, struct timeval *timeout)
{
	return 1;
}

// FileIsText
// Checks whether a file is text

Boolean FileIsText( int fd )
{
	int i;
	
	assert( fd >= 0 );
	
	if( fd < 3 )		/* If it's one of the standard streams */
	{
		return TRUE;	/* It's text */
	}
	
	for( i = 0; i < MAX_FILES; i++ )
	{
		if( __fileIsBinary[ i ] == fd )
		{
			return FALSE;
		}
	}
	
	return TRUE;
}


// n2r
// Convert newlines to returns

void n2r( char * buffer, int count)
{                                  
	assert( buffer != 0 );
	assert( count > 0 );
                                                  
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
	assert( buffer != 0 );
	assert( count > 0 );    
	               
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

short OpenApplicationAsFile( void )
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

	err = FSpOpenDF( &spec, fsCurPerm, &fd );
	if( err != noErr )
	{
		return -1;
	}
	
	if( (spec.name[0] == 3) && (spec.name[1] == 'd') && (spec.name[2] == '2') && (spec.name[3] == 'c') )
		gD2c = TRUE;
	else
		gD2c = FALSE;
	
	return fd;
}

// IsDUFile

Boolean IsDUFile( const char * name )
{
	int end = strlen( name );
	
	if( end < 4 )
		return FALSE;

	if(	name[ end-3 ] != '.' )
		return FALSE;	
	if(	name[ end-2 ] != 'd' )
		return FALSE;
	if(	name[ end-1 ] != 'u' )
		return FALSE;

	return TRUE;
}

// IsDBCFile

Boolean IsDBCFile( const char * name )
{
	int end = strlen( name );
	
	if( end < 5 )
		return FALSE;

	if(	name[ end-4 ] != '.' )
		return FALSE;	
	if(	name[ end-3 ] != 'd' )
		return FALSE;
	if(	name[ end-2 ] != 'b' )
		return FALSE;
	if(	name[ end-1 ] != 'c' )
		return FALSE;

	return TRUE;
}


void SetFileText( const char * path, int fd, int flags )
{
	int index = 0;

	assert( path != 0 );
	assert( fd >= 0 );

    if( fd >= 0 )									// If opened ok
	{
		for( index = 0; index < MAX_FILES; index++ )
		{
			if( __fileIsBinary[ index ] == 0 )
				break;
		}
		
		if( index < MAX_FILES )
		{
			// hack for compiler binary file types
			if(/*IsDBCFile( path ) ||*/ IsDUFile( path ) )
				__fileIsBinary[ index ] = fd;
			else
			{
				if( flags & O_BINARY )
					__fileIsBinary[ index ] = fd;	// Set flag
		    }
		}	
	}
}

// Error translation
// From GUSI

void FileError( int err )
{
	int dummy;
	
	if( err == -1 )
	{
		switch (errno) 
		{
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
				errno = (EINVAL);
		}
	}
}
