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

#include <MacMemory.h>

#include <abort_exit.h>
#include <fcntl.mac.h>

char * LIBDIR;


// defines

#define WRITE_BUFFER_SIZE		128		// Size of the write translation buffer
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

// prototypes

int MacOpen( const char *path, int oflag, int mode );
int MacClose( int fd );
int MacCreat(const char *path, mode_t mode);
int MacRead(int fd, char *buf, int count);
int MacWrite(int fd, const char *buf, int count);
int FindUnusedFileStatus( void );
long OpenApplicationAsFile( void );

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

static char conversionBuffer[ WRITE_BUFFER_SIZE ];


// functions

// open
// Opens a file and sets the binary flag
// Ignores the UN*X mode param

int MacOpen( const char *path, int oflag, int mode )
{
	int fileStatusRecord;
	int fd;
	
	if( strcmp( path, "OPEN THE APPLICATION FILE AS THE BYTECODE FILE" ) == 0 )	// 0 = equality
	{
		return OpenApplicationAsFile();
	}
	
	/*if( oflag & O_CREAT )
		oflag = O_RDWR | O_CREAT;		// RDWR becasue stream regression buf wrute test fails otherwise*/
	
	fd = open( path, oflag );					// Call MSL open
	
	if( fd >= 0 )									// If opened ok
	{
		fileStatusRecord = FindUnusedFileStatus();
		__fileIsText[ fileStatusRecord ].fileNum = fd;
		__fileIsText[ fileStatusRecord ].isText = !(oflag & O_BINARY );	// Set flag
	}
	return fd;										// Return the fd
}

// MacClose
// Closes the file and deallocates the file status

int MacClose( int fd )
{
	int fileNum = 0;
	
	while( fileNum < MAX_FILES )
	{
		if( __fileIsText[ fileNum ].fileNum == fd )
		{
			__fileIsText[ fileNum ].fileNum = -1;
		}
		fileNum++;
	}
	
	return close( fd );
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
	param.ioRefNum = fd;										// fd is ioRefNum
	param.ioBuffer = (char *)buf;
	param.ioReqCount = count;
	param.ioPosMode = fsAtMark;
	param.ioPosOffset = NULL;

	theError = PBReadSync((ParmBlkPtr) &param);					// Try to read

	if (theError != noErr && theError != eofErr)				// Unexpected theErrorors?
	{
		errno = theError;										// Set the errno
		count = -1;												// Return the error or count
	}	
	else														// Otherwise
	{
		readCount = param.ioActCount;							// Set the count
		if( FileIsText( fd ) )								// If the file's text
			r2n( buf, readCount );								// Convert it
	}
	
	return readCount;											// Return the count
}

// write
// Converts newlines if file is text

int MacWrite(int fd, const char *buf, int count)
{
	IOParam			param;
	OSErr			theError;
	int 			totalWritten = 0;							// How much of the buffer is left
	int				blockSize;

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
			return count;											// Naughty but what WriteChars.. does anyway
		} 
		else
			return -1;
	}
	
	if( FileIsText( fd ) )										// If it's a text file it needs translation
	{
		while( totalWritten < count )							// While there's more to write
		{
			if( totalWritten + WRITE_BUFFER_SIZE < count )		// If there's a whole buffer's worth
				blockSize = WRITE_BUFFER_SIZE;					// Set the block size to the buffer size
			else												// Otherwise
				blockSize = count - totalWritten;				// Set the exact size
			BlockMove( &( buf[ totalWritten ]), conversionBuffer, blockSize ); // Copy the block to the buffer
			n2r( conversionBuffer, blockSize );					// Translate newlines
			param.ioRefNum = fd;								// ioRefNum is fd
			param.ioBuffer = (char *)conversionBuffer;			// Write from the translated buffer
			param.ioReqCount = blockSize;						// Write the amount in the buffer
			param.ioPosMode = fsAtMark;
			param.ioPosOffset = NULL;
			param.ioVRefNum = 0;

			theError = PBWriteSync((ParmBlkPtr) &param);		// Try to write

			if (theError != noErr)								// If there's an error
			{
				errno = theError;								// Set the errno
				break;											// And don't try again!
			}
			
			totalWritten += WRITE_BUFFER_SIZE;					// Move along
		}
	}
	else														// Otherwise, it's a binary file. Don't translate
	{
		param.ioRefNum = fd;									// ioRefNum is fd
		param.ioBuffer = (char *)buf;							// Write direct and untranslated
		param.ioReqCount = count;								// Write all in one go
		param.ioPosMode = fsAtMark;
		param.ioPosOffset = NULL;
		param.ioVRefNum = 0;

		theError = PBWriteSync((ParmBlkPtr) &param);			// Try to write

		if (theError != noErr)									// If there's an error
			errno = theError;									// Set the errno
	}
	return (theError == noErr ? param.ioActCount : -1);			// Return the result
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
	long 				fd;
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
	return fd;
}
