// Convert all to Fsp versions
// Link in before anything else

// #includes

#include <stdio.h>
#include <string.h>

#include<MacTypes.h>
#include<CodeFragments.h>
#include<MixedMode.h>
#include<Processes.h>
#include<Files.h>

#include "mindy.h"
#include "thread.h"

// #defines

#define kLibraryDirName "\plibraries:"

// structs

struct load_info {
    char *name;
    int fd;
    unsigned char *buffer, *ptr, *end;
    obj_t *table, *table_end;
    int next_handle;
    boolean swap_bytes;
    boolean done;
    struct library *library;
    struct module *module;
    obj_t mtime;
    obj_t source_file;
};

// Enums

enum
{
	kLaunchDir,			// The application directory
	kShlbDir,			// The shared library directory
	kNumDirs			// The size of the array to hold these
};

// Prototypes

static OSErr ShlbInitializate( CFragInitBlockPtr params );
static void GetApplicationSearchDir( void );
static int MacRead( int fd, void *ptr, int bytes);
Boolean TryLoadLibrary( FSSpec spec, char * name );
Boolean TryLoadLibraryFile( FSSpec spec, char * name );

void MacMindyRun( FSSpec files, int numFiles, char ** argv );
void MacMindyMakeMain( void );
int MacMindyRunMain( void );
void MacMindyMainArguments( char ** argv );

extern void startup(struct thread *thread, int nargs);


// externs

extern obj_t obj_ObjectClass;
extern obj_t obj_False;
extern obj_t obj_Nil;

extern struct module module_BuiltinStuff;


// Globals

static FSSpec gSearchDirectories[ kNumDirs ];


// ShlbInitialize


static OSErr ShlbInitializate( CFragInitBlockPtr params )
{
	// Make sure it's a data fork lib
	if( params->fragLocator.where != kDataForkCFragLocator )
		return -43;
	else
	{
		// Copy in the file locator
		gSearchDirectories[ kShlbDir ] = *(params->fragLocator.u.onDisk.fileSpec);
		// Clear the application file name
		gSearchDirectories[ kShlbDir ].name[0] = 0;
	}
	return noErr;
}

// GetApplicationSearchDir

static void GetApplicationSearchDir( void )
{
	OSErr 				err;
	int 				i;
	ProcessSerialNumber	serial;
	ProcessInfoRec 		info;
	
	serial.highLongOfPSN 	= 0;
	serial.lowLongOfPSN 	= kCurrentProcess;					
	
	info.processInfoLength 	= sizeof( ProcessInfoRec );
	info.processName		= NULL;
	info.processAppSpec 	= &gSearchDirectories[ kLaunchDir ];
	
	// Get information from the system about this program
	err = GetProcessInformation( &serial, &info );
	if( err != noErr )
	{
		AlertFatal( "\pApplicationFile Failed!", "\pInvalid Process Serial Number!" );
	}
	
	// Clear the application file name
	gSearchDirectories[ kLaunchDir ].name[0] = 0;
}



// Called to read a few bytes
// Uses FSRead on info->FD, which is the fRefNum from an FspOpen call in load

static int safe_read(struct load_info *info, void *ptr, int bytes)
{
	int count = MacRead(info->fd, ptr, bytes);
    
	if ( count == 0 )
		error("premature EOF loading %s", make_byte_string(info->name));
		
    if ( count < 0)
		error("error loading %s: %s",
			make_byte_string(info->name),
			make_byte_string(strerror(count)));	// errno
    
    return count;
}

// Wraps an FSRead call in a un*x-like wrapper
// Probably an MSL routine to do this

static int MacRead( int fd, void *ptr, int bytes)
{
	// int count = read(info->fd, ptr, bytes);
    long count = bytes;						// This gets overwritten
    OSErr err;
    
    err = FSRead( fd, &count, ptr );	// Mac read
    
	if (err == -39)	// count == 0
	{
		return 0;
	}
		
    if( err != noErr )//if (count < 0)
    {
		return err;
	}
    
    return count;
}


// Called for -f
// Provide version that uses Fsp instead, and puts file ref number in info->fd
// WARNING: info->fd mustn't be called anywhere else

//void load(char *name)
void FspLoad( FSSpec * name )
{
	int fd;
    struct load_info *info;

    /*if (strcmp(name, "-") == 0)
    {
      //fd = 0;	// FD 0 is stdin
      fd = stdin;
    }
    else {*/
#if WIN32
      fd = open(name, O_RDONLY | O_BINARY, 0);
#else
      //fd = open(name, O_RDONLY/*, 0*/);
      OSErr err;
      err = FSOpenDF( name, fsCurPerm, &fd );
#endif
    //}
    if ( err != noErr ) // fd < 0
	error("Error loading %s: %s\n",
	      make_byte_string(name),
	      make_byte_string(strerror(err)));	// errno

    info = make_load_info(name, fd);

    while (1) {
	load_group(info);
	if (info->ptr == info->end) {
	    int count = MacRead(fd, info->buffer, 4096);	// BUFFER_SIZE
	    if (count < 0)
		error("error loading %s: %s",
		      make_byte_string(name),
		      make_byte_string(strerror(count)));	// errno
	    if (count == 0)	
			break;
	    info->ptr = info->buffer;
	    info->end = info->ptr + count;
	}
    }
    if (info->fd != 0)
      FSClose(info->fd);		// close
    free_load_info(info);
}

// called from varoius places in the runtime
// convert to build search path FSSpecs if called for the first time,
// Then to search via FSSpecs and call the FSSpec version of 

extern obj_t currently_loading;
extern obj_t was_loading;

void load_library(obj_t name)
{
	HFileInfo info;
	OSErr err;
	char * libName;
	char * dst = libName;
	char * src;
	
	for (src = sym_name(name); *src != '\0'; src++)
	if (isupper(*src))
	    *dst++ = tolower(*src);
	else
	    *dst++ = *src;
	
    was_loading = currently_loading;	// !!
    currently_loading = name;

    if( ! TryLoadLibrary( gSearchDirectories[ kShlbDir ], libName ) )
    {
		if( ! TryLoadLibrary( gSearchDirectories[ kLaunchDir ], libName ) )
		{
   			error("Can't find library %s", name);
   		}
   	}
}

// TryLoadLibrary
// Tries to load the library in the current dir of the spec,
// if that fails, it tries to load it in current dir:libraries:

Boolean TryLoadLibrary( FSSpec spec, char * name )
{
	char libName[ 256 ];
	
	if( TryLoadLibraryFile( spec, name ) )
		return true;
	else
	{		
		strcat( libName, kLibraryDirName );
		strcpy( libName, name );
		if( TryLoadLibraryFile( spec, libName ) )
			return true;
	}
	
	return false;
}

// TryLoadLibraryFile
// Tries to load the file in the given dir,
// first wih -lib.dbc, then with .dbc appended
// Uses FSpLoad

Boolean TryLoadLibraryFile( FSSpec * spec, char * name )
{
	FSSpec * lib = spec;
	
	strcpy( (char*)lib->name, name );
    c2pstr( lib->name );
    pstrcat( lib->name,  "-lib.dbc" );
    if ( FspAccess( lib ) == 0 ) 
    {
		FSpLoad( lib );
		currently_loading = was_loading;
		return true;
    }
    
    strcpy( (char*)lib->name, name );
    c2pstr( lib->name );
    pstrcat( lib->name, ".dbc" );
	if ( FspAccess( lib ) == 0 ) 
	{
		FSpLoad( lib );
		currently_loading = was_loading;
		return true;
    }
    
    return false;
}

int FSpAccess( FSSpec * spec )
{
	CInfoPBRec cipbr;
	HFileInfo *fpb = (HFileInfo*) &cipbr;
	DirInfo *dpb = (DirInfo*) &cipbr;
	short err;

	cipbr.hFileInfo.ioNamePtr = spec->name;
	
	if( spec->vRefNum == 0 )
		cipbr.hFileInfo.ioVRefNum = spec->parID;
	else
		cipbr.hFileInfo.ioVRefNum = spec->vRefNum;
		
	cipbr.hFileInfo.ioFDirIndex = 0;
	cipbr.hFileInfo.ioFVersNum = 0;
	
	err = PBGetCatInfo (&cipbr, 0);
	if (err != noErr)
	{
	  return -1;
	}
	
	return 0;

}


/*
	Based on, and must be kept in synch with, main in mindy.c
	Loads and runs a load of FSpSpec referenced .dbc files
	files: FSSpec array
	numFiles: number of specs
	argv: string array, null-terminated, of arguments
*/

extern char * exec_file_name;

static struct thread *mainThread;

void MacMindyRun( FSSpec files[], int numFiles, char ** argv )
{
	int i;

	MacMindyMakeMain();

	// Load files
	// Can we do this before we make main?
	        
	for( i = 0; i < numFiles ; i++)        
		FspLoad( &files[ i ] );

	// Get arguments
	
	MacMindyMainArguments( argv );
	
	MacMindyRunMain();

}


/*
	Make the main thread and the startup symbol.
	Call this before args, file, 
*/

void MacMindyMakeMain( void )
{
    exec_file_name = "mindy";

    init();

    mainThread = thread_create(symbol("main"));
    *mainThread->sp++ = make_raw_function("startup", obj_Nil,
				      TRUE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      startup);
}	



void MacMindyMainArguments( char ** argv )
{
    while (*argv != NULL)
        *mainThread->sp++ = make_byte_string(*argv++);

}
    
    
int MacMindyRunMain( void )
{    
    enum pause_reason reason;
    struct variable *var;
    
    finalize_modules();

    while (1) {
		thread_restart(mainThread);
	
		reason = do_stuff();
		if (reason != pause_NothingToRun)
		    invoke_debugger(reason);
	
		var = find_variable(module_BuiltinStuff, symbol("exit"),
				    FALSE, FALSE);
		if (var == NULL)
		    lose("main undefined?");
	
		mainThread = thread_create(symbol("exit"));
		*mainThread->sp++ = var->value;
    }
    return 0;
}

