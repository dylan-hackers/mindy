/*
	This is a minimal function to register as the code fragment main for the application.
	We just call the real main from the Mindy Shared Library we import,
	argc and argv can be rubbish because main calls ccommand to fill them anyway.
	ccommand initializes the toolbox.
	We need this because we need a main entry point declared before we resolve shared
	libraries.
*/

// includes

#include <string.h>

#include<console.h>


// consts

// Arg array length
static const int MAX_ARGS = 32;	// Must be at least 2 longer than in ccommand.h

// Search path building strings
static const char * INITIAL_SEARCH_PATH  =	":\t:libraries\t";
static const Str255 FOLDER_SEARCH_PATH	= "\p:Mindy:libraries\t";	// \t is the separator

// Magic resource to check for for CLI for self runner
static const int kCLI_RESOURCE = 'CLI?'; 
static const short kCLI_RESOURCE_NUM = 128;

// arguments for self-runner
const char *		APPLICATION_NAME = "Mindy";
const char * 		APPLICATION_FILE_MAGIC_STRING = "The Application File            ";
static const char * 	_F = "-f";


// Prototypes

extern void main( int argc, char ** argv );

static void BuildLibPath( void );

static void FindFolderPath( Handle appendPathTo, int folderType, Str255 pathInFolder );

static OSErr GetFullPath(	short vRefNum,
					long dirID,
					StringPtr name,
					short *fullPathLength,
					Handle *fullPath);

static OSErr FSpGetFullPath(	const FSSpec *spec,
						short *fullPathLength,
						Handle *fullPath);
					   
static Boolean ShouldShowCommandLine( void );

static int SelfRunnerCCommand( char ***args );

static void InitializeToolbox( void );

void DummyMain( void );


// Globals

extern char * LIBDIR;
char * gargv[ MAX_ARGS ];
char gargv0[256];


// Functions

// DummyMain

void DummyMain( void )
{
	char ** argv;
	int argc;
	
	InitializeToolbox();
	
#ifdef SELF_RUNNER
	if( ShouldShowCommandLine() )
	{
		// Get command line and extend to include magic bytecode runner string
		argc = SelfRunnerCCommand( &argv );
	}
	else
	{
		// No command line, just run
		argc = 3;
		gargv[ 0 ] = APPLICATION_NAME;
		gargv[ 1 ] = _F;
		gargv[ 2 ] = APPLICATION_FILE_MAGIC_STRING;
		gargv[ 3 ] = NULL;
		
		argv = gargv;
	}
#else
	argc = ccommand( &argv );	// Initializes Toolbox
#endif

	BuildLibPath();
	main( argc, argv );	/* Just call the real main in Mindy. */
}


// BuildLibPath

void BuildLibPath( void )
{
	Handle searchPath;
	
	// make the search path handle 
	// and fill it with the initial search path
	// use strlen neat so as to chop off trailing null!
	searchPath = NewHandle( strlen( INITIAL_SEARCH_PATH ) );
	if( (searchPath != NULL) && ( ResError() == noErr ) )
	{
		HLock( searchPath );
			BlockMove( INITIAL_SEARCH_PATH, *searchPath, strlen( INITIAL_SEARCH_PATH  ) );
		HUnlock( searchPath );
			
		// Try to get the application support folder
		FindFolderPath( searchPath, kApplicationSupportFolderType, FOLDER_SEARCH_PATH );
		
		// Try to get the extensions folder
		FindFolderPath( searchPath, kExtensionFolderType, FOLDER_SEARCH_PATH  );
		
		// lock it out of the way, 
		// null terminate it, overwriting the last \t, making a c string
		// and set LIBDIR to it
		HLockHi( searchPath );
		(*searchPath)[ GetHandleSize( searchPath ) -1 ] = '\0';
		LIBDIR = *searchPath;
	}
}


// FindFolderPath
// appendPathTo must be big enough

void FindFolderPath( Handle appendPathTo, int folderType, Str255  pathInFolder )
{
	OSErr err;
	short vRefNum;
	long dirID;
	long appendPathToLength;
	short pathLength;
	Handle path;
		
	// Try to get the application support folder
	err = FindFolder( kOnSystemDisk, folderType, FALSE, &vRefNum, &dirID );
	if( ! err )
	{		
		// Find the mindy:libraries dir and get its full path
		// Resize the destination handle to fit,
		// then copy in the path and the sub-path
		err = GetFullPath( vRefNum, dirID, "\p", &pathLength, &path );
		if( ! err )
		{
			appendPathToLength = GetHandleSize( appendPathTo );
			SetHandleSize( appendPathTo,  appendPathToLength+ pathLength + pathInFolder[ 0 ] );
			err = MemError();
			if( ! err )
			{
				HLock( appendPathTo );
				HLock( path );
					BlockMove( *path, &((*appendPathTo)[ appendPathToLength ]),  pathLength );
					BlockMove( &(pathInFolder[ 1 ]), &((*appendPathTo)[ appendPathToLength + pathLength ])  ,  pathInFolder[ 0 ] );
				HUnlock( path );
				HLock( appendPathTo );
			}
		}
	}
	DisposeHandle( path );
}


// From MoreFiles

OSErr GetFullPath(	short vRefNum,
					long dirID,
					StringPtr name,
					short *fullPathLength,
					Handle *fullPath)
{
	OSErr		result;
	FSSpec		spec;
	
	result = FSMakeFSSpec(vRefNum, dirID, name, &spec);
	if ( result == noErr )
	{
		result = FSpGetFullPath(&spec, fullPathLength, fullPath);
	}
	
	return ( result );
}


OSErr FSpGetFullPath(	const FSSpec *spec,
					    short *fullPathLength,
					    Handle *fullPath)
{
	OSErr		result;
	FSSpec		tempSpec;
	CInfoPBRec	pb;
	
	/* Make a copy of the input FSSpec that can be modified */
	BlockMoveData(spec, &tempSpec, sizeof(FSSpec));
	
	if ( tempSpec.parID == fsRtParID )
	{
		/* The object is a volume */
		
		/* Add a colon to make it a full pathname */
		++tempSpec.name[0];
		tempSpec.name[tempSpec.name[0]] = ':';
		
		/* We're done */
		result = PtrToHand(&tempSpec.name[1], fullPath, tempSpec.name[0]);
	}
	else
	{
		/* The object isn't a volume */
		
		/* Put the object name in first */
		result = PtrToHand(&tempSpec.name[1], fullPath, tempSpec.name[0]);
		if ( result == noErr )
		{
			/* Get the ancestor directory names */
			pb.dirInfo.ioNamePtr = tempSpec.name;
			pb.dirInfo.ioVRefNum = tempSpec.vRefNum;
			pb.dirInfo.ioDrParID = tempSpec.parID;
			do	/* loop until we have an error or find the root directory */
			{
				pb.dirInfo.ioFDirIndex = -1;
				pb.dirInfo.ioDrDirID = pb.dirInfo.ioDrParID;
				result = PBGetCatInfoSync(&pb);
				if ( result == noErr )
				{
					/* Append colon to directory name */
					++tempSpec.name[0];
					tempSpec.name[tempSpec.name[0]] = ':';
					
					/* Add directory name to beginning of fullPath */
					(void) Munger(*fullPath, 0, NULL, 0, &tempSpec.name[1], tempSpec.name[0]);
					result = MemError();
				}
			} while ( (result == noErr) && (pb.dirInfo.ioDrDirID != fsRtDirID) );
		}
	}
	if ( result == noErr )
	{
		/* Return the length */
		*fullPathLength = GetHandleSize(*fullPath);
	}
	else
	{
		/* Dispose of the handle and return NULL and zero length */
		DisposeHandle(*fullPath);
		*fullPath = NULL;
		*fullPathLength = 0;
	}
	
	return ( result );
}


// ShouldShowCommandLine
// Looks for a CLI? resource 128 . If it;s there, show CLI, if not, don't
// returns - TRUE : show CLI	FALSE: don't
Boolean ShouldShowCommandLine( void )
{
	EventRecord event;
	Boolean result = FALSE;
	
	// try to get the magic resource
	Handle h = Get1Resource( kCLI_RESOURCE, kCLI_RESOURCE_NUM );
	// If we succeed
	if( (h != NULL) && (! ResError()) )
	{
		// Set the flag and release the resource
		result = TRUE;
		ReleaseResource( h );
	}
	
	// Get a null event and check for the command key
	OSEventAvail( 0, &event );
	if( event.modifiers & cmdKey )
	{
		result = TRUE;
	}
		
		
	// return the result
	return result;
}


// SelfRunnerCCommand
// Build argc and argv to include the application bytecode file as the first -f option
// Uses the strings from ccommand proper, copying pointers to them into a new array
// IN:	args - The char[] pointer to fill with a reference to the argument array
// OUT: The length of the argument list (excluding the terminating null)
int SelfRunnerCCommand( char ***args )
{
	int i;
	
	// Call ccommand to get argc and argv
	// item 0 of argv is the application name
	int argc = ccommand( args );
	
	// Copy over everything from ccommand's argument list except the fileName
	// We do this before we do anything to argc
	// Note that our argv must be at least 3 longer than ccommand's args
	// We copy argc+1 to copy the terminating NULL as well
	for( i = 1; i < argc+1; i++ )
		gargv[ i + 2 ] = (*args)[ i ];
	
	// Now prepend some stuff to the argument list	
	// First item is the application name
	strcpy( gargv0, (*args)[ 0 ] );
	gargv[ 0 ] = gargv0;	
	// Second the -f switch, note that we've added it
	gargv[ 1 ] = _F;
	argc++;
	// Third is the magic string for the application bytecode file, note that we've added it
	gargv[ 2 ] = APPLICATION_FILE_MAGIC_STRING;
	argc++;
	
	// Replace the argument list with our extended one
	*args = gargv;
	
	// Return the length of the new argument list
	return argc;
}

//	InitializeToolbox
//	Fossilized but useful

void InitializeToolbox( void )
{
	MaxApplZone();				// Set the heap zone size to its limit
	
	MoreMasters();				// Get 3x32=96 more master pointers
	MoreMasters();				// In addition to our free starting 32
	MoreMasters();				// This gives us 128 free handles, which should be plenty

	InitGraf(&qd.thePort);		// Initialize QuickDraw
	InitFonts();				// Initialize the Font Manager
	InitWindows();				// Initialize the Window Manager
	InitMenus();				// Initialize theMenu Manager
	TEInit();					// Initialize TextEdit
	InitDialogs(nil);			// Initialize the Dialog Manager

	//	Flush out mouse and keyboard events.  This prevents some things like clicking through
	//	to the Finder while the app is launching, etc.
	FlushEvents( mDownMask | keyDownMask | autoKeyMask, 0 );
	
	InitCursor();				// Initialize the Cursor to an arrow 
}