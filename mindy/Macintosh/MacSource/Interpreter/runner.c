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


// defines

#define INITIAL_SEARCH_PATH 	":\t:libraries\t"
#define FOLDER_SEARCH_PATH	"\p:Mindy:libraries\t"	// \t is the separator


// Prototypes

extern void main( int argc, char ** argv );

static void BuildLibPath( void );
static void FindFolderPath( Handle appendPathTo, int folderType, Str255 pathInFolder );

static OSErr	GetFullPath(short vRefNum,
					long dirID,
					StringPtr name,
					short *fullPathLength,
					Handle *fullPath);

static OSErr	FSpGetFullPath(const FSSpec *spec,
					   short *fullPathLength,
					   Handle *fullPath);

void DummyMain( void );


// Globals

extern char * LIBDIR;


// Functions

// DummyMain

void DummyMain( void )
{
#ifdef SELF_RUNNER
	int argc = 3;
	char * argv[4] = { "Mindy", "-f", "The Application File            ", 0 };
#else
	int argc;
	char ** argv;
	argc = ccommand( &argv );
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
