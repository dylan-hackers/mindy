/*
	This is a minimal function to register as the code fragment main for the application.
	We just call the real main from the Mindy Shared Library we import,
	argc and argv can be rubbish because main calls ccommand to fill them anyway.
	ccommand initializes the toolbox.
	We need this because we need a main entry point declared before we resolve shared
	libraries.
*/

#include <string.h>

#include<console.h>

/*#define SELF_RUNNER	/* Comment in for self-runner. Use the MacTech MoveData util. */

#define SEARCH_PATH_BUFFER_SIZE	1024

extern void main( int argc, char ** argv );
void BuildLibPath( void );

OSErr	GetFullPath(short vRefNum,
					long dirID,
					StringPtr name,
					short *fullPathLength,
					Handle *fullPath);

OSErr	FSpGetFullPath(const FSSpec *spec,
					   short *fullPathLength,
					   Handle *fullPath);

extern char * LIBDIR;

void DummyMain( void );

void DummyMain( void )
{
#ifdef SELF_RUNNER
	int argc = 3;
	char * argv[4] = { "Mindy", "-f", "OPEN THE APPLICATION FILE AS THE BYTECODE FILE", 0 };
#else
	int argc;
	char ** argv;
	argc = ccommand( &argv );
#endif
	BuildLibPath();
	main( argc, argv );	/* Just call the real main in Mindy. */
}

void BuildLibPath( void )
{
	OSErr err;
	short vRefNum;
	long dirID;
	short pathLength;
	Handle path;

	LIBDIR = NewPtr( SEARCH_PATH_BUFFER_SIZE );
	
	strcpy( LIBDIR, "\t:libraries" );	// Put the local path
	
#ifdef SELF_RUNNER
	// Try to get the extensions folder
	err = FindFolder( kOnSystemDisk, kExtensionFolderType, FALSE, &vRefNum, &dirID );
	if( ! err )
	{		
		path = NewHandle( 512 );
	
		// Find the mindy:libraries dir and get its full path
		err = GetFullPath( vRefNum, dirID, "\p:mindy:libraries", &pathLength, &path );
		if( ! err )
		{
			strcat( LIBDIR, "\t" );
		
			(*path)[ pathLength ] = '\0';	// Make a C string
			
			HLock( path );
			strcat( LIBDIR, *path );
			HUnlock( path );
		}
		
		DisposeHandle( path );
	}
#endif
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
