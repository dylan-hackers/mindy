// makeSelfRunner.c


// includes

#include <Files.h>
#include <Finder.h>
#include <MacTypes.h>
#include <Resources.h>

#include "makeSelfRunner.h"


// defines

#define kCodeFragmentType	'cfrg'
#define kCodeResourceType	'PCOD'
#define kSizeType			'SIZE'

#define kCodeResourceNum	0	
#define kCodeFragmentNum	128
#define kSizeNumber			0


// typedefs

// Size resource struct
// Needs to be 68k aligned as resource manager is

#if PRAGMA_STRUCT_ALIGN
	#pragma options align=mac68k
#elif PRAGMA_STRUCT_PACKPUSH
	#pragma pack(push, 2)
#elif PRAGMA_STRUCT_PACK
	#pragma pack(2)
#endif
	typedef struct
	{
		UInt16			flags;					/* size flags */
		/* Memory sizes are in bytes */
		UInt32			preferredSize;			/* preferred mem size	*/
		UInt32			minumumSize;			/* minimum mem size		*/
	}
	SizeResource;
#if PRAGMA_STRUCT_ALIGN
	#pragma options align=reset
#elif PRAGMA_STRUCT_PACKPUSH
	#pragma pack(pop)
#elif PRAGMA_STRUCT_PACK
	#pragma pack()
#endif


// Prototypes

static OSErr StartResourceFile( FSSpec * file, short * refnum, int creator, int type );
static OSErr FetchResource( Handle * handle, int type, short number );
static OSErr MakeResource( Handle * handle, int size, int type, short id );
static OSErr CopyHandle( Handle * to, Handle from, int type, short number );


// Functions

// MakeSelfRunner
// Adds the code resources, sets the file type, and sets the 'Size' resource
// Sizes are in K

OSErr MakeSelfRunner( FSSpec * file, int creator, int type, short sizeFlags, int minimumPartition, int preferredPartition )
{
	OSErr err;
	
	// Add the ppc code and cfrg resource
	err = AddRunnerResources( file, creator, type );
	if( err != noErr )
		goto finally;
		
	// Set filethe creator and type
	err = SetFinderInfo( file, creator, type );
	if( err != noErr )
		goto finally;
		
	// Add the ppc code and cfrg resource
	err = SetSizeResource( file, sizeFlags, minimumPartition, preferredPartition );
	if( err != noErr )
		goto finally;
	
	
	// finally
	finally:
	
	return err;
}

// AddRunnerResources
// Opens, or creates resource fork using givenm params
// Copies the needed resource to the given file,
// Must have access to the PCOD 0 and cfrg 128 routines in the resource chain

OSErr AddRunnerResources( FSSpec * file, int creator, short type )
{
	OSErr err;
	short oldResFile;
	short refnum;
	Handle 	pcod = NULL,
			cfrg = NULL,
			pcodCopy = NULL,
			cfrgCopy = NULL;
			
	// Get the cfrg and the code resource for the self-runner
	err =  FetchResource( &pcod, kCodeResourceType, kCodeResourceNum );
	if( err != noErr )
		goto finally;
	err = FetchResource( &cfrg, kCodeFragmentType, kCodeFragmentNum );
	if( err != noErr )
		goto finally;
			
	// Save the old resource file
	oldResFile = CurResFile();
	
	// Create the resource file, or leave if already created
	FSpCreateResFile( file, creator, type, smSystemScript );
	if( ((err = ResError()) != noErr) && (err != -48) )	// -48 = duplicate filename (rename)
		goto finally;
		
	// Open the resource file	
	refnum = FSpOpenResFile( file, fsRdWrPerm );
	err = ResError();
	if( err != noErr )
		goto finally;
	
	// Set as current file
	UseResFile( refnum );
	
	// Copy the runner resources over
	err = CopyHandle( &pcodCopy, pcod, kCodeResourceType, kCodeResourceNum  );
	if( err != noErr )
		goto finally;
	err = CopyHandle( &cfrgCopy, cfrg, kCodeFragmentType, 0  );
	if( err != noErr )
		goto finally;
	
	// Write ENTIRE RESOURCE MAP to disk
	UpdateResFile( refnum );
	
	// finally
	finally:
	
	// Restore the old resource file
	UseResFile( oldResFile );
	
	// Release resources we used
	if( pcod != NULL )
		ReleaseResource( pcod );
	if( cfrg != NULL )
		ReleaseResource( cfrg );
	if( pcodCopy != NULL )
		ReleaseResource( pcodCopy );
	if( cfrgCopy != NULL )
		ReleaseResource( cfrgCopy );
	
	return err;
}


// SetFinderInfo
// Set the file's creator and type

OSErr SetFinderInfo( FSSpec * file, int creator, int type )
{
	OSErr err;
	FInfo info;
	
	// Get FInfo
	err = FSpGetFInfo( file, &info );
	if( err )
		goto finally;
		
	// Set creator and type
	info.fdCreator = creator;
	info.fdType = type;


	// Set FInfo
	err = FSpSetFInfo( file, &info );
	if( err )
		goto finally;
	
	// finally
	
	finally:
	
	return err;
}

// SetSizeResource
// Adds a 'Size' resource to the current resource fork
// Sizes are in K

OSErr SetSizeResource( FSSpec * file, UInt16 flags, UInt32 minimumSize, UInt32 preferredSize )
{
	OSErr err;
	Handle sizeResource = NULL;
	short oldResFile;
	short resFile;
	
	// Save the old resource file
	oldResFile = CurResFile();
	
	// Start using the runner-to-be resource file
	// Open the resource file	
	resFile = FSpOpenResFile( file, fsRdWrPerm );
	err = ResError();
	if( err != noErr )
		goto finally;
	
	// Set as current file
	UseResFile( resFile );
	
	// get or create the 'Size' resource
	err = MakeResource( &sizeResource, sizeof(SizeResource), kSizeType, kSizeNumber );
	if( err )
		goto finally;
	
	// Set resource
	
	HLock( sizeResource );
	((SizeResource*)*sizeResource)->flags = flags;
	((SizeResource*)*sizeResource)->preferredSize = preferredSize * 1024;
	((SizeResource*)*sizeResource)->minumumSize = minimumSize * 1024;
	HUnlock( sizeResource );
	
	// Write ENTIRE RESOURCE MAP to disk
	UpdateResFile( resFile );
	
	// finally
	finally:
	
	// Restore the old resource file
	UseResFile( oldResFile );
	
	// Release resources we used
	if( sizeResource != NULL )
		ReleaseResource( sizeResource );
		
	return err;
}


// FetchResource
// Gets an already existing resource or returns an error

OSErr FetchResource( Handle * handle, int type, short number )
{
	OSErr err;
	
	// Get the cfrg and the code resource for the self-runner
	*handle = GetResource( type, number );
	if( *handle == NULL )
	{
		err = -192;		// If can't find resource, return this fact
		goto finally;
	}
	if( (err = ResError()) != noErr )
		goto finally;
	
	// finally 
	finally:
	
	return err;
}


// MakeResource
// Makes or gets a resource for writing to

OSErr MakeResource( Handle * handle, int size, int type, short id )
{
	OSErr err;

	// Get or create the resource
	*handle = Get1Resource( type, id );
	if( (*handle == NULL) || (ResError() != noErr) )
	{
		*handle = NewHandle( size );
		if( *handle == NULL )
		{
			err = -109;	// Nil pointer err
			goto finally;
		}
		else if( (err = ResError())!= noErr )
			goto finally;
		
		AddResource( *handle, type, id, "\p" );
	}
	
	// finally
	finally:
	
	return err;
}


// CopyHandle

OSErr CopyHandle( Handle * to, Handle from, int type, short number )
{
	OSErr err;
	int size = GetHandleSize( from );
	
	// Get or make the resource to copy into
	err = MakeResource( to, size, type, number );
	if( err != noErr )
		goto finally;
	
	// Lock, copy, unlock
	HLock( from );
	HLock( *to );
	BlockMove( *from, **to, size );
	HUnlock( from );
	HUnlock( *to );
	
	// finally 
	finally:
	
	return err;
}
