
// includes

#include<AppleEvents.h>
#include<MacTypes.h>
#include<MixedMode.h>
#include<CodeFragments.h>
#include<AERegistry.h>
#include<ToolUtils.h>

#include<SIOUX.h>
#include<console.h>

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "dialogs.h"

// defines

#define kMindyCompEventClass	'MndC'
#define kCompileEvent			'comp'
#define kLibraryParameter 		'libr'
#define kMaximumPathLength		256

#define kMenuBar	128
#define kAppleMenu	128
#define kFileMenu	129
#define kEditMenu	130

#define kAboutChoice	1

#define kCommandLineChoice	1
#define kEasyCompileChoice 	2
#define kQuitChoice			4

#define kCompileDialog			128
#define kCompileDialogOK		1
#define kCompileDialogCancel	2
#define kCompileDialogFile		4
#define kCompileDialogLibrary	6

// Prototypes

static void InitializeToolbox( void );
static void MainEventLoop( void );
void InitializeMenus( short menuID, short applemenuID );
void DoMenuCommand( unsigned long menuAndItem );

static void InitializeCompileAppleEventHandler( void );
static void FinalizeCompileAppleEventHandler( void );

static pascal OSErr HandleCompileEvent( AEDescList* aevt, AEDescList* reply, long refCon );
static void DoCompile( int argc, char *argv[] );
void DoCompileDialog( void );
void BuildArgs( void );
static OSErr GetAEParamString( AppleEvent * ae, AEKeyword keyword, Ptr * ptr );

static void InitializeCoreAppleEvents( void );
static void FinalizeCoreAppleEvents( void );

static pascal OSErr HandleOapp ( AEDescList* aevt, AEDescList* reply, long refCon );
static pascal OSErr HandleQuit ( AEDescList* aevt, AEDescList* reply, long refCon );
static pascal OSErr HandleOdoc ( AEDescList* aevt, AEDescList* reply, long refCon );
static pascal OSErr HandlePdoc ( AEDescList* aevt, AEDescList* reply, long refCon );

// Globals

Boolean		gRunning = 1;
Ptr			gSourceFileName;
Ptr			gLibraryName;
Ptr			gLibraryCommand;

int 		gArgC;
char *		gArgV[4];

// main

void main( void )
{
	InitializeToolbox();
	InitializeCoreAppleEvents();
	InitializeCompileAppleEventHandler();
	InitializeMenus( kMenuBar, kAppleMenu );
	
	SIOUXSettings.initializeTB = 0;		// CW Specific. don't init the Toolbox
	SIOUXSettings.setupmenus = 0;		// CW specific, don't draw the SIOUX menus
	SIOUXSettings.autocloseonquit = 1;	// CW specific, do we close the SIOUX window on program termination?
	SIOUXSettings.asktosaveonclose = 0;	// CW Specific. 
	
	MainEventLoop();
	
	FinalizeCompileAppleEventHandler();
	FinalizeCoreAppleEvents();
}

// InitializeCompileAppleEventHandler

void InitializeCompileAppleEventHandler( void )
{
	OSErr	err;
	
	err = AEInstallEventHandler ( kMindyCompEventClass, kCompileEvent, NewAEEventHandlerProc (  HandleCompileEvent ), 0, 0 );
	if ( err )
	{
		printf( "Couldn't install Apple Event Handlers!\n");
		exit(0);
	}
	
	gSourceFileName = NewPtr( kMaximumPathLength );
	err = MemError();
	if( err )
	{
		printf( "Couldn't allocate memory for file name!\n");
		exit(0);
	}
	gLibraryName = NewPtr( kMaximumPathLength );
	err = MemError();
	if( err )
	{
		printf( "Couldn't allocate memory for library name!\n");
		exit(0);
	}
	gLibraryCommand = NewPtr( kMaximumPathLength );
	err = MemError();
	if( err )
	{
		printf( "Couldn't allocate memory for library command!\n");
		exit(0);
	}
	
	*gSourceFileName = '\0';	// Initailize the strings as empty
	*gLibraryName = '\0';
	
	gArgV[ 0 ] = "MindyComp";
}

// FinalizeCompileAppleEventHandler

void FinalizeCompileAppleEventHandler( void )
{
	OSErr	theErr;
	AEFilterUPP tempUPP;
	long	tempLong; 

	theErr = AERemoveEventHandler ( kMindyCompEventClass, kCompileEvent, (AEFilterUPP)HandleCompileEvent , 0 );
	if ( theErr == errAEHandlerNotFound )	// Handlers already removed.	
		return;
	else if( theErr != noErr )
	{
		printf( "Couldn't remove Apple Event Handler: COMP!\n");
	}
	else
	{
		AEGetEventHandler(  kCoreEventClass, kAEOpenApplication, &tempUPP, &tempLong, 0  );
		DisposeRoutineDescriptor( tempUPP );
	}
}

// Open Application Apple Event Handler

static pascal OSErr HandleCompileEvent( AEDescList* aevt, AEDescList* reply, long refCon )
{
	//#pragma unused(aevt, reply, refCon)
	OSErr 			err = noErr;
	DescType 		typeCode;
	Size			sizeOfParam;
	
	SIOUXSettings.autocloseonquit = 1;	// Make sure we exit silently so events don't time out
	
	gArgV[ 1 ] = '\0';	// Clear the "command line arguments"
	gArgV[ 2 ] = '\0';
	
	err = AESizeOfParam( aevt, keyDirectObject, &typeCode, &sizeOfParam );
	if( err )
	{
		return err;
	}
	else
	{
		if( (typeCode == typeChar) || (typeCode == typeStyledText) || 
			(typeCode == typeIntlText) || (typeCode == typeAlias)  )
		{
			// we've got a string
			err = GetAEParamString( aevt, keyDirectObject, &gSourceFileName );
			if( err != noErr )
				return err;

			err = GetAEParamString( aevt, kLibraryParameter, &gLibraryName );
			if( err != noErr )
			{
				*gLibraryName = '\0';
				err = noErr;
			}
		}
		else
		{
			return errAEEventNotHandled;
		}
	}
	
	BuildArgs();
	
	DoCompile( gArgC, gArgV );
	
	return err;
}

void DoCompileDialog( void )
{
	short result;
	Str255 file, lib;
	DialogPtr d = GetNewDialog( kCompileDialog, NULL, (GrafPtr)-1 );
	
	c2pstr( gSourceFileName );
	c2pstr( gLibraryName );
	
	SetDialogText( d, kCompileDialogFile, (unsigned char *)gSourceFileName ); 
	SetDialogText( d, kCompileDialogLibrary, (unsigned char *)gLibraryName );
	
	result =  HandleSimpleModalDialog( d, kCompileDialogOK, kCompileDialogCancel );
	
	if( result == kCompileDialogCancel )
	{
		DisposeDialog( d );
		return;
	}
	
	HideWindow( d );
	
	GetDialogText( d, kCompileDialogFile, (unsigned char *)gSourceFileName ); 
	GetDialogText( d, kCompileDialogLibrary, (unsigned char *)gLibraryName );
	
	p2cstr( (unsigned char *)gSourceFileName );
	p2cstr( (unsigned char *)gLibraryName );
	 
	BuildArgs();
	DoCompile( gArgC, gArgV );
	
	DisposeDialog( d );
}

void BuildArgs( void )
{
	
	long 	libraryNameLength;

	if( *gLibraryName != '\0' )
	{		
		// Make sure the library command buffer is big enough
		libraryNameLength = strlen( gLibraryName );
		if( GetPtrSize( gLibraryCommand ) - 3 < libraryNameLength  )
		{
			DisposePtr( gLibraryCommand );
			gLibraryCommand = NewPtr( libraryNameLength+3 );
		}
		
		// Make the -l switch
		strcpy( gLibraryCommand, "-l" );
		strcat( gLibraryCommand, gLibraryName );
		
		// Make the command line arguments
		gArgV[ 1 ] = gLibraryCommand;
		gArgV[ 2 ] = gSourceFileName;
		gArgV[ 3 ] = NULL;
		gArgC = 3;
	}
	else
	{
		// Make the command line argument
		gArgV[ 1 ] = gSourceFileName;
		gArgV[ 2 ] = NULL;
		gArgC = 2;
	}
}

// DoCompile

static void DoCompile( int argc, char *argv[] )
{
	CFragConnectionID		id;
	/*UniversalProcPtr 		MindyCompMain;
	ProcInfoType			procInfo = 	kCStackBased
										| RESULT_SIZE( SIZE_CODE( sizeof( int ) ) )
										| STACK_ROUTINE_PARAMETER( 1, SIZE_CODE( sizeof( int ) ) )
										| STACK_ROUTINE_PARAMETER( 2, SIZE_CODE( sizeof( char** ) ) )
										| STACK_ROUTINE_PARAMETER( 3, SIZE_CODE( sizeof( FILE * ) ) );*/
	CFragSymbolClass		sym;
	Ptr						libraryMain;
	//Ptr						compileMain;
	int(*					compileMain)(int, char**, FILE *) ;
	Str255					errName;
	OSErr 					err;
//	long 					libraryNameLength;

	
	err = GetSharedLibrary(	"\pMindy Compiler Library", kPowerPCCFragArch, kPrivateCFragCopy, &id,
							 &libraryMain, errName );
	if( err != noErr )
	{
		AlertFatal( "\pCouldn't find Shared Lib:", errName );
	}
	
	err = FindSymbol( id, "\pMindyComp", (void *)&compileMain, &sym );
	if( err != noErr )
	{
		AlertFatal( "\pAn Error Occurred.", "\pCouldn't find Compiler main!" );
	}
	
	printf( "Compiling: %s\r", argv[ 2 ] );
	
	//MindyCompMain = NewRoutineDescriptor( 	(ProcPtr)compileMain, procInfo, kPowerPCISA );

	// Do compile
	
	compileMain( argc, argv, stderr  );
	
	/*err= CallUniversalProc( MindyCompMain, procInfo, argc, argv, stderr );
	DisposeRoutineDescriptor( MindyCompMain );
	*/
	
	printf( "Done.\r" );
	
	err = CloseConnection( &id );
	if(err != noErr )
		AlertFatal( "\pError Closing Compiler Library", "\pApplication will now quit." );
}


static OSErr GetAEParamString( AppleEvent * ae, AEKeyword keyword, Ptr * ptr )
{
	OSErr 		err;
	Size 		size;
	DescType 	actualType;
	
	// Get the string 
	err = AEGetParamPtr( ae, keyword, typeChar, &actualType, *ptr, GetPtrSize( *ptr ), &size );
	
	if( err != noErr || actualType == typeNull )
	{
		**ptr = '\0';
	}
	else if( size+1 > GetPtrSize( *ptr ) )
	{
		DisposePtr( *ptr );
		*ptr = NewPtr( size+1 );
		err = GetAEParamString( ae, keyword, ptr );		
	}
	
	(*ptr)[ size ] = '\0';		// Null terminate the string
	
	return err;
}

////////////// APPLICATION SUPPORT

void MainEventLoop( void )
{
	Boolean		gotEvent;
	EventRecord	event;
	int			myPart;
	char 		myKey;
	WindowPtr	myWindow;
	OSErr		err;
	
	while( gRunning ) 
	{
		gotEvent = WaitNextEvent( everyEvent, &event, 10L, NULL );
		
		if( gotEvent )
			switch( event.what )
			{
				case kHighLevelEvent:
					err = AEProcessAppleEvent( &event );
					if( err && err != errAEEventNotHandled)
					{
						AlertCaution( "\pAn Error Occurred!", "\pError processing Apple Event." );
					}
					continue;
				case mouseDown:
					myPart = FindWindow( event.where, &myWindow );
					if( myPart == inMenuBar )
					{
						DoMenuCommand( MenuSelect(event.where) );
						continue;
					}
				case keyDown:
					myKey = event.message & charCodeMask;
					if( (event.modifiers & cmdKey) != 0 )
					{
						DoMenuCommand( MenuKey( myKey ) );
						continue;
					}
					break;
			}
		
		SIOUXHandleOneEvent( &event );	// Handles high-level events, too
	}
}

// DoMenuCommand

void DoMenuCommand( unsigned long menuAndItem )
{
	short	myMenuNum;
	short	myItemNum;
	short	myResult;
	Str255	myDAName;

	myMenuNum = HiWord( menuAndItem );
	myItemNum = LoWord( menuAndItem );
	
	switch( myMenuNum )
	{
		case kAppleMenu:
			switch( myItemNum )
			{
				case kAboutChoice:
					AlertAbout();
					break;
					
				default:
					GetMenuItemText( GetMenuHandle( kAppleMenu ), myItemNum, myDAName );
					myResult = OpenDeskAcc( myDAName );
					break;
			}
			break;
			
		case kFileMenu:
			switch( myItemNum )
			{
				case kCommandLineChoice:
					{
						int argc;
						char **argv;
						argc = ccommand( &argv );
						DoCompile( argc, argv );
					}
					break;
					
				case kEasyCompileChoice:
					DoCompileDialog();
					break;
			
				case kQuitChoice:
					gRunning = 0;
					break;
			}
			break;
	}
	
	HiliteMenu( 0 );
}

//	InitializeToolbox

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


// InitializeMenus

void InitializeMenus( short menuID, short applemenuID )
{
	Handle	menuBar;

	menuBar = GetNewMBar( menuID );
	if( menuBar == NULL || ResError() )
	{
		AlertFatal( "\pError Starting Up!", "\pCouldn't get Menu Bar!" );
	}
	SetMenuBar( menuBar );
	DisposeHandle( menuBar );
	AppendResMenu( GetMenuHandle( applemenuID ), 'DRVR' );
	DrawMenuBar();
}


///////////// CORE APPLE EVENTS

// Install Apple event handlers
// Call as the program initialises

void InitializeCoreAppleEvents ( void )
{
	OSErr	theErr;
	
	theErr = AEInstallEventHandler ( kCoreEventClass, kAEOpenApplication, NewAEEventHandlerProc ( HandleOapp ), 0, 0 );
	if ( theErr )
	{
		printf( "Couldn't install Apple Event Handlers!\n");
		exit(0);
	}

	theErr = AEInstallEventHandler ( kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerProc ( HandleOdoc ), 0, 0 );
	if ( theErr )
	{
		printf( "Couldn't install Apple Event Handlers!\n");
		exit(0);
	}

	theErr = AEInstallEventHandler ( kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerProc ( HandlePdoc ), 0, 0 );
	if ( theErr )
	{
		printf( "Couldn't install Apple Event Handlers!\n");
		exit(0);
	}

	theErr = AEInstallEventHandler ( kCoreEventClass, kAEQuitApplication, NewAEEventHandlerProc ( HandleQuit ), 0, 0 );
	if ( theErr )
	{
		printf( "Couldn't install Apple Event Handlers!\n");
		exit(0);
	}
}

// Remove Apple Event Handlers for this application.
// Call when the application finalises 

void FinalizeCoreAppleEvents( void )
{
	OSErr	theErr;
	AEFilterUPP tempUPP;
	long	tempLong; 

	theErr = AERemoveEventHandler ( kCoreEventClass, kAEOpenApplication, (AEFilterUPP)HandleOapp , 0 );
	if ( theErr == errAEHandlerNotFound )	// Handlers already removed.	
		return;
	else if( theErr != noErr )
	{
		printf( "Couldn't remove Apple Event Handler: OAPP!\n");
	}
	else
	{
		AEGetEventHandler(  kCoreEventClass, kAEOpenApplication, &tempUPP, &tempLong, 0  );
		DisposeRoutineDescriptor( tempUPP );
	}

	theErr = AERemoveEventHandler ( kCoreEventClass, kAEOpenDocuments, (AEFilterUPP)HandleOdoc , 0 );
	if ( theErr )	
	{
		printf( "Couldn't remove Apple Event Handler: ODOC!\n");
	}	
	else
	{
		AEGetEventHandler(  kCoreEventClass, kAEOpenDocuments, &tempUPP, &tempLong, 0  );
		DisposeRoutineDescriptor( tempUPP );
	}

	theErr = AERemoveEventHandler ( kCoreEventClass, kAEPrintDocuments, (AEFilterUPP)HandlePdoc, 0 );
	if ( theErr )	
	{
		printf( "Couldn't remove Apple Event Handler: PDOC!\n");
	}
	else
	{
		AEGetEventHandler(  kCoreEventClass, kAEPrintDocuments, &tempUPP, &tempLong, 0  );
		DisposeRoutineDescriptor( tempUPP );
	}

	theErr = AERemoveEventHandler ( kCoreEventClass, kAEQuitApplication, (AEFilterUPP)HandleQuit, 0 );
	if ( theErr )		
	{
		printf( "Couldn't remove Apple Event Handler: QUIT!\n");
	}
	else
	{
		AEGetEventHandler(  kCoreEventClass, kAEQuitApplication, &tempUPP, &tempLong, 0  );
		DisposeRoutineDescriptor( tempUPP );
	}
}

// Open Application Apple Event Handler

static pascal OSErr HandleOapp ( AEDescList* aevt, AEDescList* reply, long refCon )
{
	#pragma unused(aevt, reply, refCon)
	return noErr;
}

// Open Document Apple Event Handler

static pascal OSErr HandleOdoc ( AEDescList* aevt, AEDescList* reply, long refCon )
{
	#pragma unused(aevt, reply, refCon)
	return errAEEventNotHandled;
}

// Print Document Apple Event Handler

static pascal OSErr HandlePdoc ( AEDescList* aevt, AEDescList* reply, long refCon )
{
	#pragma unused(aevt, reply, refCon)
	return errAEEventNotHandled;
}

// Quit Application Apple Event Handler

static pascal OSErr HandleQuit ( AEDescList* aevt, AEDescList* reply, long refCon )
{
	#pragma unused(aevt, reply, refCon)
	gRunning = 0;					// Flag a quit
	
	return noErr;
}

