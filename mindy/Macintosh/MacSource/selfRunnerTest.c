// selfRunnerTest.c

// includes

#include<Files.h>

// Prototypes

extern OSErr MakeSelfRunner( FSSpec * from, int creator, int type );
void InitializeToolbox( void );


// Functions

void main( void )
{
	OSErr err;
	StandardFileReply sf;
	SFTypeList list;

	InitializeToolbox();
	
	StandardGetFile(  NULL, 0, list, &sf );
	if( sf.sfGood )
		err = MakeSelfRunner(  &sf.sfFile, '????', 'APPL' );
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



