module: windows

/*
	Mac Window Manager
*/

/*
	Includes
*/

c-include( "Carbon.h" );


/*
	<WindowRef>
*/

define functional class <WindowRef> (<Ptr>) 
end class;



/*
	WindowKind-s
*/

define constant $dialogKind                  = 2;
define constant $userKind                    = 8;
define constant $kDialogWindowKind           = 2;
define constant $kApplicationWindowKind      = 8;

/*
	DefProcs
*/

define constant $documentProc				= 0;
define constant $dBoxProc					= 1;
define constant $plainDBox					= 2;
define constant $altDBoxProc				= 3;
define constant $noGrowDocProc				= 4;
define constant $movableDBoxProc			= 5;
define constant $zoomDocProc				= 8;
define constant $zoomNoGrow					= 12;
define constant $rDocProc					= 16;

define constant $floatProc					= 1985;
define constant $floatGrowProc				= 1987;
define constant $floatZoomProc				= 1989;
define constant $floatZoomGrowProc			= 1991;
define constant $floatSideProc				= 1993;
define constant $floatSideGrowProc			= 1995;
define constant $floatSideZoomProc			= 1997;
define constant $floatSideZoomGrowProc		= 1999;


/*
	FrontWindow
	Remember that this may return #f if there is no front window!
*/

define method FrontWindow()
=>( result :: <object> )//type-union( <WindowRef>, <boolean> ) )
	let ptr :: <WindowRef> = make( <WindowRef>, pointer: call-out( "FrontWindow", ptr: ) );
	if( ptr = $NULL )
		#f;
	else
		ptr
	end if;
end method FrontWindow;											


/*
	ShowWindow
*/

define method ShowWindow( window :: <WindowRef> )
=> ()
	call-out( "ShowWindow", void:, ptr: window.raw-value );
	values();
end method ShowWindow;
				
				
/*
	HideWindow
*/
											
define method HideWindow( window :: <WindowRef> )
=> ()
	call-out( "HideWindow", void:, ptr: window.raw-value );
	values();
end method HideWindow;


/*
	SelectWindow
*/

define method SelectWindow( window :: <WindowRef> )
=> ()
	call-out( "SelectWindow", void:, ptr: window.raw-value );
	values();
end method SelectWindow;


/*
    BringToFront
*/

define method BringToFront( window :: <WindowRef> )
=>()
    call-out( "BringToFront", void:, ptr: window.raw-value );
    values();
end method BringToFront;


/*
    SendBehind
*/

define method SendBehind( window :: <WindowRef>, behindWindow :: <WindowRef> )
=>()
    call-out( "SendBehind", void:, ptr: window.raw-value, ptr: behindWindow.raw-value );
    values();
end method SendBehind;


/*
	SetWTitle
*/											
											
define method SetWTitle( window :: <WindowRef>, title :: <pascal-string> )
=> ()
	call-out( "SetWTitle", void:, ptr: window.raw-value, ptr: title.raw-value );
	values();
end method SetWTitle;


/*
	GetNewWindow
*/

define method GetNewWindow( id :: <integer> )
=> ( result :: <WindowRef> )
	let ptr = call-out( "GetNewCWindow", ptr:, short: id, ptr: $NULL.raw-value, ptr: $NULL.raw-value );
	make( <WindowRef>, pointer: ptr ); 
end method GetNewWindow;


/*
	NewWindow
	IMPORTANT:	We can't pass -1 as a pointer, the GC chokes on it, 
				so pass $NULL or a valid <WindowRef>.
*/

define method NewWindow(	storage :: <statically-typed-pointer>,
							bounds :: <Rect>, title :: <pascal-string>, visible :: <boolean>,
							procID :: <integer>, goAway :: <boolean>, 
							behind :: <statically-typed-pointer>, refCon :: <integer> )
=> ( result :: <WindowRef> )
	let visibleBool = if( visible ) 1 else 0 end if;
	let goAwayBool = if( goAway ) 1 else 0 end if;
	let ptr = call-out( "NewCWindow", ptr:, ptr: storage.raw-value, ptr: bounds.raw-value, 
						ptr: title.raw-value, unsigned-char: visibleBool, short: procID, 
						ptr: behind.raw-value, unsigned-char: goAwayBool, int: refCon );
	make( <WindowRef>, pointer: ptr );
end method NewWindow;

/*
	DisposeWindow
*/

define method DisposeWindow( window :: <WindowRef> )
=> ()
	call-out( "DisposeWindow", void:, ptr: window.raw-value );
	values();
end method DisposeWindow;


/*
	BeginUpdate
*/

define method BeginUpdate( window :: <WindowRef> )
=> ()
	call-out( "BeginUpdate", void:, ptr: window.raw-value );
	values();
end method BeginUpdate;


/*
	EndUpdate
*/

define method EndUpdate( window :: <WindowRef> )
=> ()
	call-out( "EndUpdate", void:, ptr: window.raw-value );
	values();
end method EndUpdate;


/*
	DrawGrowIcon
*/
					
define method DrawGrowIcon( window :: <WindowRef> )
=> ()
	call-out( "DrawGrowIcon", void:, ptr: window.raw-value );
	values();
end method DrawGrowIcon;


/*
	FindWindow
	//Actually FindWindowOfClass, so you'll need >= 8.1
*/

define method FindWindow( point :: <Point> )
=> ( windowPartCode :: <integer>, window :: <WindowRef> )

	//let part :: <integer> = 0;
	// A handle points to a pointer. That's what we need here.
	let temp :: <Handle> = make( <Handle> );
	let part = call-out( "findwindow", short: , ptr: point.raw-value, /*int: 6, */ ptr: temp.raw-value /*, short: part */ );
	let win :: <WindowRef> = pointer-at( temp, class: <WindowRef>, offset: 0 );
	
	values( part, win );

end method FindWindow;


/*
	WindowPart codes.
*/

define constant $inDesk = 0;
define constant $inMenuBar = 1;
define constant $inSysWindow = 2;
define constant $inContent = 3;
define constant $inDrag = 4;
define constant $inGrow = 5;
define constant $inGoAway = 6;
define constant $inZoomIn = 7;
define constant $inZoomOut = 8;


/*
	DragWindow
*/

define method DragWindow( window :: <WindowRef>, startPoint :: <Point> )
=> ()
	// The third param can only be NULL in Caron / X
	let r :: <Rect> = make( <Rect>, top: -32000, left: -32000, bottom: 32000, right: 32000 );
	call-out( "dragwindow", void:, ptr: window.raw-value, ptr: startPoint.raw-value, ptr: r.raw-value );
	
	values();
	
end method;


/*
	TrackGoAway
*/

define method TrackGoAway( window :: <WindowRef>, startPoint :: <Point> )
=> ( result :: <boolean> )

	let result = call-out("trackgoaway", unsigned-char:, ptr: window.raw-value, ptr: startPoint.raw-value );

	if( result = 1 ) #t else #f end if;

end method TrackGoAway;


/*
	TrackBox
*/

define method TrackBox( window :: <WindowRef>, thePoint :: <Point>, partCode :: <integer> )
=> ( result :: <boolean> )

	let result = call-out("trackbox", unsigned-char:, ptr: window.raw-value, ptr: thePoint.raw-value, short: partCode );

	if( result = 1 ) #t else #f end if;
	
end method TrackBox;


/*
	ZoomWindow
*/

define method ZoomWindow( window :: <WindowRef>, partCode :: <integer>, front :: <boolean> )
=> () 
	call-out( "ZoomWindow", void:, ptr: window.raw-value, unsigned-char: front );
	values();
end method ZoomWindow;


/*
	GrowWindow
	Deviates from Toolbox prototype to avoid <extended-integer> and Hi/LoWord
*/

define method GrowWindow( window :: <WindowRef>, point :: <Point> )
=> ( width :: <integer>, height :: <integer> )
	// The third param, bBox, can only be NULL on Carbon or MacOS X
	let result = call-out( "growwindow", long:, ptr: window.raw-value, ptr: point.raw-value, ptr: $NULL.raw-value );
	floor/(result, 65536);
end method GrowWindow;

/*
	SizeWindow
*/

define method SizeWindow( window :: <WindowRef>, h :: <integer>, v :: <integer>, update :: <boolean> )
=> ()
	let updateBoolean = if( update ) 1 else 0 end if;
	call-out( "SizeWindow", void:, ptr: window.raw-value, short: h, short:, v, unsigned-char: updateBoolean );
	values();
end method SizeWindow;


/*
	InvalWindowRect
*/

define Method InvalWindowRect( window :: <WindowRef>, r :: <Rect> )
=> ()
	call-out( "InvalWindowRect", void:, ptr: window.raw-value, ptr: r.raw-value );
	values();
end method InvalWindowRect;


/*
	GetWRefCon
*/

define method GetWRefCon( window :: <WindowRef> )
=> ( result :: <integer> )
	call-out( "GetWRefCon", long:, ptr: window.raw-value );
end method GetWRefCon;


/*
	SetWRefCon
*/

define method SetWRefCon( window :: <WindowRef>, refcon :: <integer> )
=> ( result :: <integer> )
	call-out( "SetWRefCon", void:, ptr: window.raw-value, long: refcon );
end method SetWRefCon;


/*
	GetWindowKind
*/

define method GetWindowKind( window :: <WindowRef> )
=> ( result :: <integer> )
	call-out( "GetWindowKind", short:, ptr: window.raw-value );
end method GetWindowKind;


/*
	SetWindowKind
*/

define method SetWindowKind( window :: <WindowRef>, kind :: <integer> )
=> ()
	call-out( "SetWindowKind", void:, ptr: window.raw-value, short: kind );
	
	values();
	
end method SetWindowKind;

/*
    Carbon Accessors
*/

/*
    GetWindowPort
*/

define method GetWindowPort( window :: <WindowRef> )
=> ( result :: <CGrafPtr> )
    let ptr = call-out( "GetWindowPort", ptr:, ptr: window.raw-value );

    make( <CGrafPtr>, pointer: ptr );

end method GetWindowPort;

/*
    SetPortWindowPort
*/

define method SetPortWindowPort( window :: <WindowRef> )
=> ()
    call-out( "SetPortWindowPort", void:, ptr: window.raw-value );
    values();
end method SetPortWindowPort 
