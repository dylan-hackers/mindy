module: carbon

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
		Window Classes
*/

define constant $kAlertWindowClass :: <integer> = c-expr(int: "kAlertWindowClass");
define constant $kMovableAlertWindowClass :: <integer> = c-expr(int: "kMovableAlertWindowClass");
define constant $kModalWindowClass :: <integer> = c-expr(int: "kModalWindowClass");
define constant $kMovableModalWindowClass :: <integer> = c-expr(int: "kMovableModalWindowClass");
define constant $kFloatingWindowClass :: <integer> = c-expr(int: "kFloatingWindowClass");
define constant $kDocumentWindowClass :: <integer> = c-expr(int: "kDocumentWindowClass");
define constant $kUtilityWindowClass :: <integer> = c-expr(int: "kUtilityWindowClass");
define constant $kHelpWindowClass :: <integer> = c-expr(int: "kHelpWindowClass");
define constant $kSheetWindowClass :: <integer> = c-expr(int: "kSheetWindowClass");
define constant $kToolbarWindowClass :: <integer> = c-expr(int: "kToolbarWindowClass");
define constant $kPlainWindowClass :: <integer> = c-expr(int: "kPlainWindowClass");



/*
		Window Attributes
*/

define constant $kWindowNoAttributes :: <integer> = c-expr(int: "kWindowNoAttributes");
define constant $kWindowCloseBoxAttribute :: <integer> = c-expr(int: "kWindowCloseBoxAttribute");
define constant $kWindowHorizontalZoomAttribute :: <integer> = c-expr(int: "kWindowHorizontalZoomAttribute");
define constant $kWindowVerticalZoomAttribute :: <integer> = c-expr(int: "kWindowVerticalZoomAttribute");
define constant $kWindowFullZoomAttribute :: <integer> = c-expr(int: "kWindowFullZoomAttribute");
define constant $kWindowCollapseBoxAttribute :: <integer> = c-expr(int: "kWindowCollapseBoxAttribute");
define constant $kWindowResizableAttribute :: <integer> = c-expr(int: "kWindowResizableAttribute");
define constant $kWindowSideTitlebarAttribute :: <integer> = c-expr(int: "kWindowSideTitlebarAttribute");
define constant $kWindowNoUpdatesAttribute :: <integer> = c-expr(int: "kWindowNoUpdatesAttribute");
define constant $kWindowNoActivatesAttribute :: <integer> = c-expr(int: "kWindowNoActivatesAttribute");
define constant $kWindowOpaqueForEventsAttribute :: <integer> = c-expr(int: "kWindowOpaqueForEventsAttribute");
define constant $kWindowNoShadowAttribute :: <integer> = c-expr(int: "kWindowNoShadowAttribute");
define constant $kWindowHideOnSuspendAttribute :: <integer> = c-expr(int: "kWindowHideOnSuspendAttribute");
define constant $kWindowStandardHandlerAttribute :: <integer> = c-expr(int: "kWindowStandardHandlerAttribute");
define constant $kWindowHideOnFullScreenAttribute :: <integer> = c-expr(int: "kWindowHideOnFullScreenAttribute");
define constant $kWindowInWindowMenuAttribute :: <integer> = c-expr(int: "kWindowInWindowMenuAttribute");
define constant $kWindowLiveResizeAttribute :: <integer> = c-expr(int: "kWindowLiveResizeAttribute");
define constant $kWindowStandardDocumentAttributes :: <integer> = c-expr(int: "kWindowStandardDocumentAttributes");
define constant $kWindowStandardFloatingAttributes :: <integer> = c-expr(int: "kWindowStandardFloatingAttributes");



/*
	WindowPart codes.
*/

define constant $inDesk :: <integer> = 0;
define constant $inMenuBar :: <integer> = 1;
define constant $inSysWindow :: <integer> = 2;
define constant $inContent :: <integer> = 3;
define constant $inDrag :: <integer> = 4;
define constant $inGrow :: <integer> = 5;
define constant $inGoAway :: <integer> = 6;
define constant $inZoomIn :: <integer> = 7;
define constant $inZoomOut :: <integer> = 8;


/*
	Window Regions.
*/

define constant $kWindowTitleBarRgn :: <integer>            = 0;
define constant $kWindowTitleTextRgn :: <integer>           = 1;
define constant $kWindowCloseBoxRgn :: <integer>            = 2;
define constant $kWindowZoomBoxRgn :: <integer>             = 3;
define constant $kWindowDragRgn :: <integer>                = 5;
define constant $kWindowGrowRgn :: <integer>                = 6;
define constant $kWindowCollapseBoxRgn :: <integer>         = 7;
define constant $kWindowTitleProxyIconRgn :: <integer>      = 8;    /* Mac OS 8.5 forward*/
define constant $kWindowStructureRgn :: <integer>           = 32;
define constant $kWindowContentRgn :: <integer>             = 33;   /* Content area of the window; empty when the window is collapsed*/
define constant $kWindowUpdateRgn :: <integer>              = 34;   /* Carbon forward*/
define constant $kWindowOpaqueRgn :: <integer>              = 35;   /* Mac OS X: Area of window considered to be opaque. Only valid for windows with alpha channels.*/
define constant $kWindowGlobalPortRgn :: <integer>          = 40;

// Repositioning windows

define constant $kWindowCenterOnMainScreen :: <integer>      = 1;
define constant $kWindowCenterOnParentWindow :: <integer>    = 2;
define constant $kWindowCenterOnParentWindowScreen :: <integer>  = 3;
define constant $kWindowCascadeOnMainScreen :: <integer>     = 4;
define constant $kWindowCascadeOnParentWindow :: <integer>   = 5;
define constant $kWindowCascadeOnParentWindowScreen :: <integer>  = 6;
define constant $kWindowAlertPositionOnMainScreen :: <integer>  = 7;
define constant $kWindowAlertPositionOnParentWindow :: <integer>  = 8;
define constant $kWindowAlertPositionOnParentWindowScreen :: <integer>  = 9;


/*
	FrontWindow
	Remember that this may return #f if there is no front window!
*/

define method FrontWindow()
=>( result :: <WindowRef> )
	let ptr :: <WindowRef> = make( <WindowRef>, pointer: call-out( "FrontWindow", ptr: ) );
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
	IMPORTANT:	We can't pass -1 as a pointer, the Classic MacOS GC chokes on it, 
				so pass $NULL or a valid <WindowRef>.
*/

define method NewWindow(	storage :: <statically-typed-pointer>,
							bounds :: <Rect*>, title :: <pascal-string>, visible :: <boolean>,
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

define method FindWindow( point :: <Point*> )
=> ( windowPartCode :: <integer>, window :: <WindowRef> )

	//let part :: <integer> = 0;
	// A handle points to a pointer. That's what we need here.
	let temp :: <Handle> = make( <Handle> );
	let part = call-out( "findwindow", short: , ptr: point.raw-value, /*int: 6, */ ptr: temp.raw-value /*, short: part */ );
	let win :: <WindowRef> = pointer-at( temp, class: <WindowRef>, offset: 0 );
	
	values( part, win );

end method FindWindow;


/*
	DragWindow
*/

define method DragWindow( window :: <WindowRef>, startPoint :: <Point*> )
=> ()
	// The third param can only be NULL in Caron / X
	let r :: <Rect*> = make( <Rect*>, top: -32000, left: -32000, bottom: 32000, right: 32000 );
	call-out( "dragwindow", void:, ptr: window.raw-value, ptr: startPoint.raw-value, ptr: r.raw-value );
	
	values();
	
end method;


/*
	TrackGoAway
*/

define method TrackGoAway( window :: <WindowRef>, startPoint :: <Point*> )
=> ( result :: <boolean> )

	let result = call-out("trackgoaway", unsigned-char:, ptr: window.raw-value, ptr: startPoint.raw-value );

	if( result = 1 ) #t else #f end if;

end method TrackGoAway;


/*
	TrackBox
*/

define method TrackBox( window :: <WindowRef>, thePoint :: <Point*>, partCode :: <integer> )
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

define method GrowWindow( window :: <WindowRef>, point :: <Point*> )
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

define Method InvalWindowRect( window :: <WindowRef>, r :: <Rect*> )
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
end method SetPortWindowPort; 


/*
	GetWindowFromPort()
*/

define method GetWindowFromPort( port :: <CGrafPtr> )
=> ( result :: <WindowRef> )
	make( <WindowRef>, pointer: call-out( "GetWindowFromPort", ptr:, ptr: port.raw-value ) );
end method GetWindowFromPort;


/*
		CreateNewWindow
*/

define method CreateNewWindow( windowClass :: <integer>, attributes :: <integer>, bounds :: <Rect*> )
=> ( result :: <OSStatus>, outWindow :: <WindowRef> )
	let temp :: <Handle> = make( <Handle> );
	let result = call-out( "CreateNewWindow", int:, int: windowClass, int: attributes,
												 ptr: bounds.raw-value, ptr: temp.raw-value );
	values( as(<OSErr>, result), pointer-at( temp, class: <WindowRef>, offset: 0 ) );
end method CreateNewWindow;


/*
		CreateWindowFromResource
*/

define method CreateWindowFromResource( resID :: <integer> )
=> ( result :: <OSStatus>, outWindow :: <WindowRef> )
	let temp :: <Handle> = make( <Handle> );
	let result = call-out( "CreateWindowFromResource", int:, int: resID, ptr: temp.raw-value );
	values( as(<OSErr>, result), pointer-at( temp, class: <WindowRef>, offset: 0 ) );
end method CreateWindowFromResource;


/*
	GetWindowPortBounds
	Returns GLOBAL bounds
*/

define method GetWindowPortBounds( ref :: <WindowRef> )
=>( bounds :: <Rect*> )
	let temp :: <Rect*> = make( <Rect*> );
	call-out( "GetWindowPortBounds", int:, ptr: ref.raw-value, ptr: temp.raw-value );
	
	temp;
end method GetWindowPortBounds;


/*
		GetWindowBounds
*/

define method GetWindowBounds( window :: <WindowRef>, region :: <integer>, globalBounds :: <Rect*> )
=> ( result :: <OSStatus>, outBounds :: <Rect*> )
	let temp :: <Handle> = make( <Handle> );
	let result = call-out( "GetWindowBounds", int:, ptr: window.raw-value, int: region, 
													ptr: globalBounds.raw-value );
	values( as(<OSErr>, result), globalBounds );
end method GetWindowBounds;


/*
		SetWindowBounds
*/

define method SetWindowBounds( window :: <WindowRef>, region :: <integer>, globalBounds :: <Rect*> )
=> ( result :: <OSStatus> )
	let temp :: <Handle> = make( <Handle> );
	let result = call-out( "SetWindowBounds", int:, ptr: window.raw-value, int: region, 
													ptr: globalBounds.raw-value );
	as(<OSErr>, result);
end method SetWindowBounds;


/*
		IsWindowVisible
*/

define method IsWindowVisible( window :: <WindowRef> )
=> ( result :: <boolean> )
	let result = call-out( "IsWindowVisible", int:, ptr: window.raw-value, );
	if(result = 1)
    #t;
  else
    #f;
  end if;
end method IsWindowVisible;


/*
		RepositionWindow
*/

define method RepositionWindow(inWindow :: <WindowRef>,
                               inParent :: <WindowRef>,
                               positionMethod :: <integer>)
=> ( result :: <OSStatus> )
	as(<OSErr>, call-out("RepositionWindow", int:, ptr: inWindow.raw-value,
                        ptr: inParent.raw-value, int: positionMethod));
end method RepositionWindow;


define method GetWindowRegion(window :: <WindowRef>, 
    inRegionCode :: <integer>, ioWinRgn :: <RgnHandle>)
=> (result :: <OSStatus>)
  as(<OSStatus>, call-out("GetWindowRegion", int:, ptr: window.raw-value,
    int: inRegionCode, ptr: ioWinRgn.raw-value));
end method GetWindowRegion;

define method MoveWindowStructure(window :: <WindowRef>, h :: <integer>, v :: <integer>)
=> (result :: <OSErr>)
	as(<OSErr>, call-out("MoveWindowStructure", int:, ptr: window.raw-value, short: h, short:, v));
end method MoveWindowStructure;

