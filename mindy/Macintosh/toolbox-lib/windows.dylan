module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Rob replaced stack-alloc with non-threadsafe globals

define constant $whichWindow = NewPtr( 4 );

// Windows.

define constant <WindowPtr> = <GrafPtr>;

// Standard window kinds

define constant $documentProc				= 0;
define constant $dBoxProc					= 1;
define constant $plainDBox					= 2;
define constant $altDBoxProc				= 3;
define constant $noGrowDocProc				= 4;
define constant $movableDBoxProc			= 5;
define constant $zoomDocProc				= 8;
define constant $zoomNoGrow					= 12;
define constant $rDocProc					= 16;

define constant FrontWindow = get-c-function("FrontWindow", args: #(),
											result: <WindowPtr>, file: *InterfaceLib*);
define constant ShowWindow = get-c-function("ShowWindow", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);
define constant HideWindow = get-c-function("HideWindow", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);
define constant SelectWindow = get-c-function("SelectWindow", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);
define constant SetWTitle = get-c-function("SetWTitle", args: list(<WindowPtr>, <Pascal-string>),
											result: #(), file: *InterfaceLib*);

define constant GetNewWindow =
begin
	let func = get-c-function("GetNewCWindow", args: list(<integer>, <WindowPtr>, <WindowPtr>),
								result: <WindowPtr>, file: *InterfaceLib*);
	method (windowID :: <integer>, #key storage: st = as(<WindowPtr>, 0), behind: bw = as(<WindowPtr>, -1))
		func(windowID, st, bw);
	end method;
end;


define constant NewWindow =
	get-c-function( "NewCWindow", args: list(<WindowPtr>, <Rect>, <Pascal-string>, <boolean>, <integer>, <WindowPtr>, <boolean>, <integer>),
											 result: <WindowPtr>, file: *InterfaceLib*);

define constant DisposeWindow = get-c-function("DisposeWindow", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);

define constant BeginUpdate = get-c-function("BeginUpdate", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);
define constant EndUpdate = get-c-function("EndUpdate", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);
define constant DrawGrowIcon = get-c-function("DrawGrowIcon", args: list(<WindowPtr>),
											result: #(), file: *InterfaceLib*);

define constant FindWindow =
begin
	let func = get-c-function("FindWindow", args: list(<integer>, <Ptr>),
								result: <integer>, file: *InterfaceLib*);
	method (pt :: <Point>) => (partCode :: <integer>, window :: <WindowPtr>);
		//let whichWindow = stack-alloc(<Ptr>, 4);	// sizeof(WindowPtr)
		let partCode = func(as(<integer>, pt), $whichWindow);
		values(partCode, as(<WindowPtr>, pointer-at($whichWindow)));
	end method;
end;

define constant $inDesk = 0;
define constant $inMenuBar = 1;
define constant $inSysWindow = 2;
define constant $inContent = 3;
define constant $inDrag = 4;
define constant $inGrow = 5;
define constant $inGoAway = 6;
define constant $inZoomIn = 7;
define constant $inZoomOut = 8;

define constant DragWindow =
begin
	let func = get-c-function("DragWindow", args: list(<WindowPtr>, <integer>, <Rect>),
								result: #(), file: *InterfaceLib*);
	method (window :: <WindowPtr>, clickPt :: <Point>, #key bounds: bnds :: <Rect> = qd.screenBits.bounds) => ();
		func(window, as(<integer>, clickPt), bnds);
	end method;
end;

define constant TrackGoAway =
begin
	let func = get-c-function("TrackGoAway", args: list(<WindowPtr>, <integer>),
								result: <boolean>, file: *InterfaceLib*);
	method (window :: <WindowPtr>, clickPt :: <Point>) => (result :: <boolean>);
		func(window, as(<integer>, clickPt));
	end method;
end;

define constant TrackBox =
begin
	let func = get-c-function("TrackBox", args: list(<WindowPtr>, <integer>, <integer>),
								result: <boolean>, file: *InterfaceLib*);
	method (window :: <WindowPtr>, clickPt :: <Point>, partCode :: <integer>) => (result :: <boolean>);
		func(window, as(<integer>, clickPt), partCode);
	end method;
end;

define constant ZoomWindow = get-c-function("ZoomWindow", args: list(<WindowPtr>, <integer>, <boolean>),
											result: #(), file: *InterfaceLib*);

define constant GrowWindow =
begin
	let func = get-c-function("GrowWindow", args: list(<WindowPtr>, <integer>, <Rect>),
								result: <integer>, file: *InterfaceLib*);
	method (window :: <WindowPtr>, clickPt :: <Point>, sizeRect :: <Rect>)
	  => (height :: <integer>, width :: <integer>);
		let result = func(window, as(<integer>, clickPt), sizeRect);
		floor/(result, 65536);	// split up the upper and lower halves of the result.
	end method;
end;

define constant SizeWindow = get-c-function("SizeWindow", args: list(<WindowPtr>, <integer>, <integer>, <boolean>),
											result: #(), file: *InterfaceLib*);
