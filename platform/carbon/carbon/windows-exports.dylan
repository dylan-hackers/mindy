module: dylan-user

/*
	windows
*/

define module windows

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	
	export	// Window Manager.
		<WindowRef>,
		$dialogKind, $userKind, $kDialogWindowKind, $kApplicationWindowKind,
		$documentProc, $dBoxProc, $plainDBox, $altDBoxProc, $noGrowDocProc, 
		$movableDBoxProc, $zoomDocProc, $zoomNoGrow, $rDocProc,
		$floatProc, $floatGrowProc, $floatZoomProc, $floatZoomGrowProc, $floatSideProc,
		$floatSideGrowProc, $floatSideZoomProc, $floatSideZoomGrowProc,	
		FrontWindow, ShowWindow, HideWindow, SelectWindow, BringToFront, SendBehind, SetWTitle,
		GetNewWindow, NewWindow, DisposeWindow, BeginUpdate, EndUpdate, DrawGrowIcon,
		FindWindow,
		$inDesk, $inMenuBar, $inSysWindow, $inContent, $inDrag, $inGrow, $inGoAway, $inZoomIn, $inZoomOut,
		DragWindow, TrackGoAway, TrackBox, ZoomWindow, GrowWindow, SizeWindow,
		InvalWindowRect,
		SetWRefCon, GetWRefCon, SetWindowKind, GetWindowKind,
		
		// Incomplete list
		$kAlertWindowClass,
		$kMovableAlertWindowClass,
		$kModalWindowClass,
		$kMovableModalWindowClass,
		$kFloatingWindowClass,
		$kDocumentWindowClass,
		$kUtilityWindowClass,
		$kHelpWindowClass,
		$kSheetWindowClass,
		$kToolbarWindowClass,
		$kPlainWindowClass,
		
		// Incomplete list
		$kWindowNoAttributes,
		$kWindowCloseBoxAttribute,
		$kWindowHorizontalZoomAttribute,
		$kWindowVerticalZoomAttribute,
		$kWindowFullZoomAttribute,
		$kWindowCollapseBoxAttribute,
		$kWindowResizableAttribute,
		$kWindowSideTitlebarAttribute,
		$kWindowNoUpdatesAttribute,
		$kWindowNoActivatesAttribute,
		$kWindowOpaqueForEventsAttribute,
		$kWindowNoShadowAttribute,
		$kWindowHideOnSuspendAttribute,
		$kWindowHideOnSuspendAttribute,
		$kWindowStandardHandlerAttribute,
		$kWindowHideOnFullScreenAttribute,
		$kWindowHideOnFullScreenAttribute,
		$kWindowInWindowMenuAttribute,
		$kWindowLiveResizeAttribute,
		$kWindowStandardDocumentAttributes,
		$kWindowStandardFloatingAttributes,
		
		// Region codes
		
		$kWindowTitleBarRgn, $kWindowTitleTextRgn, $kWindowCloseBoxRgn,
		$kWindowZoomBoxRgn, $kWindowDragRgn, $kWindowGrowRgn, $kWindowCollapseBoxRgn,
		$kWindowTitleProxyIconRgn, $kWindowStructureRgn, $kWindowContentRgn,
		$kWindowUpdateRgn, $kWindowOpaqueRgn,$kWindowGlobalPortRgn,
		
		CreateNewWindow, CreateWindowFromResource, GetWindowPort, SetPortWindowPort,
    GetWindowPortBounds, GetWindowFromPort,
    GetWindowBounds, SetWindowBounds;
		
end module windows;



