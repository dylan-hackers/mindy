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
                
                GetWindowPort, SetPortWindowPort;
		
end module windows;



