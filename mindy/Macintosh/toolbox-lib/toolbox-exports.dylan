Module: dylan-user

define Library Toolbox
	use Dylan;
	export Toolbox;
end library Toolbox;

define module Toolbox
	use Dylan;							// all programs need this.
	use Extensions;						// <extended-integer>
	use Extern;							// imports "load-object-file", etc.
	
	export
		get-c-function, Debugger, DebugStr,
		
		$nil,
		
		<Ptr>, 	NewPtr, DisposePtr, 
		<Handle>, NewHandle, DisposeHandle,
			
		<Pascal-string>,
		
		<OSErr>, <OSType>, os-type,
		
		<Point>, point-v, point-v-setter, point-h, point-h-setter,
		point,
		
		<Rect>, top, top-setter, left, left-setter,
				bottom, bottom-setter, right, right-setter,
		
		// Resource Manager.
		GetResource, ReleaseResource,
		
		// Sound Manager.
		SysBeep, SndPlay,
				
		// Event Manager.
		$everyEvent,
		$nullEvent, $mouseDown, $mouseUp, $keyDown, $keyUp, $autoKey, $updateEvt, $diskEvt, $activateEvt,
		$osEvt, $kHighLevelEvent,
		$cmdKey,
		
		<EventRecord>, event-what, event-message, event-when, event-where, event-modifiers,
		GetNextEvent, SystemTask, WaitNextEvent,
		
		// AppleEvents.
		$kCoreEventClass,
		$kAEOpenApplication, $kAEOpenDocuments, $kAEPrintDocuments, $kAEQuitApplication,
		
		<RoutineDescriptor>, <UniversalProcPtr>,
		<AEEventClass>, <AEEventID>, <AppleEvent>,
		<AEEventHandlerUPP>, $uppAEEventHandlerProcInfo,

		AEInstallEventHandler, AEProcessAppleEvent,
		
		TickCount, Button, StillDown, WaitMouseUp, GetMouse, GlobalToLocal,

		// QuickDraw.
		<BitMap>, bounds, <QDGlobals>, screenBits, qd,
		<RgnHandle>, NewRgn, DisposeRgn, SetEmptyRgn, SetRectRgn, RectRgn,
		
		<GrafPtr>, portRect, SetPort, GetPort,
		MoveTo, LineTo, DrawString, TextFont,
		PenMode, $patOr, $patCopy, $patXor, 

		EraseRect, FrameRect, InvertRect, PaintRect,
		PtInRect,
		
		InitCursor, HideCursor, ShowCursor,
		
		// Fonts.
		
		GetFNum,
		
		// Window Manager.
		<WindowPtr>,
		$documentProc, $dBoxProc, $plainDBox, $altDBoxProc, $noGrowDocProc, $movableDBoxProc, $zoomDocProc, $zoomNoGrow, $rDocProc,
		FrontWindow, ShowWindow, HideWindow, SelectWindow, SetWTitle,
		GetNewWindow, NewWindow, DisposeWindow, BeginUpdate, EndUpdate, DrawGrowIcon,
		FindWindow,
		$inDesk, $inMenuBar, $inSysWindow, $inContent, $inDrag, $inGrow, $inGoAway, $inZoomIn, $inZoomOut,
		DragWindow, TrackGoAway, TrackBox, ZoomWindow, GrowWindow, SizeWindow,
		
		// Dialog Manager.
		<DialogPtr>, <ModalFilterUPP>, $uppModalFilterProcInfo,
		Alert,
		
		// Menu Manager.
		<MenuBarHandle>, <MenuHandle>,
		GetNewMBar, SetMenuBar, DrawMenuBar, HiliteMenu,
		MenuSelect, MenuKey,
		GetMenuHandle, CountMItems, GetMenuItemText, EnableItem, DisableItem,
		AppendResMenu,
		
		// Desk Accessories.
		OpenDeskAcc,
		
		// OS Utils.
		GetDateTime, SecondsToDate,
		<DateTimeRec>, year, month, day, hour, minute, seconds, dayOfWeek
end module Toolbox;