module: dylan-user

/*
	carbon
*/

define library carbon

	use Dylan;
	use melange-support;
	export	mac-types, dialogs, events, gestalt, memory, files, menus, os-utils, 
			quickdraw, resources, sound, windows, controls;

end library carbon;


/*
	mac-types
*/

define module mac-types

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	
	export	// utilities
			<OSErr>, <OSStatus>, <OSType>, os-type,
			Debugger, DebugStr,
			
			// pascal string	
			<Pascal-string>;
			
end module mac-types;


/*
	memory
*/

define module memory

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	
	export	// Memory Manager
			$nil, $NULL,
			<Ptr>, 	NewPtr, DisposePtr, 
			<Handle>, NewHandle, DisposeHandle;
			
end module memory;


/*
	files
*/

define module files

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// Files
			$fsCurPerm, $fsRdPerm, $fsWrPerm, $fsRdWrPerm, 
			$fsRdWrShPerm, $fsRdDenyPerm, $fsWrDenyPerm, 
			<FSSpec>, 
			vRefNum, vrefNum-setter, parID, parID-setter, FSSpec-name, FSSpec-name-setter,
			<FSRef>,
			FSClose, FSRead, FSWrite, Allocate, GetEOF, SetEOF, GetFPos, SetFPos,
			GetVRefNum, FSpOpenRF, FSpOpenDF, FSpCreate, FSpDirCreate, FSpDelete,
			FSpRename, FSpCatMove, FSpExchangeFiles, 
			FSpMakeFSRef;
			
end module files;


/*
	quickdraw
*/

define module quickdraw

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// QuickDraw.
		<Point>, point-v, point-v-setter, point-h, point-h-setter,
		point,
		
		<Rect>, top, top-setter, left, left-setter,
				bottom, bottom-setter, right, right-setter,
		SetRect, PtInRect, InsetRect,
				
		<BitMap>, bounds, //<QDGlobals>, screenBits, qd,
		<RgnHandle>, NewRgn, DisposeRgn, SetEmptyRgn, SetRectRgn, RectRgn,
		
		<CGrafPtr>, SetPort, GetPort,
		MoveTo, LineTo, DrawString, TextFont,
		PenMode, $patOr, $patCopy, $patXor, 
		SetOrigin,

		EraseRect, FrameRect, InvertRect, PaintRect,
		EraseOval, FrameOval, InvertOval, PaintOval,
		
		ClipRect, 
		
		InitCursor, HideCursor, ShowCursor,
		ObscureCursor,
		
		TextSize, TextFont, TextMode,
		// GetFNum,
		
		<RGBColor>,
		RGBForeColor, RGBBackColor, InvertColor,
		red, blue, green, red-setter, green-setter, blue-setter,
		
		<GDHandle>, <PixMapHandle>, <GWorldPtr>,
		NewGWorld, DisposeGWorld, GetGWorld, GetGWorldPixMap,
		LockPixels, UnlockPixels, GetGWorldDevice, PixMap32Bit,
                
                GetPortBounds, SetPortBounds, GetPortVisibleRegion, SetPortVisibleRegion,
                
                QDIsPortBuffered, QDIsPortBufferDirty, QDFlushPortBuffer, QDGetDirtyRegion, QDSetDirtyRegion;
		
end module quickdraw;
		
		
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
		
		
/*
	events
*/

define module events

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Event Manager.
		$everyEvent,
		$nullEvent, $mouseDown, $mouseUp, $keyDown, $keyUp, $autoKey, $updateEvt, $diskEvt, $activateEvt,
		$osEvt, $kHighLevelEvent,
		$cmdKey, $cmdKeyBit, $suspendResumeMessage,
		$activeFlag, $charCodeMask, $keyCodeMask,
		
		<EventRecord>, event-what, event-message, event-when, event-where, event-modifiers,
		//GetNextEvent, SystemTask, 
		WaitNextEvent, FlushEvents,
		DIBadMount,
		
		
		// AppleEvents.
		$kCoreEventClass,
		$kAEOpenApplication, $kAEOpenDocuments, $kAEPrintDocuments, $kAEQuitApplication,
		
		<RoutineDescriptor>, <UniversalProcPtr>,
		<AEEventClass>, <AEEventID>, <AppleEvent>,
		<AEDesc>, <AEDescList>,
		<AEEventHandlerUPP>, $uppAEEventHandlerProcInfo,

		AEInstallEventHandler, AERemoveEventHandler, AEProcessAppleEvent,
		
		// Misc Event Stuff
		TickCount, Button, StillDown, WaitMouseUp, GetMouse, GlobalToLocal;
		//SystemClick;
		
end module events;


/*
	controls
*/

define module controls

	use Dylan;
	//use Extensions;						// <extended-integer>
	use events;
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Control manager.
		$pushButProc, $checkBoxProc, $radioButProc, $scrollBarProc, $popupMenuProc,
		
		$kControlEditTextProc, $kControlEditTextPasswordProc, $kControlEditTextInlineInputProc, 
		$kControlStaticTextProc, $kControlPictureProc, $kControlPictureNoTrackProc,

		$kControlLabelPart, $kControlMenuPart, $kControlTrianglePart, $kControlEditTextPart,
		$kControlPicturePart, $kControlIconPart, $kControlClockPart, $kControlListBoxPart,
		$kControlListBoxDoubleClickPart, $kControlImageWellPart, $kControlRadioGroupPart,
		$kControlButtonPart, $kControlCheckBoxPart, $kControlRadioButtonPart, $kControlUpButtonPart,
		$kControlDownButtonPart, $kControlPageUpPart, $kControlPageDownPart,
		$kControlClockHourDayPart, $kControlClockMinuteMonthPart, $kControlClockSecondYearPart,
		$kControlClockAMPMPart, $kControlDataBrowserPart, $kControlDataBrowserDraggedPart,	
			
		<ControlHandle>, <ControlActionUPP>,
		NewControl, DisposeControl, KillControls,
		HiliteControl, ShowControl, HideControl,
		GetControlValue, SetControlValue, MoveControl, SizeControl,
		SetControlTitle, GetControlTitle, 
		DragControl, FindControl, TrackControl, TestControl,
		DrawControls;
		
end module controls;
		
		
/*
	dialogs
*/

define module dialogs

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use controls;
	use events;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Dialog Manager.
		<DialogRef>, <ModalFilterUPP>, $uppModalFilterProcInfo,
		$kControlDialogItem, $kButtonDialogItem, $kCheckBoxDialogItem,
		$kRadioButtonDialogItem, $kResourceControlDialogItem, $kStaticTextDialogItem,
		$kEditTextDialogItem, $kIconDialogItem, $kPictureDialogItem,
		$kUserDialogItem, $kItemDisableBit,
		Alert,
		GetNewDialog, DisposeDialog,
		SetDialogDefaultItem, SetDialogCancelItem,
		IsDialogEvent, DialogSelect,
		GetDialogItem, GetDialogItemText, SetDialogItemText,
		CountDITL,
                GetDialogWindow, GetDialogPort;
		
end module dialogs;

			
/*
	gestalt
*/

define module gestalt

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
		
	export	// Gestalt
		Gestalt,
		$gestaltQuickdrawVersion, $gestaltOriginalQD, $gestalt8BitQD, $gestalt32BitQD,
		$gestalt32BitQD11, $gestalt32BitQD12, $gestalt32BitQD13, $gestaltAllegroQD;
		
end module gestalt;
		
		
/*
	menus
*/

define module menus

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	
	export	// Menu Manager.
		<MenuBarHandle>, <MenuHandle>,
		GetNewMBar, SetMenuBar, DrawMenuBar, HiliteMenu,
		MenuSelect, MenuKey,
		GetMenuHandle, GetMenuItemText, EnableMenuItem, DisableMenuItem, CountMenuItems,
		DeleteMenu,
		AppendResMenu;
		
		// Desk Accessories.
		//OpenDeskAcc;
		
end module menus;
		
		
/*
	os-utils
*/

define module os-utils

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	
	export	// OS Utils.
			GetDateTime, SecondsToDate,
			<DateTimeRec>,
			year, month, day, hour, minute, seconds, dayOfWeek,
			SysEnvirons,
			<SysEnvRec>,
			environsVersion, machineType, systemVersion, processor, hasFPU, hasColorQD;
		
end module os-utils;
		
		
/*
	resources
*/

define module resources

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// Resource Manager.
		GetResource, ReleaseResource;
		
end module resources;


/*
	sound
*/

define module sound

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// Sound Manager.
			SysBeep, 
			/*<SndChannel>,
			SndPlay*/;
		
end module sound;


/*
	navigation
*/

define module navigation

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use events;
	use quickdraw;

	export // Navigation services
		<NavEventUPP>, <NavPreviewUPP>, <NavObjectFilterUPP>,
		<NavTypeListHandle>, <NavMenuItemSpecArrayHandle>, <FileTranslationSpecArrayHandle>,
		$kNavDefaultNavDlogOptions, $kNavNoTypePopup, $kNavDontAutoTranslate,
		$kNavDontAddTranslateItems, $kNavAllFilesInPopup, $kNavAllowStationery,
		$kNavAllowPreviews, $kNavAllowMultipleFiles, $kNavAllowInvisibleFiles,
		$kNavDontResolveAliases, $kNavSelectDefaultLocation, $kNavSelectAllReadableItem,
		$kNavSupportPackages, $kNavAllowOpenPackages,$kNavDontAddRecents,
		$kNavDontUseCustomFrame, 
		<NavDialogOptions>, navDialog-version, navDialog-version-setter, 
		dialogOptionFlags, dialogOptionFlags-setter,
		navDialog-location, navDialog-location-setter, clientName, clientName-setter, 
		windowTitle, windowTitle-setter, actionButtonLabel, actionButtonLabel-setter,
		cancelButtonLabel, cancelButtonLabel-setter, savedFilename, savedFileName-setter,
		navDialog-message, navDialog-message-setter, 
		<NavReplyRecord>, navReply-version, navReply-version-setter, 
		validRecord, validRecord-setter, navReply-replacing, navReply-replacing-setter,
		isStationary, isStationary-setter, translationNeeded, translationNeeded-setter,
		navReply-selection, navReply-selection-setter, 
		NavLoad, NavUnload, NavLibraryVersion, NavGetDefaultDialogOptions,
		NavGetFile, NavPutFile, NavChooseFile, NavChooseFolder, NavDisposeReply,
		NavServicesCanRun;

end module navigation;
