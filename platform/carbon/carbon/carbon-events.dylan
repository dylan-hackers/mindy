module: carbon

c-include( "Carbon.h" );


// UPP Types

define functional class <EventHandlerUPP> (<UniversalProcPtr>)
end class <EventHandlerUPP>;

define functional class <EventComparatorUPP> (<UniversalProcPtr>)
end class <EventComparatorUPP>;

define functional class <EventLoopTimerUPP> (<UniversalProcPtr>)
end class <EventLoopTimerUPP>;


// We can't sizeof() these as they are pointers to opaque (incomplete) types

/*
	<EventLoopRef>
*/

define functional class <EventLoopRef> (<statically-typed-pointer>)
end class <EventLoopRef>; 

/*
	<EventTargetRef>
*/

define functional class <EventTargetRef> (<statically-typed-pointer>)
end class <EventTargetRef>; 

/*
	<EventRef>
*/

define functional class <EventRef> (<statically-typed-pointer>)
end class <EventRef>; 

/*
	<EventHandlerRef>
*/

define functional class <EventHandlerRef> (<statically-typed-pointer>)
end class <EventHandlerRef>; 

/*
	<EventHandlerCallRef>
*/

define functional class <EventHandlerCallRef> (<statically-typed-pointer>)
end class <EventHandlerCallRef>; 

/*
	<EventQueueRef> 
*/

define functional class <EventQueueRef>  (<statically-typed-pointer>)
end class <EventQueueRef> ; 

/*
	<EventLoopTimerRef>
*/

define functional class <EventLoopTimerRef> (<statically-typed-pointer>)
end class;

// We can sizeof() this

/*
	<EventTypeSpec*>
*/

define functional class <EventTypeSpec*> (<statically-typed-pointer>)
end class;

/*
	content-size
	The size of object a <EventTypeSpec*> contains
*/

define method content-size( cls == <EventTypeSpec*> )
=>( result :: <integer> )
	c-expr( int: "sizeof(EventTypeSpec)" );
end method content-size;


/*
	Accessors for the eventKind and eventClass components of an <EventTypeSpec*>
*/

define method eventClass-value (es :: <EventTypeSpec*>) 
=> (eventClass :: <integer>);
	unsigned-long-at(es, offset: 0);
end method eventClass-value;


define method eventClass-value-setter (value :: <integer>, es :: <EventTypeSpec*>) 
=> (value :: <integer>);
	unsigned-long-at(es, offset: 0) := value;
	value;
end method eventClass-value-setter;


define method eventKind-value (es :: <EventTypeSpec*>) 
=> (eventKind :: <integer>);
	unsigned-long-at(es, offset: 4);
end method eventKind-value;


define method eventKind-value-setter (value :: <integer>, es :: <EventTypeSpec*>) 
=> (value :: <integer>);
	unsigned-long-at(es, offset: 4) := value;
	value;
end method eventKind-value-setter;

/*
	initialize <EventTypeSpec*>
*/

define method initialize( es :: <EventTypeSpec*>, 
													#key eventClass :: <integer> = 0, 
													eventKind :: <integer> = 0 )
=> ( result :: <EventTypeSpec*> )
	next-method();

  es.eventClass-value := eventClass;
  es.eventKind-value := eventKind;
  
  es;
end method initialize;


// Values

// Errors

define constant $eventNotHandledErr :: <integer> = c-expr( int: "eventNotHandledErr" );

// Types
  
// Mouse Buttons

define constant $kEventMouseButtonPrimary :: <integer> = 1;
define constant $kEventMouseButtonSecondary :: <integer> = 2;
define constant $kEventMouseButtonTertiary :: <integer> = 3;
  
// Mouse Wheel Axis

define constant $kEventMouseWheelAxisX :: <integer> = 0;
define constant $kEventMouseWheelAxisY :: <integer> = 1;

// Event Priority

define constant EventPriority = <SInt16>;

define constant $kEventPriorityLow :: <integer> = 0;
define constant $kEventPriorityStandard :: <integer> = 1;
define constant $kEventPriorityHigh :: <integer> = 2;

// Application Activation

define constant $kEventAppActivated :: <integer> = 1;		// Suspend
define constant $kEventAppDeactivated :: <integer> = 2;		// Resume
define constant $kEventAppQuit :: <integer> = 3;
define constant $kEventAppLaunchNotification :: <integer> = 4;

// Event Classes

define constant $kEventClassMouse :: <integer> = as( <OSType>, "mous");
define constant $kEventClassKeyboard :: <integer> = as( <OSType>, "keyb");
define constant $kEventClassTextInput :: <integer> = as( <OSType>, "text");
define constant $kEventClassApplication :: <integer> = as( <OSType>, "appl");
define constant $kEventClassEPPC :: <integer> = as( <OSType>, "eppc");
define constant $kEventClassMenu :: <integer> = as( <OSType>, "menu");
define constant $kEventClassWindow :: <integer> = as( <OSType>, "wind");
define constant $kEventClassControl :: <integer> = as( <OSType>, "cntl");
define constant $kEventClassCommand :: <integer> = as( <OSType>, "cmds");
define constant $kEventClassTablet :: <integer> = as( <OSType>, "tblt");

// kEventMenuBeginTracking

define constant $kEventMenuBeginTracking :: <integer> = 1;
define constant $kEventMenuEndTracking :: <integer> = 2;
define constant $kEventMenuChangeTrackingMode :: <integer> = 3;
define constant $kEventMenuOpening :: <integer> = 4;
define constant $kEventMenuClosed :: <integer> = 5;
define constant $kEventMenuTargetItem :: <integer> = 6;
define constant $kEventMenuMatchKey :: <integer> = 7;
define constant $kEventMenuEnableItems :: <integer> = 8;

// kEventMouseDown

define constant $kEventMouseDown :: <integer> = 1;
define constant $kEventMouseUp :: <integer> = 2;
define constant $kEventMouseMoved :: <integer> = 5;
define constant $kEventMouseDragged :: <integer> = 6;
define constant $kEventMouseWheelMoved :: <integer> = 10;

// kEventProcessCommand
  
define constant $kEventProcessCommand :: <integer> = 1;
define constant $kEventCommandProcess :: <integer> = 1;
define constant $kEventCommandUpdateStatus :: <integer> = 2;

// kEventRawKeyDown

define constant $kEventRawKeyDown :: <integer> = 1;
define constant $kEventRawKeyRepeat :: <integer> = 2;
define constant $kEventRawKeyUp :: <integer> = 3;
define constant $kEventRawKeyModifiersChanged :: <integer> = 4;

// kEventUpdateActiveInputArea

define constant $kEventUpdateActiveInputArea :: <integer> = 1;
define constant $kEventUnicodeForKeyEvent :: <integer> = 2;
define constant $kEventOffsetToPos :: <integer> = 3;
define constant $kEventPosToOffset :: <integer> = 4;
define constant $kEventShowHideBottomWindow :: <integer> = 5;
define constant $kEventGetSelectedText :: <integer> = 6;

// Windows

define constant $kEventWindowUpdate :: <integer> = 1;
define constant $kEventWindowDrawContent :: <integer> = 2;
define constant $kEventWindowActivated :: <integer> = 5;
define constant $kEventWindowDeactivated :: <integer> = 6;
define constant $kEventWindowGetClickActivation :: <integer> = 7;
define constant $kEventWindowShowing :: <integer> = 22;
define constant $kEventWindowHiding :: <integer> = 23;
define constant $kEventWindowShown :: <integer> = 24;
define constant $kEventWindowHidden :: <integer> = 25;
define constant $kEventWindowBoundsChanging :: <integer> = 26;
define constant $kEventWindowBoundsChanged :: <integer> = 27;
define constant $kEventWindowResizeStarted :: <integer> = 28;
define constant $kEventWindowResizeCompleted :: <integer> = 29;
define constant $kEventWindowDragStarted :: <integer> = 30;
define constant $kEventWindowDragCompleted :: <integer> = 31;
define constant $kWindowBoundsChangeUserDrag :: <integer> = 1;			// Are these 4 right?
define constant $kWindowBoundsChangeUserResize :: <integer> = 2;
define constant $kWindowBoundsChangeSizeChanged :: <integer> = 4;
define constant $kWindowBoundsChangeOriginChanged :: <integer> = 8;
define constant $kEventWindowClickDragRgn :: <integer> = 32;
define constant $kEventWindowClickResizeRgn :: <integer> = 33;
define constant $kEventWindowClickCollapseRgn :: <integer> = 34;
define constant $kEventWindowClickCloseRgn :: <integer> = 35;
define constant $kEventWindowClickZoomRgn :: <integer> = 36;
define constant $kEventWindowClickContentRgn :: <integer> = 37;
define constant $kEventWindowClickProxyIconRgn :: <integer> = 38;
define constant $kEventWindowCursorChange :: <integer> = 40;
define constant $kEventWindowCollapse :: <integer> = 66;
define constant $kEventWindowCollapsed :: <integer> = 67;
define constant $kEventWindowCollapseAll :: <integer> = 68;
define constant $kEventWindowExpand :: <integer> = 69;
define constant $kEventWindowExpanded :: <integer> = 70;
define constant $kEventWindowExpandAll :: <integer> = 71;
define constant $kEventWindowClose :: <integer> = 72;
define constant $kEventWindowClosed :: <integer> = 73;
define constant $kEventWindowCloseAll :: <integer> = 74;
define constant $kEventWindowZoom :: <integer> = 75;
define constant $kEventWindowZoomed :: <integer> = 76;
define constant $kEventWindowZoomAll :: <integer> = 77;
define constant $kEventWindowContextualMenuSelect :: <integer> = 78;
define constant $kEventWindowPathSelect :: <integer> = 79;
define constant $kEventWindowGetIdealSize :: <integer> = 80;
define constant $kEventWindowGetMinimumSize :: <integer> = 81;
define constant $kEventWindowGetMaximumSize :: <integer> = 82;
define constant $kEventWindowConstrain :: <integer> = 83;
define constant $kEventWindowHandleContentClick :: <integer> = 85;
define constant $kEventWindowProxyBeginDrag :: <integer> = 128;
define constant $kEventWindowProxyEndDrag :: <integer> = 129;
define constant $kEventWindowFocusAcquired :: <integer> = 200;
define constant $kEventWindowFocusRelinquish :: <integer> = 201;
define constant $kEventWindowDrawFrame :: <integer> = 1000;
define constant $kEventWindowDrawPart :: <integer> = 1001;
define constant $kEventWindowGetRegion :: <integer> = 1002;
define constant $kEventWindowHitTest :: <integer> = 1003;
define constant $kEventWindowInit :: <integer> = 1004;
define constant $kEventWindowDispose :: <integer> = 1005;
define constant $kEventWindowDragHilite :: <integer> = 1006;
define constant $kEventWindowModified :: <integer> = 1007;
define constant $kEventWindowSetupProxyDragImage :: <integer> = 1008;
define constant $kEventWindowStateChanged :: <integer> = 1009;
define constant $kEventWindowMeasureTitle :: <integer> = 1010;
define constant $kEventWindowDrawGrowBox :: <integer> = 1011;
define constant $kEventWindowGetGrowImageRegion :: <integer> = 1012;
define constant $kEventWindowPaint :: <integer> = 1013;
  
    
/*
	Event Parameters
*/

define constant $kEventParamDirectObject :: <integer> = c-expr(int: "kEventParamDirectObject");
define constant $kEventParamWindowRef :: <integer> = c-expr(int: "kEventParamWindowRef");
define constant $kEventParamGrafPort :: <integer> = c-expr(int: "kEventParamGrafPort");
define constant $kEventParamDragRef :: <integer> = c-expr(int: "kEventParamDragRef");
define constant $kEventParamMenuRef :: <integer> = c-expr(int: "kEventParamMenuRef");
define constant $kEventParamEventRef :: <integer> = c-expr(int: "kEventParamEventRef");
define constant $kEventParamControlRef :: <integer> = c-expr(int: "kEventParamControlRef");
define constant $kEventParamRgnHandle :: <integer> = c-expr(int: "kEventParamRgnHandle");
define constant $kEventParamEnabled :: <integer> = c-expr(int: "kEventParamEnabled");
define constant $kEventParamDimensions :: <integer> = c-expr(int: "kEventParamDimensions");
define constant $kEventParamAvailableBounds :: <integer> = c-expr(int: "kEventParamAvailableBounds");
define constant $kEventParamAEEventID :: <integer> = c-expr(int: "kEventParamAEEventID");
define constant $kEventParamAEEventClass :: <integer> = c-expr(int: "kEventParamAEEventClass");
define constant $kEventParamCGContextRef :: <integer> = c-expr(int: "kEventParamCGContextRef");
define constant $typeWindowRef :: <integer> = c-expr(int: "typeWindowRef");
define constant $typeGrafPtr :: <integer> = c-expr(int: "typeGrafPtr");
define constant $typeGWorldPtr :: <integer> = c-expr(int: "typeGWorldPtr");
define constant $typeDragRef :: <integer> = c-expr(int: "typeDragRef");
define constant $typeMenuRef :: <integer> = c-expr(int: "typeMenuRef");
define constant $typeControlRef :: <integer> = c-expr(int: "typeControlRef");
define constant $typeCollection :: <integer> = c-expr(int: "typeCollection");
define constant $typeQDRgnHandle :: <integer> = c-expr(int: "typeQDRgnHandle");
define constant $typeQDPoint :: <integer> = c-expr(int: "typeQDPoint");
define constant $typeOSStatus :: <integer> = c-expr(int: "typeOSStatus");
define constant $typeCGContextRef :: <integer> = c-expr(int: "typeCGContextRef");
define constant $typeChar :: <integer> = c-expr(int: "typeChar");
define constant $kEventParamMouseLocation :: <integer> = c-expr(int: "kEventParamMouseLocation");
define constant $kEventParamMouseButton :: <integer> = c-expr(int: "kEventParamMouseButton");
define constant $kEventParamClickCount :: <integer> = c-expr(int: "kEventParamClickCount");
define constant $kEventParamMouseWheelAxis :: <integer> = c-expr(int: "kEventParamMouseWheelAxis");
define constant $kEventParamMouseWheelDelta :: <integer> = c-expr(int: "kEventParamMouseWheelDelta");
define constant $kEventParamMouseDelta :: <integer> = c-expr(int: "kEventParamMouseDelta");
define constant $typeMouseButton :: <integer> = c-expr(int: "typeMouseButton");
define constant $typeMouseWheelAxis :: <integer> = c-expr(int: "typeMouseWheelAxis");
define constant $kEventParamKeyCode :: <integer> = c-expr(int: "kEventParamKeyCode");
define constant $kEventParamKeyMacCharCodes :: <integer> = c-expr(int: "kEventParamKeyMacCharCodes");
define constant $kEventParamKeyModifiers :: <integer> = c-expr(int: "kEventParamKeyModifiers");
define constant $kEventParamKeyUnicodes :: <integer> = c-expr(int: "kEventParamKeyUnicodes");
define constant $typeEventHotKeyID :: <integer> = c-expr(int: "typeEventHotKeyID");
define constant $kEventParamTextInputSendRefCon :: <integer> = c-expr(int: "kEventParamTextInputSendRefCon");
define constant $kEventParamTextInputSendComponentInstance :: <integer> = c-expr(int: "kEventParamTextInputSendComponentInstance");
define constant $kEventParamTextInputSendSLRec :: <integer> = c-expr(int: "kEventParamTextInputSendSLRec");
define constant $kEventParamTextInputReplySLRec :: <integer> = c-expr(int: "kEventParamTextInputReplySLRec");
define constant $kEventParamTextInputSendText :: <integer> = c-expr(int: "kEventParamTextInputSendText");
define constant $kEventParamTextInputReplyText :: <integer> = c-expr(int: "kEventParamTextInputReplyText");
define constant $kEventParamTextInputSendUpdateRng :: <integer> = c-expr(int: "kEventParamTextInputSendUpdateRng");
define constant $kEventParamTextInputSendHiliteRng :: <integer> = c-expr(int: "kEventParamTextInputSendHiliteRng");
define constant $kEventParamTextInputSendClauseRng :: <integer> = c-expr(int: "kEventParamTextInputSendClauseRng");
define constant $kEventParamTextInputSendPinRng :: <integer> = c-expr(int: "kEventParamTextInputSendPinRng");
define constant $kEventParamTextInputSendFixLen :: <integer> = c-expr(int: "kEventParamTextInputSendFixLen");
define constant $kEventParamTextInputSendLeadingEdge :: <integer> = c-expr(int: "kEventParamTextInputSendLeadingEdge");
define constant $kEventParamTextInputReplyLeadingEdge :: <integer> = c-expr(int: "kEventParamTextInputReplyLeadingEdge");
define constant $kEventParamTextInputSendTextOffset :: <integer> = c-expr(int: "kEventParamTextInputSendTextOffset");
define constant $kEventParamTextInputReplyTextOffset :: <integer> = c-expr(int: "kEventParamTextInputReplyTextOffset");
define constant $kEventParamTextInputReplyRegionClass :: <integer> = c-expr(int: "kEventParamTextInputReplyRegionClass");
define constant $kEventParamTextInputSendCurrentPoint :: <integer> = c-expr(int: "kEventParamTextInputSendCurrentPoint");
define constant $kEventParamTextInputSendDraggingMode :: <integer> = c-expr(int: "kEventParamTextInputSendDraggingMode");
define constant $kEventParamTextInputReplyPoint :: <integer> = c-expr(int: "kEventParamTextInputReplyPoint");
define constant $kEventParamTextInputReplyFont :: <integer> = c-expr(int: "kEventParamTextInputReplyFont");
define constant $kEventParamTextInputReplyPointSize :: <integer> = c-expr(int: "kEventParamTextInputReplyPointSize");
define constant $kEventParamTextInputReplyLineHeight :: <integer> = c-expr(int: "kEventParamTextInputReplyLineHeight");
define constant $kEventParamTextInputReplyLineAscent :: <integer> = c-expr(int: "kEventParamTextInputReplyLineAscent");
define constant $kEventParamTextInputReplyTextAngle :: <integer> = c-expr(int: "kEventParamTextInputReplyTextAngle");
define constant $kEventParamTextInputSendShowHide :: <integer> = c-expr(int: "kEventParamTextInputSendShowHide");
define constant $kEventParamTextInputReplyShowHide :: <integer> = c-expr(int: "kEventParamTextInputReplyShowHide");
define constant $kEventParamTextInputSendKeyboardEvent :: <integer> = c-expr(int: "kEventParamTextInputSendKeyboardEvent");
define constant $kEventParamTextInputSendTextServiceEncoding :: <integer> = c-expr(int: "kEventParamTextInputSendTextServiceEncoding");
define constant $kEventParamTextInputSendTextServiceMacEncoding :: <integer> = c-expr(int: "kEventParamTextInputSendTextServiceMacEncoding");
define constant $kEventParamHICommand :: <integer> = c-expr(int: "kEventParamHICommand");
define constant $typeHICommand :: <integer> = c-expr(int: "typeHICommand");
define constant $kEventParamWindowFeatures :: <integer> = c-expr(int: "kEventParamWindowFeatures");
define constant $kEventParamWindowDefPart :: <integer> = c-expr(int: "kEventParamWindowDefPart");
define constant $kEventParamCurrentBounds :: <integer> = c-expr(int: "kEventParamCurrentBounds");
define constant $kEventParamOriginalBounds :: <integer> = c-expr(int: "kEventParamOriginalBounds");
define constant $kEventParamPreviousBounds :: <integer> = c-expr(int: "kEventParamPreviousBounds");
define constant $kEventParamClickActivation :: <integer> = c-expr(int: "kEventParamClickActivation");
define constant $kEventParamWindowRegionCode :: <integer> = c-expr(int: "kEventParamWindowRegionCode");
define constant $kEventParamWindowDragHiliteFlag :: <integer> = c-expr(int: "kEventParamWindowDragHiliteFlag");
define constant $kEventParamWindowModifiedFlag :: <integer> = c-expr(int: "kEventParamWindowModifiedFlag");
define constant $kEventParamWindowProxyGWorldPtr :: <integer> = c-expr(int: "kEventParamWindowProxyGWorldPtr");
define constant $kEventParamWindowProxyImageRgn :: <integer> = c-expr(int: "kEventParamWindowProxyImageRgn");
define constant $kEventParamWindowProxyOutlineRgn :: <integer> = c-expr(int: "kEventParamWindowProxyOutlineRgn");
define constant $kEventParamWindowStateChangedFlags :: <integer> = c-expr(int: "kEventParamWindowStateChangedFlags");
define constant $kEventParamWindowTitleFullWidth :: <integer> = c-expr(int: "kEventParamWindowTitleFullWidth");
define constant $kEventParamWindowTitleTextWidth :: <integer> = c-expr(int: "kEventParamWindowTitleTextWidth");
define constant $kEventParamWindowGrowRect :: <integer> = c-expr(int: "kEventParamWindowGrowRect");
define constant $kEventParamAttributes :: <integer> = c-expr(int: "kEventParamAttributes");
define constant $typeWindowRegionCode :: <integer> = c-expr(int: "typeWindowRegionCode");
define constant $typeWindowDefPartCode :: <integer> = c-expr(int: "typeWindowDefPartCode");
define constant $typeClickActivationResult :: <integer> = c-expr(int: "typeClickActivationResult");
define constant $kEventParamControlPart :: <integer> = c-expr(int: "kEventParamControlPart");
define constant $kEventParamInitCollection :: <integer> = c-expr(int: "kEventParamInitCollection");
define constant $kEventParamControlMessage :: <integer> = c-expr(int: "kEventParamControlMessage");
define constant $kEventParamControlParam :: <integer> = c-expr(int: "kEventParamControlParam");
define constant $kEventParamControlResult :: <integer> = c-expr(int: "kEventParamControlResult");
define constant $kEventParamControlRegion :: <integer> = c-expr(int: "kEventParamControlRegion");
define constant $kEventParamControlAction :: <integer> = c-expr(int: "kEventParamControlAction");
define constant $kEventParamControlIndicatorDragConstraint :: <integer> = c-expr(int: "kEventParamControlIndicatorDragConstraint");
define constant $kEventParamControlIndicatorRegion :: <integer> = c-expr(int: "kEventParamControlIndicatorRegion");
define constant $kEventParamControlIsGhosting :: <integer> = c-expr(int: "kEventParamControlIsGhosting");
define constant $kEventParamControlIndicatorOffset :: <integer> = c-expr(int: "kEventParamControlIndicatorOffset");
define constant $kEventParamControlClickActivationResult :: <integer> = c-expr(int: "kEventParamControlClickActivationResult");
define constant $kEventParamControlSubControl :: <integer> = c-expr(int: "kEventParamControlSubControl");
define constant $kEventParamControlOptimalBounds :: <integer> = c-expr(int: "kEventParamControlOptimalBounds");
define constant $kEventParamControlOptimalBaselineOffset :: <integer> = c-expr(int: "kEventParamControlOptimalBaselineOffset");
define constant $kEventParamControlDataTag :: <integer> = c-expr(int: "kEventParamControlDataTag");
define constant $kEventParamControlDataBuffer :: <integer> = c-expr(int: "kEventParamControlDataBuffer");
define constant $kEventParamControlDataBufferSize :: <integer> = c-expr(int: "kEventParamControlDataBufferSize");
define constant $kEventParamControlDrawDepth :: <integer> = c-expr(int: "kEventParamControlDrawDepth");
define constant $kEventParamControlDrawInColor :: <integer> = c-expr(int: "kEventParamControlDrawInColor");
define constant $kEventParamControlFeatures :: <integer> = c-expr(int: "kEventParamControlFeatures");
define constant $kEventParamControlPartBounds :: <integer> = c-expr(int: "kEventParamControlPartBounds");
define constant $kEventParamControlOriginalOwningWindow :: <integer> = c-expr(int: "kEventParamControlOriginalOwningWindow");
define constant $kEventParamControlCurrentOwningWindow :: <integer> = c-expr(int: "kEventParamControlCurrentOwningWindow");
define constant $typeControlActionUPP :: <integer> = c-expr(int: "typeControlActionUPP");
define constant $typeIndicatorDragConstraint :: <integer> = c-expr(int: "typeIndicatorDragConstraint");
define constant $typeControlPartCode :: <integer> = c-expr(int: "typeControlPartCode");
define constant $kEventParamCurrentMenuTrackingMode :: <integer> = c-expr(int: "kEventParamCurrentMenuTrackingMode");
define constant $kEventParamNewMenuTrackingMode :: <integer> = c-expr(int: "kEventParamNewMenuTrackingMode");
define constant $kEventParamMenuFirstOpen :: <integer> = c-expr(int: "kEventParamMenuFirstOpen");
define constant $kEventParamMenuItemIndex :: <integer> = c-expr(int: "kEventParamMenuItemIndex");
define constant $kEventParamMenuCommand :: <integer> = c-expr(int: "kEventParamMenuCommand");
define constant $kEventParamEnableMenuForKeyEvent :: <integer> = c-expr(int: "kEventParamEnableMenuForKeyEvent");
define constant $kEventParamMenuEventOptions :: <integer> = c-expr(int: "kEventParamMenuEventOptions");
define constant $typeMenuItemIndex :: <integer> = c-expr(int: "typeMenuItemIndex");
define constant $typeMenuCommand :: <integer> = c-expr(int: "typeMenuCommand");
define constant $typeMenuTrackingMode :: <integer> = c-expr(int: "typeMenuTrackingMode");
define constant $typeMenuEventOptions :: <integer> = c-expr(int: "typeMenuEventOptions");
define constant $kEventParamProcessID :: <integer> = c-expr(int: "kEventParamProcessID");
define constant $kEventParamLaunchRefCon :: <integer> = c-expr(int: "kEventParamLaunchRefCon");
define constant $kEventParamLaunchErr :: <integer> = c-expr(int: "kEventParamLaunchErr");
define constant $kEventParamTabletPointerRec :: <integer> = c-expr(int: "kEventParamTabletPointerRec");
define constant $kEventParamTabletProximityRec :: <integer> = c-expr(int: "kEventParamTabletProximityRec");
define constant $typeTabletPointerRec :: <integer> = c-expr(int: "typeTabletPointerRec");
define constant $typeTabletProximityRec :: <integer> = c-expr(int: "typeTabletProximityRec");

/*
  Control Events
*/    

define constant $kEventControlInitialize :: <integer> = c-expr(int: "kEventControlInitialize");
define constant $kEventControlDispose :: <integer> = c-expr(int: "kEventControlDispose");
define constant $kEventControlGetOptimalBounds :: <integer> = c-expr(int: "kEventControlGetOptimalBounds");
//define constant $kEventControlDefInitialize :: <integer> = c-expr(int: "kEventControlDefInitialize");
//define constant $kEventControlDefDispose :: <integer> = c-expr(int: "kEventControlDefDispose");
define constant $kEventControlHit :: <integer> = c-expr(int: "kEventControlHit");
define constant $kEventControlSimulateHit :: <integer> = c-expr(int: "kEventControlSimulateHit");
define constant $kEventControlHitTest :: <integer> = c-expr(int: "kEventControlHitTest");
define constant $kEventControlDraw :: <integer> = c-expr(int: "kEventControlDraw");
define constant $kEventControlApplyBackground :: <integer> = c-expr(int: "kEventControlApplyBackground");
define constant $kEventControlApplyTextColor :: <integer> = c-expr(int: "kEventControlApplyTextColor");
define constant $kEventControlSetFocusPart :: <integer> = c-expr(int: "kEventControlSetFocusPart");
define constant $kEventControlGetFocusPart :: <integer> = c-expr(int: "kEventControlGetFocusPart");
define constant $kEventControlActivate :: <integer> = c-expr(int: "kEventControlActivate");
define constant $kEventControlDeactivate :: <integer> = c-expr(int: "kEventControlDeactivate");
define constant $kEventControlSetCursor :: <integer> = c-expr(int: "kEventControlSetCursor");
define constant $kEventControlContextualMenuClick :: <integer> = c-expr(int: "kEventControlContextualMenuClick");
define constant $kEventControlClick :: <integer> = c-expr(int: "kEventControlClick");
define constant $kEventControlTrack :: <integer> = c-expr(int: "kEventControlTrack");
define constant $kEventControlGetScrollToHereStartPoint :: <integer> = c-expr(int: "kEventControlGetScrollToHereStartPoint");
define constant $kEventControlGetIndicatorDragConstraint :: <integer> = c-expr(int: "kEventControlGetIndicatorDragConstraint");
define constant $kEventControlIndicatorMoved :: <integer> = c-expr(int: "kEventControlIndicatorMoved");
define constant $kEventControlGhostingFinished :: <integer> = c-expr(int: "kEventControlGhostingFinished");
define constant $kEventControlGetActionProcPart :: <integer> = c-expr(int: "kEventControlGetActionProcPart");
define constant $kEventControlGetPartRegion :: <integer> = c-expr(int: "kEventControlGetPartRegion");
define constant $kEventControlGetPartBounds :: <integer> = c-expr(int: "kEventControlGetPartBounds");
define constant $kEventControlSetData :: <integer> = c-expr(int: "kEventControlSetData");
define constant $kEventControlGetData :: <integer> = c-expr(int: "kEventControlGetData");
define constant $kEventControlValueFieldChanged :: <integer> = c-expr(int: "kEventControlValueFieldChanged");
define constant $kEventControlAddedSubControl :: <integer> = c-expr(int: "kEventControlAddedSubControl");
define constant $kEventControlRemovingSubControl :: <integer> = c-expr(int: "kEventControlRemovingSubControl");
define constant $kEventControlBoundsChanged :: <integer> = c-expr(int: "kEventControlBoundsChanged");
define constant $kEventControlOwningWindowChanged :: <integer> = c-expr(int: "kEventControlOwningWindowChanged");
define constant $kEventControlArbitraryMessage :: <integer> = c-expr(int: "kEventControlArbitraryMessage");
define constant $kControlBoundsChangeSizeChanged :: <integer> = c-expr(int: "kControlBoundsChangeSizeChanged");
define constant $kControlBoundsChangePositionChanged :: <integer> = c-expr(int: "kControlBoundsChangePositionChanged");
    
    
// Methods

/*
		NewEventHandlerUPP
*/

define method NewEventHandlerUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <EventHandlerUPP> )
	let result = call-out( "NewEventHandlerUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <EventHandlerUPP>, pointer: result );
end method NewEventHandlerUPP;

define method NewEventHandlerUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <EventHandlerUPP> )
	let result = call-out( "NewEventHandlerUPP", ptr:, ptr: userRoutine.raw-value );
	make( <EventHandlerUPP>, pointer: result );
end method NewEventHandlerUPP;

/*
		DisposeEventHandlerUPP
*/

define method DisposeEventHandlerUPP( userUPP :: <EventHandlerUPP> )
=> ()
	call-out( "DisposeEventHandlerUPP", void:, ptr: userUPP.raw-value );
end method DisposeEventHandlerUPP;

/*
    RunApplicationEventLoop
*/

define method RunApplicationEventLoop() => ()
    call-out( "RunApplicationEventLoop", void: );
end method RunApplicationEventLoop;

/*
    RunCurrentEventLoop
*/

define method RunCurrentEventLoop(inTimeout :: <float>)
=> ( result :: <OSStatus> )
	as( <OSStatus>, call-out( "RunCurrentEventLoop", int:, double: inTimeout) );
end method RunCurrentEventLoop;

/*
  QuitApplicationEventLoop
*/

define method QuitApplicationEventLoop() => ()
    call-out( "QuitApplicationEventLoop", void: );
end method QuitApplicationEventLoop;

/*
    InstallWindowEventHandler
*/

define method InstallWindowEventHandler( inTarget :: <WindowRef>, 
                                        inHandler :: <EventHandlerUPP>,
                                        inNumTypes :: <integer>, 
                                        inList :: type-union( <c-vector>, <EventTypeSpec*> ), // <EventTypeSpec*>,
                                        inUserData :: <statically-typed-pointer> ) 
=> ( result :: <OSStatus>, outRef :: <EventHandlerRef> )
    let temp :: <handle> = make( <Handle> );
    let status = call-out( "InstallWindowEventHandler", int:, ptr: inTarget.raw-value, ptr: inHandler.raw-value,
                            int: inNumTypes, ptr: inList.raw-value, ptr: inUserData.raw-value, ptr: temp.raw-value );
    values( as( <OSStatus>, status ), pointer-at( temp, class: <EventHandlerRef>, offset: 0 ) );
end method InstallWindowEventHandler;

/*
    InstallControlEventHandler
*/

define method InstallControlEventHandler( inTarget :: <ControlHandle>, 
                                        inHandler :: <EventHandlerUPP>,
                                        inNumTypes :: <integer>, 
                                        inList :: type-union( <c-vector>, <EventTypeSpec*> ), // <EventTypeSpec*>,
                                        inUserData :: <statically-typed-pointer> ) 
=> ( result :: <OSStatus>, outRef :: <EventHandlerRef> )
    let temp :: <handle> = make( <Handle> );
    let status = call-out( "InstallControlEventHandler", int:, ptr: inTarget.raw-value, ptr: inHandler.raw-value,
                            int: inNumTypes, ptr: inList.raw-value, ptr: inUserData.raw-value, ptr: temp.raw-value );
    values( as( <OSStatus>, status ), pointer-at( temp, class: <EventHandlerRef>, offset: 0 ) );
end method InstallControlEventHandler;

define method AddEventTypesToHandler( inHandlerRef :: <EventHandlerRef>, inNumTypes :: <integer>,
                                      inList :: type-union( <c-vector>, <EventTypeSpec*> ) ) // <EventTypeSpec*>
=>( result :: <OSStatus> )
  let result = call-out( "AddEventTypesToHandler", int:, ptr: inHandlerRef.raw-value, int: inNumTypes,
                         ptr: inList.raw-value);
  as( <OSStatus>, result );
end method AddEventTypesToHandler;
 
define method CallNextEventHandler( inCallRef :: <EventHandlerCallRef>, inEvent :: <EventRef> )
=>( result :: <OSStatus> )
  let result = call-out( "CallNextEventHandler", int:, ptr: inCallRef.raw-value, ptr: inEvent.raw-value );
  as( <OSStatus>, result );
end method CallNextEventHandler;

define method ConvertEventRefToEventRecord( inEvent :: <EventRef> )
=>( outEvent :: <EventRecord*>, result :: <boolean> )
  let event-ptr :: <Handle> = make( <Handle> );
  let result = call-out( "ConvertEventRefToEventRecord", int:, ptr: inEvent.raw-value, ptr: event-ptr.raw-value );
  values( make( <EventRecord*>, pointer: event-ptr.raw-value ),
          if( result = 0 )
            #f;
          else
            #t;
          end if );
end method ConvertEventRefToEventRecord;

define method FlushEventQueue( inQueue :: <EventQueueRef> )
=>( result :: <OSStatus> )
  let result = call-out( "FlushEventQueue", int:, ptr: inQueue.raw-value );
  as( <OSStatus>, result );
end method FlushEventQueue;

define method GetEventClass( inEvent :: <EventRef> )
=>( result :: <integer> )
  call-out( "GetEventClass", int:, ptr: inEvent.raw-value );
end method GetEventClass;

define method GetEventKind( inEvent :: <EventRef> )
=>( result :: <integer> )
  call-out( "GetEventKind", int:, ptr: inEvent.raw-value );
end method GetEventKind;

define method GetEventParameter( inEvent :: <EventRef>, 
																 inName :: <integer>, 
																 inDesiredType :: <integer>,
																 inBufferSize :: <integer>,
																 ioBuffer :: <statically-typed-pointer> )
=> ( result :: <OSStatus> )
	let result = call-out( "GetEventParameter", int:, ptr: inEvent.raw-value,
												 int: inName, int: inDesiredType, ptr: $NULL.raw-value,
												 int: inBufferSize, ptr: $NULL.raw-value, ptr: ioBuffer.raw-value );
	as( <OSStatus>, result );
end method GetEventParameter;

define method GetMainEventQueue()
=>( result :: <EventQueueRef> )
  let temp :: <Handle> = make( <Handle>, pointer: call-out( "GetMainEventQueue", ptr: ) );
  pointer-at( temp, class: <EventQueueRef>, offset: 0 );
end method GetMainEventQueue;

define method GetMenuEventTarget( inMenu :: <MenuRef> )
=>( result :: <EventTargetRef> )
  let temp :: <Handle> = make( <Handle>, pointer: call-out( "GetMenuEventTarget", ptr:, ptr: inMenu.raw-value ) );
  pointer-at( temp, class: <EventTargetRef>, offset: 0 );
end method GetMenuEventTarget;

define method GetMainEventLoop()
=>( result :: <EventLoopRef> )
  let temp :: <Handle> = make( <Handle>, pointer: call-out( "GetMainEventLoop", ptr: ) );
  pointer-at( temp, class: <EventLoopRef>, offset: 0 );
end method GetMainEventLoop;

define method GetUserFocusEventTarget()
=>( result :: <EventTargetRef> )
  let temp :: <Handle> = make( <Handle>, pointer: call-out( "GetUserFocusEventTarget", ptr: ) );
  pointer-at( temp, class: <EventTargetRef>, offset: 0 );
end method GetUserFocusEventTarget;

define method GetWindowEventTarget( inWindow :: <WindowRef> )
=>( result :: <EventTargetRef> )
  let result = call-out( "GetWindowEventTarget", ptr:, ptr: inWindow.raw-value );
  make( <EventTargetRef>, pointer: result );
end method GetWindowEventTarget;

define method InstallEventHandler( inTarget :: <EventTargetRef>,
                                    inHandler :: <EventHandlerUPP>,
                                    inNumTypes :: <integer>,
                                    inList :: type-union( <c-vector>, <EventTypeSpec*> ),// <EventTypeSpec*>,
                                    inUserData :: <statically-typed-pointer> )
=>( result :: <OSStatus>, outRef :: <EventHandlerRef> )
  let temp :: <handle> = make( <Handle> );
  let result = call-out( "InstallEventHandler", int:, ptr: inTarget.raw-value, ptr: inHandler.raw-value,
  												int: inNumTypes,
                          ptr: inList.raw-value, ptr: inUserData.raw-value, ptr: temp.raw-value );
  values( as( <OSStatus>, result ), pointer-at( temp, class: <EventHandlerRef>, offset: 0 ) );
end method InstallEventHandler; 

/*
OSStatus InstallEventLoopTimer(EventLoopRef inEventLoop,
 EventTimerInterval inFireDelay,
 EventTimerInterval inInterval,
 EventLoopTimerUPP inTimerProc,
 void * inTimerData,
 EventLoopTimerRef * outTimer);
 */
 
define method QuitEventLoop( inEventLoop :: <EventLoopRef> )
=>( result :: <OSStatus> )
  let result = call-out( "QuitEventLoop", int:, ptr: inEventLoop.raw-value );
  as( <OSStatus>, result );
end method QuitEventLoop;

define method RemoveEventHandler( inHandlerRef :: <EventHandlerRef> )
=>( result :: <OSStatus> )
  let result = call-out( "RemoveEventHandler", int:, ptr: inHandlerRef.raw-value );
  as( <OSStatus>, result );
end method RemoveEventHandler;


define method RemoveEventLoopTimer( inTimer :: <EventLoopTimerRef> )
=>( result :: <OSStatus> )
  let result = call-out( "RemoveEventLoopTimer", int:, ptr: inTimer.raw-value );
  as( <OSStatus>, result );
end method RemoveEventLoopTimer;

// Get/Set UserFocus

define method GetUserFocusWindow()
=> ( result :: <WindowRef> )
	let temp :: <Handle> = make( <Handle>, pointer: call-out( "GetUserFocusWindow", ptr: ) );
	pointer-at( temp, class: <WindowRef>, offset: 0 );
end method GetUserFocusWindow;

define method SetUserFocusWindow( inWindow :: <WindowRef> )
=> ( result :: <OSStatus> )
	as( <OSStatus>, call-out( "SetUserFocusWindow", int:, ptr: inWindow.raw-value ) );
end method SetUserFocusWindow;

// Other Event handling 

define constant $kEventDurationForever = -1.0;
define constant $kEventDurationNoWait = 0.0;

define method GetEventDispatcherTarget()
=> ( result :: <EventTargetRef> )
	make( <EventTargetRef>, pointer: call-out( "GetEventDispatcherTarget", ptr: ) );
end method GetEventDispatcherTarget;

define method ReceiveNextEvent(inNumTypes :: <integer>, inList :: <statically-typed-pointer>,
                               inTimeOut :: <float>, inPullEvent :: <boolean> )
=> ( status :: <OSStatus>, event-ref :: <EventRef> )
	let temp :: <Handle> = make( <Handle> );
  let result = call-out( "ReceiveNextEvent", int:, int: inNumTypes, ptr: inList.raw-value, 
                                              double: inTimeOut,
                                              int: if(inPullEvent) 1 else 0 end, 
                                              ptr: temp.raw-value);
	values(as(<OSStatus>, result), pointer-at( temp, class: <EventRef>, offset: 0 ));
end method;

define method SendEventToEventTarget( inEvent :: <EventRef>, inTarget :: <EventTargetRef> )
=> (status :: <OSStatus>)
	as(<OSStatus>, call-out( "SendEventToEventTarget", int:, ptr: inEvent.raw-value, 
                            ptr: inTarget.raw-value ) );
end method SendEventToEventTarget;

define method ReleaseEvent( inEvent :: <EventRef> )
=> ()
	call-out( "ReleaseEvent", int:, ptr: inEvent.raw-value );
end method ReleaseEvent;

/*
  GetControlEventTarget
*/

define method GetControlEventTarget( inControl :: <ControlHandle> )
=> (status :: <EventTargetRef>)
	make(<EventTargetRef>, pointer: call-out( "GetControlEventTarget", ptr:, ptr: inControl.raw-value ) );
end method GetControlEventTarget;


