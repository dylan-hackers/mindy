module: dylan-user

/*
	carbon-events
*/

define module carbon-events
        use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	use windows;
  use events;
  use menus;

	export 		// carbon-events
  
            // UPPs
            
            <EventHandlerUPP>, <EventComparatorUPP>, <EventLoopTimerUPP>,
  
            // Various Classes
            
            <EventLoopRef>, <EventRef>, <EventQueueRef>, <EventTargetRef>, 
            <EventLoopTimerRef>, <EventHandlerRef>, <EventHandlerCallRef>,
            
            // Errors
            
            $eventNotHandledErr,
            
            // Type codes
        
            // Mouse Buttons
        
            $kEventMouseButtonPrimary, $kEventMouseButtonSecondary, $kEventMouseButtonTertiary,
  
            // Mouse Wheel Axis

            $kEventMouseWheelAxisX, $kEventMouseWheelAxisY,

            // Event Priority

//            <EventPriority>,

            $kEventPriorityLow, $kEventPriorityStandard, $kEventPriorityHigh,

            // Application Activation

            $kEventAppActivated, $kEventAppDeactivated, $kEventAppQuit, 
            $kEventAppLaunchNotification,
            
            // kEventClassMouse
            
            $kEventClassMouse, $kEventClassKeyboard, $kEventClassTextInput, 
            $kEventClassApplication, $kEventClassEPPC, $kEventClassMenu, $kEventClassWindow, 
            $kEventClassControl, $kEventClassCommand, $kEventClassTablet,
                        
            // kEventMenuBeginTracking
            
            $kEventMenuBeginTracking, $kEventMenuEndTracking, $kEventMenuChangeTrackingMode, 
            $kEventMenuOpening, $kEventMenuClosed, $kEventMenuTargetItem, $kEventMenuMatchKey,
            $kEventMenuEnableItems,
            
            // kEventMouseDown
            
            $kEventMouseDown, $kEventMouseUp, $kEventMouseMoved, $kEventMouseDragged, 
            $kEventMouseWheelMoved,
            
            // kEventProcessCommand
            
            $kEventProcessCommand, $kEventCommandProcess, $kEventCommandUpdateStatus,
            
            // kEventRawKeyDown
            
            $kEventRawKeyDown, $kEventRawKeyRepeat, $kEventRawKeyUp, $kEventRawKeyModifiersChanged,
            
            // kEventUpdateActiveInputArea
            
            $kEventUpdateActiveInputArea, $kEventUnicodeForKeyEvent, $kEventOffsetToPos, 
            $kEventPosToOffset, $kEventShowHideBottomWindow, $kEventGetSelectedText,
            
            // Windows
            
           	$kEventWindowUpdate, $kEventWindowDrawContent, $kEventWindowActivated, $kEventWindowDeactivated,
						$kEventWindowGetClickActivation, $kEventWindowShowing, $kEventWindowHiding, $kEventWindowShown,
						$kEventWindowHidden, $kEventWindowBoundsChanging, $kEventWindowBoundsChanged, 
						$kEventWindowResizeStarted, $kEventWindowResizeCompleted, $kEventWindowDragStarted, 
						$kEventWindowDragCompleted, $kWindowBoundsChangeUserDrag, $kWindowBoundsChangeUserResize, 
						$kWindowBoundsChangeSizeChanged, $kWindowBoundsChangeOriginChanged, $kEventWindowClickDragRgn, 
						$kEventWindowClickResizeRgn, $kEventWindowClickCollapseRgn, $kEventWindowClickCloseRgn, 
						$kEventWindowClickZoomRgn, $kEventWindowClickContentRgn, $kEventWindowClickProxyIconRgn,
						$kEventWindowCursorChange, $kEventWindowCollapse, $kEventWindowCollapsed, 
						$kEventWindowCollapseAll, $kEventWindowExpand, $kEventWindowExpanded, $kEventWindowExpandAll, 
						$kEventWindowClose, $kEventWindowClosed, $kEventWindowCloseAll, $kEventWindowZoom, 
						$kEventWindowZoomed, $kEventWindowZoomAll, $kEventWindowContextualMenuSelect, 
						$kEventWindowPathSelect, $kEventWindowGetIdealSize, $kEventWindowGetMinimumSize, 
						$kEventWindowGetMaximumSize, $kEventWindowConstrain, $kEventWindowHandleContentClick, 
						$kEventWindowProxyBeginDrag, $kEventWindowProxyEndDrag, $kEventWindowFocusAcquired, 
						$kEventWindowFocusRelinquish, $kEventWindowDrawFrame, $kEventWindowDrawPart, 
						$kEventWindowGetRegion, $kEventWindowHitTest, $kEventWindowInit, $kEventWindowDispose, 
						$kEventWindowDragHilite, $kEventWindowModified, $kEventWindowSetupProxyDragImage, 
						$kEventWindowStateChanged, $kEventWindowMeasureTitle, $kEventWindowDrawGrowBox, 
						$kEventWindowGetGrowImageRegion, $kEventWindowPaint,
						
						// Parameters
						
						$kEventParamDirectObject, $kEventParamWindowRef, $kEventParamGrafPort, $kEventParamDragRef, 
						$kEventParamMenuRef, $kEventParamEventRef, $kEventParamControlRef, $kEventParamRgnHandle, 
						$kEventParamEnabled, $kEventParamDimensions, $kEventParamAvailableBounds, $kEventParamAEEventID, 
						$kEventParamAEEventClass, $kEventParamCGContextRef, $typeWindowRef, $typeGrafPtr, $typeGWorldPtr, 
						$typeDragRef, $typeMenuRef, $typeControlRef, $typeCollection, $typeQDRgnHandle, $typeOSStatus, 
						$typeCGContextRef, $kEventParamMouseLocation, $kEventParamMouseButton, $kEventParamClickCount, 
						$kEventParamMouseWheelAxis, $kEventParamMouseWheelDelta, $kEventParamMouseDelta, $typeMouseButton, 
						$typeMouseWheelAxis, $kEventParamKeyCode, $kEventParamKeyMacCharCodes, $kEventParamKeyModifiers, 
						$kEventParamKeyUnicodes, $typeEventHotKeyID, $kEventParamTextInputSendRefCon, 
						$kEventParamTextInputSendComponentInstance, $kEventParamTextInputSendSLRec, 
						$kEventParamTextInputReplySLRec, $kEventParamTextInputSendText, $kEventParamTextInputReplyText, 
						$kEventParamTextInputSendUpdateRng, $kEventParamTextInputSendHiliteRng, 
						$kEventParamTextInputSendClauseRng, $kEventParamTextInputSendPinRng, $kEventParamTextInputSendFixLen, 
						$kEventParamTextInputSendLeadingEdge, $kEventParamTextInputReplyLeadingEdge, 
						$kEventParamTextInputSendTextOffset, $kEventParamTextInputReplyTextOffset, 
						$kEventParamTextInputReplyRegionClass, $kEventParamTextInputSendCurrentPoint, 
						$kEventParamTextInputSendDraggingMode, $kEventParamTextInputReplyPoint, 
						$kEventParamTextInputReplyFont, $kEventParamTextInputReplyPointSize, 
						$kEventParamTextInputReplyLineHeight, $kEventParamTextInputReplyLineAscent, 
						$kEventParamTextInputReplyTextAngle, $kEventParamTextInputSendShowHide, 
						$kEventParamTextInputReplyShowHide, $kEventParamTextInputSendKeyboardEvent, 
						$kEventParamTextInputSendTextServiceEncoding, $kEventParamTextInputSendTextServiceMacEncoding, 
						$kEventParamHICommand, $typeHICommand, $kEventParamWindowFeatures, $kEventParamWindowDefPart, 
						$kEventParamCurrentBounds, $kEventParamOriginalBounds, $kEventParamPreviousBounds, 
						$kEventParamClickActivation, $kEventParamWindowRegionCode, $kEventParamWindowDragHiliteFlag, 
						$kEventParamWindowModifiedFlag, $kEventParamWindowProxyGWorldPtr, $kEventParamWindowProxyImageRgn, 
						$kEventParamWindowProxyOutlineRgn, $kEventParamWindowStateChangedFlags, 
						$kEventParamWindowTitleFullWidth, $kEventParamWindowTitleTextWidth, $kEventParamWindowGrowRect, 
						$kEventParamAttributes, $typeWindowRegionCode, $typeWindowDefPartCode, $typeClickActivationResult, 
						$kEventParamControlPart, $kEventParamInitCollection, $kEventParamControlMessage, 
						$kEventParamControlParam, $kEventParamControlResult, $kEventParamControlRegion, 
						$kEventParamControlAction, $kEventParamControlIndicatorDragConstraint, 
						$kEventParamControlIndicatorRegion, $kEventParamControlIsGhosting, $kEventParamControlIndicatorOffset, 
						$kEventParamControlClickActivationResult, $kEventParamControlSubControl, 
						$kEventParamControlOptimalBounds, $kEventParamControlOptimalBaselineOffset, 
						$kEventParamControlDataTag, $kEventParamControlDataBuffer, $kEventParamControlDataBufferSize, 
						$kEventParamControlDrawDepth, $kEventParamControlDrawInColor, $kEventParamControlFeatures, 
						$kEventParamControlPartBounds, $kEventParamControlOriginalOwningWindow, 
						$kEventParamControlCurrentOwningWindow, $typeControlActionUPP, $typeIndicatorDragConstraint, 
						$typeControlPartCode, $kEventParamCurrentMenuTrackingMode, $kEventParamNewMenuTrackingMode, 
						$kEventParamMenuFirstOpen, $kEventParamMenuItemIndex, $kEventParamMenuCommand, 
						$kEventParamEnableMenuForKeyEvent, $kEventParamMenuEventOptions, $typeMenuItemIndex, 
						$typeMenuCommand, $typeMenuTrackingMode, $typeMenuEventOptions, $kEventParamProcessID, 
						$kEventParamLaunchRefCon, $kEventParamLaunchErr, $kEventParamTabletPointerRec, 
						$kEventParamTabletProximityRec, $typeTabletPointerRec, $typeTabletProximityRec,

            <EventTypeSpec>,
            event-type-spec-eventClass,event-type-spec-eventClass-setter,
            event-type-spec-eventKind, event-type-spec-eventKind-setter,
            
            // methods
            NewEventHandlerUPP, DisposeEventHandlerUPP,
            InstallWindowEventHandler, AddEventTypesToHandler, CallNextEventHandler,
            ConvertEventRefToEventRecord, FlushEventQueue, 
            GetEventClass, GetEventKind, // GetEventParameter,
            GetMainEventQueue, GetMenuEventTarget,
            GetMainEventLoop, GetUserFocusEventTarget, GetWindowEventTarget,
            InstallEventHandler,
            //InstallEventLoopTimer, 
            GetEventParameter,
            QuitEventLoop, RemoveEventHandler, RemoveEventLoopTimer,
            RunApplicationEventLoop,
            GetUserFocusWindow, SetUserFocusWindow;
            
end module carbon-events











