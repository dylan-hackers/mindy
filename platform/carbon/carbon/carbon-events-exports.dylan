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
        
            <EventTypeSpec>,
            event-type-spec-eventClass,event-type-spec-eventClass-setter,
            event-type-spec-eventKind, event-type-spec-eventKind-setter,
            
            // methods
            InstallWindowEventHandler, AddEventTypesToHandler, CallNextEventHandler,
            ConvertEventRefToEventRecord, FlushEventQueue, 
            GetEventClass, GetEventKind, // GetEventParameter,
            GetMainEventQueue, GetMenuEventTarget,
            GetMainEventLoop, GetUserFocusEventTarget, GetWindowEventTarget,
            InstallEventHandler,
            //InstallEventLoopTimer, 
            QuitEventLoop, RemoveEventHandler, RemoveEventLoopTimer
            ;
            
end module carbon-events











