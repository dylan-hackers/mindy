module: carbon-events

c-include( "Carbon/Carbon.h" );


// Types

/*

typedef CALLBACK_API( Boolean , EventComparatorProcPtr ) typedef STACK_UPP_TYPE(EventComparatorProcPtr) EventComparatorUPP;(EventRef inEvent, void *inCompareData);

typedef CALLBACK_API( OSStatus , EventvandlerProcPtr ) typedef STACK_UPP_TYPE(EventvandlerProcPtr) EventvandlerUPP;(EventvandlerCallRef invandlerCallRef, EventRef inEvent, void *inUserData);

typedef  struct  OpaqueEventLoopRef*  EventLoopRef;

typedef  struct  OpaqueEventLoopTimerRef*  EventLoopTimerRef;

typedef CALLBACK_API( void , EventLoopTimerProcPtr ) typedef STACK_UPP_TYPE(EventLoopTimerProcPtr) EventLoopTimerUPP;(EtentClassort-at(pt, offset: 2);

*/

/*
	<EventTypeSpec>
*/

define functional class <EventTypeSpec> (<Ptr>)
end class;

/*
	content-size
	The size of object a <EventTypeSpec> contains
*/

define method content-size( cls == <EventTypeSpec> )
=>( result :: <integer> )
	c-expr( int: "sizeof(EventTypeSpec)" );
end method content-size;


/*
	initialize <EventTypeSpec>
*/

define method initialize( es :: <EventTypeSpec>, eventClass :: <UInt32>, eventKind :: <UInt32>, #key, #all-keys)
=> ( result :: <EventTypeSpec> )

  event-type-spec-eventClass( es ) := eventClass;
  event-type-spec-eventKind( es ) := eventKind;
  
  es;

end method initialize;


/*
	Accessors for the eventKind and eventClass components of an <EventTypeSpec>
*/

define method event-type-spec-eventClass (es :: <EventTypeSpec>) 
=> (eventClass :: <integer>);
	integer-at(pt, offset: 0);
end method event-type-spec-eventClass;


define method event-type-spec-eventClass-setter (value :: <integer>, es :: <EventTypeSpec>) 
=> (value :: <integer>);
	integer-at(pt, offset: 0) := value;
end method event-type-spec-eventClass-setter;


define method event-type-spec-eventKind (es :: <EventTypeSpec>) 
=> (eventKind :: <integer>);
	integer-at(pt, offset: 4);
end method event-type-spec-eventKind;


define method event-type-spec-eventKind-setter (value :: <integer>, es :: <EventTypeSpec>) 
=> (value :: <integer>);
	integer-at(pt, offset: 4) := value;
end method event-type-spec-eventKind-setter;


// Values
  
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

// kEventClassMouse
/*
define constant $kEventClassMouse :: <integer> = FOUR_CHAR_CODE('mous');
define constant $kEventClassKeyboard :: <integer> = FOUR_CHAR_CODE('keyb');
define constant $kEventClassTextInput :: <integer> = FOUR_CHAR_CODE('text');
define constant $kEventClassApplication :: <integer> = FOUR_CHAR_CODE('appl');
define constant $kEventClassEPPC :: <integer> = FOUR_CHAR_CODE('eppc');
define constant $kEventClassMenu :: <integer> = FOUR_CHAR_CODE('menu');
define constant $kEventClassWindow :: <integer> = FOUR_CHAR_CODE('wind');
define constant $kEventClassControl :: <integer> = FOUR_CHAR_CODE('cntl');
define constant $kEventClassCommand :: <integer> = FOUR_CHAR_CODE('cmds');
define constant $kEventClassTablet :: <integer> = FOUR_CHAR_CODE('tblt');
*/
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


// Methods

/*
    RunApplicationEventLoop
*/

define method RunApplicationEventLoop() => ()
    call-out( void:, "RunApplicationEventLoop" );
end method RunApplicationEventLoop;

/*
    InstallWindowEventHandler
    outRef can be null
    inNumTypes should be 1 for now
*/

define method InstallWindowEventHandler( inTarget :: <WindowRef>, inHandler :: <EventHandlerUPP>,
                                        inNumTypes :: <UInt32>, inList :: <EventTypeSpec>,
                                        inUserData :: <statically-typed-pointer>, outRef :: <EventHandlerRef> ) 
=> ( result :: <OSStatus> )

    let status = call-out( integer:, "InstallWindowEventHandler", ptr: inTarget,raw-value, ptr: inHandler.raw.value,
                            integer: inNumTypes, ptr: inUserData.raw-value, ptr: outRef.raw-value );
                            
    as( <OSStatus>, status );
 
end method InstallWindowEventHandler;


/*

OSStatus AddEventTypesToHandler(EventHandlerRef inHandlerRef,
 UInt32 inNumTypes,
 const EventTypeSpec * inList);

OSStatus CallNextEventHandler(EventHandlerCallRef inCallRef,
 EventRef inEvent);

Boolean ConvertEventRefToEventRecord(EventRef inEvent,
 EventRecord * outEvent);

OSStatus FlushEventQueue(EventQueueRef inQueue);

UInt32 GetEventClass(EventRef inEvent);

UInt32 GetEventKind(EventRef inEvent);

OSStatus GetEventParameter(EventRef inEvent,
 EventParamName inName,
 EventParamType inDesiredType,
 EventParamType * outActualType, / * can be NULL * /
 UInt32 inBufferSize,
 UInt32 * outActualSize, / * can be NULL * /
 const void * ioBuffer);

EventTime GetEventTime(EventRef inEvent);

EventQueueRef GetMainEventQueue(void);



EventTargetRef GetMenuEventTarget(MenuRef inMenu);




EventLoopRef GetMainEventLoop(void);



EventTargetRef GetUserFocusEventTarget(void);



EventTargetRef GetWindowEventTarget(WindowRef inWindow);


OSStatus InstallEventHandler(EventTargetRef inTarget,
 EventHandlerUPP invandler,
 UInt32 inNumTypes,
 const EventTypeSpec * inList,
 void * inUserData,
 EventHandlerRef * outRef) / * can be NULL * /;

OSStatus InstallEventLoopTimer(EventLoopRef inEventLoop,
 EventTimerInterval inFireDelay,
 EventTimerInterval inInterval,
 EventLoopTimerUPP inTimerProc,
 void * inTimerData,
 EventLoopTimerRef * outTimer);


OSStatus QuitEventLoop(EventLoopRef inEventLoop);



OSStatus RemoveEventvandler(EventvandlerRef invandlerRef);



OSStatus RemoveEventLoopTimer(EventLoopTimerRef inTimer);

*/