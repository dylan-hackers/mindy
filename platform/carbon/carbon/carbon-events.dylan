module: carbon-events

c-include( "Carbon.h" );


// Types

define constant <EventHandlerUPP> :: <type> = <UniversalProcPtr>;
define constant <EventComparatorUPP> :: <type> = <UniversalProcPtr>;
define constant <EventLoopTimerUPP> :: <type> = <UniversalProcPtr>;

/*
	<EventLoopRef>
*/

define functional class <EventLoopRef> (<statically-typed-pointer>)
end class <EventLoopRef>; 

/*
	content-size
	The size of object a <EventLoopRef> contains
*/

define method content-size( cls == <EventLoopRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEventLoop)" );
end method content-size;

/*
	<EventTargetRef>
*/

define functional class <EventTargetRef> (<statically-typed-pointer>)
end class <EventTargetRef>; 

/*
	content-size
	The size of object a <EventTargetRef> contains
*/

define method content-size( cls == <EventTargetRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEventTarget)" );
end method content-size;


/*
	<EventRef>
*/

define functional class <EventRef> (<statically-typed-pointer>)
end class <EventRef>; 

/*
	content-size
	The size of object a <EventRef> contains
*/

define method content-size( cls == <EventRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEvent)" );
end method content-size;

/*
	<EventHandlerRef>
*/

define functional class <EventHandlerRef> (<statically-typed-pointer>)
end class <EventHandlerRef>; 

/*
	content-size
	The size of object a <EventHandlerRef> contains
*/

define method content-size( cls == <EventHandlerRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEventHandler)" );
end method content-size;


/*
	<EventHandlerCallRef>
*/

define functional class <EventHandlerCallRef> (<statically-typed-pointer>)
end class <EventHandlerCallRef>; 

/*
	content-size
	The size of object a <EventHandlerCallRef> contains
*/

define method content-size( cls == <EventHandlerCallRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEventHandlerCall)" );
end method content-size;

/*
	<EventQueueRef> 
*/

define functional class <EventQueueRef>  (<statically-typed-pointer>)
end class <EventQueueRef> ; 

/*
	content-size
	The size of object a <EventQueueRef>  contains
*/

define method content-size( cls == <EventQueueRef>  )
=>( result :: <integer> )
	c-expr( int: "sizeof(OpaqueEventQueue)" );
end method content-size;

/*
	<EventLoopTimerRef>
*/

define functional class <EventLoopTimerRef> (<statically-typed-pointer>)
end class;

/*
	content-size
	The size of object a <EventLoopTimerRef> contains
*/

define method content-size( cls == <EventLoopTimerRef> )
=>( result :: <integer> )
	c-expr( int: "sizeof(EventLoopTimer)" );
end method content-size;

/*
	<EventTypeSpec>
*/

define functional class <EventTypeSpec> (<statically-typed-pointer>)
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

define method initialize( es :: <EventTypeSpec>, #key eventClass :: <integer>, eventKind :: <integer>, #all-keys)
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
	unsigned-long-at(es, offset: 0);
end method event-type-spec-eventClass;


define method event-type-spec-eventClass-setter (value :: <integer>, es :: <EventTypeSpec>) 
=> (value :: <integer>);
	unsigned-long-at(es, offset: 0) := value;
end method event-type-spec-eventClass-setter;


define method event-type-spec-eventKind (es :: <EventTypeSpec>) 
=> (eventKind :: <integer>);
	unsigned-long-at(es, offset: 4);
end method event-type-spec-eventKind;


define method event-type-spec-eventKind-setter (value :: <integer>, es :: <EventTypeSpec>) 
=> (value :: <integer>);
	unsigned-long-at(es, offset: 4) := value;
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


// Methods

/*
    RunApplicationEventLoop
*/

define method RunApplicationEventLoop() => ()
    call-out( void:, "RunApplicationEventLoop" );
end method RunApplicationEventLoop;

/*
    InstallWindowEventHandler
*/

define method InstallWindowEventHandler( inTarget :: <WindowRef>, 
                                        inHandler :: <EventHandlerUPP>,
                                        inNumTypes :: <integer>, 
                                        inList :: type-union( <c-vector>, <EventTypeSpec> ), // <EventTypeSpec>,
                                        inUserData :: <statically-typed-pointer> ) 
=> ( result :: <OSStatus>, outRef :: <EventHandlerRef> )
    let temp :: <handle> = make( <Handle> );
    let status = call-out( "InstallWindowEventHandler", int:, ptr: inTarget.raw-value, ptr: inHandler.raw-value,
                            int: inNumTypes, ptr: inList.raw-value, ptr: inUserData.raw-value, ptr: temp.raw-value );
    values( as( <OSStatus>, status ), make( <EventHandlerRef>, pointer: temp.raw-value ) );
end method InstallWindowEventHandler;

define method AddEventTypesToHandler( inHandlerRef :: <EventHandlerRef>, inNumTypes :: <integer>,
                                      inList :: type-union( <c-vector>, <EventTypeSpec> ) ) // <EventTypeSpec*>
=>( result :: <OSStatus> )
  let result = call-out( int:, "AddEventTypesToHandler", ptr: inHandlerRef.raw-value, int: inNumTypes,
                         ptr: inList.raw-value);
  as( <OSStatus>, result );
end method AddEventTypesToHandler;
 
define method CallNextEventHandler( inCallRef :: <EventHandlerCallRef>, inEvent :: <EventRef> )
=>( result :: <OSStatus> )
  let result = call-out( int:, "CallNextEventHandler", ptr: inCallRef.raw-value, ptr: inEvent.raw-value );
  as( <OSStatus>, result );
end method CallNextEventHandler;

define method ConvertEventRefToEventRecord( inEvent :: <EventRef> )
=>( outEvent :: <EventRecord>, result :: <boolean> )
  let event-ptr :: <Handle> = make( <Handle> );
  let result = call-out( int:, "ConvertEventRefToEventRecord", ptr: inEvent.raw-value, ptr: event-ptr.raw-value );
  values( make( <EventRecord>, pointer: event-ptr.raw-value ),
          if( result = 0 )
            #f;
          else
            #t;
          end if );
end method ConvertEventRefToEventRecord;

define method FlushEventQueue( inQueue :: <EventQueueRef> )
=>( result :: <OSStatus> )
  let result = call-out( int:, "FlushEventQueue", ptr: inQueue.raw-value );
  as( <OSStatus>, result );
end method FlushEventQueue;

define method GetEventClass( inEvent :: <EventRef> )
=>( result :: <integer> )
  call-out( int:, "GetEventClass", ptr: inEvent.raw-value );
end method GetEventClass;

define method GetEventKind( inEvent :: <EventRef> )
=>( result :: <integer> )
  call-out( int:, "GetEventKind", ptr: inEvent.raw-value );
end method GetEventKind;

//OSStatus GetEventParameter(EventRef inEvent,
// EventParamName inName,
// EventParamType inDesiredType,
// EventParamType * outActualType, / * can be NULL * /
// integer inBufferSize,
// integer * outActualSize, / * can be NULL * /
// const void * ioBuffer);

define method GetMainEventQueue()
=>( result :: <EventQueueRef> )
  let result = call-out( ptr:, "GetMainEventQueue" );
  make( <EventQueueRef>, pointer: result );
end method GetMainEventQueue;

define method GetMenuEventTarget( inMenu :: <MenuRef> )
=>( result :: <EventTargetRef> )
  let result = call-out( ptr:, "GetMenuEventTarget", ptr: inMenu.raw-value );
  make( <EventTargetRef>, pointer: result );
end method GetMenuEventTarget;

define method GetMainEventLoop()
=>( result :: <EventLoopRef> )
  let result = call-out( ptr:, "GetMainEventLoop" );
  make( <EventLoopRef>, pointer: result );
end method GetMainEventLoop;

define method GetUserFocusEventTarget()
=>( result :: <EventTargetRef> )
  let result = call-out( ptr:, "GetUserFocusEventTarget" );
  make( <EventTargetRef>, pointer: result );
end method GetUserFocusEventTarget;

define method GetWindowEventTarget( inWindow :: <WindowRef> )
=>( result :: <EventTargetRef> )
  let result = call-out( ptr:, "GetWindowEventTarget", ptr: inWindow.raw-value );
  make( <EventTargetRef>, pointer: result );
end method GetWindowEventTarget;

define method InstallEventHandler( inTarget :: <EventTargetRef>,
                                    inHandler :: <EventHandlerUPP>,
                                    inNumTypes :: <integer>,
                                    inList :: type-union( <c-vector>, <EventTypeSpec> ),// <EventTypeSpec*>,
                                    inUserData :: <statically-typed-pointer> )
=>( result :: <OSStatus>, outRef :: <EventHandlerRef> )
  let temp :: <handle> = make( <Handle> );
  let result = call-out( int:, "InstallEventHandler", ptr: inTarget.raw-value, ptr: inHandler.raw-value,
                          ptr: inList.raw-value, ptr: inUserData.raw-value, ptr: temp.raw-value );
  values( as( <OSStatus>, result ), make( <EventHandlerRef>, pointer: temp.raw-value ) );
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
  let result = call-out( int:, "QuitEventLoop", ptr: inEventLoop.raw-value );
  as( <OSStatus>, result );
end method QuitEventLoop;

define method RemoveEventHandler( inHandlerRef :: <EventHandlerRef> )
=>( result :: <OSStatus> )
  let result = call-out( int:, "RemoveEventHandler", ptr: inHandlerRef.raw-value );
  as( <OSStatus>, result );
end method RemoveEventHandler;


define method RemoveEventLoopTimer( inTimer :: <EventLoopTimerRef> )
=>( result :: <OSStatus> )
  let result = call-out( int:, "RemoveEventLoopTimer", ptr: inTimer.raw-value );
  as( <OSStatus>, result );
end method RemoveEventLoopTimer;
