module: events


/*
	Event Manager.
*/


/*
	Toolbox includes
*/

c-include("Carbon/Carbon.h");


/*
	Event Codes
*/
										
define constant $everyEvent = -1;

define constant $nullEvent = 0;
define constant $mouseDown = 1;
define constant $mouseUp = 2;
define constant $keyDown = 3;
define constant $keyUp = 4;
define constant $autoKey = 5;
define constant $updateEvt = 6;
define constant $diskEvt = 7;
define constant $activateEvt = 8;
define constant $osEvt = 15;
define constant $kHighLevelEvent = 23;

/*
	Modifier Masks.
*/

define constant $suspendResumeMessage = #x0001;
define constant $activeFlag = #x0001;

define constant $charCodeMask = #x000000FF;
define constant $keyCodeMask = #x0000FF00;

define constant $cmdKey :: <integer> = 256;
define constant $cmdKeyBit = 8;


/*
	<EventRecord>
*/

define functional class <EventRecord> ( <Ptr> ) 
end class <EventRecord>;


/*
	Make sure we allocate the correct size for <EventRecord>
*/

define method content-size( cls == <EventRecord> )
=>( result :: <integer> )
	16;
end method content-size;


/*
	<EventRecord> accessors.
*/

define method event-what ( event :: <EventRecord> )
=> ( what :: <integer> );
	unsigned-short-at(event, offset: 0);
end method event-what;


define method event-message ( event :: <EventRecord> )
=> ( message :: <integer> );
	unsigned-long-at(event, offset: 2 );
end method event-message;


define method event-when ( event :: <EventRecord> )
=> ( when :: <integer>);
	unsigned-long-at(event, offset: 6);
end method event-when;


define method event-where ( event :: <EventRecord> )
=> ( where :: <Point> );
	// v and h ARE the "wrong" way round in the C struct.
	make( <Point>, v: signed-short-at( event, offset: 10 ), h: signed-short-at( event, offset: 12 ) ); 
end method event-where;


define method event-modifiers ( event :: <EventRecord> )
=> ( modifiers :: <integer> );
	unsigned-short-at( event, offset: 14 );
end method event-modifiers;


/*define constant GetNextEvent = get-c-function("GetNextEvent", args: list(<integer>, <EventRecord>),
											result: <boolean>, file: *InterfaceLib*);
define constant SystemTask = get-c-function("SystemTask", args: #(),
											result: #(), file: *InterfaceLib*);
*/				

// type-union on mousergn to allow $NULL
							
define method WaitNextEvent( eventMask :: <integer>, record :: <EventRecord>, sleep :: <integer>, mouseRgn :: type-union( <RgnHandle>, <Ptr> )  )
=> ( result :: <boolean> )
	if ( call-out( 	"WaitNextEvent", unsigned-char:, int: eventMask, ptr: record.raw-value, 
					unsigned-int: sleep, ptr: mouseRgn.raw-value )  == 1 ) #t else #f end if;
end method WaitNExtEvent;


/*
	FlushEvents
*/

define method FlushEvents( startMask :: <integer>, stopMask :: <integer> )
=> ()
	call-out( "FlushEvents", void:, int: startMask, int: stopMask );
	values();
end method FlushEvents;


/*
	<RoutineDescriptor> and friends.
*/

define functional class <RoutineDescriptor> (<statically-typed-pointer>)
end class;

/*
	Make sure we allocate the correct size for <RoutineDescriptor>
*/

define method content-size( cls == <RoutineDescriptor> )
=>( result :: <integer> )
	c-expr( int: "sizeof(RoutineDescriptor)" );
end method content-size;

define constant <UniversalProcPtr> = <RoutineDescriptor>;

define constant <AEEventClass> = <OSType>;
define constant <AEEventID> = <OSType>;

define constant $kCoreEventClass :: <AEEventClass> = os-type("aevt");

define constant $kAEOpenApplication :: <AEEventID> = os-type("oapp");
define constant $kAEOpenDocuments :: <AEEventID> = os-type("odoc");
define constant $kAEPrintDocuments :: <AEEventID> = os-type("pdoc");
define constant $kAEQuitApplication :: <AEEventID> = os-type("quit");

define functional class <AppleEvent> (<statically-typed-pointer>)
end class;

/*
	Make sure we allocate the correct size for <AppleEvent>
*/

define method content-size( cls == <AppleEvent> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AppleEvent)" );
end method content-size;

define constant <AEEventHandlerUPP> = <UniversalProcPtr>;
define constant $uppAEEventHandlerProcInfo = 4064;


/*
	AEDesc
*/

define functional class <AEDesc> (<statically-typed-pointer>)
end class;

define method content-size( cls == <AEDesc> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AEDesc)" );
end method content-size;


/*
	AEDescList
*/

define functional class <AEDescList> (<statically-typed-pointer>)
end class;

define method content-size( cls == <AEDescList> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AEDescList)" );
end method content-size;

/*
    NewAEEventHandlerUPP
    Should take a <callback-function> and get the callback from that, but the class
    isn't exported.
*/

define method NewAEEventHandlerUPP(procPtr :: <raw-pointer>)	
 => (result :: <AEEventHandlerUPP>);
	let result-value = call-out("NewAEEventHandlerUPP", ptr:, ptr: procPtr);
	make(<AEEventHandlerUPP>, pointer: result-value);
end method NewAEEventHandlerUPP;


/*
	AEInstallEventHandler
*/

define method AEInstallEventHandler( class :: <AEEventClass>, id :: <AEEventID>, upp :: <AEEventHandlerUPP>, refcon :: <integer>, isSysHandler :: <boolean> )
=> ( result :: <OSErr> ) 
	let sysHandler :: <integer> = if( isSysHandler ) 1 else 0 end if;
	call-out( "AEInstallEventHandler", int:, int: class, int: id, ptr: upp.raw-value, int: refcon, unsigned-char: sysHandler );
end method AEInstallEventHandler;


/*
	AERemoveEventHandler
*/

define method AERemoveEventHandler( class :: <AEEventClass>, id :: <AEEventID>, upp :: <AEEventHandlerUPP>,  isSysHandler :: <boolean> )
=> ( result :: <OSErr> ) 
	let sysHandler :: <integer> = if( isSysHandler ) 1 else 0 end if;
	call-out( "AERemoveEventHandler", int:, int: class, int: id, ptr: upp.raw-value, unsigned-char: sysHandler );
end method AERemoveEventHandler;


/*
	AEProcessAppleEvent
*/

define method AEProcessAppleEvent( event :: <EventRecord> )
=> ( result :: <OSErr> ) 
	call-out( "AEProcessAppleEvent", int:, ptr: event.raw-value );
end method AEProcessAppleEvent;


/*
	Misc. Events.h functions you can't live without.
*/

define method TickCount()
=> ( result :: <integer> ) 
	call-out("TickCount", unsigned-int: );
end method TickCount;


define method Button()
=> ( result :: <boolean> )
	if ( call-out( "Button", unsigned-char: ) == 1 ) #t else #f end if;
end method Button;											
											
											
define method StillDown()
=> ( result :: <boolean> )
	if ( call-out( "StillDown", unsigned-char: ) == 1 ) #t else #f end if;												
end method StillDown;


define method WaitMouseUp()
=> ( result :: <boolean> )
	if ( call-out( "WaitMouseUp", unsigned-char: ) == 1 ) #t else #f end if;
end method WaitMouseUp;


define method GetMouse( pt :: <Point> )
=> ()
	call-out( "GetMouse", void:, ptr: pt.raw-value );
	values;
end method GetMouse;
											
											
define method GlobalToLocal( pt :: <Point> )
=> ()
	call-out( "GlobalToLocal", void:, ptr: pt.raw-value );
	values;
end method GlobalToLocal;


/*define method SystemClick( event :: <EventRecord>, window :: <WindowPtr> )
=> ()
	call-out( "SystemClick", void:, ptr: event.raw-value, ptr: window.raw-value );
	values();
end method SystemClick;
*/

/*define method DIBadMount( point :: <Point>, message :: <integer> )
=> ( result :: <integer> )
	call-out( "DIBadMount", short:, ptr: point.raw-value, unsigned-int: message );	
end method DIBadMount;*/
