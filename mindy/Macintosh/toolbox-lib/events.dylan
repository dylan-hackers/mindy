module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Event Manager.
											
define constant $everyEvent = -1;

// event codes.
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

// modifier masks.
define constant $cmdKey = 256;

define class <EventRecord> (<Ptr>) end class;

define method make(cls == <EventRecord>, #key what: what)
=>(result :: <EventRecord>)
	as(<EventRecord>, NewPtr(16));
end method make;

define method event-what (event :: <EventRecord>) => (what :: <integer>);
	signed-short-at(event, offset: 0);
end method event-what;

define method event-message (event :: <EventRecord>) => (message :: <integer>);
	unsigned-long-at(event, offset: 2);
end method event-message;

define method event-when (event :: <EventRecord>) => (when :: <integer>);
	unsigned-long-at(event, offset: 6);
end method event-when;

define method event-where (event :: <EventRecord>) => (where :: <Point>);
	as (<Point>, event + 10);
end method event-where;

define method event-modifiers (event :: <EventRecord>) => (modifiers :: <integer>);
	signed-short-at(event, offset: 14);
end method event-modifiers;

define constant GetNextEvent = get-c-function("GetNextEvent", args: list(<integer>, <EventRecord>),
											result: <boolean>, file: *InterfaceLib*);
define constant SystemTask = get-c-function("SystemTask", args: #(),
											result: #(), file: *InterfaceLib*);
define constant WaitNextEvent = get-c-function("WaitNextEvent", args: list(<integer>, <EventRecord>, <integer>, <RgnHandle>),
											result: <boolean>, file: *InterfaceLib*);

define class <RoutineDescriptor> (<machine-pointer>) end class;
define constant <UniversalProcPtr> = <RoutineDescriptor>;

define constant <AEEventClass> = <OSType>;
define constant <AEEventID> = <OSType>;

define constant $kCoreEventClass :: <AEEventClass> = os-type("aevt");

define constant $kAEOpenApplication :: <AEEventID> = os-type("oapp");
define constant $kAEOpenDocuments :: <AEEventID> = os-type("odoc");
define constant $kAEPrintDocuments :: <AEEventID> = os-type("pdoc");
define constant $kAEQuitApplication :: <AEEventID> = os-type("quit");

define class <AppleEvent> (<Ptr>) end class;

define constant <AEEventHandlerUPP> = <UniversalProcPtr>;
define constant $uppAEEventHandlerProcInfo = 4064;

define constant AEInstallEventHandler = get-c-function("AEInstallEventHandler", args: list(<AEEventClass>, <AEEventID>, <AEEventHandlerUPP>, <integer>, <boolean>),
											result: <OSErr>, file: *InterfaceLib*);

define constant AEProcessAppleEvent = get-c-function("AEProcessAppleEvent", args: list(<EventRecord>),
											result: <OSErr>, file: *InterfaceLib*);

define constant TickCount = get-c-function("TickCount", args: #(),
											result: <integer>, file: *InterfaceLib*);

define constant Button = get-c-function("Button", args: #(),
											result: <boolean>, file: *InterfaceLib*);
define constant StillDown = get-c-function("StillDown", args: #(),
											result: <boolean>, file: *InterfaceLib*);
define constant WaitMouseUp = get-c-function("WaitMouseUp", args: #(),
											result: <boolean>, file: *InterfaceLib*);

define constant GetMouse = get-c-function("GetMouse", args: list(<Point>),
											result: #(), file: *InterfaceLib*);
define constant GlobalToLocal = get-c-function("GlobalToLocal", args: list(<Point>),
											result: #(), file: *InterfaceLib*);
