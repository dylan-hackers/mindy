module: carbon-event-handling
synopsis: 
author: 
copyright: 


// Constants


// $window-event-handler-callback
// The callback method for our event handler

define constant $window-event-handler-callback =
	callback-method(	arg1 :: <raw-pointer>,
										arg2 :: <raw-pointer>,
										arg3 :: <raw-pointer> )
	=> ( result :: <integer> );
		let result = window-event-handler(	
										make( <EventHandlerCallRef>, pointer: arg1 ),
										make( <EventRef>, pointer: arg2 ),
										make( <statically-typed-pointer>, pointer: arg3 ) );
		values( result );
	end;			
	

// Globals


// *window-event-handler-upp*
// Our window event handler UPP

define variable *window-event-handler-upp* :: false-or( <EventHandlerUPP> ) = #f;


// Methods


// make-window

define method make-window()
=> ()
	let bounds :: <Rect*> = make( <Rect*>, top: 60, left: 10, right: 210, bottom: 160 );
	
  let (err, window) = 
  	CreateNewWindow( $kDocumentWindowClass, 
			logior( $kWindowStandardDocumentAttributes, $kWindowStandardHandlerAttribute),
			bounds );

	// Add the event types individually
	
	let list :: <EventTypeSpec*> = make( <EventTypeSpec*>, eventClass: $kEventClassWindow, eventKind: $kEventWindowDrawContent );

	*window-event-handler-upp* := NewEventHandlerUPP( $window-event-handler-callback );
	let (err, event-handler) = InstallWindowEventHandler( window, *window-event-handler-upp*, 1, list, $NULL );
	
	list.eventClass-value := $kEventClassWindow;
	list.eventKind-value := $kEventWindowBoundsChanged;
	AddEventTypesToHandler( event-handler, 1, list );
	
	list.eventClass-value := $kEventClassWindow;
	list.eventKind-value := $kEventWindowClose;
	AddEventTypesToHandler( event-handler, 1, list );
	
	ShowWindow(window);
end method make-window;

		
// event-handler

define method window-event-handler( myHandler :: <EventHandlerCallRef>, event :: <EventRef>, 
														 userData :: <statically-typed-pointer> )
=>( result :: <OSStatus> )
	let result :: <OSStatus> = $eventNotHandledErr;
	
	// This is a bit horrible. We need a pointer to a WindowRef so we use a Handle
	// which is a pointer to a 4-byte block of memory that's usually used as a pointer.
	// We then get the WindowRef out of it and whistle nonchalantly.
	let temp :: <Handle> = make( <Handle> );
	GetEventParameter( event, $kEventParamDirectObject, $typeWindowRef, 
										 4, temp ); // 4 is the size of a WindowRef
	let window :: <WindowRef> = pointer-at( temp, class: <WindowRef>, offset: 0 );
	
	let event-kind = GetEventKind( event );

	if( event-kind = $kEventWindowDrawContent )
		handle-window-update( window );
		result := $noErr;
	elseif( event-kind = $kEventWindowBoundsChanged )
		InvalWindowRect( window, GetWindowPortBounds( window ) );
		result := $noErr;
	elseif( event-kind = $kEventWindowClose )
		DisposeEventHandlerUPP( *window-event-handler-upp* );
		DisposeWindow( window );
		result := $noErr;
	end if;
	
	result;
end method window-event-handler;


// handle-window-update

define method handle-window-update( window :: <WindowRef> )
=> ()
	SetPortWindowPort( window );
	MoveTo( 40, 40 );
	DrawString( as( <pascal-string>, "Carbon" ) );
	MoveTo( 40, 60 );
	DrawString( as( <pascal-string>, "Events" ) );
end;


// main

define function main( name, arguments )
	make-window();
  RunApplicationEventLoop();
  exit-application( 0 );
end function main;


// Top-level calls


// Invoke our main() function.

main(application-name(), application-arguments());
