module: simple

/*
	NOTES
	
	-How Do I start?
	-Subclass <simple-application> and implement the initialize-application() GF for your subclass.
	
	-If you want to handle menu events differently at the application level, override
	application-menu-choice(). It may be more appropriate to override window-menu-choice().
	If your overriding application-menu-choice() doesn't handle the menu event, call next-method()
	to make sure the default menu functionality is called.
	
	-To give yourself credit for doing this, override about-box().
*/


/*
	<simple-application>
*/

define open class <simple-application> ( <object> )
	
	slot background :: <boolean>, init-value: #f; 
	
	slot front-window, init-value: #f;
	
	slot initialized :: <boolean>, init-value: #f;
	
	slot quit :: <boolean>, init-value: #f;
        
        slot debugging :: <boolean>, init-value: #t;

end class <simple-application>;


/*
	*application*
*/

define variable *application* :: type-union( <simple-application>, <boolean> ) = #f;

/*
	initialise
*/

define method initialize( app :: <simple-application>, #rest keys, #key, #all-keys )
=> ( app :: <simple-application> )

	if( can-run?( app ) )
		app.initialized := #t;
	else
		app.initialized := #f;
	end if;
	
	app;

end method initialize;


/*
	can-run?
*/

define open generic can-run?( app :: <simple-application> ) => ( can :: <boolean> );

define method can-run?( app :: <simple-application> )
=> ( can :: <boolean> )

	#t;

end method can-run?;


/*
	run
*/

define open generic run( app :: <simple-application> ) => ();

define method run( app :: <simple-application> )
=> ()

        *application* := app;

	InitCursor();
        
	RegisterAppearanceClient();	// Get access to StandardAlert
        
	block()

            initialize-application( app );
    
            initialize-menus( app );
    
            install-apple-events();
    
            until( app.quit = #t )
                    next-event( app );
            end until;
            
            remove-apple-events();
            
            finalize-application( app );
        
        exception( c1 :: type-union( <simple-error>, <simple-warning>, <simple-restart> ) )    
            if( *application*.debugging )
                StandardAlert( $kAlertStopAlert, "A Condition was thrown.",
                                    apply(format-to-string, c1.condition-format-string, c1.condition-format-arguments), 
                                    as( <AlertStdAlertParam>, $NULL ) );
            end if;
        
        exception( c2 :: <type-error> )    
            if( *application*.debugging )
                StandardAlert( $kAlertStopAlert, "A Condition was thrown.",
                                    format-to-string( "Type Error: expected an instance of %=, but got %=",
                                        c2.type-error-expected-type, c2.type-error-value), 
                                    as( <AlertStdAlertParam>, $NULL ) );
            end if;
                                
	exception( c3 :: <condition> )	// And all other <condition> subtypes
            if( *application*.debugging )
                StandardAlert( $kAlertStopAlert, "A Condition was thrown.",
                                    format-to-string( "%=", c3 ), 
                                    as( <AlertStdAlertParam>, $NULL ) );
            end if;
	end block;
        
        UnregisterAppearanceClient;
            
	values();
	
end method run;


/*
	initialize-application
*/

define open generic initialize-application( app :: <simple-application> ) => ();

define method initialize-application( app :: <simple-application> )
=> ()

	values();
	
end method initialize-application;


/*
	initialize-menus
*/

define open generic initialize-menus( app :: <simple-application> );

define method initialize-menus( app :: <simple-application> )
=> ()

	let menuBar :: <Handle> = GetNewMBar( $StdMenubarID );
	
	if( menuBar ~= $NULL )
		SetMenuBar( menuBar );
		DisposeHandle( menuBar );
	end if;
		
	let theMenu :: <MenuHandle> = GetMenuHandle( $AppleMenuID );		
	if ( theMenu )
		AppendResMenu( theMenu, os-type( "DRVR" ) );
	end if;
	
	DrawMenuBar();	
	
	values();
	
end method initialize-menus;

/*
	finalize-application
*/

define open generic finalize-application( app :: <simple-application> ) => ();

define method finalize-application( app :: <simple-application> )
=> ()
	
	values();
	
end method finalize-application;



/*
	next-event
*/

define open generic next-event( app :: <simple-application> ) => ();

define method next-event( app :: <simple-application> )
=> ()

	let event :: <EventRecord*> = make( <EventRecord*> );
	let got-event :: <boolean> = WaitNextEvent( $everyEvent, event, 3, $NULL );
	
	if( got-event)
		dispatch-event( app, event );
	else
		application-idle( app, event );
	end if;

	values();
	
end method next-event;


/*
	application-idle
*/

define open generic application-idle( app :: <simple-application>, idle-event :: <EventRecord*> ) => ();

define method application-idle( app :: <simple-application>, idle-event :: <EventRecord*> )
=> ()

	if( app.front-window ~= #f )
		window-idle( app.front-window, idle-event );
	end if;
		
	values();
	
end method application-idle;


/*
	dispatch-event
*/

define open generic dispatch-event( app :: <simple-application>, event :: <EventRecord*> );

define method dispatch-event( app :: <simple-application>, event :: <EventRecord*> )
=> ( result :: <boolean> )
	
	select( event.what-value )
		
		$mouseDown =>	dispatch-mouse-event( app, event, event.where-value );
		
		$autoKey, $keyDown =>	let theKey :: <character> = as( <character>, logand(event.message-value, $charCodeMask) );//event.event-char;
								let theCode :: <integer> = as( <integer>, logand(event.message-value, $keyCodeMask) );
								//as( <character>, theKey ), theCode, event.modifiers-value
								application-key( app, event, theKey, theCode );

		$activateEvt => let makeActive :: <boolean> = if( logand(event.modifiers-value, $activeFlag) ) #t else #f end if;					
						application-window-activate( app, make( <WindowRef>, pointer: event.message-value ), event, makeActive );
									
		$updateEvt =>	application-window-update( app, make( <WindowRef>, pointer: event.message-value ), event );

	/*	$diskEvt =>		let dPt :: <Point*> = make( <Point*>, h: 100, v: 100 );					
						if( floor/( event.message-value, 65536 ) ~= 0)
							DIBadMount( dPt, event.message-value );
						end if;
	*/
		$kHighLevelEvent => application-apple-event( app, event );

		$osEvt =>		application-os-event( app, event );
		
		otherwise =>	#f;

		
	end select;
	
	values();
	
end method dispatch-event;


/*
	dispatch-mouse-event
*/

define open generic dispatch-mouse-event( app :: <simple-application>, event :: <EventRecord*>, point :: <Point*> ) => (); 

define method dispatch-mouse-event( app :: <simple-application>, event :: <EventRecord*>, point :: <Point*> )
=> ()

	let ( partCode :: <integer>, targetWindow ) = FindWindow( event.where-value );

	if( targetWindow ~= #f )
		
		let window-object = window-to-object( targetWindow );
				
		select( partCode )
		
			$inDesk => #f;
			
			$inMenuBar =>	let( menu, item ) = MenuSelect( event.where-value );
							if( menu ~= 0 )
								application-menu-choice( app, menu, item );
							end if;
							HiliteMenu( #f );
			
			//$inSysWindow =>	SystemClick( event, targetWindow );
			
			
			$inContent =>	if((app.front-window.modal = #t) & ( window-object ~= app.front-window ))
								SysBeep( 8 );
							elseif ( targetWindow = FrontWindow() )
								if( window-object )
									focus( window-object );
									GlobalToLocal( point );
									window-click( window-object, event, point );	
								end if;
							else
								SelectWindow( targetWindow );
							end if;

			$inDrag =>		if((app.front-window.modal = #t) & ( window-object ~= app.front-window ))
								SysBeep( 8 );
							else
								DragWindow( targetWindow, event.where-value );
							end if;
			
			$inGrow =>		let (newHeight, newWidth) = GrowWindow( targetWindow, event.where-value );
							if((app.front-window.modal = #t) & ( window-object ~= app.front-window ))
								SysBeep( 8 );
							elseif( newWidth + newHeight ~= 0 )
								// finally, do the actual resize
								if( window-object ) 
									resize( window-object, newWidth, newHeight );
								else
									SizeWindow( targetWindow, newWidth, newHeight, #t );
									let invalr :: <Rect*> = make( <Rect*>, top: 0, left: 0, bottom: newHeight, right: newWidth);
									InvalWindowRect( app.front-window.windowRef, invalr );
								end if;
							end if;
			
			$inGoAway =>	if((app.front-window.modal = #t) & ( window-object ~= app.front-window ))
								SysBeep( 8 );
							elseif( TrackGoAway( targetWindow, event.where-value ) )
								// if the user clicks in the go-away box, the window will be closed. If the
								// option key is down, we close all of the windows.
								
								//if ( theEvent.modifiers & optionKey )
								//	close-all-windows( *application* );
								//else
									if ( can-close?( window-object ) )
										close-window( window-object );
									end if
								//end if
							end if;
	/*		
			case inZoomIn:
			case inZoomOut:
				if( TrackBox( targetWindow, theEvent.where, partCode ))
				{
					// if the user clicked in the zoom box, the window will be zoomed
					
					if( zappWindow )
						zappWindow->Zoom( partCode );
				}
				break;
	*/

		end select;

	end if;
	
	values();
	
end method dispatch-mouse-event;


/*
	application-key
*/

define open generic application-key( app :: <simple-application>, event :: <EventRecord*>, key :: <character>, keyCode :: <integer> ) => ();

define method application-key( app :: <simple-application>, event :: <EventRecord*>, key :: <character>, keyCode :: <integer> )
=> ()			

	if( logbit?( $cmdKeyBit, event.modifiers-value ) )	// beats logand with untyped $cmdkey
		let (menu, item) = MenuKey( key );
		if( menu ~= 0 )
			application-menu-choice( app, menu, item );
			HiliteMenu( #f );
		end if;	
	else
		ObscureCursor();
		if( app.front-window ~= #f ) 
			window-key( app.front-window, event, key, keyCode );
		end if;
	end if;
	
	values();
	
end method application-key;


/*
	application-menu-choice
*/

define open generic application-menu-choice( app :: <simple-application>, menu :: <integer>, item :: <integer> ) => ();

define method application-menu-choice( app :: <simple-application>, menu :: <integer>, item :: <integer> )
=> ()
	if( ~ window-menu-choice( app.front-window, menu, item ) )
		if( menu = $AppleMenuID )
			if( item = 1 )			// Handle the about box
				about-box( app );
			else
			/*	// launch Apple Menu item
				let appleItemName :: <pascal-string> = make( <pascal-string> );
				GetMenuItemText( GetMenuHandle( menu ), item, appleItemName );
				let savePort :: <GrafPtr> = GetPort();
				OpenDeskAcc( appleItemName );
				SetPort( savePort );*/
			end if;
		else if( menu = $FileMenuID )
				/*if(item = CountMenuItems( GetMenuHandle( menu ) ) )	// Assume QUIT is last option
					// QUIT!
					app.quit := #t;
				end if;*/
			end if;
		end if;
	end if;
end method application-menu-choice;


/*
	application-window-activate
*/

define open generic application-window-activate( app :: <simple-application>, window :: <WindowRef>, event :: <EventRecord*>, makeActive :: <boolean> );

define method application-window-activate( app :: <simple-application>, window :: <WindowRef>, event :: <EventRecord*>, makeActive :: <boolean> )
=> ()
	let window-object = window-to-object( window );
	if( window-object )
		if( window-object.finished ) 
			finalize( window-object );
			if( app.front-window == window-object )
				let win = FrontWindow();
				if( win ) 
					app.front-window := window-to-object( win );
				else 
					app.front-window := #f;
				end if;
			end if;
		else
			focus( window-object );
			activate( window-object, event, makeActive );
			if( makeActive )
				app.front-window := window-object;
			end if;
		end if;
	end if;
	
	values();
	
end method application-window-activate;


/*
	application-window-update
*/

define open generic application-window-update( app :: <simple-application>, window :: <WindowRef>, event :: <EventRecord*> );

define method application-window-update( app :: <simple-application>, window :: <WindowRef>, event :: <EventRecord*> )
=> ()

	let window-object = window-to-object( window );
	
	if( window-object )
	
		update( window-object, event );
	
	end if;
	
	values();
		
end method application-window-update;


/*
	application-apple-event
*/

define open generic application-apple-event( app :: <simple-application>,  event :: <EventRecord*> ) => ();

define method application-apple-event( app :: <simple-application>, event :: <EventRecord*> )
=> ()
	
	let err :: <OSErr> = AEProcessAppleEvent( event );
	
	//-if (err ~= errAEEventNotHandled &
	//-	err ~= errAECantSupplyType )
		//-FailOSErr(err);
	//-end if;
        
        values();
	
end method application-apple-event;


/*
	application-os-event
*/

define open generic application-os-event( app :: <simple-application>, theEvent :: <EventRecord*> ) => (); 

define method application-os-event( app :: <simple-application>, event :: <EventRecord*> )
=> ()

	if( logand( message-value( event ), $suspendResumeMessage ) )
		app.background := #f;
	else
		app.background := #t;
	end if;

	if( app.front-window ~= #f ) 
		Activate( app.front-window, event, ~ app.background );
	end if;

	values();

end method application-os-event;


/*
	about-box
*/

define open generic about-box( app :: <simple-application> ) => ();

define method about-box( app :: <simple-application> )
=> ()
	
	StandardAlert( $kAlertStopAlert, "About This Application",
                                "This application was made using d2c from Gwydion Dylan maintainers. http://www.gwydiondylan.org/", 
                                as( <AlertStdAlertParam>, $NULL ) );

	values();
	
end method about-box;