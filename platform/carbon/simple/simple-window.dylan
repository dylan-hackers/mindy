module: simple

/*
	NOTES

	-How do I start?
	-You should subclass <simple-window> and draw() to draw your windows' contents.
	
	-If you wish to trap events, override the window-key, window-click or 
	window-menu-choice methods.
	
	-If your window has resources (handles, movies) it must allocate and dispose of,
	override initialize-window() and finalize-window().
	
	-Why are windows created programatically rather than from a resource?
	-It's easier to start this way. Configuration information could still be stored
	in resources.
*/


/*
	<simple-window>
*/

define open abstract class <simple-window> ( <object> )

	slot finished :: <boolean>, init-value: #f;		// if true, app should delete this window

	slot windowRef :: <WindowRef>, init-value: as( <WindowRef>, $NULL );
	
	slot grafPort :: <CGrafPtr>, init-value: as( <CGrafPtr>, $NULL );
	
	slot modal :: <boolean>, init-value: #f;
	
	slot draggable :: <boolean>, init-value: #t;
	
	slot resizeable :: <boolean>, init-value: #t;
	
	slot closeable :: <boolean>, init-value: #t;
	
	slot zoomable :: <boolean>, init-value: #f;
	
	slot floating :: <boolean>, init-value: #f;
	
end class <simple-window>;


/*
	refcon <-> window object mapping 
*/

define constant *objects-by-windowRef* :: <table> = make( <table> );

define method window-to-object( window ::<WindowRef> )
=> ( result :: type-union( <simple-window>, <boolean> ) ) //- #f

    //- Check for #f and throw an exception if not found!
    element( *objects-by-windowRef*, as( <integer>, window.raw-value ), default: #f );
	
end method window-to-object;


/*
	initialize
*/

define method initialize(	window :: <simple-window>,
                                #key is-modal :: <boolean> = #f,  
                                can-drag :: <boolean> = #t,
                                can-resize :: <boolean> = #f, 
                                can-close :: <boolean> = #t, 
                                can-zoom :: <boolean> = #f,
                                is-floating :: <boolean> = #f,
                                bounds :: <Rect*> = make( <Rect*>, top: 40, left: 40, right: 400, bottom: 300 ),
                                title :: <pascal-string> = as( <pascal-string>, "Untitled" ) )
=> ( window :: <simple-window> )

	window.modal := is-modal;
	window.draggable := can-drag;
	window.resizeable := can-resize;
	window.closeable := can-close;
	window.zoomable := can-zoom;
	window.floating := is-floating;

	// create the window
	
	make-window( window, bounds, title );
	
	if( window.windowRef ~= $NULL )
	
		window.grafPort := GetWindowPort( window.windowRef );
		
		*objects-by-windowRef*[ as( <integer>, window.windowRef.raw-value ) ] := window;
	
		initialize-window( window );
		
		SelectWindow( window.windowRef );

		window;
		
	else
		// LATER: handle this!
		#f;
	
	end if;
	
end method initialize;


/*
	make-window
*/

define method make-window(  window :: <simple-window>, bounds :: <Rect*>, title :: <pascal-string>  )
=> ()

	let defProc =	case
						
                                ( window.floating & window.resizeable & window.zoomable ) => $floatZoomGrowProc;
                                
                                ( window.floating & window.resizeable ) => $floatGrowProc;
                                
                                ( window.floating & window.zoomable ) => $floatZoomProc;
                                
                                ( window.floating ) =>	$floatProc; 
            
                                ( ( ~ window.draggable ) | window.modal ) => $dBoxproc;
                                
                                ( window.modal ) => $movableDBoxProc;
                                
                                ( ~ window.resizeable ) => $noGrowDocProc;
            
                                ( window.zoomable & ~ window.resizeable ) => $zoomNoGrow;
                                
                                ( window.zoomable ) => $zoomDocProc;
                                
                                otherwise => $documentProc;
                        
                        end case;

	window.windowRef := NewWindow( $NULL, bounds, title, #t, defProc, window.closeable, $NULL, 0 );
        
        BringToFront( window.windowRef );
	
	values();
	
end method make-window;


/*
	finalize
*/

define open generic finalize( window :: <simple-window> ) => ();

define method finalize( window :: <simple-window> )
=> ()

	remove-key!( *objects-by-windowRef*, window.windowRef );

	finalize-window( window );
	
	if( window.windowRef ~= $NULL )
		DisposeWindow( window.windowRef );
		window.windowRef := $NULL;
	end if;
	if( *application* & *application*.front-window = window )
		*application*.front-window := #f;
	end if;
	values();
	
end method finalize;


/*
	initialize-window
	override for subclasses
*/

define open generic initialize-window( window :: <simple-window> ) => ();

define method initialize-window( window :: <simple-window> )
=> ()
	
	values();
	
end method initialize-window;


/*
	finalize-window
	override for subclasses
*/

define open generic finalize-window( window :: <simple-window> ) => ();

define method finalize-window( window :: <simple-window> )
=> ()
	
	values();
	
end method finalize-window;


/*
	focus
*/

define open generic focus( window :: <simple-window> ) => ();

define method focus( window :: <simple-window> )
=> ()

	SetPort( window.grafPort );
	SetOrigin( 0, 0 );
	ClipRect( GetPortBounds( window.grafPort ) );
	
	values();
	
end method focus;


/*
	refresh
*/

define open generic refresh( window :: <simple-window>, subrect :: type-union( <Rect*>, <boolean> ) ) => ();

define method refresh( window :: <simple-window>, subrect :: type-union( <Rect*>, <boolean> ) ) // type-union( <Rect*>, #f );
=> ()

	focus( window );
	
	if( subrect )
		InvalWindowRect( window.windowRef, subrect );
	else
		InvalwindowRect( window.windowref, GetPortBounds( window.grafPort ) );	//- Not Carbon!
	end if;
	
	values();
	
end method refresh;


/*
	draw
*/

define open generic draw( window :: <simple-window> );

define method draw( window :: <simple-window> )
=> ()

	values();

end method draw;


/*
	activate
*/

define open generic activate( window :: <simple-window>, event :: <EventRecord*>, activating :: <boolean>  );

define method activate( window :: <simple-window>, event :: <EventRecord*>, activating :: <boolean>  )
=> ()

	values();

end method activate;


/*
	update
*/

define open generic update( window :: <simple-window>, event :: <EventRecord*>  );

define method update( window :: <simple-window>, event :: <EventRecord*>  )
=> ()

	let savePort :: <CGrafPtr> = GetPort();
		if( window ~= #f )
		 	focus( window );
		 end if;
			
		BeginUpdate( window.windowRef );
		
		if( window ~= #f ) 
			
			draw( window );
			
			//DrawControls( window );
	
			if( window.resizeable )
				DrawGrowIcon( window.windowRef );
			end if;
			
		end if;
		
		EndUpdate( window.windowRef );
			
		SetPort( savePort );

	values();

end method update;


/*
	resize
*/

define open generic resize( window :: <simple-window>, width :: <integer>, height :: <integer> );

define method resize( window :: <simple-window>, width :: <integer>, height :: <integer> )
=> ()

	SizeWindow( window.windowRef, width, height, #t );
	
	refresh( window, #f );

	values();

end method resize;


/*
	can-close?
*/

define open generic can-close?( win :: <simple-window> ) => ( result :: <boolean> ); 

define method can-close?( window :: <simple-window> )
=> ( result :: <boolean> )
	
	#t;
	
end method can-close?;


/*
	close
*/

define open generic close-window( window :: <simple-window> ) => ();

define method close-window( window :: <simple-window> )
=> ()

	window.finished = #t; 

	if( window.windowRef ~= $NULL ) 
		HideWindow( window.windowRef );
	end if;

	values();

end method close-window;


/*
	window-idle
*/

define open generic window-idle( win :: <simple-window>, idle-event :: <EventRecord*> ); 

define method window-idle( win :: <simple-window>, idle-event :: <EventRecord*> )
=> ()

	values();

end method window-idle;


/*
	window-click
*/

define open generic window-click( window :: <simple-window>, event :: <EventRecord*>, localPoint :: <Point*>  ) => ();

define method window-click( window :: <simple-window>, event :: <EventRecord*>, localPoint :: <Point*>  )
=> ()

	values();

end method window-click;


/*
	window-key
*/

define open generic window-key( win :: <simple-window>, event :: <EventRecord*>, charCode :: <character>, keyCode :: <integer> );

define method window-key( win :: <simple-window>, event :: <EventRecord*>, charCode :: <character>, keyCode :: <integer> )
=> ()

	values();

end method window-key;


/*
	window-menu-choice
	Returns true if the window handled the event, 
	false if it didn't and the application should handle it.
*/

define open generic window-menu-choice( win :: <simple-window>, menu :: <integer>, item :: <integer> ) => ( result :: <boolean> );

define method  window-menu-choice(  win :: <simple-window>, menu :: <integer>, item :: <integer> )
=> ( result :: <boolean> )

    let handled = #f;

    if( menu = $FileMenuID )
        if(item = 1)	// Close window
            if( win.closeable )
                close-window( win );
            end if;
            handled := #t;
        end if;
    end if;

    handled;
    
end method window-menu-choice;
