module: my-simple

//-	TODO: Use embedded controls to handle multiple radio button iteration


define class <my-window> (<simple-window>)
end class <my-window>;

define method initialize(	window :: <my-window>,
                                #key all-keys )
=> ( window :: <my-window> )
    
    next-method();
    let bounds :: <Rect*> = make( <Rect*>, top: 50, left: 50, right: 130, bottom: 70 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Button" ),
                                                #t, 0, 0, 255, $pushButProc, 0 );
                                                
    let bounds :: <Rect*> = make( <Rect*>, top: 100, left: 50, right: 150, bottom: 140 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Checkbox" ),
                                                #t, 0, 0, 255, $checkBoxProc, 0 );
                                                
    let bounds :: <Rect*> = make( <Rect*>, top: 150, left: 50, right: 230, bottom: 190 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Radio Button" ),
                                                #t, 0, 0, 255, $radioButProc, 0 );

    let bounds :: <Rect*> = make( <Rect*>, top: 200, left: 50, right: 230, bottom: 240 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Scroll Bar" ),
                                                #t, 0, 0, 255, $scrollbarProc, 0 );
    
    let bounds :: <Rect*> = make( <Rect*>, top: 250, left: 50, right: 130, bottom: 290 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Popup Menu" ),
                                                #t, 0, 0, 255, $popupMenuProc, 0 );

    let bounds :: <Rect*> = make( <Rect*>, top: 50, left: 250, right: 330, bottom: 70 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Edit Text Control" ),
                                                #t, 0, 0, 255, $kControlEditTextProc, 0 );
    SetKeyboardFocus( window.windowRef, control, $kControlEditTextPart );

    let bounds :: <Rect*> = make( <Rect*>, top: 100, left: 250, right: 330, bottom: 120 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Password text Control" ),
                                                #t, 0, 0, 255, $kControlEditTextPasswordProc, 0 );

    let bounds :: <Rect*> = make( <Rect*>, top: 150, left: 250, right: 330, bottom: 170 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Inline text Control" ),
                                                #t, 0, 0, 255, $kControlEditTextInlineInputProc, 0 );
    
    let bounds :: <Rect*> = make( <Rect*>, top: 200, left: 250, right: 330, bottom: 220 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Static text Control" ),
                                                #t, 0, 0, 255, $kControlStaticTextProc, 0 );
                                                
                                                
                                                
    window;
    
end method initialize;

define method draw( window :: <my-window> )
=> ()

    DrawControls( window.windowRef );	//- Should use UpdateControls!
    values();

end method draw;

define method window-click( win :: <my-window>, event :: <EventRecord*>, localPoint :: <Point*>  )
=> ()
    
    let( control, part ) = FindControl( localPoint, win.windowRef );	//- Use FindControlUnderMouse!
    if( control ~= $NULL )
        HandleControlClick( control, localPoint, event.modifiers-value, as( <ControlActionUPP>, $NULL ) );
        handle-control( control, win, localPoint ); // We update state
    end if;

    values();

end method window-click;

define method window-key( win :: <my-window>, event :: <EventRecord*>, charCode :: <character>, keyCode :: <integer> )
=> ()

    if( charCode = '\t' )
        AdvancekeyboardFocus( win.windowRef );
    else
        let control = GetKeyboardFocus( win.windowRef );
        if( control ~= $NULL )
            HandleControlKey( control, as( <SInt16>, keyCode), as( <SInt16>, charCode), event.modifiers-value );
        end if;
    end if;
    values();

end method window-key;

define method window-idle( win :: <my-window>, idle-event :: <EventRecord*> )
=> ()

    IdleControls( win.windowRef );
    values();

end method window-idle;

/*
	handle-checkbox
*/

define method handle-checkbox-control( control :: <ControlHandle> )
=> ()
	
	let state = GetControlValue( control );	// Get whether the box is checked or not
	let flipped-state =	if( state = 1 )		// Toggle it
                                        0;
                                else
                                        1;
                                end if;	
	SetControlValue( control, flipped-state );		// Set the toggled state
	
	values();
	
end method handle-checkbox-control;

/*
	handle-radio-button
*/

define method handle-radio-button-control( control :: <ControlHandle> )
=> ()
	
	let state = GetControlValue( control );	// Get whether the radio button is checked or not
	let flipped-state =	if( state = 1 )		// Toggle it
                                        0;
                                else
                                        1;
                                end if;	
	SetControlValue( control, flipped-state );		// Set the toggled state
	
	values();
	
end method handle-radio-button-control;

/*
	handle-control
*/

define method handle-control( control :: <ControlHandle>, win :: <simple-window>, where :: <Point*> )
=> ( result :: <boolean> )

	let part = TestControl( control, where );
	
	select( part )

		$kControlRadioButtonPart =>	handle-radio-button-control( control );
                                                #t;
		
		$kControlCheckBoxPart =>	handle-checkbox-control( control );
                                                #t;
		
		$kControlEditTextPart => 	SetKeyboardFocus( win.windowRef, control, $kControlEditTextPart );
                                                #t;
		
		$kControlButtonPart =>		#t;
						
		otherwise =>	#f;
	
	end select;

end method handle-control;