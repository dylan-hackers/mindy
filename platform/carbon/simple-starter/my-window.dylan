module: my-simple

//	This is the code for your custom window class.
//	This file will contain most of the "guts" of your application.

define class <my-window> (<simple-window>)
end class <my-window>;

define method draw( window :: <my-window> )
=> ()

	let r = make( <Rect>, top: 0, left: 0, right: 2000, bottom: 2000 );
	EraseRect( r );
	MoveTo( 50, 50 );
	DrawString( as( <pascal-string>, "Hello!" ) );

	values();

end method draw;


define method window-click( window :: <my-window>, event :: <EventRecord>, localPoint :: <Point> )
=> ()

	let r = make( <Rect>, top: 0, left: 0, right: 2000, bottom: 2000 );

	focus( window );

	InvertRect( r );

	values();

end method window-click;


define method window-idle( window :: <my-window>, event :: <EventRecord> )
=> ()

	focus( window );

	let col = make( <RGBColor> );
	let r = make( <Rect>, top: 10, left: 10, right: 20, bottom: 20 );
	
	if( modulo( event.event-when, 60 ) < 30 )
		col.red := 65535;
		col.green := 0;
		col.blue := 0;
	else
		col.red := 0;
		col.green := 0;
		col.blue := 65535;
	end if;
	
	RGBForeColor( col );
	PaintRect( r );

         QDFlushPortBuffer( window.grafPort, 
                            GetPortVisibleRegion( window.grafPort, NewRgn() ) );

	values();

end method window-idle;
