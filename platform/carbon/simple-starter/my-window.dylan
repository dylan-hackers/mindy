module: my-simple

//	This is the code for your custom window class.
//	This file will contain most of the "guts" of your application.

define class <my-window> (<simple-window>)
end class <my-window>;

define method initialize(	window :: <my-window>,
                                #key all-keys )
=> ( window :: <my-window> )
    
    next-method();
    let bounds :: <Rect> = make( <Rect>, top: 50, left: 50, right: 230, bottom: 70 );
    let control :: <ControlHandle> = NewControl( window.windowRef, bounds, 
                                                as( <pascal-string>, "Button" ),
                                                #t, 0, 0, 255, $pushButProc, 0 );
    window;
    
end method initialize;

define method draw( window :: <my-window> )
=> ()

    DrawControls( window.windowRef );	//- Should use UpdateControls!
    values();

end method draw;

