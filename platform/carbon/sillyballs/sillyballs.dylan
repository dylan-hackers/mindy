module: sillyballs
author: Gareth Baker and Rob Myers yarrel @netscape.net
synopsis: Test for the Toolbox library
copyright: Gwydion Dylan Maintainers 2000

// Constants

define constant	$ball-width  = 20;
define constant	$ball-height = 20;
define constant	$bob-size    = 8; // Size of text in each ball.
define constant $hbw         = truncate/($ball-width, 2);
define constant $hbh         = truncate/($ball-height, 2);
define constant $hbz         = truncate/($bob-size, 2);

// main
// make a window, write in it, then quit on a mouse click

define method main ( argv0 :: <byte-string>, #rest noise )

  local method newBall( wind-rect :: <Rect*> )

          let ball-color :: <RGBColor*> = make( <RGBColor*> );

          // 
          //	Make a random new color for the ball.
          //
          ball-color.red-value   := random(65535);
          ball-color.green-value := random(65535);
          ball-color.blue-value  := random(65535);
          
          
          RGBForeColor(ball-color);
          
          let new-top = truncate/(((random(1000)) * wind-rect.bottom-value), 1000);
          let new-left = truncate/(((random(1000)) * wind-rect.right-value), 1000);
          let ball-rect :: <Rect*> = make( <Rect*>,
                                           top: new-top,
                                           left: new-left,
                                           bottom: new-top + $ball-height,
                                           right: new-left + $ball-width );

          //
          //	Move pen to the new location, and paint the colored ball.
          //
          MoveTo( new-left, new-top );
          PaintOval( ball-rect );
          
          //
          //	Move the pen to the middle of the new ball position, for the text
          //
          MoveTo( ball-rect.left-value + $hbw - $bob-size, 
                  ball-rect.top-value + $hbh + $hbz - 1);
          
          //
          //	Invert the color and draw the text there.  This won't look quite right in
          //	1 bit mode, since the foreground and background colors will be the same.
          //	Color QuickDraw special cases this to not invert the color, to avoid
          //	invisible drawing.
          //
          InvertColor( ball-color ); 
          RGBForeColor( ball-color );
          
          DrawString( as( <pascal-string>, "d2c" ) );
          
        end method newBall;
  

  block (return)

    // Bring this process to the front. This is necessary because we don't have an event
    // loop, which would receive an activate event. Without one, our window may be behind
    // the windows of other applications.
    let (err, psn) = GetCurrentProcess();
    if (err = 0)
      err = SetFrontProcess( psn );
      unless (err = 0)
        return();
      end unless;
    end if;
    
    // Define the bounds of the window
    let window-rect :: <Rect*> = make( <Rect*>, top: 100, left: 100, bottom: 350, right: 450 );
    
    // Make its name as a pascal string
    let window-title :: <pascal-string> = as( <pascal-string>, "Click Mouse to Exit." );
    
    // Make a new window without using a resource
    // Dylan definition translates to NewCWindow!!
    //break();
    let my-window :: <WindowRef> = NewWindow( $NULL,          // Mac allocates storage
                                              window-rect,    // The bounds
                                              window-title,   // The title
                                              #t,             // Initially visible
                                              $noGrowDocProc, // window definition proc (WDEF)
                                              #f,             // No go-away box
                                              $NULL,          // At the back, BOEHM CHOKES ON PTR = -1
                                              0 );            // No refcon
    
    // Show the window
    BringToFront( my-window );  // Work around having to create windows at the back to avoid ptr: -1
    ShowWindow( my-window );
    
    // Set it as the current graphics port
    SetPortWindowPort( my-window );
    
    // Set the text size once
    TextSize( $bob-size );      // smaller font for drawing.
    
    // Wait for a mouse click
    while ( ~Button() )
      newBall( window-rect );
      QDFlushPortBuffer( GetWindowPort( my-window ), 
                         GetPortVisibleRegion( GetWindowPort( my-window ), NewRgn() ) );
    end while;
    
    // We've finished with the window, so dispose of it
    DisposeWindow( my-window );
    
  end block;
end method main;
