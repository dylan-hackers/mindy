Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	      Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
              Carbon Modifications Copyright (c) 2001 Gwydion Dylan Maintainers.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
  The Mac has multiple displays, which may have varying characteristics.
	We just use the main device.
  We always calculate its bounds dynamically in case QuickTime, DrawSprocket
  or another program has hidden the menu bar, or the Appearance has changed
  (this last case is unlikely in MacOS X).
  Since we are not Display Manager aware, we won't be able to move windows
  when the screens bounds or resolution changes. Hopefully MacOS will nudge
  our windows into position if their title bars or bounds end up off-screen
  or under the menu bar when the display changes.
*/

// A Mac pixel is almost a point
define constant $points-per-mm = 72.0 / 25.4;

// Mirror the display, set its region, and set its characteristics
define sealed method initialize-display 
    (_port :: <carbon-port>, _display :: <standard-display>) => ()
  //---*** Not sure what to do for this one:
  //--- sheet-direct-mirror(_display) := --screen--;
  //--- fill in _port.%display, too
  //---*** Need to do the equivalent of 'w::init-screen-device'
  /*let (width, height) = display-size(_display);
  display-pixel-width(_display)  := width;
  display-pixel-height(_display) := height;
  display-mm-width(_display) := floor(display-pixel-width(_display) * $points-per-mm);
  display-mm-height(_display) := floor(display-pixel-height(_display) *
  $points-per-mm);*/
  // XXX - LMGetScrHRes(), LMGetScrVRes may help
  display-pixels-per-point(_display) := 1.0; // XXX - Can we get the actual amount using Display manager?
end method initialize-display;

define method display-pixel-width
    (_display :: <standard-display>)
 => (width :: <integer>)
  let(display-width, display-height) = display-size(_display);
  display-width;
end method display-pixel-width;

define method display-pixel-height
    (_display :: <standard-display>)
 => (width :: <integer>)
  let(display-width, display-height) = display-size(_display);
  display-height;
end method display-pixel-height;

define method display-mm-width
    (_display :: <standard-display>)
 => (width :: <integer>)
  floor(display-pixel-width(_display) * $points-per-mm);
end method display-mm-width;

define method display-mm-height
    (_display :: <standard-display>)
 => (width :: <integer>)
  floor(display-pixel-height(_display) * $points-per-mm);
end method display-mm-height;

define sealed method display-size
    (_display :: <standard-display>)
 => (width :: <integer>, height :: <integer>)
  // Get the main GDevice's pixmap and get its bounds
  let screen-bits :: <BitMap*> = GetQDGlobalsScreenBits();
  let screen-bounds :: <Rect*> = screen-bits.bounds-value;
  let display-width = screen-bounds.right-value - screen-bounds.left-value;
  let display-height = screen-bounds.bottom-value - screen-bounds.top-value;
  // Allow for the menu bar (if visible) in our height and top
  let display-top :: <integer> = 0;
  display-top := GetMBarHeight();
  display-height := display-height - display-top;
  sheet-region(_display) :=
    set-box-edges(sheet-region(_display),
		  0, display-top, display-width, display-height);
  values(display-width, display-height);
end method display-size;

