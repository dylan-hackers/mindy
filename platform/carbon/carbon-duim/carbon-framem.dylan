Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   		Scott McKay, Andy Armstrong, Rob Myers
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////////////////////////////////////////////////////////////////////////////////
/// Useful constants
////////////////////////////////////////////////////////////////////////////////

// XXX - This should be fetched from Appearance
define constant $top-level-border     = 0;
define constant $top-level-y-spacing  = 3;		// in pixels
define constant $default-window-title = "DUIM Window";


////////////////////////////////////////////////////////////////////////////////
/// Carbon Frame Manager
////////////////////////////////////////////////////////////////////////////////

define sealed class <carbon-frame-manager> (<basic-frame-manager>)
end class <carbon-frame-manager>;


define sealed domain make (singleton(<carbon-frame-manager>));
define sealed domain initialize (<carbon-frame-manager>);


define method make-frame-manager
    (_port :: <carbon-port>,
     #key palette, class = <carbon-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


////////////////////////////////////////////////////////////////////////////////
/// Glue to Frames
////////////////////////////////////////////////////////////////////////////////

define method note-frame-title-changed
    (framem :: <carbon-frame-manager>, frame :: <frame>) => ()
    // Update the window title
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let title = as(<pascal-string>, frame-title(frame) | "" );
    SetWTitle(mirror.mirror-window, title)
  end;
end method note-frame-title-changed;


define method note-frame-icon-changed
    (framem :: <carbon-frame-manager>, frame :: <frame>) => ()
  ignoring("note-frame-icon-changed");
end method note-frame-icon-changed;