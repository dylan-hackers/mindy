Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   Scott McKay, Andy Armstrong, Rob Myers
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
  A <carbon-mirror> is a high-level graphical object that may be drawn.
  This is a Control, a GWorld or a Window, not a PixMap or a Device.
  All <carbon-mirror> objects have a CGrafPtr: 
    for Windows and GWorlds this is their own CGrafPtr, 
    for Controls this is the CGrafPtr of their containing Window.
  The mapping of DUIM types to Carbon types is:
    <top-level-sheet> => WindowRef => <top-level-sheet-mirror>
    <gadget> => ControlHandle => <gadget-mirror>
    <pixmap> => GWorld => <pixmap-mirror>
*/


////////////////////////////////////////////////////////////////////////////////
/// Carbon panes
////////////////////////////////////////////////////////////////////////////////

define open abstract class <carbon-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <carbon-pane-mixin>;

// Returns #t, meaning that the port will take care of repainting
define method port-handles-repaint?
    (_port :: <carbon-port>, sheet :: <mirrored-sheet-mixin>)
 => (true? :: <boolean>)
  #t
end method port-handles-repaint?;


////////////////////////////////////////////////////////////////////////////////
/// Base Mirrors
////////////////////////////////////////////////////////////////////////////////

//--- This class wraps up the real window system object
define open abstract class <carbon-mirror> (<mirror>)
  sealed slot mirror-grafport :: <CGrafPtr>,
     required-init-keyword: grafport:;
  sealed slot mirror-sheet :: <sheet>,
    init-keyword: sheet:;
  // XXX - Move these to a better place?
  sealed slot %ink-cache :: <object-table> = make(<table>);
  sealed slot %region :: <region>, 
    init-keyword: region:;
end class <carbon-mirror>;

define method initialize
    (mirror :: <carbon-mirror>, #key) => ()
  next-method();
  sheet-direct-mirror(mirror-sheet(mirror)) := mirror;
end method initialize;

define sealed method do-make-mirror
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (mirror :: <carbon-mirror>)
  let mirror = make-carbon-mirror(sheet);
  let parent = sheet-device-parent(sheet);
  set-mirror-parent(mirror, sheet-direct-mirror(parent));
  install-event-handlers(sheet, mirror);
  update-mirror-attributes(sheet, mirror);
  mirror;
end method do-make-mirror;

////////////////////////////////////////////////////////////////////////////////
/// Empty methods on non-window mirrors
////////////////////////////////////////////////////////////////////////////////

define method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  //--- Deallocate all window system resources
  debug-message("destroy-mirror <carbon-mirror>");
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method map-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  //--- Do it
end method map-mirror;

define method unmap-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  //--- Do it
end method unmap-mirror;

define method raise-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>,
     #key activate? = #t) => ()
  //--- Do it
end method raise-mirror;

define method lower-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  //--- Do it
end method lower-mirror;

define method mirror-visible? 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>)
 => (visible? :: <boolean>)
  //--- Do it
  #t;
end method mirror-visible?;

// Returns the edges of the mirror in its parent's coordinate space
define method mirror-edges 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  //box-edges(mirror.%region)
  values(0, 0, 100, 100);	//--- kludge city
end method mirror-edges;

// Sets the edges of the mirror in its parent's coordinate space
define method set-mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom)
end method set-mirror-edges;

// Ditto...
define method install-event-handlers
    (sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method install-event-handlers;

// Ditto...
define method update-mirror-attributes
    (sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method update-mirror-attributes;

////////////////////////////////////////////////////////////////////////////////
/// Sheet Mirrors
////////////////////////////////////////////////////////////////////////////////

/// Mirror creation and destruction

define abstract class <control-mirror> (<carbon-mirror>)
  sealed slot mirror-control :: false-or(<ControlHandle>),
    required-init-keyword: control:;
end class <control-mirror>;

define sealed domain make (singleton(<control-mirror>));
define sealed domain initialize (<control-mirror>);

define sealed inline method make
    (mirror :: subclass(<control-mirror>), #rest args, #key sheet)
 => (mirror :: <control-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  apply(next-method, mirror,
	region: make-bounding-box(left, top, right, bottom),
	args);
end method make;

define method initialize
    (mirror :: <control-mirror>, #key) => ()
  next-method();
  let control = mirror-control(mirror);
    control-mirror(control) := mirror;
end method initialize;

define sealed method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>) => ()
  let control = mirror-control(mirror);
  if(control)
    mirror-control(mirror) := #f;
    DisposeControl(control);
  end if;
end method destroy-mirror;

//---*** WHAT ABOUT THIS?  WHO IS SUPPOSED TO CALL IT?
// Called by main WM_DESTROY handler
define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <control-mirror>) => ()
  ignoring("note-mirror-destroyed")
  // let handle :: <HWND> = window-handle(mirror);
  // window-mirror(handle) := #f;
  // window-handle(mirror) := $NULL-HWND
end method note-mirror-destroyed;

/// Mirror manipulation

// For non-top-level sheets, we just show the control
define sealed method map-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>) => ()
  let control :: <ControlHandle> = mirror-control(mirror);
  debug-message("Showing %=", sheet);
  ShowControl(control);
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>) => ()
  let control :: <ControlHandle> = mirror-control(mirror);
  HideControl(control);
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>,
     #key activate? = #t)
 => ()
  let control = mirror-control(mirror);
  ignoring("raise-mirror for <control-mirror>");
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>) => ()
  let control = mirror-control(mirror);
  ignoring("lower-mirror for <control-mirror>");
end method lower-mirror;

define sealed method mirror-visible? 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>)
 => (visible? :: <boolean>)
  let control :: <ControlHandle> = mirror-control(mirror);
  IsControlVisible(control);
end method mirror-visible?;

define sealed method mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region);
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <control-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => ()
  debug-message(format-to-string("set-mirror-edges %= %=",
                sheet, mirror));
  let width  = right - left;
  let height = bottom - top;
  let old-region = mirror.%region;
  let (old-left, old-top)     = box-position(old-region);
  let (old-width, old-height) = box-size(old-region);
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom);
  if (left ~== old-left | top ~== old-top)
    MoveControl(mirror.mirror-control, left, top);
  end;
  if (width ~== old-width | height ~== old-height)
    SizeControl(mirror.mirror-control, width, height);
  end;
end method set-mirror-edges;

// Returns the position of the sheet in "absolute" (screen) coordinates
define sealed method sheet-screen-position
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let ancestor  = sheet-device-parent(sheet);
  let transform = sheet-delta-transform(sheet, ancestor);
  // Get the position of the sheet in its mirrored parent's coordinates
  let (x, y) = transform-position(transform, 0, 0);
  let mirror = sheet-direct-mirror(ancestor);
  client-to-screen-position(mirror, x, y);
end method sheet-screen-position;

// Given a position (x, y) within a mirror, convert it to a position on the screen
define sealed method client-to-screen-position
    (mirror :: <control-mirror>, x :: <integer>, y :: <integer>)
 => (screen-x :: <integer>, screen-y :: <integer>)
  ignoring("client-to-screen-position");
  values(x, y);
end method client-to-screen-position;

// Map control-mirrors to ControlHandles

define constant $mirror-ControlHandle-table  :: <object-table> = make(<table>);

define sealed method control-mirror(handle :: <ControlHandle>) 
=> (mirror :: false-or(<control-mirror>))
  element($mirror-ControlHandle-table, pointer-address(handle), default: #f);
end method control-mirror;

define sealed method control-mirror-setter(mirror :: <control-mirror>, handle :: <ControlHandle>)
 => (mirror :: <control-mirror>)
  element($mirror-ControlHandle-table, pointer-address(handle)) := mirror;
end method control-mirror-setter;

define sealed method control-mirror-setter(mirror :: singleton(#f), handle :: <ControlHandle>) 
=> (mirror :: singleton(#f))
  remove-key!($mirror-ControlHandle-table, pointer-address(handle));
  #f;
end method control-mirror-setter;

define sealed method ControlHandle-sheet(handle :: <ControlHandle>)
 => (sheet :: false-or(<mirrored-sheet-mixin>))
  let mirror = control-mirror(handle);
  mirror & mirror-sheet(mirror);
end method ControlHandle-sheet;

define method set-mirror-parent
    (child :: <control-mirror>, parent :: <control-mirror>)
 => ()
  EmbedControl(child.mirror-control, parent.mirror-control);
end method set-mirror-parent;

////////////////////////////////////////////////////////////////////////////////
/// Fixed container mirrors
///
/// The class of mirror that can contain other mirrors
////////////////////////////////////////////////////////////////////////////////

define class <fixed-container-mirror> (<control-mirror>)
end class <fixed-container-mirror>;

define class <drawing-area-mirror> (<control-mirror>)
end class <drawing-area-mirror>;

define method make-carbon-mirror 
    (sheet :: <mirrored-sheet-mixin>)
=> (mirror :: <control-mirror>)
  do-make-carbon-mirror(sheet);
end method make-carbon-mirror;

define method do-make-carbon-mirror 
    (sheet :: <mirrored-sheet-mixin>)
=> (mirror :: <control-mirror>)
  debug-message("do-make-carbon-mirror <mirrored-sheet-mixin>");
  let ancestor  = sheet-device-parent(sheet);
  let mirror = sheet-direct-mirror(ancestor);
  let window :: <WindowRef> = GetWindowFromPort(mirror.mirror-grafport);
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let bounds-rect :: <Rect*> = make(<Rect*>, top: top, left: left, bottom: bottom, right: right);
  ensure-rect-valid("<fixed-container-mirror> bounds", bounds-rect);
  let control :: <ControlHandle> = NewControl(window, bounds-rect, $empty-pascal-string, #t,
                                              $kControlSupportsEmbedding, 0, 0, $kControlUserPaneProc, 0);
  debug-message(format-to-string("ControlHandle %d", control.pointer-address));
  //---*** We really want to switch this off entirely...
  make(<fixed-container-mirror>,
       grafport: mirror.mirror-grafport,
       control: control,
       sheet:  sheet);
end method do-make-carbon-mirror;

define method do-make-carbon-mirror
    (sheet :: <standard-repainting-mixin>)
  => (mirror :: <control-mirror>)
  debug-message("do-make-carbon-mirror <standard-repainting-mixin>");
  let ancestor  = sheet-device-parent(sheet);
  let mirror = sheet-direct-mirror(ancestor);
  let window :: <WindowRef> = GetWindowFromPort(mirror.mirror-grafport);
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let bounds-rect :: <Rect*> = make(<Rect*>, top: top, left: left, bottom: bottom, right: right);
  ensure-rect-valid("<darwing-area-mirro> bounds", bounds-rect);
  let control :: <ControlHandle> = NewControl(window, bounds-rect, $empty-pascal-string, #t,
                                              0, 0, 0, $kControlUserPaneProc, 0);
  debug-message(format-to-string("ControlHandle %d", control.pointer-address));
  make(<drawing-area-mirror>,
       grafport: mirror.mirror-grafport,
       control: control,
       sheet:  sheet);
end method do-make-carbon-mirror;

define method install-event-handlers
    (sheet :: <mirrored-sheet-mixin>, mirror :: <carbon-mirror>) => ()
  debug-message(format-to-string("install-event-handlers %=, %= [do-nothing]", sheet, mirror));
  // Do nothing
end method install-event-handlers;

// Install Drawing Area Events

define method install-event-handlers
    (sheet :: <mirrored-sheet-mixin>, mirror :: <drawing-area-mirror>) => ()
  debug-message("install-event-handlers <mirrored-sheet-mixin>, <drawing-area-mirror>");
  let control :: <ControlHandle> = mirror.mirror-control;
  install-control-event-handler(control, $kEventClassControl, $kEventControlDraw, 
                               $control-draw-event-handler-upp);
end method install-event-handlers;

////////////////////////////////////////////////////////////////////////////////
/// Port defaults
////////////////////////////////////////////////////////////////////////////////

define method port-default-foreground
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  //--- Consult resources here...
  ignoring("port-default-foreground");
  #f
end method port-default-foreground;

define method port-default-background
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>))
  //--- Consult resources here...
  ignoring("port-default-background");
  #f
end method port-default-background;

define method port-default-background
    (_port :: <carbon-port>, sheet :: <drawing-pane>) => (background :: false-or(<ink>))
  $white;
end method port-default-background;

// Viewports try to take their background from their child
define method port-default-background
    (_port :: <carbon-port>, sheet :: <viewport>) => (background :: false-or(<ink>))
  let child = sheet-child(sheet);
  if (child)
    port-default-background(_port, child)
  else
    next-method();
  end;
end method port-default-background;

define method port-default-text-style
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  //--- Consult resources here...
  ignoring("port-default-text-style");
  #f;
end method port-default-text-style;
