Module:       carbon-duim
Synopsis:     Carbon top level window handling
Author:       Andy Armstrong, Scott McKay, Rob Myers
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
              Carbon Modifications Copyright (c) 2001 Gwydion Dylan Maintainers.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////////////////////////////////////////////////////////////////////////////////
/// top-level-sheet
////////////////////////////////////////////////////////////////////////////////

// This doesn't remove the initializer precedence warning

define open abstract class <carbon-top-level-sheet-mixin>
    (<standard-repainting-mixin>,
     <permanent-medium-mixin>,		// We need this so we can be repainted
     <carbon-pane-mixin>)
end class <carbon-top-level-sheet-mixin>;

define sealed class <carbon-top-level-sheet>
    (<carbon-top-level-sheet-mixin>,
     <top-level-sheet>)
end class <carbon-top-level-sheet>;

define sealed domain make (singleton(<carbon-top-level-sheet>));
define sealed domain initialize (<carbon-top-level-sheet>);

define sealed method initialize
    (sheet :: <carbon-top-level-sheet>, #key) => ()
  next-method();
  sheet-accepts-focus?(sheet) := #f;
end method initialize;

define method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-top-level-sheet>, #f)
end method class-for-make-pane;

define sealed method top-level-mirror
    (sheet :: <sheet>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  debug-message("top-level-mirror(<sheet>)");
  let sheet  = top-level-sheet(sheet);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;

define sealed method top-level-mirror
    (frame :: <frame>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  debug-message("top-level-mirror(<frame>)");
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;

define method handle-repaint
    (sheet :: <top-level-sheet>, medium :: <medium>, region :: <region>) => ()
  DrawControls(top-level-mirror(sheet, error?: #t).mirror-window);
end method handle-repaint;

////////////////////////////////////////////////////////////////////////////////
/// top-level-mirror
////////////////////////////////////////////////////////////////////////////////

define sealed class <top-level-mirror> (<control-mirror>)
  sealed slot mirror-window :: <WindowRef>,
    required-init-keyword: window:;
  sealed slot %dialog-mirrors :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <top-level-mirror>;

define sealed domain make (singleton(<top-level-mirror>));
define sealed domain initialize (<top-level-mirror>);

define method do-make-carbon-mirror 
    (sheet :: <carbon-top-level-sheet-mixin>)
=> (mirror :: <control-mirror>)
  debug-message("do-make-carbon-mirror <carbon-top-level-sheet-mixin>");
  let frame = sheet-frame(sheet);
  let title = frame-title(frame) | "DUIM Window";
  let (sheet-left, sheet-top, sheet-right, sheet-bottom) = sheet-native-edges(sheet);
  let geometry = frame-geometry(frame);
  let window-left = geometry[0] | 0;
  let window-top = geometry[1] | 0;
  let window-right = if(geometry[2]) window-left + geometry[2] else sheet-right end;
  let window-bottom = if(geometry[3]) window-top + geometry[3] else sheet-bottom end;
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  // Make the Carbon Window   
  let bounds-rect :: <Rect*> = make(<Rect*>, 
                                    top: 0, 
                                    left: 0, 
                                    right: 100, 
                                    bottom: 100);
  let (err :: <OSErr>, window :: <WindowRef>) = 
  	CreateNewWindow($kDocumentWindowClass, 
                        frame-window-attributes(frame),
                        bounds-rect);	
  debug-message(format-to-string("WindowRef: %d", window.pointer-address));
  check-result("CreateNewWindow", window);
  set-window-edges(window, window-left, window-top, window-right, window-bottom);
  // Make the root control
  let (err, root-control) = CreateRootControl(window);
  check-result("CreateRootControl", err);
  // Wrap them in a mirror and tie everything together
  let(left-edge, top-edge, right-edge, bottom-edge) = get-window-edges(window);
  debug-message(format-to-string("got ltrb %d %d %d %d",
                                  window-left, window-top, 
                                  window-right, window-bottom));
  let mirror = make(<top-level-mirror>,
        window: window,
        grafport: GetWindowPort(window),
        control: root-control,
		    sheet: sheet,
		    region: make-bounding-box(left-edge, top-edge, right-edge, bottom-edge));
  // Set up the Carbon window
  SetWTitle(window, as(<pascal-string>, title));
  let background = get-default-background(port(sheet), sheet);
  mirror
end method do-make-carbon-mirror;

define method initialize
    (mirror :: <top-level-mirror>, #key) => ()
  next-method();
  let window = mirror-window(mirror);
  window-mirror(window) := mirror;
end method initialize;

define method frame-window-attributes
    (frame :: <basic-frame>)
 => (style :: <integer>)
 /* let style
    = logior( $kWindowStandardDocumentAttributes, 
							$kWindowStandardHandlerAttribute,
              if (frame-iconified?(frame)) $WS-ICONIC else 0 end,
              if (frame-maximized?(frame)) $WS-MAXIMIZE else 0 end),
              if (frame-always-on-top?(frame)) $WS-EX-TOPMOST else 0 end);
  style;*/
  logior( $kWindowStandardDocumentAttributes, 
          $kWindowStandardHandlerAttribute);
end method frame-window-attributes;

define method set-mirror-parent
    (child :: <top-level-mirror>, parent == #f)
 => ()
  ignoring("set-mirror-parent for <top-level-mirror>")
end method set-mirror-parent;

// Map top-level-mirrors to WindowRefs

define constant $mirror-windowref-table  :: <object-table> = make(<table>);

define sealed method window-mirror(handle :: <WindowRef>) 
=> (mirror :: false-or(<top-level-mirror>))
  element($mirror-windowref-table, pointer-address(handle), default: #f);
end method window-mirror;

define sealed method window-mirror-setter(mirror :: <top-level-mirror>, handle :: <WindowRef>)
 => (mirror :: <top-level-mirror>)
  element($mirror-windowref-table, pointer-address(handle)) := mirror;
end method window-mirror-setter;

define sealed method window-mirror-setter(mirror :: singleton(#f), handle :: <WindowRef>) 
=> (mirror :: singleton(#f))
  remove-key!($mirror-windowref-table, pointer-address(handle));
  #f;
end method window-mirror-setter;

define sealed method WindowRef-sheet(handle :: <WindowRef>)
 => (sheet :: false-or(<mirrored-sheet-mixin>))
  let mirror = window-mirror(handle);
  mirror & mirror-sheet(mirror);
end method WindowRef-sheet;

define method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  next-method();
  debug-message("destroy-mirror(<top-level-mirror>)");
  /*let background-brush = mirror.%background-brush;
  when (background-brush)
    mirror.%background-brush := #f
  end;*/
  let ref :: <WindowRef> = mirror.mirror-window;
  unless (ref = $NULL)
    debug-message("DisposeWindow(mirror.mirror-window)");
    DisposeWindow(ref);	// error-checking probably won't buy anything...
  end;
  unless (instance?(sheet, <top-level-sheet>))
    window-mirror(mirror.mirror-window) := #f;
    mirror.mirror-window := $NULL
  end;
end method destroy-mirror;

define method map-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  ShowWindow(mirror.mirror-window);
end method map-mirror;

define method unmap-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  HideWindow(mirror.mirror-window);
end method unmap-mirror;

define method raise-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>,
     #key activate? = #t) => ()
  ignore(activate?);
  BringToFront(mirror.mirror-window);
end method raise-mirror;

define method lower-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  SendBehind(mirror.mirror-window,$NULL);
end method lower-mirror;

define method mirror-visible? 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>)
 => (visible? :: <boolean>)
  // The mirror is being drawn. It may be under another window, though...
  // XXX - Use IsEmptyRgn(GetPortVisRgn(GetWindowPort(mirror.mirror-window)))???
  IsWindowVisible(mirror.mirror-window);
end method mirror-visible?;

// Returns the edges of the mirror in its parent's coordinate space
define method mirror-edges 
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

// Sets the edges of the mirror in its parent's coordinate space
define method set-mirror-edges
    (_port :: <carbon-port>, sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  debug-message(format-to-string("set-mirror-edges <top> ltrb %d %d %d %d",
                                  left, top, right, bottom));
  set-window-edges(mirror.mirror-window, left, top, right, bottom);
  let(left-edge, top-edge, right-edge, bottom-edge) = get-window-edges(mirror.mirror-window);
  mirror.%region := set-box-edges(mirror.%region, left-edge, top-edge,
    right-edge, bottom-edge);
end method set-mirror-edges;

////////////////////////////////////////////////////////////////////////////////
/// Dialog Handling
////////////////////////////////////////////////////////////////////////////////

/*define method mirror-registered-dialogs
    (mirror :: <top-level-mirror>) => (dialogs :: <sequence>)
  mirror.%dialog-mirrors
end method mirror-registered-dialogs;

define method register-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    add!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method register-dialog-mirror;

define method unregister-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    remove!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method unregister-dialog-mirror;*/


////////////////////////////////////////////////////////////////////////////////
/// Top Level Layout
////////////////////////////////////////////////////////////////////////////////


define class <top-level-layout> 
    (<mirrored-sheet-mixin>, 
     <single-child-wrapping-pane>)
end class <top-level-layout>;

define method frame-wrapper
    (framem :: <carbon-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (wrapper :: false-or(<top-level-layout>))
  with-frame-manager (framem)
    make(<top-level-layout>,
	 child: top-level-layout-child(framem, frame, layout))
  end
end method frame-wrapper;

define method top-level-layout-child
    (framem :: <carbon-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (layout :: false-or(<column-layout>))
  let menu-bar      = frame-menu-bar(frame);
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  with-frame-manager (framem)
    let indented-children
      = make-children(tool-bar & tool-bar-decoration(tool-bar), layout);
    let indented-children-layout
      = unless (empty?(indented-children))
	  with-spacing (spacing: 2)
	    make(<column-layout>,
		 children: indented-children,
		 y-spacing: $top-level-y-spacing)
          end
        end;
    make(<column-layout>,
	 children: make-children(menu-bar, indented-children-layout, status-bar),
	 y-spacing: $top-level-y-spacing)
  end
end method top-level-layout-child;

define function make-children
    (#rest maybe-children)
 => (children :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  for (child in maybe-children)
    when (child)
      add!(children, child)
    end
  end;
  children
end function make-children;


define method update-frame-layout
    (framem :: <carbon-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  let wrapper = sheet-child(top-sheet);
  let layout = frame-layout(frame);
  let new-child = top-level-layout-child(framem, frame, layout);
  sheet-child(wrapper) := new-child;
  relayout-parent(new-child)
end method update-frame-layout;

define sealed method update-frame-wrapper
    (framem :: <carbon-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  if (top-sheet)
    let wrapper = sheet-child(top-sheet);
    let layout = frame-layout(frame);
    let new-child = top-level-layout-child(framem, frame, layout);
    sheet-child(wrapper) := new-child;
    relayout-parent(new-child)
  end
end method update-frame-wrapper;


/// Geometry updating

define sealed method handle-move
    (sheet :: <top-level-sheet>, mirror :: <top-level-mirror>,
     x :: <integer>, y :: <integer>)
 => (handled? :: <boolean>)
  let (old-x, old-y) = sheet-position(sheet);
  unless (x = old-x & y = old-y)
    let frame = sheet-frame(sheet);
    duim-debug-message("Sheet %= moved to %=, %= (from %=, %=)",
		       sheet, x, y, old-x, old-y);
    set-sheet-position(sheet, x, y)
  end;
  #t
end method handle-move;

/*define sealed method handle-duim-configure-event
    (sheet :: <top-level-sheet>, widget :: <GtkWidget*>,
     event :: <GdkEventConfigure*>)
 => (handled? :: <boolean>)
  let frame  = sheet-frame(sheet);
  let left   = event.x-value;
  let top    = event.y-value;
  let width  = event.width-value;
  let height = event.height-value;
  let region = make-bounding-box(left, top, left + width, top + height);
  let (old-width, old-height) = box-size(sheet-region(sheet));
  //---*** Switch back to duim-debug-message
  debug-message("Resizing %= to %dx%d -- was %dx%d",
		sheet, width, height, old-width, old-height);
  distribute-event(port(sheet),
		   make(<window-configuration-event>,
			sheet: sheet,
			region: region));
  #t
end method handle-duim-configure-event;*/
