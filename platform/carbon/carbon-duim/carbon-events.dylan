Module:    carbon-duim
Synopsis:  Carbon event processing implementation
Author:    David Gray, Scott McKay, Andy Armstrong, Rob Myers
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
              Carbon Modifications Copyright (c) 2001 Gwydion Dylan Maintainers.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// XXX - Note to Fun-O: I'm sorry about the d2c-specific callbacks,
//			 but our c-ffi support is limited at present

////////////////////////////////////////////////////////////////////////////////
/// Utilities
////////////////////////////////////////////////////////////////////////////////

define method add-handler-type(event-handler, the-class :: <integer>, the-kind :: <integer>)
=>()
  let spec :: <EventTypeSpec*> = make(<EventTypeSpec*>, 
                                      eventClass: the-class, 
                                      eventKind: the-kind);
	AddEventTypesToHandler(event-handler, 1, spec);
  values();
end method add-handler-type;

////////////////////////////////////////////////////////////////////////////////
/// Window Events
////////////////////////////////////////////////////////////////////////////////

// window-event-objects

define method window-event-objects(event :: <EventRef>)
=>(sheet :: <sheet>, mirror :: <mirror>, window :: <WindowRef>)
	// XXX - Fix to allocate a proper sizeof(WindowRef) buffer!
	let temp :: <Handle> = make(<Handle>);
  // 4 is the size of a WindowRef
	GetEventParameter(event, $kEventParamDirectObject, $typeWindowRef, 4, temp);
	let window :: <WindowRef> = pointer-at(temp, class: <WindowRef>, offset: 0);
  debug-message(format-to-string("WindowRef: %d", window.pointer-address));
  // Get the DUIM objects
  let sheet = WindowRef-sheet(window);
  let mirror = sheet-direct-mirror(sheet);
  values(sheet, mirror, window);
end method window-event-objects;

// install-window-event-handler

define method install-window-event-handler(window :: <WindowRef>, 
                                          eventClass :: <integer>, 
                                          eventKind :: <integer>, upp) => ()
	let spec :: <EventTypeSpec*> = make(<EventTypeSpec*>, eventClass: eventClass, 
                                      eventKind: eventKind);
	let (err, event-handler) = InstallWindowEventHandler(window, upp, 1, spec, $NULL);
end method install-window-event-handler;

// Draw Event

define method window-draw-event-handler(myHandler :: <EventHandlerCallRef>, 
                                        event :: <EventRef>, 
                                        userData :: <C-void*>)
=>(result :: <OSStatus>)
  debug-message("window-draw-event-handler()");
  let(sheet, mirror, window) = window-event-objects(event);
  if(mirror)
    let old-port :: <CGrafPtr> = GetPort();
    SetPortWindowPort(mirror.mirror-window);
    block ()
      //--- Maybe should do 'invalidate-cached-drawing-state' here?
      // XXX - Should this be local?
      let bounds :: <Rect*> = GetWindowPortBounds(mirror.mirror-window);
      let region = make-bounding-box(bounds.left-value, bounds.top-value, 
                                    bounds.right-value, bounds.bottom-value);
      // We call 'handle-event' instead of 'distribute-event' because we
      // want the repainting to happen between BeginPaint and EndPaint
      debug-message(format-to-string("repainting left: %d top: %d right: %d bottom: %d",
                    bounds.left-value, bounds.top-value, bounds.right-value, bounds.bottom-value));
      handle-event(sheet,
      make(<window-repaint-event>,
      sheet: sheet,
      region: region))
    cleanup
      debug-message("restoring old port and invalidating medium drawing state cache");
      SetPort(old-port);
      unless (GetWindowPort(mirror.mirror-window) = old-port)
        let medium = sheet-has-medium?(sheet) & sheet-medium(sheet);
        when (medium)
          medium-drawing-state-cache(medium) := 0
        end;
      end;
    end;
  else
    debug-message("Ignoring kEventWindowDrawContent for window with no mirror");
    warn("Ignoring kEventWindowDrawContent for window with no mirror");
  end if;
  $noErr;
end method window-draw-event-handler;

define C-callable-wrapper window-draw-event-handler-callback
of window-draw-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $window-draw-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(window-draw-event-handler-callback);

// Bounds Changed Event

define method window-bounds-changed-event-handler(myHandler :: <EventHandlerCallRef>, 
                                        event :: <EventRef>, 
                                        userData :: <C-void*>)
=>(result :: <OSStatus>)
  let(sheet, mirror, window) = window-event-objects(event);
  InvalWindowRect(window, GetWindowPortBounds(window));
  // XXX - Resize it!
  $noErr;
end method window-bounds-changed-event-handler;

define C-callable-wrapper window-bounds-changed-event-handler-callback
of window-bounds-changed-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $window-bounds-changed-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(window-bounds-changed-event-handler-callback);

// Close Event

define method window-close-event-handler(myHandler :: <EventHandlerCallRef>, 
                                        event :: <EventRef>, 
                                        userData :: <C-void*>)
=>(result :: <OSStatus>)
  debug-message("window-close-event-handler()");
  let(sheet, mirror, window) = window-event-objects(event);
  // If the user asked to close via the window manager, just call
  // 'exit-frame' on the frame.  'exit-frame' will take care of
  // exiting the frame in an orderly way, destroying its sheets.
  //---*** What if there's more than one "top level" sheet, e.g., MDI?
  let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
  when(frame)
    debug-message("exiting frame %=", frame);
    // If the 'top-level' frame has been destroyed (i.e. one with no owner)
    // then we should quit the application.
    /*unless(frame-owner(frame))
      QuitApplicationEventLoop();
    end unless;*/
    exit-frame(frame, destroy?: #t);
    // At this point, 'sheet' is the frame's top-level sheet
    //unregister-keyboard-interrupt-handler(_port, sheet);
  end;
  $noErr;
end method window-close-event-handler;

define C-callable-wrapper window-close-event-handler-callback
of window-close-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $window-close-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(window-close-event-handler-callback);

// Install Window Events

define method install-event-handlers
    (sheet :: <carbon-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  debug-message("install-event-handlers <carbon-top-level-sheet-mixin>, <top-level-mirror>");
  let window :: <WindowRef> = mirror.mirror-window;
  install-window-event-handler(window, $kEventClassWindow, $kEventWindowDrawContent, 
                               $window-draw-event-handler-upp);
  install-window-event-handler(window, $kEventClassWindow, $kEventWindowBoundsChanged, 
                               $window-bounds-changed-event-handler-upp);
  install-window-event-handler(window, $kEventClassWindow, $kEventWindowClose, 
                               $window-close-event-handler-upp);
end method install-event-handlers;

////////////////////////////////////////////////////////////////////////////////
/// Control Events
////////////////////////////////////////////////////////////////////////////////

// control-event-objects

define method control-event-objects(event :: <EventRef>)
=>(sheet :: <sheet>, mirror :: <mirror>, control :: <ControlHandle>)
	// XXX - Fix to allocate a proper sizeof(ControlHandle) buffer!
	let temp :: <Handle> = make(<Handle>);
  // 4 is the size of a ControlHandle
	GetEventParameter(event, $kEventParamDirectObject, $typeControlRef, 4, temp);
	let control = pointer-at(temp, class: <ControlHandle>, offset: 0);
  debug-message(format-to-string("ControlHandle: %d", control.pointer-address));
  // Get the DUIM objects
  let sheet = ControlHandle-sheet(control);
  let mirror = sheet-direct-mirror(sheet);
  debug-message(format-to-string("sheet: %=", sheet));
  debug-message(format-to-string("mirror: %=", mirror));
  values(sheet, mirror, control);
end method control-event-objects;

// install-control-event-handler

define method install-control-event-handler(control :: <ControlHandle>, 
                                          eventClass :: <integer>, 
                                          eventKind :: <integer>, upp) => ()
	let spec :: <EventTypeSpec*> = make(<EventTypeSpec*>, eventClass: eventClass, 
                                      eventKind: eventKind);
	let (err, event-handler) = InstallControlEventHandler(control, upp, 1, spec, $NULL);
end method install-control-event-handler;

// Control Draw Event

define method control-draw-event-handler(myHandler :: <EventHandlerCallRef>, 
                                            event :: <EventRef>, 
                                            userData :: <C-void*>)
=>(result :: <OSStatus>)
  let(sheet, mirror, control) = control-event-objects(event);
  debug-message(format-to-string("control-draw-event-handler %d", control.pointer-address));
  //repaint-sheet(sheet, mirror.%region, medium: sheet-medium(sheet), force?: #f);
  let rect :: <Rect*> = make(<Rect*>);
  GetControlBounds(control, rect);
  let x      = rect.left-value;
  let y      = rect.top-value;
  let width  = rect.right-value - rect.left-value;
  let height = rect.bottom-value - rect.top-value;
  let region = make-bounding-box(x, y, x + width, y + height);
  duim-debug-message("Repainting %=: %d, %d %d x %d",
		     sheet, x, y, width, height);
  // We call 'handle-event' instead of 'distribute-event' because we
  // want the repainting to happen between BeginPaint and EndPaint
  handle-event(sheet,
	       make(<window-repaint-event>,
		    sheet: sheet,
		    region: region));
  $noErr;
end method control-draw-event-handler;

define C-callable-wrapper control-draw-event-handler-callback
of control-draw-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-draw-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-draw-event-handler-callback);
  
/*  
  kEventParamKeyModifiers     typeUInt32
  kEventParamMouseButton      typeMouseButton
  kEventParamClickCount       typeUInt32  
*/

define method control-mouse-button-event-handler
    (event :: <EventRef>, event-class :: <class>)
 => (result :: <OSStatus>)
  let(sheet, mirror, control) = control-event-objects(event);
  debug-message(format-to-string("%= %= %=", sheet, mirror, control));
  // 2 is the size of a Point
  let position :: <Point*> = make(<Point*>);
  GetEventParameter(event, $kEventParamMouseLocation, $typeQDPoint, 4, position);
  let native-x  = position.h-value;
  let native-y  = position.v-value;
  let button    = 1; 	// XXX - Get the button! Carbon supports 3!
  let modifiers = 0;	// XXX - Do this!
  // XXX - Check the click count and send a double-click event if needed
  let event-class = event-class;
  if (event-class)
    let (x, y) = untransform-position(sheet-native-transform(sheet), 
                                      native-x, native-y);
    let _port = port(sheet);
    port-modifier-state(_port)    := modifiers;
    let pointer = port-pointer(_port);
    pointer-button-state(pointer) := button;
    let _port = port(sheet);
    distribute-event(_port,
          make(event-class,
        sheet: sheet,
        pointer: pointer,
        button: button,
        modifier-state: modifiers,
        x: x, y: y));
  end;
  $noErr;
end method control-mouse-button-event-handler;

// Control Mouse-down Event

define method control-mouse-down-event-handler
    (myHandler :: <EventHandlerCallRef>, event :: <EventRef>, userData :: <C-void*>)
 => (result :: <OSStatus>)
  debug-message("control-mouse-down-event-handler");
  control-mouse-button-event-handler(event, <button-press-event>);
end method control-mouse-down-event-handler;

define C-callable-wrapper control-mouse-down-event-handler-callback
of control-mouse-down-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-mouse-down-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-mouse-down-event-handler-callback);

// Control Mouse-Up Event

define method control-mouse-up-event-handler(myHandler :: <EventHandlerCallRef>, 
                                            event :: <EventRef>, 
                                            userData :: <C-void*>)
=>(result :: <OSStatus>)
  debug-message("control-mouse-up-event-handler");
  control-mouse-button-event-handler(event, <button-release-event>);
end method control-mouse-up-event-handler;

define C-callable-wrapper control-mouse-up-event-handler-callback
of control-mouse-up-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-mouse-up-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-mouse-up-event-handler-callback);
  
// Key events  
  
// Key Handler

define method control-key-event-handler
    (event :: <EventRef>, event-class :: <class>)
 => (result :: <OSStatus>)
  debug-message("Key handler");
  let(sheet, mirror, control) = control-event-objects(event);
  let modifiers = 0; //gtk-state->duim-state(_port, logand(state, $key-event-modifier-mask));
  // XXX - Does this work on Fun-Dev? On d2c we get a pointer to 1 byte of space
  let char-pointer :: <C-pointer> = make(<C-pointer>);
  let err :: <OSErr> = 
    GetEventParameter(event, $kEventParamKeyMacCharCodes, $typeChar, 1, char-pointer);
  let char :: <character> = C-char-at(char-pointer);
  let keysym = make(<string>, size: 1, fill: char);
  //port-modifier-state(_port) := modifiers;
  let _port = port(sheet);
  distribute-event(_port,
    make(event-class,
      sheet:     sheet,
      key-name:  keysym,
      character: char,
      modifier-state: modifiers));
  $noErr;
end method control-key-event-handler;

// Key Press Event Handler

define method control-key-press-event-handler
    (myHandler :: <EventHandlerCallRef>, event :: <EventRef>, userData :: <C-void*>)
 => (result :: <OSStatus>)
  debug-message("control-key-press-event-handler");
  control-key-event-handler(event, <key-press-event>);
end method control-key-press-event-handler;

define C-callable-wrapper control-key-press-event-handler-callback
of control-key-press-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-key-press-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-key-press-event-handler-callback);

// Key Release Event Handler

define method control-key-release-event-handler
    (myHandler :: <EventHandlerCallRef>, event :: <EventRef>, userData :: <C-void*>)
 => (result :: <OSStatus>)
  debug-message("control-key-release-event-handler");
  control-key-event-handler(event, <key-press-event>);
end method control-key-release-event-handler;

define C-callable-wrapper control-key-release-event-handler-callback
of control-key-release-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-key-release-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-key-release-event-handler-callback);
  
// Value Changed

define method control-value-changed-event-handler
    (myHandler :: <EventHandlerCallRef>, event :: <EventRef>, userData :: <C-void*>)
 => (result :: <OSStatus>)
  debug-message("Value Changed Event handler");
  let(sheet, mirror, control) = control-event-objects(event);
  let int-pointer :: <C-int*> = make(<C-int*>);
  let err :: <OSErr> = 
    GetEventParameter(event, $kEventParamKeyMacCharCodes, $typeChar, 1, int-pointer);
  let value :: <integer> = pointer-value(int-pointer);
  //port-modifier-state(_port) := modifiers;
  let _port = port(sheet);
  distribute-event(_port,
    make(<value-changed-gadget-event>,
      gadget: sheet, // false-or <gadget>
      value: value)); // untyped
  $noErr;
end method control-value-changed-event-handler;

define C-callable-wrapper control-value-changed-event-handler-callback
of control-value-changed-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $control-value-changed-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(control-value-changed-event-handler-callback);  
  
// Text Field value changing is handled a little differently

define method text-control-text-changed
    (control :: <ControlHandle>)
 => ()
  debug-message("text-control-text-changed");
  let sheet = ControlHandle-sheet(control);
  if(sheet.text-field-maximum-size)
    let (err :: <OSErr>, buffer-size :: <integer>) = 
      GetControlDataSize(control, $kControlEditTextPart, gadget-control-data-tag(sheet));
    if(buffer-size >= sheet.text-field-maximum-size)
      #f; // XXX - Truncate!!!!
    end if;
  end if;
end method text-control-text-changed;

// XXX - RETURN MUST BE VOID, SO USE D2C APIs!

define C-callable-wrapper text-control-text-changed-callback
of text-control-text-changed
  parameter control :: <ControlHandle>;
  // void result
end;

define constant $text-control-text-changed-upp :: <ControlEditTextValidationUPP> = 
  NewControlEditTextValidationUPP(text-control-text-changed-callback);
  
define method text-control-key-filter
    (control :: <ControlHandle>, keyCode :: <C-int*>, 
     charCode :: <C-int*>, modifiers :: <C-int*>)
=> (result :: <OSErr>)
  debug-message("text-control-key-filter");
  let sheet = ControlHandle-sheet(control);
  if(sheet.text-field-maximum-size)
    let (err :: <OSErr>, buffer-size :: <integer>) = 
      GetControlDataSize(control, $kControlEditTextPart, gadget-control-data-tag(sheet));
    // XXX - Check for delete/backspace !!!
    if(buffer-size >= sheet.text-field-maximum-size)
      0; // block
    else
      handle-text-gadget-changing(sheet);
      1; // pass
    end if;
  else
    handle-text-gadget-changing(sheet);
    1; // pass
  end if;
end method text-control-key-filter;

define C-callable-wrapper text-control-key-filter-callback
of text-control-key-filter
  parameter control :: <ControlHandle>;
  parameter keyCode :: <C-short*>;		// Actually a short
  parameter charCode :: <C-short*>;	// Actually a short
  parameter modifiers :: <C-int*>;	// Actually a typedef'd integer
  //result status :: <OSErr>;				// Actually a typedef'd integer
end;

define constant $text-control-key-filter-upp :: <ControlKeyFilterUPP> = 
  NewControlKeyFilterUPP(text-control-key-filter-callback);
  
// live feedback

// scroll bar
  
define method scroll-bar-live-action-handler
    (control :: <ControlHandle>, part :: <integer>) => ()
	let start-value :: <integer> = GetControlValue(control);
	let delta =
    select(part)
      $kControlUpButtonPart =>
        if(start-value > GetControl32BitMinimum(control))
          -1;
        end if;
      $kControlDownButtonPart =>
        if(start-value < GetControl32BitMaximum(control))
          1;
        end if;
      $kControlPageUpPart =>
        if(start-value > GetControl32BitMinimum(control))
          0 - GetControlViewSize(control);
        end if;
      $kControlPageDownPart =>
        if (start-value < GetControl32BitMaximum(control))
          GetControlViewSize(control);
        end if;
      otherwise =>
        #f;
    end select;
	if(delta)
    let sheet = ControlHandle-sheet(control);
    // We call handle-event because we want this to be handled immediately
    handle-event(sheet,
      make(<value-changing-gadget-event>,
        gadget: sheet, // false-or <gadget>
        value: start-value + delta)); // untyped
  end if;
end method scroll-bar-live-action-handler;

/*define C-callable-wrapper scroll-bar-live-action-handler-callback
of scroll-bar-live-action-handler
  parameter control :: <ControlHandle>;
  parameter part :: <integer>;		// Actually a short
  // void result
end;*/

define constant scroll-bar-live-action-handler-callback =
	callback-method(arg1 :: <raw-pointer>, arg2 :: <integer>) => ();
		scroll-bar-live-action-handler(make( <Controlhandle>, pointer: arg1), arg2);
  end;	

define constant $scroll-bar-live-action-handler-upp :: <ControlActionUPP> = 
  NewControlActionUPP(scroll-bar-live-action-handler-callback);

// slider
  
define method slider-live-action-handler
    (control :: <ControlHandle>, part :: <integer>) => ()
	let start-value :: <integer> = GetControlValue(control);
  let sheet = ControlHandle-sheet(control);
  // We call handle-event because we want this to be handled immediately
  handle-event(sheet,
    make(<value-changing-gadget-event>,
      gadget: sheet, // false-or <gadget>
      value: start-value)); // untyped
end method slider-live-action-handler;

/*define C-callable-wrapper slider-live-action-handler-callback
of slider-live-action-handler
  parameter control :: <ControlHandle>;
  parameter part :: <integer>;		// Actually a short
  // void result
end;*/

define constant slider-live-action-handler-callback =
	callback-method(arg1 :: <raw-pointer>, arg2 :: <integer>) => ();
		slider-live-action-handler(make( <Controlhandle>, pointer: arg1), arg2);
  end;	

define constant $slider-live-action-handler-upp :: <ControlActionUPP> = 
  NewControlActionUPP(slider-live-action-handler-callback);
  
////////////////////////////////////////////////////////////////////////////////
/// Data Browser Events
////////////////////////////////////////////////////////////////////////////////  

define method browser-item-handler
		(browser :: <ControlRef>, itemID :: <integer>, property :: <integer>,
		 itemData :: <DataBrowserItemDataRef>, changeValue :: <integer>)
 => (result :: <OSStatus>)  
 	let status :: <OSStatus> = $noErr;
	debug-message("Browser item handler");
 	if(changeValue == 0)
		status := 
			select(property)
				/*(($kDataBrowserItemIsActiveProperty =>
					SetDataBrowserItemDataBooleanValue(itemData, #t);*/
				otherwise
					if(property == $column-base)
						debug-message(format-to-string("Get browser item %d", itemID));
						let str :: <string> = get-item-string(browser, 1, itemID);
						debug-message("Setting list item string");
						if(str)
							with-CFString(cfstr = str)
								status := SetDataBrowserItemDataText(itemData, cfstr);
							end with-CFString;
						end if;
					end if;
					$noErr;
			end select;
 	end if;
 	status;
end method browser-item-handler;
 
//define C-callable-wrapper browser-item-handler-callback
//of browser-item-handler
//  parameter browser :: <ControlHandle>;
//  parameter itemID :: <integer>;
//  parameter property :: <integer>,
//	parameter itemData :: <DataBrowserItemDataRef>;
//	parameter changeValue :: <integer>
//  // OSStatus result
//end;

define constant browser-item-handler-callback =
	callback-method(arg1 :: <raw-pointer>, arg2 :: <integer>, arg3 :: <integer>,
	arg4 :: <raw-pointer>, arg5 :: <integer>) => (result :: <integer>);
		browser-item-handler(make( <Controlhandle>, pointer: arg1), arg2,
			arg3, make(<DataBrowserItemDataRef>, pointer: arg4), arg5);
  end;	

define constant $browser-item-handler-upp :: <DataBrowserItemDataUPP> = 
  NewDataBrowserItemDataUPP(browser-item-handler-callback);

define method browser-notification-handler
		(browser :: <ControlRef>, item :: <integer>, message :: <integer>)
 => ()
	debug-message("Browser notification handler");
 	let control = ControlHandle-sheet(browser);
	select(message)
		// XXX - Just use $kDataBrowserSelectionSetChanged
		$kDataBrowserItemSelected => handle-selection-changed(control);
		$kDataBrowserItemDeselected => handle-selection-changed(control);
		$kDataBrowserItemDoubleClicked => activate-carbon-gadget(control);
		// Ignore other messages
		otherwise => #f;
	end select;
end method browser-notification-handler;

//define C-callable-wrapper browser-notification-handler-callback
//of browser-notification-handler
//  parameter browser :: <ControlHandle>;
//  parameter item :: <integer>;
//  parameter message :: <integer>;
//end;

define constant browser-notification-handler-callback =
	callback-method(arg1 :: <raw-pointer>, arg2 :: <integer>, arg3 :: <integer>)
	=>();
		browser-notification-handler(make( <Controlhandle>, pointer: arg1), arg2, arg3);
  end;	

define constant $browser-notification-handler-upp :: <DataBrowserItemNotificationUPP> = 
  NewDataBrowserItemNotificationUPP(browser-notification-handler-callback);

////////////////////////////////////////////////////////////////////////////////
/// Command Events (Menus)
////////////////////////////////////////////////////////////////////////////////

/*

// command-event-objects

define method command-event-objects(event :: <EventRef>)
=>(sheet :: <sheet>, mirror :: <mirror>, control :: <ControlHandle>)
	// XXX - Fix to allocate a proper sizeof(ControlHandle) buffer!
	let temp :: <Handle> = make(<Handle>);
  // 4 is the size of a ControlHandle
	GetEventParameter(event, $kEventParamDirectObject, $typeControlRef, 4, temp);
	let control = pointer-at(temp, class: <ControlHandle>, offset: 0);
  debug-message(format-to-string("ControlHandle: %d", control.pointer-address));
  // Get the DUIM objects
  let sheet = ControlHandle-sheet(control);
  let mirror = sheet-direct-mirror(sheet);
  debug-message(format-to-string("sheet: %=", sheet));
  debug-message(format-to-string("mirror: %=", mirror));
  values(sheet, mirror, control);
end method command-event-objects;

// install-command-event-handler

define method install-command-event-handler(control :: <ControlRef>, upp) => ()
	let spec :: <EventTypeSpec*> = make(<EventTypeSpec*>, eventClass: $kEventClassCommand, 
                                      eventKind: $kEventCommandProcess);
	let (err, event-handler) = InstallControlEventHandler(control, upp, 1, spec, $NULL);
end method install-command-event-handler;

define method command-event-handler(myHandler :: <EventHandlerCallRef>, 
                                        event :: <EventRef>, 
                                        userData :: <C-void*>)
=>(result :: <OSStatus>)
  let(sheet, mirror, window) = window-event-objects(event);
  InvalWindowRect(window, GetWindowPortBounds(window));
  $noErr;
end method command-event-handler;

define C-callable-wrapper command-event-handler-callback
of command-event-handler
  parameter myHandler :: <EventHandlerCallRef>;
  parameter event :: <EventRef>;
  parameter userData :: <C-void*>;
  //result status :: <OSErr>;
end;

define constant $command-event-handler-upp :: <EventHandlerUPP> = 
  NewEventHandlerUPP(command-event-handler-callback);

*/

////////////////////////////////////////////////////////////////////////////////
/// Apple Events
////////////////////////////////////////////////////////////////////////////////

// Quit Apple Event

define method quit-apple-event-handler
    ( appleEvt :: <AppleEvent*>, reply :: <AppleEvent*>, refcon :: <C-void*>) //refcon :: <integer> )
 => (res :: <OSErr>)
  debug-message("quit-apple-event-handler()");
  // Tell Carbon to exit the main event loop
  if($blocking-event-loop)
    QuitApplicationEventLoop();
  end if;
  // Tell DUIM to exit the main event loop
  //exit-frame(current-frame());
  do-frames(rcurry(exit-frame, destroy?: #t), frame-manager: #f, z-order: #"top-down");
	$noErr;
end method;

define C-callable-wrapper quit-apple-event-handler-callback
of quit-apple-event-handler
  parameter appleEvt :: <AppleEvent*>;
  parameter reply :: <AppleEvent*>;
  parameter refcon :: <C-void*>;	// <integer>
  //result status :: <OSErr>;
end;

define constant $quit-apple-event-handler-upp = 
                  NewAEEventHandlerUPP(quit-apple-event-handler-callback);
                  
// Install Apple Event Handlers

define method install-apple-event-handlers
    () => ()
  let err :: <OSErr> = AEInstallEventHandler($kCoreEventClass, $kAEQuitApplication, 
                                              $quit-apple-event-handler-upp, 0, #f);
  if (err ~= $noErr)
          error("Failed to install Quit Apple Event handler.");
  end if;
end method install-apple-event-handlers;

// XXX - This should go somewhere better

install-apple-event-handlers();

