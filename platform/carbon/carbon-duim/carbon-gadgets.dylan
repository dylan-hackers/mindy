Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
  TODO:
  			Setting intial selection in lists.
  			
        Option Boxes?
        Combo Boxes?	- Popup Menu combined with a text field
        
        Vertically pad buttons?
*/

/// Useful constants

define constant $button-box-x-spacing = 8;
define constant $button-box-y-spacing = 8;

/// Some magic Carbon constants

//---*** All of the following should be computed
define constant $default-label :: <byte-string>  = "";

// The thickness of the border we use
define constant $gadget-border-thickness :: <integer> = 2;	// in pixels

define constant $default-vertical-spacing :: <integer> = 1;

define constant $minimum-visible-characters :: <integer> = 25;
define constant $minimum-visible-lines      :: <integer> =  3;

define constant $push-button-extra-text-width  :: <integer> = 12;
define constant $push-button-extra-text-height :: <integer> = 4;
define constant $push-button-extra-icon-width  :: <integer> = 2;
define constant $push-button-extra-icon-height :: <integer> = 2;
define constant $button-icon-width             :: <integer> = 16;

define constant $list-box-minimum-visible-lines :: <integer> = 3;
define constant $list-box-default-visible-lines :: <integer> = 5;
define constant $list-box-extra-height          :: <integer> = 2;

define constant $option-box-maximum-popup-height :: <integer> = 200;
define constant $option-box-extra-height         :: <integer> =   8;

define constant $minimum-scroll-shaft-length :: <integer> = 50;

define constant $text-field-extra-width  :: <integer> = 0;	// in pixels
define constant $text-field-extra-height :: <integer> = 8;	// in pixels

define constant $text-editor-extra-width  :: <integer> = 0;	// in pixels
define constant $text-editor-extra-height :: <integer> = 12;	// in pixels

/// carbon gadgets

define class <gadget-mirror> (<control-mirror>)
end class <gadget-mirror>;

define open abstract class <carbon-gadget-mixin>
    (<gadget>,
     <carbon-pane-mixin>)
end class <carbon-gadget-mixin>;

define method gadget-control(sheet :: <carbon-gadget-mixin>)
 => (control)
  debug-message(format-to-string("gadget-control %=", sheet));
  let mirror = sheet-mirror(sheet);
  let control = mirror & mirror-control(mirror);
  control;
end;

define sealed method make-carbon-mirror
    (gadget :: <carbon-gadget-mixin>)
 => (mirror :: <gadget-mirror>)
  debug-message(format-to-string("make-carbon-mirror %=", gadget));
  let ancestor  = sheet-device-parent(gadget);
  let mirror = sheet-direct-mirror(ancestor);
  let window :: <WindowRef> = GetWindowFromPort(mirror.mirror-grafport);
  let (left, top, right, bottom) = sheet-native-edges(gadget);
  let bounds-rect :: <Rect*> = make(<Rect*>, top: top, left: left, bottom: bottom, right: right);
  let control :: <ControlRef> = make-gadget-control(gadget, window, bounds-rect);
  initialize-gadget-control(gadget, control);
  make(<gadget-mirror>,
    grafport: mirror.mirror-grafport,
    control: control,
    sheet:  gadget);
end method make-carbon-mirror;

define method make-gadget-control
    (gadget :: <carbon-gadget-mixin>, window :: <WindowRef>, bounds-rect :: <Rect*>)
 => (control :: <ControlRef>)
  debug-message(format-to-string("make-gadget-control %=", gadget));
  let (value :: <integer>, min :: <integer>, max :: <integer>, proc :: <integer>, refcon :: <integer>) = 
    gadget-control-parameters(gadget);
  NewControl(window, bounds-rect, $empty-pascal-string, #t, value, min, max, proc, refcon);
end method make-gadget-control;

define method initialize-gadget-control(gadget :: <carbon-gadget-mixin>, control :: <Controlhandle>)
=> ()
  ignore(gadget, control);
  // do-nothing
end method initialize-gadget-control;

// Utilites

define sealed method defaulted-gadget-label
    (gadget :: <gadget>) => (label)
  gadget-label(gadget) | $default-label
end method defaulted-gadget-label;

define method do-compose-space 
    (pane :: <carbon-gadget-mixin>, #key width, height)
 => (space-requirement :: <space-requirement>)
  debug-message(format-to-string("do-compose-space %=", pane));
  let r :: <Rect*> = make(<Rect*>);
  let err :: <OSErr> = GetBestControlRect(gadget-control(pane), r);
  let best-width :: <integer> = r.right-value - r.left-value;
  let best-height :: <integer> = r.bottom-value - r.top-value;
  debug-message(format-to-string("width: %d height: %d", best-width, best-height));
  make(<space-requirement>,
       width:  best-width,  min-width:  best-width,  
       max-width: best-width,
       height: best-height, min-height: best-height, 
       max-height: best-height);
end method do-compose-space;

define method note-gadget-enabled
    (client, gadget :: <carbon-gadget-mixin>) => ()
  ActivateControl(gadget-control(gadget));
end method note-gadget-enabled;

define method note-gadget-disabled 
    (client, gadget :: <carbon-gadget-mixin>) => ()
  DeactivateControl(gadget-control(gadget));
end method note-gadget-disabled;

define method default-foreground-setter
    (fg :: <ink>, pane :: <carbon-pane-mixin>) => (foreground :: <ink>)
  next-method();
  // Changing the foreground colour of the gadget is a no-no for system controls on MacOS
  fg
end method default-foreground-setter;

define method default-background-setter
    (bg :: <ink>, pane :: <carbon-pane-mixin>) => (background :: <ink>)
  next-method();
  // Changing the background colour of the gadget is a no-no for system controls on MacOS
  bg
end method default-background-setter;

define sealed method activate-carbon-gadget
    (gadget :: <action-gadget>) => (activated? :: <boolean>)
  when (gadget-activate-callback(gadget))
    distribute-activate-callback(gadget);
    #t
  end
end method activate-carbon-gadget;

/// Viewports

define sealed class <carbon-viewport>
    (<viewport>,
     <carbon-pane-mixin>,
     <permanent-medium-mixin>,
     <single-child-composite-pane>,
     <sealed-constructor-mixin>)
end class <carbon-viewport>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-viewport>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-viewport>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values($kControlSupportsEmbedding, 0, 0, $kControlUserPaneProc, 0);
end method gadget-control-parameters;

define method install-event-handlers
    (sheet :: <carbon-viewport>, mirror :: <gadget-mirror>) => ()
  let control :: <ControlHandle> = mirror.mirror-control;
  install-control-event-handler(control, $kEventClassControl, $kEventControlDraw, 
                               $control-draw-event-handler-upp);
end method install-event-handlers;

/*define method handle-event
    (pane :: <carbon-viewport>, event :: <window-repaint-event>) => ()
  // Set the origin
  let (x :: <integer>, y :: <integer>) = scroll-position(pane);
  SetOrigin(x, y);
  // Draw the contents
  let child = sheet-child(viewport);
  when (child)
    next-method();
  end when;
  // Re-set the origin
  SetOrigin(0, 0);
end method handle-event;*/

/// Buttons

define class <carbon-button-mixin> (<carbon-gadget-mixin>, <button>)
end class <carbon-button-mixin>;

define sealed method button-box-spacing
    (framem :: <carbon-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  select (gadget-orientation(box))
    #"horizontal" => $button-box-x-spacing;
    #"vertical"   => $button-box-y-spacing;
  end
end method button-box-spacing;

define method install-event-handlers
    (sheet :: <carbon-button-mixin>, mirror :: <gadget-mirror>) => ()
  debug-message(format-to-string("install-event-handlers Sheet %= mirror %=", sheet, mirror));
  let control :: <ControlHandle> = mirror.mirror-control;
  debug-message(format-to-string("control  %=", control));
  debug-message(format-to-string("Installing button event handler on %d", control.pointer-address));
  /*install-control-event-handler(control, $kEventClassControl, $kEventMouseDown, 
                               $control-mouse-down-event-handler-upp);*/
  install-control-event-handler(control, $kEventClassControl, $kEventControlHit, 
                               $control-mouse-up-event-handler-upp);
end method install-event-handlers;

// Push buttons

define sealed class <carbon-push-button>
    (<carbon-button-mixin>,
     <push-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-push-button>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-push-button>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-push-button>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 255, $kControlPushButtonProc, 0);
end method gadget-control-parameters;

define method initialize-gadget-control(gadget :: <carbon-button-mixin>, control :: <Controlhandle>)
=> ()
  with-pascal-string(p-string = defaulted-gadget-label(gadget))
    SetControlTitle(control, p-string);
  end;
end method initialize-gadget-control;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <carbon-push-button>)
 => (default? :: <boolean>)
  next-method();
  // XXX - The mirror value is set when the mirror is made. is this right?
  ignoring("gadget-default?-setter");
  default?;
end method gadget-default?-setter;

define method handle-event
    (pane :: <push-button>, event :: <button-release-event>) => ()
  ignore(event);
  debug-message("handle-event <push-button>, <button-release-event>");
  handle-button-gadget-click(pane);
end method handle-event;

// Radio Buttons and Check Boxes

define class <carbon-toggle-button-mixin> (<carbon-button-mixin>)
end class <carbon-toggle-button-mixin>;

define method initialize-gadget-control
    (gadget :: <carbon-toggle-button-mixin>, control :: <Controlhandle>) => ()
  next-method();	// Set the title
  let control-value :: <integer> = if (gadget-value(gadget)) 1 else 0 end;
  SetControlValue(control, control-value);
end method initialize-gadget-control;

define method handle-event
    (pane :: <carbon-toggle-button-mixin>,
     event :: <button-release-event>) => ()
  debug-message("handle-event toggle-button");
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;

define method update-mirror-attributes
    (gadget :: <carbon-toggle-button-mixin>,
     mirror :: <gadget-mirror>) => ()
  let control = mirror.mirror-control;
  let selected? = gadget-value(gadget);
  debug-message(format-to-string("update-mirror-attributes: %=", selected?));
  let control-value :: <integer> = if (selected?) 1 else 0 end;
  SetControlValue(control, control-value);
end method update-mirror-attributes;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-toggle-button-mixin>)
    => ()
  next-method();
  debug-message("note-gadget-value-changed");
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-attributes(gadget, mirror)
end method note-gadget-value-changed;

// Radio Buttons

define sealed class <carbon-radio-button>
    (<carbon-toggle-button-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <carbon-radio-button>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-radio-button>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-radio-button>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 255, $kControlRadioButtonProc, 0);
end method gadget-control-parameters;

// Check boxes

define sealed class <carbon-check-button>
    (<carbon-toggle-button-mixin>,
     <check-button>,
     <leaf-pane>)
end class <carbon-check-button>;

define method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-check-button>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-check-button>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 255, $kControlCheckBoxProc, 0);
end method gadget-control-parameters;

define method handle-event 
    (pane :: <carbon-check-button>, event :: <button-release-event>) => ()
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;
      
/// List gadgets

// Utilities

define constant $row-base :: <integer> = 1;	// 0 is invalid
define constant $column-base :: <integer> = 1024; // 0-1023 are reserved by Apple

define method get-item-string(control :: <ControlRef>, column :: <integer>, row :: <integer>)
=> (item :: <string>)
  let gadget = ControlHandle-sheet(control);
  let items = gadget-items(gadget);
  collection-gadget-item-label(gadget, items[row - $row-base]);
end method get-item-string;

// Base class

define sealed class <carbon-list-control-mixin> 
    (<carbon-gadget-mixin>,
     <collection-gadget>,
     <sealed-constructor-mixin>)
end class <carbon-list-control-mixin>;

define method make-gadget-control
    (gadget :: <carbon-list-control-mixin>, window :: <WindowRef>, bounds-rect :: <Rect*>)
 => (control :: <ControlRef>)
	debug-message("make-gadget-control <carbon-list-control-mixin>");
  let (status :: <OSStatus>, control :: <ControlRef>) = 
    CreateDataBrowserControl(window, bounds-rect, $kDataBrowserListView);
  check-result("CreateDataBrowserControl in initialize-gadget-control", status);
  control;
end method make-gadget-control;

define method initialize-gadget-control(gadget :: <carbon-list-control-mixin>, control :: <Controlhandle>)
=> ()
	debug-message("initialize-gadget-control <carbon-list-control-mixin>");
	let status :: <OSStatus> = $noErr;
  let column-desc :: <DataBrowserListViewColumnDesc*> =
		make(<DataBrowserListViewColumnDesc*>);
	column-desc.propertyDesc-value.propertyID-value									:= $column-base;

	column-desc.headerBtnDesc-value.minimumWidth-value								:= 0;
	column-desc.headerBtnDesc-value.maximumWidth-value								:= 2000;
	column-desc.headerBtnDesc-value.btnFontStyle-value.just-value					:= $teFlushDefault;

	column-desc.propertyDesc-value.propertyType-value								:= $kDataBrowserTextType;
	column-desc.propertyDesc-value.propertyFlags-value								:= $kDataBrowserListViewDefaultColumnFlags;

	column-desc.headerBtnDesc-value.titleString-value 								:= as(<CFStringRef>, "duim list");
	
	//column-desc.headerBtnDesc-value.initialOrder-value								:= $kDataBrowserOrderIncreasing;
	//column-desc.headerBtnDesc-value.version-value 										:= $kDataBrowserListViewLatestHeaderDesc;
	//column-desc.headerBtnDesc-value.btnFontStyle-value.flags-value		:= logior($kControlUseFontMask, $kControlUseJustMask);

	//column-desc.headerBtnDesc-value.btnContentInfo-value.contentType-value	:= $kControlContentTextOnly;
	//column-desc.headerBtnDesc-value.btnFontStyle-value.font-value 		:= $kControlFontViewSystemFont;
	//column-desc.headerBtnDesc-value.btnFontStyle-value.size-value			:= 10;	// XXX - What should the size be?
	//column-desc.headerBtnDesc-value.titleOffset-value 								:= 0;
	
	status := AddDataBrowserListViewColumn(control, column-desc, 1);
	check-result("AddDataBrowserListViewColumn in initialize-gadget-control", status);
	// I have no idea, see controls.h in the HI framework
	status := SetDataBrowserTarget(control, $row-base);
	check-result("SetDataBrowserTarget in initialize-gadget-control", status);
end method initialize-gadget-control;

define sealed method do-compose-space 
    (pane :: <carbon-list-control-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
	debug-message("do-compose-space <carbon-list-control-mixin>");
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $list-box-default-visible-lines,
			     minimum-lines: $list-box-minimum-visible-lines,
			     extra-height:  $list-box-extra-height)
end method do-compose-space;

//--- Are the heuristics here reasonable?
define sealed method compose-space-for-list-box
    (pane :: <carbon-gadget-mixin>,
     #key width, height,
          extra-height  = $list-box-extra-height,
	  default-lines = $list-box-default-visible-lines,
	  minimum-lines = $list-box-minimum-visible-lines,
          extra-lines   = 0,
          icon-height   = 0)
 => (space-req :: <space-requirement>)
  let _port = port(pane);
  let text-style  = get-default-text-style(_port, pane);
  let line-height = 12; // XXX - max(font-height(text-style, _port), icon-height);
  let char-width  = 10; // XXX - font-width(text-style, _port);
  let vsp         = $default-vertical-spacing;
  let nlines      = gadget-lines(pane);
  let thickness*2 = $gadget-border-thickness * 2;
  let min-width   = $minimum-visible-characters * char-width + thickness*2;
  let max-width   = $fill;
  local method lines->height (nlines :: <integer>) => (height :: <integer>)
	  nlines * line-height + (nlines - 1) * vsp + thickness*2 + extra-height
	end method;
  //---*** This all needs to take into account the size of the scrollbars
  select (gadget-scroll-bars(pane))
    #f, #"none", #"horizontal" =>
      // If there are no vertical scroll bars, make the gadget just tall
      // enough to hold all the items
      let nlines = nlines | size(gadget-items(pane));
      let best-width  = min-width;	//--- compute this from the labels
      let best-height = lines->height(nlines + extra-lines);
      let min-height = best-height;
      let max-height = $fill;
      let width  = constrain-size(width  | best-width,  min-width,  max-width);
      let height = constrain-size(height | best-height, min-height, max-height);
      make(<space-requirement>,
	   width:     width,     height:     height,
	   min-width: min-width, min-height: min-height,
	   max-width: max-width, max-height: max-height);
    otherwise =>
      // If there is a vertical scroll bar, use the requested height
      let nlines     = nlines | default-lines;
      let min-lines  = minimum-lines;
      let min-height = lines->height(min-lines);
      let max-height = $fill;
      let best-width  = min-width;	//--- compute this from the labels
      let best-height = lines->height(nlines + extra-lines);
      let width  = constrain-size(width  | best-width,  min-width,  max-width);
      let height = constrain-size(height | best-height, min-height, max-height);
      make(<space-requirement>,
	   width:     width,     height:     height,
	   min-width: min-width, min-height: min-height,
	   max-width: max-width, max-height: max-height);
  end
end method compose-space-for-list-box;

define method update-mirror-attributes
    (gadget :: <carbon-list-control-mixin>, mirror :: <gadget-mirror>) => ()
  debug-message("update-mirror-attributes <carbon-list-control-mixin>");
  next-method();
  let scroll-bars = gadget-scroll-bars(gadget);
  let horiz :: <boolean> = #f; //(scroll-bars = #"horizontal") | (scroll-bars = #"both");
  let vert :: <boolean> = (scroll-bars = #"vertical") | (scroll-bars = #"both");
  let status :: <OSStatus> = SetDataBrowserHasScrollBars(mirror.mirror-control, horiz, vert);
  debug-message(format-to-string("Setting list selection mode to %=", gadget-selection-mode(gadget)));
  SetDataBrowserSelectionFlags(mirror.mirror-control,
     select (gadget-selection-mode(gadget))
       #"none"     => 0;	// XXX - Is this right?
       #"single"   => $kDataBrowserSelectOnlyOne;
       #"multiple" => $kDataBrowserCmdTogglesSelection;	// XXX - Is this right?
     end);
  if (~ instance?(gadget, <table-control>))
    SetDataBrowserListViewHeaderBtnHeight(mirror.mirror-control, 0);
    //---*** How should we decide this?
    //carbon-clist-set-column-width(widget, 0, 500)
  end;
  update-list-control-items(gadget, mirror)
end method update-mirror-attributes;

define method install-event-handlers
    (sheet :: <carbon-list-control-mixin>, mirror :: <gadget-mirror>) => ()
	debug-message("install-event-handlers <carbon-list-control-mixin>");
  next-method();
  let callbacks :: <DataBrowserCallbacks*> = make(<DataBrowserCallbacks*>);
  callbacks.version-value := $kDataBrowserLatestCallbacks;
  let status :: <OSStatus> = InitDataBrowserCallbacks(callbacks);
	callbacks.u-value.v1-value.itemDataCallback-value := $browser-item-handler-upp;
	callbacks.u-value.v1-value.itemNotificationCallback-value := $browser-notification-handler-upp;
	status := SetDataBrowserCallbacks(mirror.mirror-control, callbacks);
  check-result("install-event-handlers <carbon-list-control-mixin>", status);
  debug-message("installed");
end method install-event-handlers;

define method list-selection
    (gadget :: <carbon-list-control-mixin>, mirror :: <gadget-mirror>)
 => (vector :: <vector>)
  let browser :: <ControlRef> = mirror.mirror-control;
  let vector :: <vector> = make(<vector>);
  let upper-bounds :: <integer> = gadget.gadget-items.size + $row-base;
  for(item :: <integer> from $row-base below upper-bounds)
    let (status :: <OSStatus>, state :: <integer>) = 
      GetDataBrowserItemState(browser, item);
      if(logand(state, $kDataBrowserItemIsSelected) = 1)
        vector := add!(vector, item - $row-base);
      end if;
  end for;
  vector;
end method list-selection;

define sealed method note-gadget-items-changed
    (gadget :: <carbon-list-control-mixin>) => ()
  debug-message("note-gadget-items-changed <carbon-list-control-mixin>");
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method note-gadget-items-changed;

define method update-gadget
    (gadget :: <carbon-list-control-mixin>) => ()
  debug-message("update-gadget <carbon-list-control-mixin>");
  // No, we don't call 'next-method' here!
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method update-gadget;

define sealed method update-list-control-items
    (gadget :: <carbon-list-control-mixin>, mirror :: <gadget-mirror>)
 => ()
  debug-message("update-list-control-items");
  let browser = mirror.mirror-control;
  let items = gadget-items(gadget);
  let label-function = gadget-label-key(gadget);
  let (status :: <OSStatus>, num-items :: <integer>) = 
    GetDataBrowserItemCount(browser, $kDataBrowserNoItem, #f, $kDataBrowserItemNoState);
  RemoveDataBrowserItems(browser, $kDataBrowserNoItem, num-items, $NULL, $column-base);
  debug-message(format-to-string("%d items removed", num-items));
  debug-message(format-to-string("adding %d items", items.size));
	let status :: <OSStatus> =  
		AddDataBrowserItems(browser, $kDataBrowserNoItem, items.size, $NULL, $column-base);
  check-result("AddDataBrowserItems in initialize-gadget-control", status);
end method update-list-control-items;

define sealed method update-gadget-selection
    (gadget :: <carbon-list-control-mixin>) => ()
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      ignoring("update-gadget-selection");
    #"multiple" =>
      ignoring("update-gadget-selection");
  end
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-list-control-mixin>) => ()
  next-method();
  debug-message("note-gadget-value-changed <carbon-list-control-mixin>");
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <carbon-list-control-mixin>) => (handled? :: <boolean>)
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    // #"single", #"multiple"
    otherwise =>
      let selection = list-selection(gadget, sheet-direct-mirror(gadget));
      distribute-selection-changed-callback(gadget, selection);
  end;
  #t
end method handle-selection-changed;


// List boxes

define sealed class <carbon-list-box> 
    (<carbon-list-control-mixin>,
     <list-box>,
     <leaf-pane>)
end class <carbon-list-box>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-list-box>, #f)
end method class-for-make-pane;

// List controls

//---*** Need to implement add-item etc...
define sealed class <carbon-list-control> 
    (<carbon-list-control-mixin>,
     <list-control>,
     <leaf-pane>)
end class <carbon-list-control>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-list-control>, #f)
end method class-for-make-pane;

/// Option boxes

define sealed class <carbon-option-box> 
    (<carbon-list-control-mixin>,
     <option-box>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
	sealed slot popup-menu-ref :: <MenuRef>;
end class <carbon-option-box>;

// XXX - Is there a better place for this?

define method initialize
    (popup :: <carbon-option-box>, #key) => ()
  next-method();
  popup.popup-menu-ref := make-popup-menu();
end method initialize;

// XXX - Is there a better place for this?

define sealed method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <carbon-option-box>, mirror :: <control-mirror>) => ()
  next-method();
  DisposeMenu(sheet.popup-menu-ref);
end method destroy-mirror;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-option-box>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-option-box>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  // style, menu, offset, proc, 
  values(0, -12345, -1, $kControlRadioButtonProc, 0);
end method gadget-control-parameters;

define sealed method note-gadget-items-changed
    (gadget :: <carbon-option-box>) => ()
  next-method();
  let menu :: <menuRef> = gadget.popup-menu-ref;
  delete-menu-contents(menu);
  add-menu-contents(menu, gadget);
end method note-gadget-items-changed;

define sealed method update-gadget-selection
    (gadget :: <carbon-option-box>) => ()
  ignoring("update-gadget-selection on <option-box>")
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-option-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

/// Menus

define sealed class <carbon-menu-bar-pane>
    (<carbon-pane-mixin>,
     <multiple-child-composite-pane>,
     <menu-bar>)
end class <carbon-menu-bar-pane>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-menu-bar-pane>, #f)
end method class-for-make-pane;


define sealed class <carbon-menu-pane>
    (<carbon-pane-mixin>,
     <multiple-child-composite-pane>,
     <menu>)
end class <carbon-menu-pane>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-menu-pane>, #f)
end method class-for-make-pane;


define sealed class <carbon-push-menu-button>
    (<carbon-button-mixin>,
     <push-menu-button>)
end class <carbon-push-menu-button>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-push-menu-button>, #f)
end method class-for-make-pane;


define sealed class <carbon-radio-menu-button>
    (<carbon-button-mixin>,
     <radio-menu-button>)
end class <carbon-radio-menu-button>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-radio-menu-button>, #f)
end method class-for-make-pane;


define sealed class <carbon-check-menu-button>
    (<carbon-button-mixin>,
     <check-menu-button>)
end class <carbon-check-menu-button>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-check-menu-button>, #f)
end method class-for-make-pane;

/*
//--- It might be the case that this does not need to be mirrored,
//--- in which case this class and the 'class-for-make-pane' go away.
define sealed class <carbon-push-menu-box-pane>
    (<carbon-pane-mixin>,
     <push-menu-box-pane>)
end class <carbon-push-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <push-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-push-menu-box-pane>, #f)
end method class-for-make-pane;


//--- Same as <carbon-push-menu-box-pane>
define sealed class <carbon-radio-menu-box-pane>
    (<carbon-pane-mixin>,
     <radio-menu-box-pane>)
end class <carbon-radio-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <radio-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-radio-menu-box-pane>, #f)
end method class-for-make-pane;


//--- Same as <carbon-push-menu-box-pane>
define sealed class <carbon-check-menu-box-pane>
    (<carbon-pane-mixin>,
     <check-menu-box-pane>)
end class <carbon-check-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <check-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-check-menu-box-pane>, #f)
end method class-for-make-pane;
*/

/// Labels

define sealed class <carbon-label> 
    (<carbon-gadget-mixin>,
     <label>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-label>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-label>, #f)
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: <carbon-label>)
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlStaticTextProc, 0);
end method gadget-control-parameters;

define method initialize-gadget-control(gadget :: <carbon-label>, control :: <Controlhandle>)
=> ()
   with-c-string(c-string = defaulted-gadget-label(gadget))
    SetControlData(control, $kControlNoPart, $kControlStaticTextTextTag, c-string.size, c-string);
  end;
end method initialize-gadget-control;

define sealed method update-mirror-label
    (gadget :: <carbon-label>, mirror :: <control-mirror>) => ()
  with-c-string(c-string = defaulted-gadget-label(gadget))
    let widget = mirror.mirror-control;
    SetControlData(widget, $kControlNoPart, $kControlStaticTextTextTag, c-string.size, c-string);
  end;
end method update-mirror-label;

/// Separators

define sealed class <carbon-separator>
    (<carbon-gadget-mixin>,
     <separator>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-separator>;

define method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-separator>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-separator>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlSeparatorLineProc, 0);
end method gadget-control-parameters;

// Separators can be vertical or horizontal (the default for Carbon and DUIM)

define sealed method do-compose-space
    (pane :: <carbon-separator>, 
    #key width, height)
 => (space-requirement :: <space-requirement>)
  let requirements :: <space-requirement> = next-method();
  select (gadget-orientation(pane))
    /*#"horizontal" =>
     requirements;
    #"vertical" =>
      make(<space-requirement>,
        min-height: space-requirement-min-width(pane, requirements), 
        height: space-requirement-width(pane, requirements), 
        max-height: space-requirement-max-width(pane, requirements),
        width: space-requirement-height(pane, requirements));*/
    #"horizontal" =>
      make(<space-requirement>,
        min-width: 1, 
        width: width | 1, 
        max-width: $fill,
        height: 5);
    #"vertical" =>
      make(<space-requirement>,
        min-height: 1, 
        height: height | 1, 
        max-height: $fill,
        width: 5);
  end
end method do-compose-space;


/// Text gadgets

// Mixin class for text fields, password fields, and text editors, i.e.
// all TextEdit objects.
define open abstract class <carbon-text-gadget-mixin>
    (<carbon-gadget-mixin>,
     <text-field>)
  sealed slot %changed? :: <boolean> = #f;
  sealed constant slot %current-selection :: <simple-text-range>
    = make(<text-range>, start: -1, end: -1);
end class <carbon-text-gadget-mixin>;

define method install-event-handlers
    (sheet :: <carbon-text-gadget-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  debug-message("install-event-handlers <carbon-text-gadget-mixin>");
  let control :: <ControlHandle> = gadget-control(sheet);
  let err :: <OSErr> = SetControlData(control, $kControlNoPart,
    $kControlEditTextValidationProcTag, 4, $text-control-text-changed-upp);
  // XXX - This bombs out when the UPP is retrieved! Why???
  //let err :: <OSErr> = SetControlData(control, $kControlNoPart,
  //  $kControlEditTextKeyFilterTag, 4, $text-control-key-filter-upp);
end method install-event-handlers;

define sealed method update-mirror-attributes
    (gadget :: <carbon-text-gadget-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  // Set the initial text selection
  text-selection(gadget) := gadget.%current-selection
end method update-mirror-attributes;

// This is called right after gadget buffer text changes in DUIM
define sealed method note-gadget-text-changed 
    (gadget :: <carbon-text-gadget-mixin>) => ()
  debug-message("note-gadget-text-changed");
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

// XXX - We have to check for text length here!
define sealed method handle-text-gadget-changing
    (gadget :: <carbon-text-gadget-mixin>) => ()
  debug-message("handle-text-gadget-changing");
  let old-text = gadget.gadget-text-buffer;
  let control = gadget-control(gadget);
  // --- TODO: use a stretchy buffer to avoid copying on each character?
  let (err :: <OSErr>, buffer-size :: <integer>) = 
    GetControlDataSize(control, $kControlEditTextPart, gadget-control-data-tag(gadget));
  let chars = make(<c-string>, size: buffer-size);
  let err :: <OSErr> = GetControlData(control, $kControlEditTextPart, gadget-control-data-tag(gadget),
    buffer-size, chars);
  let new-text = unless (old-text = chars)
		   gadget.gadget-text-buffer := chars;
  end;
  when (new-text)
    gadget.%changed? := #t;
    distribute-text-changing-callback(gadget, new-text)
  end;
end method handle-text-gadget-changing;

define sealed method handle-text-gadget-changed
    (gadget :: <carbon-text-gadget-mixin>) => ()
  debug-message("handle-text-gadget-changed");
  when (gadget.%changed?)
    let text = gadget-text-buffer(gadget);
    distribute-text-changed-callback(gadget, text);
    gadget.%changed? := #f
  end
end method handle-text-gadget-changed;

define sealed method text-selection
    (gadget :: <carbon-text-gadget-mixin>) => (range :: type-union(<text-range>, one-of(#f)))
  let selection :: <ControlEditTextSelectionRec*> = make(<ControlEditTextSelectionRec*>);
  let err :: <OSErr> = GetControlData(gadget-control(gadget), $kControlEditTextPart,
    gadget-control-data-tag(gadget), 4, selection);
  let start-pos = selection.selStart-value;
  let end-pos = selection.selEnd-value;
  when (start-pos < end-pos)
    make(<text-range>, start: start-pos, end: end-pos)
  end;
end method text-selection;

define sealed method selected-text
    (gadget :: <carbon-text-gadget-mixin>) => (string :: false-or(<string>))
  let control = gadget-control(gadget);
  let selection :: <ControlEditTextSelectionRec*> = make(<ControlEditTextSelectionRec*>);
  let err :: <OSErr> = GetControlData(control, $kControlEditTextPart,
    gadget-control-data-tag(gadget), 4, selection);
  let start-pos = selection.selStart-value;
  let end-pos = selection.selEnd-value;
  if (start-pos >= end-pos)
    #f
  elseif (start-pos = 0 & end-pos = gadget.gadget-text-buffer.size)
    gadget.gadget-text-buffer
  else
    let temp-string = make(<c-string>, size: selection.selEnd-value);
    let err :: <OSErr> = GetControlData(control, $kControlEditTextPart,
      $kControlEditTextTextTag, selection.selEnd-value, temp-string);
    as(<string>, copy-sequence(temp-string, start: selection.selStart-value, 
      end: selection.selEnd-value));
  end;
end method selected-text;

define method widget-range-bounds (widget, range == #t)
 => (start-pos :: <integer>, end-pos :: <integer>)
  values(0, -1);
end method widget-range-bounds;

define method widget-range-bounds (widget, range == #f)
 => (start-pos :: <integer>, end-pos :: <integer>)
  //let pos = carbon-editable-get-position(widget);
  //values(pos, pos);
  values(0, 0);
end method widget-range-bounds;

define method widget-range-bounds (widget, range :: <text-range>)
 => (start-pos :: <integer>, end-pos :: <integer>)  
  let start-pos = range.text-range-start;
  let end-pos = range.text-range-end;
  if (start-pos < end-pos)
    values(start-pos, end-pos);
  else
    widget-range-bounds(widget, #f);
  end;
end method widget-range-bounds;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)),
     gadget :: <carbon-text-gadget-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  let control = gadget-control(gadget);
  let (start-pos, end-pos) = widget-range-bounds(control, range);
  let selection :: <ControlEditTextSelectionRec*> = make(<ControlEditTextSelectionRec*>);
  selection.selStart-value := start-pos;
  selection.selEnd-value := end-pos;
  let err :: <OSErr> = SetControlData(control, $kControlEditTextPart,
    gadget-control-data-tag(gadget), 4, selection);
  range;
end method text-selection-setter;

define sealed method text-caret-position
    (gadget :: <carbon-text-gadget-mixin>)
 => (position :: <integer>)
  let control = gadget-control(gadget);
  ignoring("text-caret-position");
  1;
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <carbon-text-gadget-mixin>)
 => (position :: false-or(<integer>))
  if (position)
    let control = gadget-control(gadget);
    ignoring("text-caret-position-setter");
    position
  end;
end method text-caret-position-setter;

/// Text and password fields

/// Text fields

define abstract class <carbon-text-field-mixin>
    (<carbon-text-gadget-mixin>,
     <text-field>)
  // sealed constant each-subclass slot %carbon-text-visibility,
  //  required-init-keyword: carbon-text-visibility:;
end class <carbon-text-field-mixin>;

define open generic %carbon-text-visibility
    (gadget :: <carbon-text-field-mixin>)
 => (fixed? :: <boolean>);
 
define method initialize-gadget-control(gadget :: <carbon-text-field-mixin>, control :: <Controlhandle>)
=> ()
  // XXX - This next line is going to be a problem
  let max = text-field-maximum-size(gadget);
  let text = gadget-text-buffer(gadget);
  let visibility = %carbon-text-visibility(gadget);
  debug-message(format-to-string("visibility %=", visibility));
  if (visibility) 
    ShowControl(control); 
  end if;
  unless (empty?(text))
    with-c-string (c-text = text)
      SetControlData(control, $kControlEditTextPart, gadget-control-data-tag(gadget), c-text.size, c-text);
    end;
  end;
end method initialize-gadget-control;

// Updates the EditText control from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <carbon-text-field-mixin>, mirror :: <gadget-mirror>) => ()
  ignore(mirror);
  let control = gadget-control(gadget);
  let new-text = gadget-text-buffer(gadget);
  with-c-string (c-text = new-text)
    SetControlData(control, $kControlEditTextPart, gadget-control-data-tag(gadget), c-text.size, c-text);
  end;
end method update-gadget-text;

/// Text fields
// Single line, edit selects

define sealed class <carbon-text-field>
    (<carbon-text-field-mixin>,
     <text-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-text-visibility: = $true;
end class <carbon-text-field>;

define method %carbon-text-visibility
    (gadget :: <carbon-text-field>)
 => (fixed? :: <boolean>);
  #t
end method %carbon-text-visibility;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-text-field>, #f)
end method class-for-make-pane;

define method gadget-control-parameters(gadget :: <carbon-text-field>)
=> (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlEditTextProc, 0);
end method gadget-control-parameters;

define method gadget-control-data-tag
    (gadget :: <carbon-text-field>) => (result :: <integer>)
  ignore(gadget);
  $kControlEditTextTextTag;
end method gadget-control-data-tag;

/// Password fields

define sealed class <carbon-password-field>
    (<carbon-text-field-mixin>,
     <password-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-text-visibility: = $false;
end class <carbon-password-field>;

define method %carbon-text-visibility
    (gadget :: <carbon-password-field>)
 => (fixed? :: <boolean>)
  #f
end method %carbon-text-visibility;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-password-field>, #f)
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: <carbon-password-field>) 
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlEditTextPasswordProc, 0);
end method gadget-control-parameters;

define method gadget-control-data-tag
    (gadget :: <carbon-password-field>) => (result :: <integer>)
  $kControlEditTextPasswordTag;
end method gadget-control-data-tag;

/// Text editors
// Multiline, allows enter

define sealed class <carbon-text-editor>
    (<carbon-text-gadget-mixin>,
     <text-editor>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-text-editor>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-text-editor>, #f)
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: <carbon-text-editor>) 
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlEditTextProc, 0);
end method gadget-control-parameters;

define method gadget-control-data-tag
    (gadget :: <carbon-text-editor>) => (tag)
  ignore(gadget);
  $kControlEditTextTextTag;
end method gadget-control-data-tag;

// We can't tell a control how many rows/columns to be, so we cheat here
// XXX - TODO: FIXME!!!
define method do-compose-space 
    (pane :: <carbon-text-editor>, #key width, height)
 => (space-requirement :: <space-requirement>)
  debug-message(format-to-string("do-compose-space %=", pane));
  let lines = gadget-lines(pane);
  let columns = gadget-columns(pane);
  if(lines & columns)
    let best-width :: <integer> = (12 * columns) + 2;
    let best-height :: <integer> = (12 * lines) + 2;
    debug-message(format-to-string("width: %d height: %d", best-width, best-height));
    make(<space-requirement>,
        width:  best-width,  min-width:  best-width,  
        max-width: best-width,
        height: best-height, min-height: best-height, 
        max-height: best-height);
  else
    next-method();
  end if;
end method do-compose-space;

// XXX - Make sure text editors have this behaviour
/*define sealed method make-carbon-mirror
    (gadget :: <carbon-text-editor>)
 => (mirror :: <gadget-mirror>)
  let lines = gadget-lines(gadget);
  let columns = gadget-columns(gadget);
  let word-wrap? = text-field-word-wrap?(gadget);
  let text = gadget-text-buffer(gadget);
  let widget = carbon-TEXT(carbon-text-new(null-pointer(<carbonAdjustment*>),
                                     null-pointer(<carbonAdjustment*>)));
  assert(~null-pointer?(widget), "carbon-text-new failed");
  // Note that this is happening before install-event-handlers, so don't
  // need to disable events.
  when (lines | columns)
    ignoring("lines:/columns:")
  end;
  carbon-text-set-word-wrap(widget, if (word-wrap?) $true else $false end);
  set-text-widget-text(widget, text);
  make(<gadget-mirror>,
       widget: widget,
       sheet:  gadget)
end method make-carbon-mirror;*/

/// Borders

define sealed class <carbon-border>
    (<carbon-gadget-mixin>,
     <border>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-border>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <carbon-group-box> else <carbon-border> end;
  values(border-class, #f)
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: subclass(<carbon-border>))
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 0, $kControlGroupBoxTextTitleProc, 0);
end method gadget-control-parameters;

/// Group boxes, aka labelled borders

define sealed class <carbon-group-box>
    (<carbon-border>,
     <group-box>,
     <sealed-constructor-mixin>)
end class <carbon-group-box>;

define method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <group-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-group-box>, #f)
end method class-for-make-pane;

define method initialize-gadget-control(gadget :: <carbon-group-box>, control :: <Controlhandle>)
=> ()
  with-pascal-string(p-string = defaulted-gadget-label(gadget))
    SetControlTitle(control, p-string);
  end;
end method initialize-gadget-control;

/// Progress Bars

define sealed class <carbon-progress-bar>
    (<carbon-gadget-mixin>,
     <progress-bar>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-progress-bar>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <progress-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-progress-bar>, #f);
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: <carbon-progress-bar>)
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 255, $kControlKindProgressBar, 0);
end method gadget-control-parameters;

define sealed method note-gadget-value-changed
    (gadget :: type-union(<carbon-progress-bar>, <carbon-slider>)) => ()
  next-method();
  let control :: false-or(<ControlHandle>) = gadget-control(gadget);
  when(control)
    SetControl32BitValue(control, gadget-value(gadget));
  end when;
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: type-union(<carbon-progress-bar>, <carbon-slider>)) => ()
  format-out("note-gadget-value-range-changed");
  next-method();
  let control :: false-or(<ControlHandle>) = gadget-control(gadget);
  when(control)
    let (lower, upper, step) = gadget-range-values(gadget);
    SetControl32BitMinimum(control, lower);
    SetControl32BitMaximum(control, upper);
  end when;
end method note-gadget-value-range-changed;


/// Sliders

define sealed class <carbon-slider>
    (<carbon-gadget-mixin>,
     <slider>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-slider>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <slider>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-slider>, #f);
end method class-for-make-pane;

define method make-gadget-control
    (gadget :: <carbon-slider>, window :: <WindowRef>, bounds-rect :: <Rect*>)
 => (control :: <ControlRef>)
  debug-message(format-to-string("make-gadget-control %=", gadget));
  let (status :: <OSStatus>, control :: <ControlRef>) = 
  	CreateSliderControl(window, bounds-rect, 0, 0, 255, $kControlSliderPointsDownOrRight,
  	 slider-tick-marks(gadget), #t, $slider-live-action-handler-upp);
	check-result("CreateSliderControl in make-gadget-control", status);
	control;
end method make-gadget-control;

// Sliders and scroll-bars can be oriented vertically (the Carbon default)
// or horizontally (the DUIM default)

define sealed method do-compose-space
    (pane :: type-union(<carbon-slider>, <carbon-scroll-bar>), 
    #key width, height)
 => (space-requirement :: <space-requirement>)
  let requirements :: <space-requirement> = next-method();
  select (gadget-orientation(pane))
    #"horizontal" =>
      make(<space-requirement>,
        min-width: space-requirement-min-height(pane, requirements), 
        width: space-requirement-height(pane, requirements), 
        max-width: space-requirement-max-height(pane, requirements),
        height: space-requirement-width(pane, requirements));
    #"vertical" =>
     requirements;
  end
end method do-compose-space;

define method install-event-handlers
    (sheet :: <carbon-slider>, mirror :: <gadget-mirror>) => ()
  debug-message(format-to-string("install-event-handlers Sheet %= mirror %=", sheet, mirror));
  let control :: <ControlHandle> = mirror.mirror-control;
  debug-message(format-to-string("control  %=", control));
  debug-message(format-to-string("Installing value changed event handler on %d", control.pointer-address));
  install-control-event-handler(control, $kEventClassControl, $kEventControlValueFieldChanged, 
                               $control-value-changed-event-handler-upp);
end method install-event-handlers;

// XXX - Please can we export event-value from duim/gadgets/gadget-mixins.dylan!
define method handle-event
    (pane :: <carbon-slider>, event :: <value-changed-gadget-event>) => ()
  // We can't get event-value because it isn't exported
  let value :: <integer> = GetControl32BitValue(event.event-gadget.gadget-control);
  gadget-value(pane, do-callback?: #t) := value;
end method handle-event;

/// Scroll bars

define sealed class <carbon-scroll-bar>
    (<carbon-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-scroll-bar>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <scroll-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-scroll-bar>, #f)
end method class-for-make-pane;

define method gadget-control-parameters
    (gadget :: <carbon-scroll-bar>)
 => (value, min, max, proc, refcon)
  ignore(gadget);
  values(0, 0, 255, $kControlScrollBarLiveProc, 0);
end method gadget-control-parameters;

define function gadget-range-values
    (gadget :: <range-gadget-mixin>)
 => (start-value :: <real>, end-value :: <real>, increment :: <real>)
  let range = gadget-value-range(gadget);
  let n = range.size;
  select (n)
    0 => 
      values(0, 0, 0);
    1 => 
      let start = range[0];
      values(start, start, 0);
    otherwise =>
      let start = range[0];
      values(start, range[n - 1], range[1] - start)
  end;
end gadget-range-values;

define method scroll-bar-adjusted-contents
    (gadget :: <carbon-scroll-bar>)
 => (value :: <integer>,
     lower :: <integer>, upper :: <integer>,
     step-increment :: <integer>, page-increment :: <integer>,
     page-size :: <integer>)
  let range-value = gadget-value(gadget);
  let (range-start, range-end, range-step) = gadget-range-values(gadget);
  let slug-size = gadget-slug-size(gadget);
  let page-increment = max(slug-size, range-step);
  
  values(range-value, range-start, range-end, range-step, page-increment, slug-size)
end scroll-bar-adjusted-contents;

define method install-event-handlers
    (sheet :: <carbon-scroll-bar>, mirror :: <gadget-mirror>) => ()
  debug-message(format-to-string("install-event-handlers Sheet %= mirror %=", sheet, mirror));
  let control :: <ControlHandle> = mirror.mirror-control;
  debug-message(format-to-string("control  %=", control));
  debug-message(format-to-string("Installing value changed event handler on %d", control.pointer-address));
  install-control-event-handler(control, $kEventClassControl, $kEventControlValueFieldChanged, 
                               $control-value-changed-event-handler-upp);
  SetControlAction(control, $scroll-bar-live-action-handler-upp);
end method install-event-handlers;

// XXX - Please can we export event-value from duim/gadgets/gadget-mixins.dylan!
define method handle-event
    (pane :: <carbon-scroll-bar>, event :: <value-changed-gadget-event>) => ()
  // We can't get event-value because it isn't exported
  let position :: <integer> = GetControl32BitValue(event.event-gadget.gadget-control);
  // scroll-to-position sets the value and calls the callback
  scroll-to-position(pane, position);
end method handle-event;

define sealed method note-gadget-slug-size-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-slug-size-changed;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-range-changed;

define sealed method note-scroll-bar-changed
    (gadget :: <carbon-scroll-bar>) => ()
  let control = gadget-control(gadget);
  when (control)
    let (value, lower, upper, step-inc, page-inc, page-size)
      = scroll-bar-adjusted-contents(gadget);
      SetControl32BitValue(control, value);
      SetControl32BitMinimum(control, lower);
      SetControl32BitMaximum(control, upper);
      SetControlViewSize(control, page-size);
  end when;
end method note-scroll-bar-changed;