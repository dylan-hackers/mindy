Module:       gtk-duim
Synopsis:     GTK dialog implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $dialog-border         = 10;
define constant $dialog-spacing        = 10;
define constant $exit-button-min-width = 100;


/// DUIM dialogs

define sealed class <dialog-mirror> (<top-level-mirror>)
  sealed constant slot %owner :: false-or(<frame>),
    required-init-keyword: owner:;
end class <dialog-mirror>;

define sealed domain make (singleton(<dialog-mirror>));
define sealed domain initialize (<dialog-mirror>);

ignore(mirror-registered-dialogs);

define sealed method make-top-level-mirror
    (sheet :: <top-level-sheet>, frame :: <dialog-frame>)
 => (mirror :: <top-level-mirror>)
  let widget = GTK-WINDOW(gtk-window-new($GTK-WINDOW-DIALOG));
  let owner = frame-owner(frame);
  make(<dialog-mirror>,
       widget: widget,
       sheet:  sheet,
       owner: owner)
end method make-top-level-mirror;

define sealed method cancel-frame
    (dialog :: <dialog-frame>) => (handled? :: <boolean>)
  let button = dialog-cancel-button(dialog);
  when (button & gadget-enabled?(button))
    handle-gadget-activation(button)
  end
end method cancel-frame;

define sealed method map-mirror
    (_port :: <gtk-port>,
     sheet :: <gtk-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  ensure-dialog-position(sheet-frame(sheet), mirror);
  next-method();
  let dialog = sheet-frame(sheet);
  let owner  = frame-owner(dialog);
  owner & register-dialog-mirror(owner, mirror)
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <gtk-port>,
     sheet :: <gtk-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  next-method();
  let dialog = sheet-frame(sheet);
  let owner  = frame-owner(dialog);
  owner & unregister-dialog-mirror(owner, mirror)
end method unmap-mirror;

define sealed method ensure-dialog-position
    (frame :: <dialog-frame>, mirror :: <dialog-mirror>) => ()
  ignoring("ensure-dialog-position")
end method ensure-dialog-position;

define sealed method compute-dialog-position
    (frame :: <dialog-frame>)
 => (x :: <integer>, y :: <integer>)
  //--- Is there a better way to get this?  'frame-position' always
  //--- gets the info from the top level sheet which isn't what
  //--- we want here.
  let sheet = top-level-sheet(frame);
  let (width, height) = sheet-size(sheet);
  let geometry = frame-geometry(frame);
  let frame-x = geometry[0];
  let frame-y = geometry[1];
  let frame-width = geometry[2];
  let frame-height = geometry[3];
  let width = frame-width | width;
  let height = frame-height | height;
  if (frame-x & frame-y)
    values(frame-x, frame-y)
  else
    let _display = display(sheet);
    let owner = frame-owner(frame);
    let owner-top-sheet = owner & top-level-sheet(owner);
    let owner-mirror = owner-top-sheet & sheet-direct-mirror(owner-top-sheet);
    let (screen-width, screen-height) = sheet-size(_display);
    if (owner-mirror)
      // Center the dialog over the client area
      let (owner-x, owner-y) = client-to-screen-position(owner-mirror, 0, 0);
      // let owner-handle = window-handle(owner-mirror);
      // let (owner-width, owner-height) = get-client-size(owner-handle);
      let (owner-width, owner-height) = sheet-size(owner);
      duim-debug-message("  Owner currently %d x %d, at %d, %d",
			 owner-width, owner-height, owner-x, owner-y);
      duim-debug-message("  Dialog currently %d x %d",
			 width, height);
      let x
	= max(min(screen-width - width,
		  owner-x + floor/(owner-width  - width, 2)),
	      0);
      let y
	= max(min(screen-height - height,
		  owner-y + floor/(owner-height - height, 2)),
	      0);
      values(x, y)
    else
      // Center the dialog on the screen
      values(max(floor/(screen-width  - width, 2),  0),
	     max(floor/(screen-height - height, 2), 0))
    end
  end
end method compute-dialog-position;


/// Piggy-back on the default dialogs from gadget-panes for now

define method top-level-layout-child
    (framem :: <gtk-frame-manager>, 
     dialog :: <dialog-frame>,
     layout :: false-or(<sheet>))
 => (layout :: false-or(<sheet>))
  default-dialog-frame-wrapper(framem, dialog, layout);
end method top-level-layout-child;

define sealed method update-frame-layout
    (framem :: <gtk-frame-manager>, frame :: <dialog-frame>) => ()
  update-default-dialog-layout(framem, frame)
end method update-frame-layout;

define method make-exit-button
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>,
     callback :: false-or(<callback-type>), label :: <string>,
     #rest initargs, 
     #key enabled? = (callback ~= #f), #all-keys)
 => (button :: false-or(<push-button>))
  when (callback)
    with-frame-manager (framem)
      apply(make, <push-button>,
	    activate-callback: method (button)
				 let dialog = sheet-frame(button);
				 execute-callback(dialog, callback, dialog)
			       end,
	    label: label,
	    enabled?: enabled?,
	    min-width: $exit-button-min-width,
	    initargs)
    end
  end
end method make-exit-button;

define sealed method default-dialog-border
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  $dialog-border
end method default-dialog-border;

define sealed method default-dialog-spacing
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  $dialog-spacing
end method default-dialog-spacing;

define sealed method default-dialog-button-spacing
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  select (dialog-exit-buttons-position(dialog))
    #"left", #"right" => 8;
    #"top", #"bottom" => 8;
    otherwise         => 8;
  end
end method default-dialog-button-spacing;

define sealed method default-dialog-extra-size
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>)
 => (width :: <integer>, height :: <integer>)
  ignore(framem);
  ignoring("default-dialog-extra-size");
  values(0, 0)
end method default-dialog-extra-size;


/// Dialog flow control

// Generate an ordinary exit event
define sealed method do-exit-dialog
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  // Under GTK, we need to re-enable the owner before we dismiss
  // the dialog so that the focus gets returned to the right place
  let owner  = frame-owner(dialog);
  let modal? = (frame-mode(dialog) == #"modal");
  when (owner & modal?)
    frame-enabled?(owner) := #t
  end;
  frame-mapped?(dialog) := #f;
  distribute-event(port(dialog),
		   make(<dialog-exit-event>,
			frame: dialog,
			destroy-frame?: destroy?))
end method do-exit-dialog;

// Generate an "error" exit event
define sealed method do-cancel-dialog 
    (framem :: <gtk-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  let owner  = frame-owner(dialog);
  let modal? = (frame-mode(dialog) == #"modal");
  when (owner & modal?)
    frame-enabled?(owner) := #t
  end;
  frame-mapped?(dialog) := #f;
  distribute-event(port(dialog),
		   make(<dialog-cancel-event>,
			frame: dialog,
			destroy-frame?: destroy?))
end method do-cancel-dialog;


/// Notify user

define class <notification-dialog> (<dialog-frame>)
  slot notification-dialog-result :: one-of(#"yes", #"no", #"ok", #"cancel"),
    init-value: #"cancel";
  constant slot notification-dialog-exit-style :: <notification-exit-style>,
    required-init-keyword: exit-style:;
  constant slot notification-dialog-yes-callback :: false-or(<function>) = #f,
    init-keyword: yes-callback:;
  constant slot notification-dialog-no-callback :: false-or(<function>) = #f,
    init-keyword: no-callback:;
end class;

define sealed method do-notify-user
    (framem :: <gtk-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key title, documentation, exit-boxes, name,
          exit-style :: false-or(<notification-exit-style>) = #f,
          foreground, background, text-style)
 => (ok? :: <boolean>, exit-type)
  ignore(exit-boxes);
  let (x, y)
    = begin
	let (x, y) = sheet-size(owner);
	let (x, y) = values(floor/(x, 2), floor/(y, 2));
	with-device-coordinates (sheet-device-transform(owner), x, y)
	  values(x, y)
	end
      end;
  let title
    = title | select (style)
		#"information"   => "Note";
		#"question"      => "Note";
		#"warning"       => "Warning";
		#"error"         => "Error";
		#"serious-error" => "Error";
		#"fatal-error"   => "Error";
	      end;
  let exit-style
    = exit-style | if (style == #"question") #"yes-no" else #"ok" end;
  local
    method notify-callback(dialog :: <notification-dialog>,
                           result :: one-of(#"yes", #"no", #"ok"))
     => ();
      dialog.notification-dialog-result := result;
      exit-dialog(dialog);
    end method;
  let exit-options
    = select(exit-style)
        #"ok" =>
          vector(exit-callback: rcurry(notify-callback, #"ok"),
                 cancel-callback: #f);
        #"ok-cancel" =>
          vector(exit-callback: rcurry(notify-callback, #"ok"));
        #"yes-no" =>
          vector(yes-callback: rcurry(notify-callback, #"yes"),
                 no-callback: rcurry(notify-callback, #"no"),
                 exit-callback: #f,
                 cancel-callback: #f);
        #"yes-no-cancel" =>
          vector(yes-callback: rcurry(notify-callback, #"yes"),
                 no-callback: rcurry(notify-callback, #"no"),
                 exit-callback: #f);
      end;
        
  let dialog = apply(make,
                     <notification-dialog>,
                     x: x, y: y,
                     title: title,
                     foreground: foreground,
                     background: background,
                     exit-style: exit-style,
                     layout: make(<label>,
                                 label: message,
                                 text-style: text-style),
                     exit-options);
  if (start-dialog(dialog))
    select (dialog.notification-dialog-result)
      #"yes"    => values(#t, #"yes");
      #"no"     => values(#f, #"no");
      #"ok"     => values(#t, #"ok");
      #"cancel" => values(#f, #"cancel");
    end;
  else
    values(#f, #"cancel");
  end if;
end method do-notify-user;

define sealed method make-exit-buttons
    (framem :: <gtk-frame-manager>, dialog :: <notification-dialog>)
 => (buttons :: <sequence>)
  select(dialog.notification-dialog-exit-style)
    #"ok", #"ok-cancel" =>
      next-method();
    #"yes-no", #"yes-no-cancel" =>
      let yes-button
        = make-exit-button(framem, dialog,
                           dialog.notification-dialog-yes-callback, "Yes");
      let no-button
        = make-exit-button(framem, dialog,
                           dialog.notification-dialog-no-callback, "No");
      unless (frame-default-button(dialog))
        frame-default-button(dialog) := yes-button;
      end;
      concatenate(vector(yes-button, no-button), next-method());
  end select;
end method make-exit-buttons;


/// Choose file

define variable *gtk-choose-file-locator* :: false-or(<string>) = #f;

define sealed method do-choose-file
    (framem :: <gtk-frame-manager>, owner :: <sheet>, 
     direction :: one-of(#"input", #"output"),
     #key title :: false-or(<string>),
          documentation :: false-or(<string>),
          exit-boxes,
	  if-exists,
          if-does-not-exist = #"ask",
	  default :: false-or(<string>),
          default-type = $unsupplied,
	  filters,
          default-filter,
     #all-keys)
 => (locator :: false-or(<string>), filter :: false-or(<integer>))
  ignore(if-exists);
  let parent-widget = mirror-widget(sheet-mirror(owner));
  // let (directory, pattern)
  //   = gtk-directory-and-pattern(default, default-type);
  let (x, y)
    = begin
        let (x, y) = sheet-size(owner);
        let (x, y) = values(floor/(x, 2), floor/(y, 2));
        with-device-coordinates (sheet-device-transform(owner), x, y)
          values(x, y)
        end
      end;

  let title = title | "Open";
  let widget =
    with-c-string (c-string = title)
      GTK-FILE-SELECTION(gtk-file-selection-new(c-string));
    end;

  if(default)
    with-c-string(c-string = default)
      gtk-file-selection-set-filename(widget, c-string);
    end;
  end if;
  

  gtk-signal-connect*(GTK-OBJECT(widget.ok-button-value),
                      "clicked", file-selection-ok-clicked-callback,
                      widget);
  gtk-signal-connect*(GTK-OBJECT(widget.cancel-button-value),
                      "clicked", file-selection-cancel-clicked-callback,
                      widget);
  gtk-signal-connect*(GTK-OBJECT(widget),
                      "destroy", file-selection-destroy-callback, widget);

  *gtk-choose-file-locator* := #f;
  gtk-window-set-modal(GTK-WINDOW(widget), $true);
  gtk-widget-show(widget);
  gtk-main();

  values(*gtk-choose-file-locator*, #f);
end method do-choose-file;

define widget-callback file-selection-ok-clicked-callback
  = handle-file-selection-ok-clicked;

define function handle-file-selection-ok-clicked
    (widget :: <GtkWidget*>, data :: <gpointer>)
 => ();
  let file-selection = GTK-FILE-SELECTION(data);
  *gtk-choose-file-locator*
    := as(<byte-string>, gtk-file-selection-get-filename(file-selection));
  gtk-widget-destroy(GTK-WIDGET(file-selection));
end function;

define widget-callback file-selection-cancel-clicked-callback
  = handle-file-selection-cancel-clicked;

define function handle-file-selection-cancel-clicked
    (widget :: <GtkWidget*>, data :: <gpointer>)
 => ();
  gtk-widget-destroy(GTK-WIDGET(data));
end function;

define widget-callback file-selection-destroy-callback
  = handle-file-selection-destroy;

define function handle-file-selection-destroy
    (widget :: <GtkWidget*>, data :: <gpointer>)
 => ();
  ignore(widget);
  ignore(data);
  gtk-main-quit();
end function;

/*
define function gtk-directory-and-pattern
    (default :: false-or(<string>), default-type)
 => (directory :: false-or(<string>), pattern :: false-or(<string>))
  if (~default)
    values(#f, #f)
  else
    let type
      = select (type by instance?)
          <string> =>
            type;
          <symbol> =>
            gethash($file-type-table, type) | as-lowercase(as(<string>, type));
          otherwise =>
            "";
        end;
    //---*** USE PROPER LOCATORS FUNCTIONS
    let default    = as(<locator>, default);
    let directory  = directory-namestring(default);
    let wild-name? = (locator-name(default) = "*");
    let wild-type? = (locator-type(default) = "*");
    let pattern    = when (wild-name? ~== wild-type?)
                       as(<string>, make-locator(name: locator-name(default),
                                                 type: locator-type(default)))
                     end;
    values(directory, pattern)
  end
end function gtk-directory-and-pattern;
*/

//---*** This should be in Locators or File-System
define table $file-type-table :: <table>
  = { #"dylan"       => "dylan",
      #"c"           => "c",
      #"c-include"   => "h",
      #"cpp"         => "cpp",
      #"cpp-include" => "hpp",
      #"text"        => "text",
      #"project"     => "hdp",
      #"lid"         => "lid",
      #"executable"  => "exe",
      #"resource"    => "res",
      #"library"     => "lib" };

 
/// Choose directory

define sealed method do-choose-directory
    (framem :: <gtk-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>),
          documentation :: false-or(<string>),
          exit-boxes,
	  default :: false-or(<string>),
     #all-keys)
 => (locator :: false-or(<string>))
  let parent-widget = mirror-widget(sheet-mirror(owner));
  // let (directory, pattern)
  //   = gtk-directory-and-pattern(default, default-type);
  let (x, y)
    = begin
        let (x, y) = sheet-size(owner);
        let (x, y) = values(floor/(x, 2), floor/(y, 2));
        with-device-coordinates (sheet-device-transform(owner), x, y)
          values(x, y)
        end
      end;

  let title = title | "Choose Directory";
  let widget =
    with-c-string (c-string = title)
      GTK-FILE-SELECTION(gtk-file-selection-new(c-string));
    end;

  if(default)
    with-c-string(c-string = default)
      gtk-file-selection-set-filename(widget, c-string);
    end;
  end if;
  
  gtk-signal-connect*(GTK-OBJECT(widget.ok-button-value),
                      "clicked", file-selection-ok-clicked-callback,
                      widget);
  gtk-signal-connect*(GTK-OBJECT(widget.cancel-button-value),
                      "clicked", file-selection-cancel-clicked-callback,
                      widget);
  gtk-signal-connect*(GTK-OBJECT(widget),
                      "destroy", file-selection-destroy-callback, widget);
  gtk-widget-hide(widget.file-list-value.parent-value);

  *gtk-choose-file-locator* := #f;
  gtk-window-set-modal(GTK-WINDOW(widget), $true);
  gtk-widget-show(widget);
  gtk-main();

  values(*gtk-choose-file-locator*, #f);
end method do-choose-directory;

 
/// Color chooser
/*
define variable *custom-colors* :: <LPCOLORREF>
    = make(<LPCOLORREF>, element-count: 16);

define sealed method do-choose-color
    (framem :: <gtk-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<color>),
     #all-keys)
 => (color :: false-or(<color>));
  let handle = dialog-owner-handle(owner);
  with-stack-structure (color :: <LPCHOOSECOLOR>)
    color.lStructSize-value  := size-of(<CHOOSECOLOR>);
    color.hwndOwner-value    := handle;
    color.hInstance-value    := application-instance-handle();
    color.rgbResult-value    := if (default) %color->native-color(default)
				else $native-black end;
    color.Flags-value        := %logior($CC-ANYCOLOR,
					if (default) $CC-RGBINIT else 0 end,
					$CC-SHOWHELP);
    color.lpCustColors-value := *custom-colors*;
    if (ChooseColor(color))
      let colorref = color.rgbResult-value;
      %native-color->color(colorref)
    else
      ensure-no-dialog-error("ChooseColor")
    end
  end
end method do-choose-color;


/// Text style chooser

define sealed method do-choose-text-style
    (framem :: <gtk-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<text-style>),
     #all-keys)
 => (text-style :: false-or(<text-style>));
  let _port = port(framem);
  let hDC :: <hDC> = _port.%memory-hDC;
  let handle = dialog-owner-handle(owner);
  with-stack-structure (logfont :: <LPLOGFONT>)
    with-stack-structure (cf :: <LPCHOOSEFONT>)
      cf.lStructSize-value := size-of(<CHOOSEFONT>);
      cf.hwndOwner-value   := handle;
      cf.hInstance-value   := application-instance-handle();
      cf.hDC-value         := hDC;
      cf.lpLogFont-value   := logfont;
      cf.Flags-value       := %logior($CF-SHOWHELP,
				      $CF-FORCEFONTEXIST,
				      $CF-SCREENFONTS,
				      $CF-EFFECTS);
      cf.lpszStyle-value   := $NULL-string;
      if (ChooseFont(cf))
	make-text-style-from-font(_port, logfont)
      else
	ensure-no-dialog-error("ChooseFont")
      end
    end
  end
end method do-choose-text-style;
*/
