Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	      Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
              Carbon Code is Copyright (c) 2001 Gwydion Dylan Maintainers.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $blocking-event-loop :: <boolean> = #f;


/// Carbon port class

define sealed class <carbon-port> (<basic-port>)
  // The currently recorded focus
  sealed slot %focus :: false-or(<WindowRef>) = #f;
end class <carbon-port>;

define method initialize (_port :: <carbon-port>, #key) => ()
  next-method();
  InitCursor();
end method initialize;


register-port-class(#"carbon", <carbon-port>, default?: #t);
 
 
define sideways method class-for-make-port
    (type == #"carbon", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<carbon-port>, concatenate(initargs, #(event-processor-type:, #"n")));
end method class-for-make-port;

// #"local" is the 'default' port type used if none is specified
/*define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"carbon", initargs)
end method class-for-make-port;*/

define method port-type (_port :: <carbon-port>) => (type :: <symbol>)
  #"carbon"
end method port-type;

define method port-name (_port :: <carbon-port>) => (name :: false-or(<string>))
  ignoring("port-name");
  #f
end method port-name;


/// Beeping, etc

define method force-display (_port :: <carbon-port>) => ()
  // QdFlushPortBuffer on the current focus?
  ignoring("force-display")
end method force-display;

define method synchronize-display (_port :: <carbon-port>) => ()
  // XXX - until(QDFinished())
  ignoring("synchronize-display")
end method synchronize-display;

define method beep (_port :: <carbon-port>) => ()
  SysBeep(8);
end method beep;


/// Pointers

define sealed method do-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let (dx, dy) = sheet-screen-position(_port, sheet);
		with-stack-structure(point :: <Point*>)
		GetMouse(point);
    // XXX - Make sure we are in the correct window/grafport!
    GlobalToLocal(point);
		values(point.h-value - dx, point.v-value - dy)
  end
end method do-pointer-position;

define sealed method do-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  with-stack-structure(point :: <Point*>)
		GetMouse(point);
		values(point.h-value, point.v-value)
  end
end method do-pointer-position;

// XXX - Ignore for now as Apple hate this. Use pointer device manager if needed.

define method do-set-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. sheet
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define method do-set-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. the display
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define method do-set-pointer-cursor
    (_port :: <carbon-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  //--- Set the pointer cursor
  ignoring("do-pointer-position")
end method do-set-pointer-cursor;


define method do-set-sheet-cursor
    (_port :: <carbon-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  //--- Set the cursor for the sheet
  ignoring("do-pointer-position")
end method do-set-sheet-cursor;


//--- Define the keysyms for the port


/// Input focus handling
// XXX - There is code for this in carbon-duim-win32-conversion

define sealed method note-focus-in
    (_port :: <carbon-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-in")
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <carbon-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-out")
end method note-focus-out;


// Events

/*
    process-next-event
*/

define thread variable *run-application-event-loop* :: <boolean> = #t;

define sealed method process-next-event
    (_port :: <carbon-port>, #key timeout = #f)
 => (timed-out? :: <boolean>)
  if($blocking-event-loop)
    // Smart version. Do we need to be an <immediate-input-mixin> for this to work?
    if(*run-application-event-loop*)
      debug-message("RunApplicationEventLoop()");
      *run-application-event-loop* := #f;
      RunApplicationEventLoop();
    end;
    #f;
  else
    // Dumb version.
    let inTimeOut = if(timeout) timeout else $kEventDurationForever end if;
    // This gives us ownership of the event!
    let (status :: <OSStatus>, next-event :: <EventRef>) = ReceiveNextEvent(0, $NULL, inTimeOut, #t);
    if(status = $noErr)
      // Dispatch the event
      let status :: <OSStatus> = SendEventToEventTarget(next-event, GetEventDispatcherTarget());  
      // Now dispose of it
      ReleaseEvent(next-event);
      #f;
    else
      #t;
    end if;
  end if;
end method process-next-event;
