module:     X-Inspector
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/x-inspector.dylan,v 1.6 1996/04/24 12:29:37 wlott Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================


define class <inspector-state> (<object>)
  //
  // Lock used to arbitrate access to the state object.
  constant slot state-lock :: <spinlock> = make(<spinlock>);
  //
  // Set to #t when we are done.
  slot state-done? :: <boolean> = #f;
  //
  // Event that is signaled when we are done.
  constant slot state-done :: <event> = make(<event>);
  //
  // All the windows that have been created for this state.
  constant slot state-windows :: <stretchy-vector> = make(<stretchy-vector>);
end class <inspector-state>;


// Create the widgets for a single <body-component>, and put these
// widgets inside window.
//
define function do-component 
    (component :: <body-component>, window :: <window>,
     state :: <inspector-state>)
    => ();
  let (#rest strings) = split("#!|!#", component.description);
  let frame = make(<frame>, in: window, side: "top", anchor: "w");
  let label?  // If string starts with #!....
    = if (substring-position(component.description, "#!") == 0)
	odd?;
      else
	even?;
      end if;
  for (i from 0 below strings.size)
    if (i.label?)
      make(<label>, text: strings[i], in: frame, side: "left", anchor: "w");
    else
      make(<button>, text: strings[i], in: frame, side: "left", anchor: "w",
	   relief: "raised", 
	   command: method ()
		      let obj = component.related-objects[truncate/(i, 2)];
		      xinspect-one-object(obj, state);
		    end method);
    end if;
  end for;
end function do-component;

// Graphically display obj.object-info.  The display is interactive.
//
define function xinspect-one-object
    (obj :: <object>, state :: <inspector-state>) => ();
  grab-lock(state.state-lock);
  if (state.state-done?)
    release-lock(state.state-lock);
  else
    let window = make(<toplevel>);
    add!(state.state-windows, window);
    release-lock(state.state-lock);

    call-tk-function("wm minsize ", tk-as(<string>, window), " 1 1");
    let window-title = tk-quote(concatenate("Inspect ", short-string(obj)));
    call-tk-function("wm title ", tk-as(<string>, window),
		     " \"", window-title, "\"");
    let info = obj.object-info;
    for (attrib in info)
      make(<label>, text: attrib.attrib-header, in: window, 
	   side: "top", anchor: "w");
      if (attrib.attrib-body.size > 5 
	    & every?(method (component)
		       component.related-objects.size == 1;
		     end method,
		     attrib.attrib-body))
	let frame = make(<frame>, anchor: "w", side: "top", 
			 in: window, expand: #t, fill: "both");
	make(<frame>, relief: "sunken",
	     width: 50, fill: "y", anchor: "w", side: "left", in: frame);
	let listbox = make(<listbox>, relief: "sunken", in: frame,
			   side: "left", expand: #t, fill: "both");
	scroll(listbox, orient: "vertical", fill: "y",
	       in: frame, side: "left", relief: "sunken");
	apply(insert, listbox, 0,
	      map(stripped-description, attrib.attrib-body));
	bind(listbox, "<Double-Button-1>",
	     method ()
	       let index = listbox.current-selection.first;
	       let component = attrib.attrib-body[index];
	       xinspect-one-object(component.related-objects.first, state);
	     end method);
      else
	let frame = make(<frame>, anchor: "w", side: "top", in: window);
	// padding
	make(<frame>, relief: "sunken",
	     width: 50, fill: "y", anchor: "w", side: "left", in: frame);
	let descr-frame = make(<frame>, side: "right", in: frame, 
			       expand: #t, fill: "both");
	for (component in attrib.attrib-body)
	  do-component(component, descr-frame, state);
	end for;
      end if;
    end for;
    
    let button-frame = make(<frame>, side: "top", in: window);
    if (instance?(obj, <class>))
      make(<button>, text: "See Class Diagram",
	   relief: "raised", side: "left", anchor: "w", in: button-frame,
	   command: method () 
		      let title = concatenate("Diagram ", short-string(obj));
		      view-class-hierarchy(obj, title);
		    end method);
    end if;
    make(<button>, text: "Close",
	 command: method ()
		    grab-lock(state.state-lock);
		    if (state.state-done?)
		      release-lock(state.state-lock);
		    else
		      remove!(state.state-windows, window);
		      let last-window? = state.state-windows.empty?;
		      if (last-window?)
			state.state-done? := #t;
		      end if;
		      release-lock(state.state-lock);
		      destroy-window(window);
		      if (last-window?)
			signal-event(state.state-done);
		      end if;
		    end if;
		  end,
	 relief: "raised", side: "left", anchor: "w", in: button-frame);
    make(<button>, text: "Quit",
	 command: method ()
		    grab-lock(state.state-lock);
		    let already-done? = state.state-done?;
		    state.state-done? := #t;
		    release-lock(state.state-lock);
		    // Once the done flag has been set, nobody changes windows
		    // so we don't need to protect it anymore.
		    for (window in state.state-windows)
		      destroy-window(window);
		    end for;
		    unless (already-done?)
		      signal-event(state.state-done);
		    end unless;
		  end method,
	 relief: "raised", side: "left", anchor: "w", in: button-frame);
  end if;
end function xinspect-one-object;



// Interface to the outside world.

define function xinspect (#rest objs) => ();
  let state = make(<inspector-state>);
  if (objs.empty?)
    xinspect-one-object($all-libraries, state);
  else
    for (obj in objs)
      xinspect-one-object(obj, state);
    end for;
  end if;
  grab-lock(state.state-lock);
  until (state.state-done?)
    wait-for-event(state.state-done, state.state-lock);
    grab-lock(state.state-lock);
  end until;
  release-lock(state.state-lock);
end function xinspect;

*xinspect-function* := xinspect;
