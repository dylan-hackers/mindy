module:     X-Inspector
library:    X-Inspector
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/x-inspector.dylan,v 1.2 1996/04/07 18:37:59 nkramer Exp $

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

define library X-inspector
  use dylan;
  use inspector-base;
  use tk;
  use string-extensions;
end library X-inspector;

define module X-inspector
  use dylan;
  use extensions;
  use inspector-base, export: { *show-elements* };
  use regular-expressions, import: { split };
  use substring-search, import: { substring-position };
  use tk;
  use tk-extension;
  export
    xinspect;
end module X-inspector;

define method do-component 
    (component :: <body-component>, window :: <window>) => ();
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
		      xinspect(component.related-objects[truncate/(i, 2)]);
		    end method);
    end if;
  end for;
end method do-component;

define method xinspect (obj :: <object>) => ();
  let window = make(<toplevel>);
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
      let listbox = make(<listbox>, relief: "sunken", in: frame, side: "left",
			 expand: #t, fill: "both");
      scroll(listbox, orient: "vertical", fill: "y",
	     in: frame, side: "left", relief: "sunken");
      apply(insert, listbox, 0, map(stripped-description, attrib.attrib-body));
      bind(listbox, "<Double-Button-1>",
	   method ()
	     let index = listbox.current-selection.first;
	     let component = attrib.attrib-body[index];
	     xinspect(component.related-objects.first);
	   end method);
    else
      let frame = make(<frame>, anchor: "w", side: "top", in: window);
      // padding
      make(<frame>, relief: "sunken",
	   width: 50, fill: "y", anchor: "w", side: "left", in: frame);
      let descr-frame = make(<frame>, side: "right", in: frame, 
			     expand: #t, fill: "both");
      for (component in attrib.attrib-body)
	do-component(component, descr-frame);
      end for;
    end if;
  end for;
  let button-frame = make(<frame>, side: "top", in: window);
  make(<button>, text: "Close", command: method () destroy-window(window) end,
       relief: "raised", side: "left", anchor: "w", in: button-frame);
  make(<button>, text: "Quit", command: exit,
       relief: "raised", side: "left", anchor: "w", in: button-frame);
end method xinspect;

/*
define method main (prog-name :: <byte-string>, #rest args)
  xinspect(xinspect);
//  map-window(*root-window*);
end method main;
*/
