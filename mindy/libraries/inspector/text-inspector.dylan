module:     Text-Inspector
library:    Text-Inspector
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/text-inspector.dylan,v 1.3 1996/04/07 18:36:41 nkramer Exp $

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

define library text-inspector
  use dylan;
  use inspector-base;
  use string-extensions;
  use streams;
  use print;
end library text-inspector;

define module text-inspector
  use dylan;
  use extensions;
  use standard-io, import: { *standard-input* };
  use streams, import: { read-line };
  use print;
  use character-type;
  use string-conversions;
  use substring-search;
  use inspector-base, export: { *show-elements* };
  export
    display-object-info, inspect;
end module text-inspector;

define method display-object-info (object :: <object>) => ();
  let info = object.object-info;
  for (attribute in info)
    condition-format(*debug-output*, "%s\n", attribute.attrib-header);
    for (body in attribute.attrib-body)
      condition-format(*debug-output*, "    %s\n", body.stripped-description);
    end for;
  end for;
end method display-object-info;

// Just like display-object-info except it sticks numbers into 
//
define method show-object (object :: <object>) => ();
  let count = 0;
  let info = object.object-info;
  for (attribute in info)
    condition-format(*debug-output*, "%s\n", attribute.attrib-header);
    for (body in attribute.attrib-body)
      let descr
	= if (body.related-objects.size < 1)
	    body.description;
	  else
	    let string = substring-replace(body.description, "!#", "");
	    for (subobject in body.related-objects)
	      count := count + 1;
	      string 
		:= substring-replace(string, "#!",
				     concatenate("[",
						 integer-to-string(count),
						 "] "),
				     count: 1);
	    end for;
	    string;
	  end if;
      condition-format(*debug-output*, "    %s\n", descr);
    end for;
  end for;
end method show-object;

// The only use of this (hopefully) is to be caught when
// get-nth-object can't find the object in question
//
define class <no-such-object> (<error>)
end class <no-such-object>;

define function get-nth-subobject (object :: <object>, n :: <integer>)
 => subobjet :: <object>;
  let info = object.object-info;
  block (return)
    for (attrib in info)
      for (component in attrib.attrib-body)
	for (sub-object in component.related-objects)
	  n := n - 1;
	  if (n == 0)
	    return(sub-object);
	  end if;
	end for;
      end for;
    end for;
    signal(make(<no-such-object>));
  end block;
end function get-nth-subobject;

// Routine for printing the help page.  This should (possibly in a future
// revision) be taken from a file rather than hard coded.
//
define method show-help () => ();
  condition-format
    (*debug-output*,
     "\n"
       "Inspector online help\n"
       "(all commands may be abbreviated by their first letter)\n"
       "1, 2, ...      Inspects the corresponding object\n"
       "history        Shows the inspected object stack\n"
       "up             Moves up the inspected object stack\n"
       "print          Prints the object,\n"
       "               using the standard print functions\n"
       "view           Redisplays the current object\n"
       "?, help        Displays this page\n"
       "quit, exit     Quits the object inspector\n");
end method show-help;

// Prints the "inspect>" line and reads in the input.  The input string
// is returned to inspect.  This also flushes the inspect-stream buffer.
//
define method command-prompt () => command :: <string>;
  condition-format(*debug-output*, "inspect> ");
  condition-force-output(*debug-output*);
  read-line(*standard-input*, signal-eof?: #f);
end method command-prompt;

// This is the main loop of the inspector.  This method processes commands, and
// responds appropriately. All keywords have defaults, and all of the keywords
// get passed on to object-info whenever it is called.
//
define method inspect (object :: <object>) => ();
  // Create a deque to hold the previously created objects.  object
  // contains the current object, which is *not* in the history deque.
  let history = make(<deque>);
  condition-format(*debug-output*, "\n");
  show-object(object);
  block (quit-inspector)
    while (#t)
      let command = as-lowercase!(command-prompt());
      condition-format(*debug-output*, "\n");
      select (command by \=)
	"u", "up" =>
	  if (history.empty?)
	    condition-format(*debug-output*, "This is the first object\n");
	  else
	    object := pop(history);
	    show-object(object);
	  end if;
	  
	"print", "p" =>
	  print(object, *debug-output*, pretty?: #t);
	  condition-format(*debug-output*, "\n\n");
	
	"history", "hi" => 
	  // Add the current object, so that it will print out as well
	  push(history, object);
	  condition-format(*debug-output*, "\n");
	  // Go backwards through the stack, so that the current
	  // object is on the bottom of the list.
	  for (i from history.size - 1 to 0 by -1)
	    condition-format(*debug-output*, "%s\n", 
			     short-string(history[i]));
	  end for;
	  object := pop(history);
	  condition-format(*debug-output*, "\n");

	"view", "v" => 
	  show-object(object);
	  
	"?", "help", "h", "he" => 
	  show-help();

	"quit", "q", "e", "exit" => 
	  quit-inspector();

	otherwise => 
	  if (every?(digit?, command))
	    block ()
	      let new-object = get-nth-subobject(object, 
						 string-to-integer(command));
	      push(history, object);
	      show-object(object := new-object);
	    exception (<no-such-object>)
	      condition-format(*debug-output*, 
			       "There is no %sth object to inspect\n",
			       command);
	    end block;
	  else
	    condition-format(*debug-output*,
			     "unknown command (type ? for help)\n");
	  end if;
      end select;
    end while;
  end block;
end method inspect;
