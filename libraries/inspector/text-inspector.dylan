module:     Text-Inspector
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

define library text-inspector
  use dylan;
  use streams;
  use standard-io;
  use print;
  use string-extensions;
  use inspector-base;
  export
    text-inspector;
end library text-inspector;

define module text-inspector
  use dylan;
  use extensions;
  use system, import: { add-debug-variable };
  use streams, import: { read-line };
  use standard-io, import: { *standard-input* };
  use print;
  use character-type;
  use string-conversions;
  use substring-search;
  use inspector-base, export: { *show-elements*, $all-libraries };
  export
    display-object-info, inspect;
end module text-inspector;


// Writes object.object-info to *debug-output* in a readable fashion.
//
define function display-object-info (object :: <object>) => ();
  let info = object.object-info;
  for (attribute in info)
    condition-format(*debug-output*, "%s\n", attribute.attrib-header);
    for (body in attribute.attrib-body)
      condition-format(*debug-output*, "    %s\n", body.stripped-description);
    end for;
  end for;
end function display-object-info;

// Just like display-object-info except it sticks numbers into the
// display, so the user knows what to type to get a certain sub-object
//
// ### A sufficiently bizarre object-info string will screw us up
// (such as if someone inspected a string containing "#!" or "!#").
// If they inspect something like that, they'll get what they deserve.
//
define function show-object (object :: <object>) => ();
  let count = 0;
  let info = object.object-info;
  for (attribute in info)
    condition-format(*debug-output*, "%s\n", attribute.attrib-header);
    for (body in attribute.attrib-body)
      let descr
	= if (body.related-objects.size < 1)
	    body.description;
	  else
	    let string = body.description;
	    for (subobject in body.related-objects)
	      count := count + 1;
	      string 
		:= substring-replace(string, "#!",
				     concatenate("[",
						 integer-to-string(count),
						 "] "),
				     count: 1);
	    end for;
	    // We do things in this order for a reason: If we did it
	    // in the opposite order and we were given the string
	    // "#!#()!#", we'd shoot ourselves in the foot.  (The
	    // current arrangement still isn't foolproof, but you see
	    // #'s at the beginning of a string a lot more often than
	    // you see them at the end)
	    substring-replace(string, "!#", "");
	  end if;
      condition-format(*debug-output*, "    %s\n", descr);
    end for;
  end for;
end function show-object;

// The only use of this (hopefully) is to be caught when
// get-nth-object can't find the object in question.  We need this
// because we need some way of passing out of bandwidth information,
// and the old unique chunk of memory trick just doesn't seem
// appropriate here.
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
define function show-help () => ();
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
       "store          Store current object in the next unused debugger\n"
       "               variable\n"
       "view           Redisplays the current object\n"
       "?, help        Displays this page\n"
       "quit, exit     Quits the object inspector\n");
end function show-help;

// Prints the "inspect>" line and reads in the input.  The input string
// is returned to inspect.  This also flushes the inspect-stream buffer.
//
define function command-prompt () => command :: <string>;
  condition-format(*debug-output*, "inspect> ");
  condition-force-output(*debug-output*);
  let str = read-line(*standard-input*, on-end-of-stream: #"eos");
  if (str == #"eos") #f else str end;
end function command-prompt;

// This is the main loop of the inspector.  This method processes commands, and
// responds appropriately. All keywords have defaults, and all of the keywords
// get passed on to object-info whenever it is called.
//
define function inspect-one-obj (object :: <object>) => ();
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
	  // One would think that "print(object, *debug-output*,
	  // pretty?: #t)" would do the job, but one would be wrong.
	  // If *debug-output* == #"Cheap-IO", we'd be screwed.
	  let printed-obj = print-to-string(object, pretty?: #t);
	  condition-format(*debug-output*, "%s\n\n", printed-obj);
	  
	"store", "s" =>
	  add-debug-variable(object);

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
end function inspect-one-obj;

define function inspect (#rest objs) => ();
  if (objs.empty?)
    inspect-one-obj($all-libraries);
  elseif (objs.size > 1)
    error("Can only inspect one object at a time");
  else
    inspect-one-obj(objs.first);
  end if;
end function inspect;

*inspect-function* := inspect;
