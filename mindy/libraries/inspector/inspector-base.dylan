module:     Inspector-base
library:    Inspector-base
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/inspector-base.dylan,v 1.1 1996/04/07 01:43:44 nkramer Exp $

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

define library inspector-base
  use dylan;
  use streams;
  use print;
  use string-extensions;
  export
    inspector-base;
end library inspector-base;

define module inspector-base
  use dylan;
  use extensions;
  use introspection;
  use streams;
  use standard-io;
  use print;
  use string-conversions;
  use character-type;
  use substring-search;
  use regular-expressions, import: {join};
  export
    <body-component>, description, related-objects, 
    <object-attribute>, attrib-header, attrib-body,
    object-info, *show-elements*;
end module inspector-base;

define variable *show-elements* = #t;

define class <body-component> (<object>)
  constant slot description :: <string>, required-init-keyword: #"description";
  constant slot related-objects :: <sequence>, 
    required-init-keyword: #"related-objects";
end class <body-component>;

// attrib-bodys are sequences of <body-component>s
//
define class <object-attribute> (<object>)
  constant slot attrib-header :: <string>, required-init-keyword: #"header";
  constant slot attrib-body :: <sequence>, required-init-keyword: #"body";
end class <object-attribute>;

// And an object can be described by a sequence of <object-attribute>s.

define constant $component-none
  = make(<body-component>, description: "none", related-objects: #());

define function make-simple-body (objs :: <sequence>)
 => body :: <sequence>;
  let obj-body = make(<deque>);
  for (obj in objs)
    push-last(obj-body, make(<body-component>, 
			      description: short-string(obj),
			      related-objects: list(obj)));
  end for;
  if (objs.empty?)
    push-last(obj-body, $component-none);
  end if;
  obj-body;
end function make-simple-body;

define function make-one-liner-body (obj :: <object>)
 => body :: <sequence>;
  deque(make(<body-component>, 
	     description: short-string(obj),
	     related-objects: list(obj)));
end function make-one-liner-body;

// short-string is basically print-to-string without some of the
// annoying text that comes with it.
//
define method short-string (obj :: <object>)
  print-to-string(obj);
end method short-string;

define method short-string (cls :: <class>)
  as(<string>, cls.class-name);
end method short-string;

define method short-string (u :: <union>)
  concatenate("type-union(", 
	      join(", ", map(short-string, u.union-members)), 
	      ")");
end method short-string;

define method short-string (s :: <singleton>)
  concatenate("singleton(", short-string(s.singleton-object), ")");
end method short-string;

define method short-string (s :: <subclass>)
  concatenate("subclass(", short-string(s.subclass-of), ")");
end method short-string;

define method short-string (kwd :: <symbol>)
  concatenate("#\"", as(<string>, kwd), "\"");
end method short-string;

define method short-string (fun :: <function>)
  as(<string>, fun.function-name | "Anonymous function");
end method short-string;

define function deque (#rest objs) => d :: <deque>;
  as(<deque>, objs);
end function deque;

define method function-info (fun :: <function>) => info :: <deque>;
  let info = make(<deque>);
  let (nargs, rest?, keywords) = fun.function-arguments;
  let args-body
    = if (nargs == 0 & ~rest?)
	deque($component-none);
      else
	make-simple-body(fun.function-specializers);
      end if;
  if (rest?)
    push-last(args-body, make(<body-component>, 
			      description: "#rest", related-objects: #[]));
  end if;

  push-last(info, make(<object-attribute>,
		       header: "Arguments:", body: args-body));

  let kwds-body 
    = if (keywords == #"all")
	deque(make(<body-component>, description: "all", 
		   related-objects: #()));
      elseif (keywords == #f)
	deque(make(<body-component>, description: "not accepted",
		   related-objects: #()));
      elseif (keywords.empty?)
	deque($component-none);
      else
	make-simple-body(keywords);
      end if;
  push-last(info, make(<object-attribute>, 
		       header: "Keywords:", body: kwds-body));

  // Because #rest has a type, the return values is slightly different
  // than thespecializers code.
  let (return-types, rest-type) = fun.function-return-values;
  let returns-body 
    = if (return-types.empty? & ~rest-type)
	deque($component-none);
      else
	make-simple-body(return-types)
      end if;
  if (rest-type)
    push-last(returns-body, 
	      make(<body-component>, 
		   description: concatenate("#rest ", short-string(rest-type)),
		   related-objects: #[]));
  end if;
  push-last(info, make(<object-attribute>,
		       header: "Returns:", body: returns-body));

  info;
end method function-info;

define method object-info (gf :: <generic-function>) => info :: <sequence>;
  let info = function-info(gf);
  let header-string
    = if (gf.function-name)
	concatenate("Generic Function ", as(<string>, gf.function-name));
      else
	concatenate("Anonymous Generic Function");
      end if;
  push(info, make(<object-attribute>, 
		  header: header-string, body: #[]));
  let methods = gf.generic-function-methods;
  let meth-body = make(<deque>);
  for (meth in methods)
    push-last(meth-body, make(<body-component>, 
			      description: short-string(meth),
			      related-objects: list(meth)));
  end for;
  push-last(info, make(<object-attribute>,
		       header: "Methods:", body: meth-body));
  info;
end method object-info;

define method object-info (fun :: <function>) => info :: <sequence>;
  let info = function-info(fun);
  let header-string
    = if (fun.function-name)
	concatenate("Function ", as(<string>, fun.function-name));
      else
	concatenate("Anonymous Function");
      end if;
  push(info, make(<object-attribute>, 
		  header: header-string, body: #[]));
  info;
end method object-info;

define method object-info (meth :: <method>) => info :: <sequence>;
  let info = function-info(meth);
  let header-string
    = if (meth.function-name)
	concatenate("Function ", as(<string>, meth.function-name));
      else
	concatenate("Anonymous Function");
      end if;
  push(info, make(<object-attribute>, 
		  header: header-string, body: #[]));
  info;
end method object-info;

define method object-info (u :: <union>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Type union", body: #()));
  let members-body = make(<deque>);
  for (type in u.union-members)
    push-last(members-body,
	      make(<body-component>, description: short-string(type),
		   related-objects: list(type)));
  end for;
  push-last(info, make(<object-attribute>, header: "Union Members:",
		       body: members-body));
  info;
end method object-info;

define method object-info (s :: <singleton>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Singleton", body: #()));
  push-last(info, 
	    make(<object-attribute>, header: "Singleton object:",
		 body: make(<body-component>, 
			    description: short-string(s.singleton-object),
			    related-objects: list(s.singleton-object))));
  info;
end method object-info;

define method object-info (subclass :: <subclass>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Subclass", body: #()));
  push-last(info, 
	    make(<object-attribute>, header: "Subclass of:",
		 body: make(<body-component>, 
			    description: short-string(subclass.subclass-of),
			    related-objects: list(subclass.subclass-of))));
  info;
end method object-info;

define constant $no-object = pair(#f, #f);
define function slot-info (cls :: <class>, #key object = $no-object) 
 => info :: <sequence>;
  let slots-body = make(<deque>);
  for (slt in cls.slot-descriptors)
    let getter = slt.slot-getter;
    let setter = slt.slot-setter;
    let type = slt.slot-type;
    let (value-string, type-and-value)
      = if (object == $no-object)
	  values("", list(type));
	else
	  values(concatenate(" = #!", short-string(object), "!#"),
		 list(type, object));
	end if;

    let (descr, related)
      = if (setter == #f & slt.slot-allocation ~== #"virtual")
	  values(concatenate("constant ", 
			     as(<string>, slt.slot-allocation), " slot #!",
			     as(<string>, getter.function-name),
			     "!# :: #!", short-string(slt.slot-type), "!#",
			     value-string),
		 pair(getter, type-and-value));
	else
	  values(concatenate(as(<string>, slt.slot-allocation), " slot #!",
			     as(<string>, getter.function-name),
			     "!# :: #!", short-string(type),
			     value-string, "!#, setter: #!", 
			     short-string(as(<string>, setter.function-name)),
			     "!#"),
		 concatenate(list(getter), type-and-value, list(setter)));
	end if;
    push-last(slots-body,
	      make(<body-component>, 
		   description: descr, related-objects: related));
  end for;
  deque(make(<object-attribute>, 
	     header: "Slots:", body: slots-body));
end function slot-info;

define method object-info (cls :: <class>) => info :: <sequence>;
  let info = slot-info(cls);
  let header-string
    = if (cls.class-name)
	concatenate("Class ", as(<string>, cls.class-name));
      else
	concatenate("Anonymous Class");
      end if;
  push(info, make(<object-attribute>, 
		  header: header-string, body: #[]));

  push-last(info, make(<object-attribute>,
		       header: "Superclasses:", 
		       body: make-simple-body(cls.direct-superclasses)));
  push-last(info, make(<object-attribute>,
		       header: "Subclasses:", 
		       body: make-simple-body(cls.direct-subclasses)));
  info;
end method object-info;

define method object-info (seq :: <sequence>) => info :: <sequence>;
  let info = slot-info(seq.object-class, object: seq);
  push(info, make(<object-attribute>, header: "Sequence of type", 
		  body: make-one-liner-body(seq.object-class)));
  if (*show-elements*)
    push-last(info, make(<object-attribute>,
			 header: "Elements:",
			 body: make-simple-body(seq)));
  end if;
  info;
end method object-info;

define function proper-list? (l :: <list>) => answer :: <boolean>;
  let cells-seen = make(<object-table>);
  block (return)
    for (ptr = l then ptr.tail, until: ptr == #())
      if (~ instance?(ptr, <list>))
	return(#f);  // dotted pair
      elseif (key-exists?(cells-seen, ptr))
	return(#f);  // circular list
      end if;
      cells-seen[ptr] := ptr;
    end for;
    #t;
  end block;
end function proper-list?;

// Pairs are strange.  Sometimes they're sequences, sometimes
// they're not.
//
define method object-info (p :: <pair>) => info :: <sequence>;
  let info = make(<deque>);
  if (*show-elements* & p.proper-list?)
    push(info, make(<object-attribute>, header: "List", body: #()));
    push-last(info, make(<object-attribute>,
			 header: "Elements:",
			 body: make-simple-body(p)));
  else
    push(info, make(<object-attribute>, header: "Pair", body: #()));
    push-last(info, make(<object-attribute>,
			 header: "Head:",
			 body: make-one-liner-body(p.head)));
    push-last(info, make(<object-attribute>,
			 header: "Tail:",
			 body: make-one-liner-body(p.tail)));
  end if;
  info;
end method object-info;

define method object-info (coll :: <explicit-key-collection>)
 => info :: <sequence>;
  let info = slot-info(coll.object-class, object: coll);
  push(info, make(<object-attribute>, 
		  header: "Explicit key collection of type", 
		  body: make-one-liner-body(coll.object-class)));
  if (*show-elements*)
    let elt-body = make(<deque>);
    for (elt keyed-by key in coll)
      let descr
	= concatenate("key: #!", short-string(key), "!#, element: #!", 
		      short-string(element), "!#");
      push-last(elt-body, make(<body-component>,
			       description: descr,
			       related-objects: list(key, elt)));
    end for;
    push-last(info, make(<object-attribute>,
			 header: "Elements:", body: elt-body));
  end if;
  info;
end method object-info;

define method object-info (obj :: <object>) => info :: <sequence>;
  let info = slot-info(obj.object-class, object: obj);
  push(info, make(<object-attribute>, header: "Object of type", 
		  body: make-one-liner-body(obj.object-class)));
  info;
end method object-info;

///////////////////////////////////////////////////
// Now we're getting into stuff that really should be in a separate
// module or even library


define method display-object-info (object :: <object>) => ();
  let info = object.object-info;
  for (attribute in info)
    condition-format(*debug-output*, "%s\n", attribute.attrib-header);
    for (body in attribute.attrib-body)
      let descr1 = substring-replace(body.description, "#!", "");
      let descr = substring-replace(descr1, "!#", "");
      condition-format(*debug-output*, "    %s\n", body.description);
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
	= if (body.related-objects.size < 2)
	    body.description;
	  else
	    let string = substring-replace(body.description, "!#", "");
	    for (subobject in body.related-objects)
	      count := count + 1;
	      string 
		:= substring-replace(string, "#!",
				     concatenate("{",
						 integer-to-string(count),
						 "}"),
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
