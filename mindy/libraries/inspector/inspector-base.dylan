module:     Inspector-base
library:    Inspector-base
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/inspector-base.dylan,v 1.4 1996/04/07 22:23:46 nkramer Exp $

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
    <body-component>, description, related-objects, stripped-description,
    <object-attribute>, attrib-header, attrib-body,
    object-info, *show-elements*, short-string;
end module inspector-base;


define variable *show-elements* = #t;

// The basic data structure for describing an object is a sequence of
// <object-attribute>s.  Each attribute has a body, which is a
// sequence of body components.  Each body component has a description
// and a sequence of related objects.  Within the description, the
// magic characters #! and !# designate a substring as being
// selectable (in much the same way that <a href=..> and </a> work in
// HTML).  The object that the substring refers to is taken to be the
// corresponding element of related-objects: The first occurence of
// #!..!# is the first element of related-objects, the second
// occurence of #!..!# maps to the second element of related-objects,
// and so on.

define class <body-component> (<object>)
  constant slot description :: <string>, required-init-keyword: #"description";
  constant slot related-objects :: <sequence>, 
    required-init-keyword: #"related-objects";
end class <body-component>;

// Returns a description without #! and !#
//
define function stripped-description (component :: <body-component>)
 => stripped-descr :: <string>;
  let s1 = substring-replace(component.description, "#!", "");
  substring-replace(s1, "!#", "");
end function stripped-description;

// attrib-bodys are sequences of <body-component>s
//
define class <object-attribute> (<object>)
  constant slot attrib-header :: <string>, required-init-keyword: #"header";
  constant slot attrib-body :: <sequence>, required-init-keyword: #"body";
end class <object-attribute>;

define constant $component-none
  = make(<body-component>, description: "none", related-objects: #());

// Make a body where the description for each component is simply the
// short-string of the object.
//
define function make-simple-body (objs :: <sequence>)
 => body :: <deque>;
  let obj-body = make(<deque>);
  for (obj in objs)
    push-last(obj-body, 
	      make(<body-component>, 
		   description: concatenate("#!", short-string(obj), "!#"),
		   related-objects: list(obj)));
  end for;
  if (objs.empty?)
    push-last(obj-body, $component-none);
  end if;
  obj-body;
end function make-simple-body;

// Like make-simple-body except there's only one component
//
define function make-one-liner-body (obj :: <object>)
 => body :: <deque>;
  deque(make(<body-component>, 
	     description: concatenate("#!", short-string(obj), "!#"),
	     related-objects: list(obj)));
end function make-one-liner-body;


// short-string is basically print-to-string without some of the
// annoying text that comes with it.
//
define open generic short-string (obj :: <object>) => string :: <string>;

define method short-string (obj :: <object>) => string :: <string>;
  concatenate("Instance of ", obj.object-class.short-string);
end method short-string;

define method short-string (cls :: <class>) => string :: <string>;
  as(<string>, cls.class-name);
end method short-string;

define method short-string (u :: <union>) => string :: <string>;
  concatenate("type-union(", 
	      apply(join, ", ", map(short-string, u.union-members)), 
	      ")");
end method short-string;

define method short-string (s :: <singleton>) => string :: <string>;
  concatenate("singleton(", short-string(s.singleton-object), ")");
end method short-string;

define method short-string (s :: <subclass>) => string :: <string>;
  concatenate("subclass(", short-string(s.subclass-of), ")");
end method short-string;

define method short-string (kwd :: <symbol>) => string :: <string>;
  concatenate("#\"", as(<string>, kwd), "\"");
end method short-string;

define method short-string (fun :: <function>) => string :: <string>;
  concatenate
    (as(<string>, fun.function-name | "Anonymous function"),
     "(",
     apply(join, ", ", map(short-string, fun.function-specializers)),
     ")");
end method short-string;


// Assorted helper functions

// Convenient way to make deques, much like list(), vector(), and range()
//
define function deque (#rest objs) => d :: <deque>;
  as(<deque>, objs);
end function deque;

// #t if list is a proper list, #f if it is a circular list or if
// somewhere along the line the tail of a <pair> isn't a <list>.
//
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


// Takes an object and returns a sequence of <object-attribute>s to
// describe the object.
//
define open generic object-info (obj :: <object>) => info :: <sequence>;

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
  push-last(info, make(<object-attribute>,
		       header: "Methods:", 
		       body: make-simple-body(gf.generic-function-methods)));
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

define function function-info (fun :: <function>) => info :: <deque>;
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
		   description: concatenate("#rest #!", 
					    short-string(rest-type),
					    "!#"),
		   related-objects: list(rest-type)));
  end if;
  push-last(info, make(<object-attribute>,
		       header: "Returns:", body: returns-body));

  info;
end function function-info;

define method object-info (u :: <union>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Type union", body: #()));
  push-last(info, make(<object-attribute>, header: "Union Members:",
		       body: make-simple-body(u.union-members)));
  info;
end method object-info;

define method object-info (s :: <singleton>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Singleton", body: #()));
  push-last(info, 
	    make(<object-attribute>, header: "Singleton object:",
		 body: make-one-liner-body(s.singleton-object)));
  info;
end method object-info;

define method object-info (subclass :: <subclass>) => info :: <sequence>;
  let info = make(<deque>);
  push(info, make(<object-attribute>, header: "Subclass", body: #()));
  push-last(info, 
	    make(<object-attribute>, header: "Subclass of:",
		 body: make-one-liner-body(subclass.subclass-of)));
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
			     short-string(setter.function-name),
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
	concatenate(if (cls.abstract?) "Abstract " else "" end,
		    "Class ", as(<string>, cls.class-name));
      else
	concatenate("Anonymous ",
		    if (cls.abstract?) "Abstract " else "" end,
		    "Class");
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
