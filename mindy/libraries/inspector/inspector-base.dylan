module:     Inspector-base
library:    Inspector-base
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /scm/cvs/src/mindy/libraries/inspector/inspector-base.dylan,v 1.1 1998/05/03 19:55:23 andreas Exp $

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
  use print;
  use string-extensions;
  use regular-expressions;
  export
    inspector-base;
end library inspector-base;

define module inspector-base
  use dylan;
  use extensions;
  use introspection;
  use namespace-introspection;
  use print, import: { print-to-string };
  use substring-search;
  use string-conversions;
  use regular-expressions, import: {join};
  export
    <body-component>, description, related-objects, stripped-description,
    <object-attribute>, attrib-header, attrib-body,
    object-info, *show-elements*, short-string,
    $all-libraries;
end module inspector-base;


define variable *show-elements* :: <boolean> = #t;

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
  = make(<body-component>, description: "None", related-objects: #());

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


// short-string is like print-object, except it prints out types and
// functions in a way that doesn't make me want to vomit.
// short-string also returns a string rather than print to a stream.
// We'd like to be able to use print-object for all other objects, but
// we're afraid of being spammed.  Actually, any print-object's that
// forces a line-break is bad, because that'll screw the X-inspector.
// (It'll also look pretty crappy in the text inspector if we don't
// start using pretty-printing)
//
define open generic short-string (obj :: <object>) => string :: <string>;

define method short-string (obj :: <object>) => string :: <string>;
  concatenate("{Instance of ", obj.object-class.short-string, "}");
end method short-string;

define method short-string
    (obj :: type-union(<boolean>, <empty-list>, <string>, <character>,
		       <number>, <symbol>))
 => string :: <string>;
  print-to-string(obj);
end method short-string;

define method short-string (cls :: <class>) => string :: <string>;
  as(<string>, cls.class-name);
end method short-string;

// ### The "false-or" hack is highly unportable.
// ### Perhaps we should also have a "one-of" hack...
//
define method short-string (u :: <union>) => string :: <string>;
  if (u.union-members.size == 2 & u.union-members.second == <false>)
    concatenate("false-or(", short-string(u.union-members.first), ")");
  else
    concatenate("type-union(", 
		apply(join, ", ", map(short-string, u.union-members)), 
		")");
  end if;
end method short-string;

define method short-string (s :: <singleton>) => string :: <string>;
  concatenate("singleton(", short-string(s.singleton-object), ")");
end method short-string;

define method short-string (s :: <subclass>) => string :: <string>;
  concatenate("subclass(", short-string(s.subclass-of), ")");
end method short-string;

define method short-string (fun :: <function>) => string :: <string>;
  concatenate
    (as(<string>, fun.function-name | "Anonymous function"),
     "(",
     apply(join, ", ", map(short-string, fun.function-specializers)),
     ")");
end method short-string;

define method short-string (library :: <library>) => string :: <string>;
  concatenate("Library ", as(<string>, library.library-name));
end method short-string;

define method short-string (module :: <module>) => string :: <string>;
  concatenate("Module ", as(<string>, module.module-name));
end method short-string;

define method short-string (binding :: <binding>) => string :: <string>;
  if (binding.constant-and-untyped?)
    short-string(binding.binding-value);
  else
    concatenate(capitalize!(copy-sequence(as(<string>, 
					     binding.binding-kind))),
		" Binding ", as(<string>, binding.binding-name));
  end if;
end method short-string;


// Assorted helper functions

define function capitalize! (s :: <string>) => s :: <string>;
  s.first := s.first.as-uppercase;
  s;
end function capitalize!;

define function constant-and-untyped? (binding :: <binding>)
 => answer :: <boolean>;
  select (binding.binding-kind)
    #"variable" => #f;
    #"undefined" => #f;
    #"constant" => binding.binding-type == #f;
    otherwise => #t;
  end select;
end function constant-and-untyped?;

// Convenient way to make deques, much like list(), vector(), and range()
//
define function deque (#rest objs) => d :: <deque>;
  as(<deque>, objs);
end function deque;

// #t if list is a proper list, #f if it is a circular list or if
// somewhere along the line the tail of a <pair> isn't a <list>.
//
define function proper-list? (l :: <list>) => answer :: <boolean>;
  local method repeat (slow :: <list>, fast)
	  if (fast == #())
	    // End of the list.
	    #t;
	  elseif (slow == fast)
	    // Circular.
	    #f;
	  elseif (~instance?(fast, <pair>))
	    // Improper.
	    #f;
	  else
	    let fast-tail = fast.tail;
	    if (fast-tail == #())
	      // End of the list.
	      #t;
	    elseif (~instance?(fast-tail, <pair>))
	      // Improper.
	      #f;
	    else
	      // More to go, advance them both.  Note: we don't have to
	      // type-check slow.tail because fast has already progressed
	      // though it.
	      repeat(slow.tail, fast-tail.tail);
	    end if;
	  end if;
	end method repeat;
  repeat(l, l.tail);
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

// ### This might be a good time to use pretty-printing...
//
define function instance-slot-info (object :: <object>)
 => info :: <sequence>;
  let slots-body = make(<deque>);
  let cls = object.object-class;
  for (slt in cls.slot-descriptors)
    let getter-name = slt.slot-getter.function-name;
    let type = slt.slot-type;
    let (value, bound?) = slot-value(slt, object);
    if (~bound?)
      let descr = concatenate("slot ", as(<string>, getter-name),
			      " is unbound");
      push-last(slots-body, make(<body-component>, 
				 description: descr, 
				 related-objects: #()));
    else
      let descr = concatenate("slot ", as(<string>, getter-name),
			      " = #!", short-string(value), "!#");
      push-last(slots-body, make(<body-component>, 
				 description: descr, 
				 related-objects: vector(value)));
    end if;
  end for;
  deque(make(<object-attribute>, header: "Slots:", 
	     body: if (slots-body.empty?)
		     vector($component-none);
		   else
		     slots-body;
		   end));
end function instance-slot-info;

define function class-slot-info (cls :: <class>) => info :: <sequence>;
  let slots-body = make(<deque>);
  for (slt in cls.slot-descriptors)
    let getter = slt.slot-getter;
    let setter = slt.slot-setter;
    let type = slt.slot-type;
    let allocation-string = if (slt.slot-allocation == #"instance")
			      "";
			    else
			      as(<string>, slt.slot-allocation);
			    end if;
    let (descr, related)
      = if (setter == #f & slt.slot-allocation ~== #"virtual")
	  values(concatenate("constant ", allocation-string, " slot #!",
			     as(<string>, getter.function-name),
			     "!# :: #!", short-string(slt.slot-type), "!#"),
		 vector(getter, type));
	else
	  values(concatenate(allocation-string, " slot #!",
			     as(<string>, getter.function-name),
			     "!# :: #!", short-string(type), 
			     "!#, setter: #!", 
			     as(<string>, setter.function-name),
			     "!#"),
		 vector(getter, type, setter));
	end if;
    push-last(slots-body, make(<body-component>, 
			       description: descr, related-objects: related));
  end for;
  deque(make(<object-attribute>, 
	     header: "Slots:", body: slots-body));
end function class-slot-info;

define method object-info (cls :: <class>) => info :: <sequence>;
  let info = class-slot-info(cls);
  let class-adjectives
    = if (cls.abstract?)
	if (cls.instantiable?) "Abstract Instantiable " else "Abstract " end;
      else
	"";
      end if;
  let header-string
    = if (cls.class-name)
	concatenate(class-adjectives, "Class ", as(<string>, cls.class-name));
      else
	concatenate("Anonymous ", class-adjectives, "Class");
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
  let info = instance-slot-info(seq);
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
  let info = instance-slot-info(coll);
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

// relobj is a "related object"
//
define function map-into-body-components
    (descr-function :: <function>, relobj-function :: <function>, 
     objects :: <sequence>)
 => body-components :: <sequence>;
  map(method (obj)
	make(<body-component>, description: obj.descr-function, 
	     related-objects: list(obj.relobj-function));
      end method,
      objects);
end function map-into-body-components;

define function owned-names (namespace :: <namespace>) 
 => buncha-symbols :: <sequence>;
  remove(namespace.visible-names, #f,
	 test: method (symbol, ignored)
		 namespace ~== name-owner(resolve-name(symbol, namespace));
	       end method);
end function owned-names;

define function symbol-less-than (sym1 :: <symbol>, sym2 :: <symbol>) 
 => answer :: <boolean>;
  as(<string>, sym1).as-uppercase < as(<string>, sym2).as-uppercase;
end function symbol-less-than;

define function namespace-info (namespace :: <namespace>)
 => info :: <deque>;
  let make-name = method (symbol)
		    concatenate("#!", as(<string>, symbol), "!#");
		  end method;
  let make-relobj = method (symbol)
		      resolve-name(symbol, namespace)
		    end method;
  let exported-components
    = map-into-body-components(make-name, make-relobj,
			       sort(namespace.exported-names,
				    test: symbol-less-than));
  let owned-components
    = map-into-body-components(make-name, make-relobj,
			       sort(namespace.owned-names,
				    test: symbol-less-than));
  deque(make(<object-attribute>, 
	     header: "Exported names",
	     body: exported-components),
	make(<object-attribute>,
	     header: "Owned names",
	     body: owned-components));
end function namespace-info;

define function add-info-for-names! (name :: <name>, info :: <deque>) => ();
  push(info, make(<object-attribute>,
		  header: "Owner:", 
		  body: make-one-liner-body(name.name-owner)));
end function add-info-for-names!;

define method object-info (module :: <module>)
 => info :: <sequence>;
  let info = namespace-info(module);
  add-info-for-names!(module, info);
  push(info, make(<object-attribute>, header: short-string(module),
		  body: #()));
  info;
end method object-info;

define method object-info (library :: <library>)
 => info :: <sequence>;
  let info = namespace-info(library);
  push(info, make(<object-attribute>, header: short-string(library),
		  body: #()));
  push-last(info, make(<object-attribute>,
		       header: "Other libraries",
		       body: make-one-liner-body($all-libraries)));
  info;
end method object-info;

define method object-info (binding :: <binding>)
 => info :: <sequence>;
  if (binding.constant-and-untyped?)
    object-info(binding.binding-value);
  else
    let info = make(<deque>);
    add-info-for-names!(binding, info);
    if (binding.binding-kind ~== #"undefined")
      push(info, make(<object-attribute>,
		      header: "Currently bound to:",
		      body: make-one-liner-body(binding.binding-value)));
    end if;
    if (binding.binding-type ~== #f)
      push(info, make(<object-attribute>,
		      header: "Type constraint:",
		      body: make-one-liner-body(binding.binding-type)));
    end if;
    push(info, make(<object-attribute>, header: short-string(binding),
		    body: #()));
    info;
  end if;
end method object-info;

// Info for objects which are summed up by their short-string method.
//
define function literal-info (obj :: <object>) => info :: <sequence>;
  deque(make(<object-attribute>, header: "Object of type", 
	     body: make-one-liner-body(obj.object-class)),
	make(<object-attribute>, header: "Value:",
	     body: deque(make(<body-component>, description: short-string(obj),
			      related-objects: #()))));
end function literal-info;

define method object-info (obj :: <complex>) => info :: <sequence>;
  literal-info(obj);
end method object-info;

define method object-info (obj :: <string>) => info :: <sequence>;
  literal-info(obj);
end method object-info;

define method object-info (obj :: <symbol>) => info :: <sequence>;
  literal-info(obj);
end method object-info;

define method object-info (obj :: <boolean>) => info :: <sequence>;
  literal-info(obj);
end method object-info;

define method object-info (obj :: <object>) => info :: <sequence>;
  let info = instance-slot-info(obj);
  push(info, make(<object-attribute>, header: "Object of type", 
		  body: make-one-liner-body(obj.object-class)));
  info;
end method object-info;

// I'm not sure whether I like name-owner or name-home better, so
// we'll define both..
//
define function name-owner (name)
  name.name-home;
end function name-owner;

// Hacks for letting us see all the libraries in a nice format

// All instances of this class are equivalent, because the only
// interesting part about them is their class, which is used for
// dispatching.  Nevertheless, keep $all-libraries as the only
// instance of this class, please.
//
define class <loaded-libraries> (<object>)
end class <loaded-libraries>;

define constant $all-libraries :: <loaded-libraries>
  = make(<loaded-libraries>);

define method short-string (loaded-libs :: <loaded-libraries>)
 => string :: <string>;
  "All loaded libraries";
end method short-string;

define method object-info (loaded-libs :: <loaded-libraries>)
 => info :: <sequence>;
  deque(make(<object-attribute>,
	     header: "All libraries currently loaded:",
	     body: make-simple-body(get-all-libraries())));
end method object-info;
