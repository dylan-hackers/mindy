module: Dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/misc.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
//  This file contains the stuff that doesn't quite fit anywhere else.
//

define method identity (x)
  x;
end;

// false-or -- Exported.
//
// False-or takes a type and returns a type that is the union of the argument
// type and the type singleton(#f).
//
define constant false-or
    = method (type :: <type>) => new-type :: <type>;
	binary-type-union(type, singleton(#f));
      end;

define constant type-union =
  method (type :: <type>, #rest more-types) => result :: <type>;
    reduce(binary-type-union, type, more-types);
  end;

define method as (c :: <class>, thing)
 => result :: <object>;
  if (instance?(thing, c))
    thing;
  else
    error("%= cannot be converted to %=", thing, c);
  end;
end;

// The built-in "as(<symbol>...)" method only handles byte-strings.
define method as(cls == <symbol>, str :: <string>)
 => sym :: <symbol>;
  as(<symbol>, as(<byte-string>, str));
end method as;


define constant \| =
  method (#rest ignore)
    error("| is syntax only and can't be used as a function.");
  end;

define constant \& =
  method (#rest ignore)
    error("& is syntax only and can't be used as a function.");
  end;

define constant \:= =
  method (#rest ignore)
    error(":= is syntax only and can't be used as a function.");
  end;

define method make 
    (c == <generic-function>,
     #key debug-name, required: req, rest?, key, all-keys?)
 => gf :: <generic-function>;
  let req = select (req by instance?)
	      <integer> =>
		if (req < 0)
		  error("required: can't be negative: %d",
			req);
		end;
	        req;
	      <sequence> =>
		do(rcurry(check-type, <type>), req);
	        size(req);
	    end;
  if (instance?(key, <collection>))
    do(rcurry(check-type, <symbol>), key);
    if (rest?)
      error("rest?: cannot be true when keywords are supplied.");
    end;
  elseif (key)
    error("bogus value for key:, must be either #f or a "
	    "collection of symbols.");
  elseif (all-keys?)
    error("all-keys?: cannot be true as long as key: is #f.");
  end;
  make-generic-function(debug-name, req, rest?, key, all-keys?,
			#(), <object>);
end;

define method find-method
    (gf :: <generic-function>, specializers :: <sequence>) 
 => found-method :: false-or(<method>);
  find-method(gf, as(<list>, specializers));
end method find-method;

define constant $not-supplied = pair(#f, #f);

define sealed generic instantiable? (type :: <type>) => answer :: <boolean>;

define method instantiable? (type :: <type>) => answer :: <boolean>;
  ~empty?(sorted-applicable-methods(make, type));
end method instantiable?;

define method instantiable? (cls :: <class>) => answer :: <boolean>;
  #f;
end method instantiable?;

define method instantiable? (cls :: <defined-class>) => answer :: <boolean>;
  ~cls.abstract? 
    | block ()
	// instantiable if there is an applicable method, and it isn't
	// the default make method.
	let methods = sorted-applicable-methods(make, cls);
	~methods.empty?
	  & methods.first ~== find-method(make, list(<defined-class>));
      end block;
end method instantiable?;
