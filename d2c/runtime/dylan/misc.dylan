rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/misc.dylan,v 1.12 1996/09/15 15:33:40 nkramer Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


// assert -- exported from Extensions.  Users of assert() should not
// have side-effects in the expression that is passed to assert(),
// because if we ever turn assertions off, that would mean the program
// runs differently in debug mode than it does in release mode.
//
define function assert (value) => ();
  unless (value)
    error("Assertion failed.");
  end;
end function assert;

// <not-supplied-marker> -- internal.
//
// The class of $not-supplied.
// 
define class <not-supplied-marker> (<object>)
end;

// $not-supplied -- exported from Extensions.
//
// a magic marker used to flag unsupplied keywords.
// 
define constant $not-supplied :: <not-supplied-marker>
    = make(<not-supplied-marker>);


// <never-returns> -- exported from Extensions.
//
// The empty type.  When used as a function result type, it means the function
// never returns.
//
define constant <never-returns> :: <type> = type-union();

define flushable generic values-sequence (sequence :: <sequence>);

define inline method values-sequence (sequence :: <sequence>)
  let vec :: <simple-object-vector> = as(<simple-object-vector>, sequence);
  values-sequence(vec);
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive(values-sequence, vector);
end;


define movable generic values (#rest values);

define inline method values (#rest values)
  %%primitive(values-sequence, values);
end;


define inline method object-address (object :: <object>)
    => res :: <raw-pointer>;
  %%primitive(object-address, object);
end method object-address;


define inline method ignore (#rest noise) => ();
end method ignore;


%%primitive(magic-internal-primitives-placeholder);
