rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/misc.dylan,v 1.6 1995/12/05 22:11:03 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


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
  values-sequence(as(<simple-object-vector>, sequence));
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive values-sequence (vector);
end;


define movable generic values (#rest values);

define inline method values (#rest values)
  %%primitive values-sequence (values);
end;


%%primitive magic-internal-primitives-placeholder ();
